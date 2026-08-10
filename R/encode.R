## Deterministic JSON encoding, per docs/rctl-json-contract.md in
## cornball-ai/runix. Backend: janssonr (the R Jansson binding, MIT), matching
## the rest of the Runix stack. janssonr writes whole doubles losslessly as bare
## integer literals, so big whole numbers survive without the precision loss
## jsonlite's formatter introduces at 1e15 -- and without any verbatim escape
## hatch. Its to_json is strict: it refuses NA, named atomic vectors, AsIs,
## factors, POSIXct, and data frames. So prepare() first normalizes every value
## into a janssonr-safe shape -- objects are named lists, arrays are unnamed
## lists, scalars are length-1 unclassed atomics, and a JSON null is NULL --
## before encoding. Every rule here is fixture-tested in test_encode.R,
## including golden envelope bytes.

ENVELOPE_SCHEMA_VERSION <- 1L

## RFC 3339 UTC; microsecond precision only when the source carries it.
rfc3339 <- function(x) {
    frac <- any(!is.na(x) & as.numeric(x) %% 1 != 0)
    if (frac) {
        fmt <- "%Y-%m-%dT%H:%M:%OS6Z"
    } else {
        fmt <- "%Y-%m-%dT%H:%M:%SZ"
    }
    out <- format(x, fmt, tz = "UTC")
    out[is.na(x)] <- NA_character_
    out
}

## Strings must always be valid UTF-8; invalid sequences become U+FFFD with a
## diagnostic on stderr. Jansson (like yyjson) refuses invalid UTF-8 outright,
## so this pre-replacement is what keeps machine mode emitting instead of dying
## -- the library is the backstop, not the policy.
fix_utf8 <- function(x) {
    ## audit finding: the encoder assumes UTF-8 and never transcodes, so
    ## declared-latin1 strings must be converted before they reach it
    x <- enc2utf8(x)
    bad <- !is.na(x) & !validUTF8(x)
    if (any(bad)) {
        message("rctl: replaced invalid UTF-8 in ", sum(bad), " value(s)")
        x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "�")
    }
    x
}

## Whole doubles >= 1e15 are emitted bare and exact -- janssonr writes them
## losslessly, so no verbatim token is needed. Beyond 2^53, or non-integral,
## a double cannot round-trip exactly; refuse rather than corrupt. Inside a data
## frame column such a big value is always a refusal.
BIG <- 1e15

## Normalize one atomic vector to a janssonr-safe value: NA -> NULL, a named
## atomic -> a JSON object (named list), a clean length-1 -> a scalar, and a
## clean multi-element or empty vector -> left as-is (janssonr arrays it, or
## emits []). Class-bearing inputs (factor/POSIXct/AsIs) and UTF-8 are already
## resolved by prepare() before this is reached.
.atomic_json <- function(x) {
    nm <- names(x)
    if (!is.null(nm)) {
        out <- lapply(seq_along(x),
                      function(i) if (is.na(x[[i]])) NULL else x[[i]])
        names(out) <- nm
        return(out)
    }
    if (length(x) == 1L) {
        if (is.na(x)) {
            return(NULL)
        }
        return(x[[1L]])
    }
    if (anyNA(x)) {
        return(lapply(x, function(v) if (is.na(v)) NULL else v))
    }
    x
}

## A data frame becomes a JSON array of row objects. Each cell is prepared with
## in_frame = TRUE (a column has no scalar escape hatch, so an out-of-range big
## number is refused there). Zero rows -> empty array; a list column or a nested
## data frame cell is prepared recursively.
.df_rows <- function(df) {
    n <- nrow(df)
    if (n == 0L) {
        return(list())
    }
    nm <- names(df)
    cols <- lapply(df, function(col) {
        lapply(seq_len(n), function(i) prepare(col[[i]], in_frame = TRUE))
    })
    lapply(seq_len(n), function(i) {
        row <- lapply(cols, function(pc) pc[[i]])
        names(row) <- nm
        row
    })
}

## Normalize an arbitrary R value into a janssonr-safe structure. Recursive:
## classes are resolved to their JSON meaning, NA becomes NULL, and containers
## become named/unnamed lists.
prepare <- function(x, in_frame = FALSE) {
    ## never trust an incoming verbatim class (none is produced any more)
    if (inherits(x, "json")) {
        x <- unclass(x)
    }
    ## AsIs (the error envelope's class vector): force a JSON array, never an
    ## unboxed scalar. Strip AsIs and recurse element-wise as a list.
    if (inherits(x, "AsIs")) {
        x <- unclass(x)
        if (!is.list(x)) {
            x <- as.list(x)
        }
        return(lapply(x, prepare, in_frame = in_frame))
    }
    if (inherits(x, "POSIXct")) {
        return(prepare(rfc3339(x), in_frame = in_frame))
    }
    if (is.factor(x)) {
        return(prepare(as.character(x), in_frame = in_frame))
    }
    if (is.data.frame(x)) {
        return(.df_rows(x))
    }
    if (is.list(x)) {
        return(lapply(x, prepare, in_frame = in_frame))
    }
    if (is.double(x)) {
        big <- !is.na(x) & abs(x) >= BIG
        if (any(big)) {
            if (in_frame) {
                stop_rctl("value too large for deterministic encoding ",
                          "inside a data frame column (>= 1e15)")
            }
            if (any(x[big] != trunc(x[big])) || any(abs(x[big]) >= 2 ^ 53)) {
                stop_rctl("value too large for deterministic encoding ",
                          "(non-integral or >= 2^53)")
            }
            ## legal big whole double(s): janssonr emits them losslessly
        }
    }
    if (is.character(x)) {
        x <- fix_utf8(x)
    }
    .atomic_json(x)
}

## Build one envelope line. The whole body is normalized once, then encoded with
## janssonr::to_json (compact, insertion-order, trailing newline added here).
envelope <- function(operation, ok, key, value) {
    body <- list(schema_version = ENVELOPE_SCHEMA_VERSION,
                 operation = operation, ok = ok)
    body[[key]] <- value
    paste0(janssonr::to_json(prepare(body)), "\n")
}

envelope_success <- function(operation, result) {
    envelope(operation, TRUE, "result", result)
}

## Mutation error fields carried through to the envelope when present on the
## condition, so a machine-readable failure is as truthful about post-state
## as a success result (Phase 2 contract). The audit fields (correlation_id,
## audit_scope, audit_persisted) let a failed mutation be correlated with its
## durable audit records, exactly as a success result is (durable-audit).
ERROR_PASSTHROUGH <- c("observed", "observed_failed", "observed_reason",
                       "elapsed", "polkit_action", "correlation_id",
                       "audit_scope", "audit_persisted")

envelope_error <- function(operation, cond) {
    resource <- if (is.null(cond$resource)) {
        NA_character_
    } else {
        as.character(cond$resource)[1L]
    }
    ## I() keeps a single-element class vector a JSON array; prepare() turns
    ## every field (incl. NA resource -> null, invalid UTF-8 in the message)
    ## into janssonr-safe form when the whole body is encoded.
    body <- list(class = I(setdiff(class(cond), c("error", "condition"))),
                 message = conditionMessage(cond),
                 retryable = is_retryable(cond), resource = resource)
    for (f in ERROR_PASSTHROUGH) {
        if (!is.null(cond[[f]])) {
            body[[f]] <- cond[[f]]
        }
    }
    envelope(operation, FALSE, "error", body)
}

## Retryability comes from the shared runix registry, not a hardcoded table:
## each subsystem declares its retryable classes in .onLoad (pkgstate registers
## pkgstate_cache_race), and rctl classifies via runix::is_retryable(). By the
## time a subsystem condition reaches the envelope its package is loaded --
## dispatch required it -- so its classes are already registered. Agents branch
## on the class vector, never the message.
is_retryable <- function(cond) {
    runix::is_retryable(cond)
}
