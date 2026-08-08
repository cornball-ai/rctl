## Deterministic JSON encoding, per docs/rctl-json-contract.md in
## cornball-ai/runix. Backend: yyjsonr (C yyjson, zero R deps, MIT) —
## writes doubles losslessly, so big whole numbers survive without the
## precision loss jsonlite's formatter introduces at 1e15. Every rule here
## is fixture-tested in inst/tinytest/test_encode.R, including golden
## envelope bytes.

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

## Strings must always be valid UTF-8; invalid sequences become U+FFFD
## with a diagnostic on stderr. yyjson refuses invalid UTF-8 outright, so
## this pre-replacement is what keeps machine mode emitting instead of
## dying — the library is the backstop, not the policy.
fix_utf8 <- function(x) {
    ## audit finding: yyjsonr's glue assumes UTF-8 and never transcodes,
    ## so declared-latin1 strings must be converted before they reach it
    x <- enc2utf8(x)
    bad <- !is.na(x) & !validUTF8(x)
    if (any(bad)) {
        message("rctl: replaced invalid UTF-8 in ", sum(bad), " value(s)")
        x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "�")
    }
    x
}

## The ONLY producer of json-verbatim values: an internally generated,
## validated integer token. Arbitrary strings never pass through
## json_verbatim — prepare() strips any json class arriving on input.
num_token <- function(v) {
    s <- sprintf("%.0f", v)
    if (!grepl("^-?[0-9]+$", s)) {
        stop_rctl("internal: numeric token failed validation: ", s)
    }
    structure(s, class = "json")
}

## Whole doubles >= 1e15 are emitted as verbatim integer tokens (bare,
## exact); inside a data frame column there is no verbatim path, so
## refuse rather than corrupt.
BIG <- 1e15

prepare <- function(x, in_frame = FALSE) {
    if (inherits(x, "json")) {
        ## never trust incoming verbatim values
        x <- unclass(x)
    }
    if (inherits(x, "POSIXct")) {
        return(rfc3339(x))
    }
    if (is.data.frame(x)) {
        x[] <- lapply(x, prepare, in_frame = TRUE)
        return(x)
    }
    if (is.list(x)) {
        return(lapply(x, prepare))
    }
    if (is.character(x)) {
        return(fix_utf8(x))
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
            if (length(x) == 1L) {
                return(num_token(x))
            }
            return(lapply(x, function(v) {
                if (!is.na(v) && abs(v) >= BIG) {
                    num_token(v)
                } else {
                    v
                }
            }))
        }
    }
    x
}

write_opts <- function() {
    yyjsonr::opts_write_json(dataframe = "rows", auto_unbox = TRUE,
                             json_verbatim = TRUE, str_specials = "null",
                             num_specials = "null")
}

envelope <- function(operation, ok, key, value) {
    body <- list(schema_version = ENVELOPE_SCHEMA_VERSION,
                 operation = operation, ok = ok)
    body[[key]] <- value
    paste0(yyjsonr::write_json_str(body, opts = write_opts()), "\n")
}

envelope_success <- function(operation, result) {
    envelope(operation, TRUE, "result", prepare(result))
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
    body <- list(class = I(setdiff(class(cond), c("error", "condition"))),
                 message = fix_utf8(conditionMessage(cond)),
                 retryable = is_retryable(cond), resource = resource)
    for (f in ERROR_PASSTHROUGH) {
        if (!is.null(cond[[f]])) {
            body[[f]] <- prepare(cond[[f]])
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
