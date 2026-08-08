## Deterministic JSON encoding, per docs/rctl-json-contract.md in
## cornball-ai/runix. Every rule here is fixture-tested in
## inst/tinytest/test_encode.R.

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
## with a diagnostic on stderr.
fix_utf8 <- function(x) {
    bad <- !is.na(x) & !validUTF8(x)
    if (any(bad)) {
        message("rctl: replaced invalid UTF-8 in ", sum(bad), " value(s)")
        x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "�")
    }
    x
}

## jsonlite renders whole doubles >= 1e15 in scientific notation with
## precision loss. Scalars that large are emitted verbatim; inside a data
## frame column there is no safe path, so refuse rather than corrupt.
BIG <- 1e15

prepare <- function(x, in_frame = FALSE) {
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
                return(structure(sprintf("%.0f", x), class = "json"))
            }
            return(lapply(x, function(v) {
                if (!is.na(v) && abs(v) >= BIG) {
                    structure(sprintf("%.0f", v), class = "json")
                } else {
                    v
                }
            }))
        }
    }
    x
}

envelope <- function(operation, ok, key, value) {
    body <- list(schema_version = jsonlite::unbox(ENVELOPE_SCHEMA_VERSION),
                 operation = jsonlite::unbox(operation),
                 ok = jsonlite::unbox(ok))
    body[[key]] <- value
    paste0(jsonlite::toJSON(body, auto_unbox = TRUE, na = "null",
                            dataframe = "rows", digits = NA, null = "null",
                            json_verbatim = TRUE), "\n")
}

envelope_success <- function(operation, result) {
    envelope(operation, TRUE, "result", prepare(result))
}

envelope_error <- function(operation, cond) {
    resource <- if (is.null(cond$resource)) {
        jsonlite::unbox(NA_character_)
    } else {
        jsonlite::unbox(as.character(cond$resource)[1L])
    }
    envelope(operation, FALSE, "error", list(
            class = I(setdiff(class(cond), c("error", "condition"))),
            message = jsonlite::unbox(fix_utf8(conditionMessage(cond))),
            retryable = jsonlite::unbox(is_retryable(cond)),
            resource = resource
        ))
}

## The retryability table: agents branch on class, never message. Classes
## absent from this table are not retryable.
RETRYABLE_CLASSES <- c("rdpkg_cache_race")

is_retryable <- function(cond) {
    any(class(cond) %in% RETRYABLE_CLASSES)
}
