# Strict encoder policy (janssonr backend). The envelope encoder maps a fixed
# set of JSON shapes and fails closed on non-idiomatic inputs rather than
# silently reinterpreting them. Producer (rctl) and consumers are both janssonr,
# so the wire shape stays tight. Pinned per the rctl --json contract.

enc <- function(x) rctl:::envelope_success("t", x)
dec <- function(s) janssonr::from_json(s)
refused <- function(expr) {
    expect_inherits(tryCatch(expr, error = identity), "rctl_error")
}

# --- Refused: ambiguous / non-idiomatic inputs -------------------------------

# a named atomic vector (an object is a named list, not a named vector),
# including a single element and one carrying NA
refused(enc(c(a = 1L, b = 2L)))
refused(enc(c(a = 1L)))
refused(enc(c(a = NA)))

# duplicate or empty object keys
refused(enc(structure(list(1L, 2L), names = c("a", "a"))))
refused(enc(structure(list(1L), names = "")))

# factors (a classed atomic; callers pass character)
refused(enc(list(f = factor("x"))))

# AsIs vectors, length one and many (rctl forces arrays with list(), not I())
refused(enc(list(c = I("x"))))
refused(enc(list(c = I(c("x", "y")))))

# POSIXlt (a list under the hood); POSIXct is the accepted timestamp type
refused(enc(list(t = as.POSIXlt("2026-07-29 12:00:00", tz = "UTC"))))

# an unknown classed atomic, refused rather than reinterpreted as its storage
refused(enc(list(x = structure(5, class = "myclass"))))

# non-finite numbers
refused(enc(list(a = NaN)))
refused(enc(list(a = Inf)))
refused(enc(list(a = -Inf)))

# the 2^53 boundary: below is exact, at and above is refused
expect_true(grepl('"a":9007199254740991', enc(list(a = 2^53 - 1)), fixed = TRUE))
refused(enc(list(a = 2^53)))

# a big value inside a data frame column (no per-cell scalar escape hatch)
refused(enc(data.frame(x = c(1, 1786145400000000))))

# --- Accepted: legitimate rctl shapes ----------------------------------------

# a classed result list (runix_result and friends) serializes as its content
rr <- structure(list(ok = TRUE, n = 3L),
    class = c("systemd_result", "runix_result"))
d <- dec(enc(rr))
expect_true(d$result$ok)
expect_equal(d$result$n, 3L)

# NA of every type, and an NA timestamp, become null
d <- dec(enc(list(a = NA, b = NA_integer_, c = NA_real_, d = NA_character_,
    when = as.POSIXct(NA_character_, tz = "UTC"))))
expect_null(d$result$a)
expect_null(d$result$b)
expect_null(d$result$c)
expect_null(d$result$d)
expect_null(d$result$when)

# signed zero is emitted faithfully and deterministically
expect_true(grepl('"z":-0.0', enc(list(z = -0.0)), fixed = TRUE))

# zero-row data frame -> empty array; zero-column -> objects with no keys
expect_true(grepl('"result":[]',
    enc(data.frame(a = character(), b = integer())), fixed = TRUE))
expect_true(grepl('"result":[{},{}]',
    enc(as.data.frame(matrix(nrow = 2, ncol = 0))), fixed = TRUE))

# list / nested-data-frame columns serialize recursively
df <- data.frame(a = 1:2)
df$b <- list(list(x = 1L), list(x = 2L))
d <- dec(enc(df))
expect_equal(d$result[[1L]]$b$x, 1L)
expect_equal(d$result[[2L]]$b$x, 2L)

# --- Golden envelope bytes: exact wire format, representative operations ------

expect_identical(
    rctl:::envelope_success("services.units",
        data.frame(unit = c("a.service", "b.service"),
            active = c(TRUE, NA), stringsAsFactors = FALSE)),
    paste0('{"schema_version":1,"operation":"services.units","ok":true,',
        '"result":[{"unit":"a.service","active":true},',
        '{"unit":"b.service","active":null}]}\n'))

expect_identical(
    rctl:::envelope_success("t",
        list(usec = 1786145400000000, m = 22671360, f = 0.5, n = 42L)),
    paste0('{"schema_version":1,"operation":"t","ok":true,',
        '"result":{"usec":1786145400000000,"m":22671360,"f":0.5,"n":42}}\n'))

cond <- tryCatch(rctl:::stop_rctl("boom", class = "rctl_usage_error"),
    error = identity)
expect_identical(
    rctl:::envelope_error("packages.origins", cond),
    paste0('{"schema_version":1,"operation":"packages.origins","ok":false,',
        '"error":{"class":["rctl_usage_error","rctl_error","runix_error"],',
        '"message":"boom","retryable":false,"resource":null}}\n'))

mut <- structure(class = c("runix_operation_failed", "rsystemd_error",
    "runix_error", "error", "condition"),
    list(message = "failed", resource = "x.service",
        observed = list(active_state = "failed"), correlation_id = "cid-42",
        audit_scope = "caller", audit_persisted = TRUE))
expect_identical(
    rctl:::envelope_error("services.restart", mut),
    paste0('{"schema_version":1,"operation":"services.restart","ok":false,',
        '"error":{"class":["runix_operation_failed","rsystemd_error",',
        '"runix_error"],"message":"failed","retryable":false,',
        '"resource":"x.service","observed":{"active_state":"failed"},',
        '"correlation_id":"cid-42","audit_scope":"caller",',
        '"audit_persisted":true}}\n'))
