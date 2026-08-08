# Encoder fixtures, per the rctl --json contract. Each rule the contract
# pins is asserted here against the emitted JSON text.

enc <- function(x) rctl:::envelope_success("t", x)
dec <- function(s) jsonlite::fromJSON(s, simplifyVector = FALSE)

# --- Envelope shape: single compact document, trailing newline ---

s <- enc(list(a = 1L))
expect_true(endsWith(s, "\n"))
expect_false(grepl("\n", sub("\n$", "", s), fixed = TRUE))
d <- dec(s)
expect_equal(d$schema_version, 1L)
expect_equal(d$operation, "t")
expect_true(d$ok)

# --- NA of every type becomes null, never the string "NA" ---

s <- enc(list(a = NA, b = NA_integer_, c = NA_real_, d = NA_character_))
expect_true(grepl('"a":null', s, fixed = TRUE))
expect_true(grepl('"b":null', s, fixed = TRUE))
expect_true(grepl('"c":null', s, fixed = TRUE))
expect_true(grepl('"d":null', s, fixed = TRUE))
expect_false(grepl('"NA"', s, fixed = TRUE))

# NA inside data frame cells too
df <- data.frame(x = c("a", NA), y = c(1L, NA), stringsAsFactors = FALSE)
s <- enc(df)
expect_true(grepl('"x":null', s, fixed = TRUE))
expect_true(grepl('"y":null', s, fixed = TRUE))

# --- Timestamps: RFC 3339 UTC, microseconds only when carried ---

t0 <- as.POSIXct("2026-07-29 19:31:16", tz = "UTC")
s <- enc(list(when = t0))
expect_true(grepl('"when":"2026-07-29T19:31:16Z"', s, fixed = TRUE))

tf <- as.POSIXct(1754600000.123456, origin = "1970-01-01", tz = "UTC")
s <- enc(list(when = tf))
expect_true(grepl("T", s, fixed = TRUE))
expect_true(grepl("\\.[0-9]{6}Z", s))

# NA timestamps are null; non-UTC input still renders as UTC
s <- enc(list(when = as.POSIXct(NA_character_, tz = "UTC")))
expect_true(grepl('"when":null', s, fixed = TRUE))
tz <- as.POSIXct("2026-07-29 14:31:16", tz = "America/Chicago")
s <- enc(list(when = tz))
expect_true(grepl('"when":"2026-07-29T19:31:16Z"', s, fixed = TRUE))

# --- Data frames become arrays of row objects ---

df <- data.frame(unit = c("a.service", "b.service"),
    active = c(TRUE, FALSE), stringsAsFactors = FALSE)
d <- dec(enc(df))
expect_equal(length(d$result), 2L)
expect_equal(d$result[[1L]]$unit, "a.service")
expect_true(d$result[[1L]]$active)
expect_false(d$result[[2L]]$active)

# --- UTF-8: invalid bytes replaced with U+FFFD, diagnostic to stderr ---

bad <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
expect_message(s <- enc(list(msg = bad)), pattern = "invalid UTF-8")
expect_true(grepl("a�b", s, fixed = TRUE))
expect_true(validUTF8(s))

# --- Large numbers: no scientific notation, no precision loss ---

s <- enc(list(usec = 1786145400000000))
expect_true(grepl('"usec":1786145400000000', s, fixed = TRUE))
expect_false(grepl("e+", s, fixed = TRUE))

s <- enc(list(big = 2^50))
expect_true(grepl('"big":1125899906842624', s, fixed = TRUE))

# small doubles stay plain numbers (yyjson marks doubles with .0,
# type-faithful and deterministic); integers stay bare
s <- enc(list(m = 22671360, f = 0.5, n = 42L))
expect_true(grepl('"m":22671360.0', s, fixed = TRUE))
expect_true(grepl('"f":0.5', s, fixed = TRUE))
expect_true(grepl('"n":42', s, fixed = TRUE))
expect_false(grepl("e+", s, fixed = TRUE))

# a big value inside a data frame column is a refusal, never corruption
dfbig <- data.frame(x = c(1, 1786145400000000))
e <- tryCatch(enc(dfbig), error = identity)
expect_inherits(e, "rctl_error")

# non-integral big values are refused
e <- tryCatch(enc(list(x = 1e15 + 0.5)), error = identity)
expect_inherits(e, "rctl_error")

# --- Error envelopes: class vector most-specific-first, retryability
# --- from the class table, resource null when unattributable ---

cond <- tryCatch(rctl:::stop_rctl("boom", class = "rctl_usage_error"),
    error = identity)
s <- rctl:::envelope_error("packages.origins", cond)
d <- dec(s)
expect_false(d$ok)
expect_equal(d$error$class[[1L]], "rctl_usage_error")
expect_equal(d$error$class[[2L]], "rctl_error")
expect_equal(d$error$class[[3L]], "runix_error")
expect_false(d$error$retryable)
expect_null(d$error$resource)

race <- structure(
    class = c("rdpkg_cache_race", "rdpkg_error", "runix_error", "error",
        "condition"),
    list(message = "cache changed", call = NULL)
)
d <- dec(rctl:::envelope_error("packages.origins", race))
expect_true(d$error$retryable)

withres <- structure(
    class = c("rctl_environment_error", "rctl_error", "runix_error",
        "error", "condition"),
    list(message = "missing", call = NULL, resource = "rdpkg")
)
d <- dec(rctl:::envelope_error("capabilities", withres))
expect_equal(d$error$resource, "rdpkg")

# single-element class vectors stay JSON arrays
one <- structure(class = c("weird_error", "error", "condition"),
    list(message = "x", call = NULL))
s <- rctl:::envelope_error("t", one)
expect_true(grepl('"class":["weird_error"]', s, fixed = TRUE))

# --- Golden envelope bytes: the exact wire format is pinned ---

expect_equal(enc(list(a = 1L, b = "x", t = TRUE)), paste0(
    '{"schema_version":1,"operation":"t","ok":true,',
    '"result":{"a":1,"b":"x","t":true}}\n'))

# --- json-verbatim smuggling is neutralized: incoming json-class values
# --- are re-encoded as plain strings; only the internal validated
# --- numeric token may pass verbatim ---

s <- enc(list(x = structure('{"evil":true}', class = "json")))
expect_true(grepl('"x":"{\\"evil\\":true}"', s, fixed = TRUE))
e <- tryCatch(rctl:::num_token(Inf), error = identity)
expect_inherits(e, "rctl_error")
