# rctl mutation dispatch: thin adapter over rsystemd. Fixture-driven by
# injecting rsystemd's fake runner, so these run offline — but they need
# rsystemd installed (a Suggest).

if (!requireNamespace("rsystemd", quietly = TRUE)) {
    exit_file("rsystemd not installed")
}
options(rsystemd.poll_interval = 0)

json_out <- function(argv) {
    out <- capture.output(code <- main(argv))
    list(code = code, doc = jsonlite::fromJSON(paste(out, collapse = ""),
        simplifyVector = FALSE))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# scope-aware fake systemctl (any(args=="show"), --user may precede).
fake_systemctl <- function(states, effect_status = 0L,
    effect_stderr = character(), advance_after = 1L) {
    st <- new.env()
    st$i <- 1L
    st$issued <- FALSE
    render <- function(s) {
        c(paste0("Id=", s$unit %||% "x.service"), "Description=fake",
            paste0("LoadState=", s$load_state %||% "loaded"),
            paste0("ActiveState=", s$active_state),
            paste0("SubState=", s$sub_state %||% "running"),
            paste0("UnitFileState=", s$unit_file_state %||% "enabled"),
            "FragmentPath=/x", "ActiveEnterTimestamp=",
            paste0("MainPID=", s$main_pid %||% 0L),
            "MemoryCurrent=[not set]", "NRestarts=0",
            paste0("InvocationID=", s$invocation_id %||% ""),
            paste0("StateChangeTimestampMonotonic=", s$scm %||% 1000L))
    }
    function(cmd, args) {
        if (any(args == "show")) {
            idx <- min(st$i, length(states))
            out <- render(states[[idx]])
            if (st$issued) st$i <- st$i + 1L
            list(status = 0L, output = out, stderr = character())
        } else {
            st$issued <- TRUE
            st$i <- st$i + advance_after - 1L
            list(status = effect_status, output = character(),
                stderr = effect_stderr)
        }
    }
}
svc <- function(active, inv, scm = 1000L) {
    list(unit = "cups.service", active_state = active, sub_state = "running",
        unit_file_state = "enabled", main_pid = 0L, invocation_id = inv,
        scm = scm)
}

with_systemctl <- function(fake, expr) {
    old <- rsystemd:::set_runner(fake)
    on.exit(rsystemd:::set_runner(old), add = TRUE)
    force(expr)
}

# --- capabilities exposes mutating_operations ---------------------------

r <- json_out(c("capabilities", "--json"))
expect_equal(r$code, 0L)
mut <- unlist(r$doc$result$mutating_operations)
expect_true("services.restart" %in% mut)
expect_true("services.start" %in% mut)
expect_false("services.units" %in% mut) # read-only not listed
expect_false("packages.installed" %in% mut)

# --- capabilities advertises the audit authority (fleet policy gate) -----

expect_false(is.null(r$doc$result$audit))
expect_true(is.logical(unlist(r$doc$result$audit$system_durable_audit)))
# a system-scope mutation's record scope is one of the authority-matrix values
expect_true(unlist(r$doc$result$audit$audit_scope) %in% c("system", "caller"))
# the two fields are derived from one probe, so they can never contradict:
# system-durable iff the record would be written under the "system" scope.
adurable <- isTRUE(unlist(r$doc$result$audit$system_durable_audit))
ascope <- unlist(r$doc$result$audit$audit_scope)
expect_equal(adurable, identical(ascope, "system"))

# forcing the durable branch must report "system" scope, never a stale "caller".
# (Regression: independent probe + audit_scope_for calls once reported
# {system_durable_audit = TRUE, audit_scope = "caller"} on a broker-backed host.)
orig_sda <- runix::system_durable_audit_available
assignInNamespace("system_durable_audit_available", function(...) TRUE, "runix")
cap_durable <- rctl:::audit_capability()
assignInNamespace("system_durable_audit_available", orig_sda, "runix")
expect_true(isTRUE(cap_durable$system_durable_audit))
expect_equal(cap_durable$audit_scope, "system")

# --- restart success: runix_result flows through the envelope unchanged --

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("active", "AAAA"), svc("active", "BBBB", scm = 2000L)),
    advance_after = 2L))
r <- json_out(c("services", "restart", "cups.service", "--json"))
rsystemd:::set_runner(NULL)
expect_equal(r$code, 0L)
expect_true(r$doc$ok)
expect_equal(r$doc$operation, "services.restart")
res <- r$doc$result
expect_true(res$changed)
expect_true(res$state_changed)
expect_equal(res$completion$method, "invocation_id")
expect_equal(res$completion$invocation_after, "BBBB")
expect_false(is.null(res$audit)) # audit rides along
expect_equal(res$audit$outcome, "ok")

# --- --preview is a success with preview:true, no effect ----------------

seen <- new.env()
seen$effect <- FALSE
old <- rsystemd:::set_runner(function(cmd, args) {
    if (!any(args == "show")) seen$effect <- TRUE
    fake_systemctl(list(svc("inactive", NA_character_)))(cmd, args)
})
r <- json_out(c("services", "start", "cups.service", "--preview", "--json"))
rsystemd:::set_runner(NULL)
expect_equal(r$code, 0L)
expect_true(r$doc$ok)
expect_true(r$doc$result$preview)
expect_false(seen$effect)

# --- authorization denial -> typed error envelope with passthrough ------

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("inactive", NA_character_)),
    effect_status = 1L,
    effect_stderr = "Interactive authentication required.",
    advance_after = 1L))
r <- json_out(c("services", "start", "cups.service", "--json"))
rsystemd:::set_runner(NULL)
expect_equal(r$code, 1L)             # refused, not usage/env error
expect_false(r$doc$ok)
expect_equal(r$doc$error$class[[1L]], "runix_unauthorized")
expect_false(r$doc$error$retryable)
expect_equal(r$doc$error$polkit_action,
    "org.freedesktop.systemd1.manage-units")
expect_equal(r$doc$error$resource, "cups.service")

# --- timeout -> error envelope carrying observed post-state -------------

old <- rsystemd:::set_runner(fake_systemctl(
    list(svc("inactive", NA_character_),
         list(unit = "s.service", active_state = "activating",
             sub_state = "start", unit_file_state = "enabled",
             main_pid = 0L, invocation_id = "T1", scm = 1500L)),
    advance_after = 2L))
r <- json_out(c("services", "start", "slow.service", "--timeout=0.05",
    "--json"))
rsystemd:::set_runner(NULL)
expect_equal(r$code, 1L)
expect_equal(r$doc$error$class[[1L]], "runix_timeout")
expect_equal(r$doc$error$observed$active_state, "activating")
expect_false(r$doc$error$observed_failed)

# --- scope threads through: --scope=user reaches rsystemd ---------------

calls <- new.env()
calls$argv <- list()
base <- fake_systemctl(list(svc("active", "U1"),
    svc("active", "U2", scm = 2000L)), advance_after = 2L)
old <- rsystemd:::set_runner(function(cmd, args) {
    calls$argv[[length(calls$argv) + 1L]] <- args
    base(cmd, args)
})
r <- json_out(c("services", "restart", "u.service", "--scope=user",
    "--json"))
rsystemd:::set_runner(NULL)
expect_equal(r$code, 0L)
has_user <- vapply(calls$argv, function(a) any(a == "--user"), logical(1))
expect_true(all(has_user)) # --user on every call, observation included

# --- usage errors: missing unit, bad scope value ------------------------

r <- json_out(c("services", "restart", "--json"))
expect_equal(r$code, 2L)
expect_equal(r$doc$error$class[[1L]], "rctl_usage_error")

old <- rsystemd:::set_runner(fake_systemctl(list(svc("active", "A"))))
r <- json_out(c("services", "start", "x.service", "--scope=bogus",
    "--json"))
rsystemd:::set_runner(NULL)
expect_equal(r$code, 1L) # rsystemd rejects the scope value (well-formed CLI)
expect_true(grepl("scope", r$doc$error$message))
