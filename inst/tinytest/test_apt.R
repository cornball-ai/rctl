# rctl apt.* surface: thin dispatch over pkgops's preview + commit entrypoints.
# Fixture-driven by injecting pkgops's OWN seams (the planner runner, the effect-
# session ops, and pkcheck), so real pkgops code runs but nothing spawns the
# planner or reaches the broker. Needs pkgops installed (a Suggest).

if (!requireNamespace("pkgops", quietly = TRUE)) {
    exit_file("pkgops not installed")
}

H <- strrep("b", 64L)
CID <- "20250101000000000000-0123456789abcdef"

json_out <- function(argv) {
    out <- capture.output(code <- main(argv))
    list(code = code, doc = janssonr::from_json(paste(out, collapse = "")))
}

# --- fake planner runner (drives the preview half) ---------------------------
mk_runner <- function(json, exit = 0L) {
    force(json)
    force(exit)
    function(cmd, args, input) list(status = exit, output = json,
                                    stderr = character())
}
# One ok planner reply; verb echoes the request (a preview echoes "apt.install",
# never the "-preview" CLI key).
resp <- function(verb, pkgs, resource) {
    sprintf(paste0('{"schema_version":1,"status":"ok","verb":"%s",',
                   '"packages":%s,"plan_schema":1,"resource":%s,',
                   '"plan_hash":"%s","records":[],"detail":null}'),
            verb, pkgs, resource, H)
}
INSTALL_OK <- resp("apt.install", '["nginx"]', '"nginx"')

# --- fake effect-session ops (drives the commit half) ------------------------
newlog <- function() {
    e <- new.env(parent = emptyenv())
    e$seq <- character(0)
    e
}
commit_ops <- function(log, commit_result) {
    list(capability = function(socket_path, plan_schema, ...) {
        log$seq <- c(log$seq, "capability")
        invisible(TRUE)
    }, refuse = function(socket_path, operation, resource, status, ...) {
        log$seq <- c(log$seq, "refuse")
        list(correlation_id = CID, audit_persisted = TRUE)
    }, open = function(socket_path, operation, resource, plan_schema, plan_hash,
                       ...) {
        log$seq <- c(log$seq, "open")
        structure(list(handle = "fake", correlation_id = CID),
                  class = "runix_effect_session")
    }, commit = function(session, packages, lock_timeout, deadline_ms, ...) {
        log$seq <- c(log$seq, "commit")
        log$commit <- list(packages = packages, lock_timeout = lock_timeout,
                           deadline_ms = deadline_ms)
        commit_result
    }, write_outcome = function(session, record, ...) {
        log$seq <- c(log$seq, "write_outcome")
        list(status = "ok", detail = NULL)
    })
}
cr <- function(status, effect_issued, detail = NULL) {
    structure(list(session_status = "ok", status = status,
                   effect_issued = effect_issued, correlation_id = CID,
                   detail = detail), class = "runix_commit_result")
}

# Run one rctl invocation with pkgops's preview runner faked.
with_preview <- function(json, expr) {
    old <- pkgops:::set_runner(mk_runner(json))
    on.exit(pkgops:::set_runner(old), add = TRUE)
    force(expr)
}
# Run one rctl invocation with the preview runner AND the commit seams faked.
with_commit <- function(json, log, pkcheck, commit_result, expr) {
    old_r <- pkgops:::set_runner(mk_runner(json))
    old_o <- pkgops:::set_session_ops(commit_ops(log, commit_result))
    old_p <- pkgops:::set_pkcheck(pkcheck)
    on.exit({
        pkgops:::set_runner(old_r)
        pkgops:::set_session_ops(old_o)
        pkgops:::set_pkcheck(old_p)
    }, add = TRUE)
    force(expr)
}

# --- preview op renders the advisory; the CLI "-preview" key maps to the verb -
r <- with_preview(INSTALL_OK,
                  json_out(c("apt", "install-preview", "nginx", "--json")))
expect_equal(r$code, 0L)
expect_true(r$doc$ok)
expect_equal(r$doc$operation, "apt.install-preview")
expect_equal(r$doc$result$verb, "apt.install")      # the planner verb, not the key
expect_equal(r$doc$result$plan_hash, H)
expect_equal(r$doc$result$advisory_verdict, "ok")

# --- a whole-system preview takes no positional -------------------------------
r <- with_preview(resp("apt.update", "[]", '"@indexes"'),
                  json_out(c("apt", "update-preview", "--json")))
expect_equal(r$code, 0L)
expect_equal(r$doc$result$verb, "apt.update")

# --- arity: nullary verbs reject a positional, target verbs require one --------
for (nv in c("update", "configure", "upgrade", "dist-upgrade")) {
    r <- json_out(c("apt", nv, "nginx", "--json"))    # a package where none allowed
    expect_equal(r$code, 2L)
    expect_equal(r$doc$error$class[[1L]], "rctl_usage_error")
}
for (tv in c("install", "remove", "purge", "hold", "unhold")) {
    r <- json_out(c("apt", tv, "--json"))             # no package where one required
    expect_equal(r$code, 2L)
    expect_equal(r$doc$error$class[[1L]], "rctl_usage_error")
}

# --- commit success: machine-mode auth, args + outcome flow through -----------
log <- newlog()
r <- with_commit(INSTALL_OK, log, function(a) 0L, cr("ok", TRUE),
                 json_out(c("apt", "install", "nginx", "--lock-timeout=300",
                           "--deadline-ms=45000", "--json")))
expect_equal(r$code, 0L)
expect_true(r$doc$ok)
expect_equal(r$doc$operation, "apt.install")
expect_equal(r$doc$result$verb, "apt.install")
expect_true(r$doc$result$effect_issued)
expect_equal(r$doc$result$authorized_via, "pkcheck")   # interactive=FALSE ran pkcheck
expect_equal(log$seq, c("capability", "open", "commit", "write_outcome"))
expect_equal(log$commit$lock_timeout, 300)             # --lock-timeout threaded
expect_equal(log$commit$deadline_ms, 45000L)           # --deadline-ms threaded
expect_equal(log$commit$packages, "nginx")

# --- refusal (unauthorized): exit 1, typed class, full field passthrough ------
log <- newlog()
r <- with_commit(INSTALL_OK, log, function(a) 1L, cr("ok", TRUE),
                 json_out(c("apt", "install", "nginx", "--json")))
expect_equal(r$code, 1L)                               # refused, not usage/env
expect_false(r$doc$ok)
expect_equal(r$doc$error$class[[1L]], "runix_unauthorized")
expect_false(r$doc$error$retryable)
expect_equal(log$seq, c("capability", "refuse"))       # effect never opened
# the pkgops-specific fields now survive into the error envelope (item 3):
expect_equal(r$doc$error$verb, "apt.install")
expect_equal(r$doc$error$plan_hash, H)
expect_equal(r$doc$error$status, "unauthorized")
expect_false(r$doc$error$effect_issued)
expect_equal(r$doc$error$resource, "nginx")
expect_equal(r$doc$error$correlation_id, CID)

# --- a commit failure carries its detail through ------------------------------
log <- newlog()
r <- with_commit(INSTALL_OK, log, function(a) 0L,
                 cr("operation_failed", TRUE, "dpkg exited 100"),
                 json_out(c("apt", "install", "nginx", "--json")))
expect_equal(r$code, 1L)
expect_equal(r$doc$error$class[[1L]], "runix_operation_failed")
expect_equal(r$doc$error$detail, "dpkg exited 100")
expect_true(r$doc$error$effect_issued)

# --- apt_locked is retryable (pkgops registers it) ----------------------------
log <- newlog()
r <- with_commit(INSTALL_OK, log, function(a) 0L, cr("apt_locked", FALSE),
                 json_out(c("apt", "install", "nginx", "--json")))
expect_equal(r$code, 1L)
expect_equal(r$doc$error$class[[1L]], "runix_apt_locked")
expect_true(r$doc$error$retryable)

# --- --preview dry-run returns the advisory and opens NO intent ---------------
log <- newlog()
r <- with_commit(INSTALL_OK, log, function(a) stop("pkcheck must not run"),
                 cr("ok", TRUE),
                 json_out(c("apt", "install", "nginx", "--preview", "--json")))
expect_equal(r$code, 0L)
expect_true(r$doc$ok)
expect_equal(r$doc$result$verb, "apt.install")
expect_equal(r$doc$result$advisory_verdict, "ok")      # it is the preview object
expect_equal(length(log$seq), 0L)                      # nothing opened, nothing minted

# --- the lazy pkgops reference: a handler builds without resolving the fn -----
# (proves operations()/capabilities still build when pkgops is absent; the real
# absent-library path is a manual verification, this is its unit proxy)
expect_inherits(rctl:::apt_commit_op("apt.x", "no_such_preview", "no_such_commit",
                                     TRUE), "function")

# --- capabilities advertises the apt surface + the pkgops subsystem -----------
r <- json_out(c("capabilities", "--json"))
expect_equal(r$code, 0L)
ops <- unlist(r$doc$result$operations)
mut <- unlist(r$doc$result$mutating_operations)
expect_true("apt.install" %in% ops)
expect_true("apt.install-preview" %in% ops)
expect_true("apt.install" %in% mut)                    # the commit mutates
expect_false("apt.install-preview" %in% mut)           # the preview does not
expect_false(is.null(r$doc$result$subsystems$pkgops))  # pkgops is probed

# capabilities lists the apt ops even when pkgops is reported absent (the op
# registry is independent of the runtime probe).
old <- rctl:::set_has_pkg(function(p) p != "pkgops")
r <- json_out(c("capabilities", "--json"))
rctl:::set_has_pkg(old)
expect_true("apt.install" %in% unlist(r$doc$result$operations))
expect_false(unlist(r$doc$result$subsystems$pkgops$present))
