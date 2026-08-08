# main()/dispatch: exit codes, envelopes on stdout in all failure modes,
# capability absence as data, argv parsing.

json_out <- function(argv) {
    out <- capture.output(code <- main(argv))
    list(code = code, doc = jsonlite::fromJSON(paste(out, collapse = ""),
        simplifyVector = FALSE))
}

# --- capabilities succeeds and exits 0 even with all subsystems absent ---

old <- rctl:::set_has_pkg(function(pkg) FALSE)
r <- json_out(c("capabilities", "--json"))
rctl:::set_has_pkg(old)
expect_equal(r$code, 0L)
expect_true(r$doc$ok)
expect_false(r$doc$result$subsystems$pkgstate$present)
expect_false(r$doc$result$subsystems$rsystemd$present)
expect_true("packages.installed" %in% unlist(r$doc$result$operations))

# --- absent subsystem on a real operation: exit 3, typed envelope,
# --- resource names the missing package ---

rctl:::set_has_pkg(function(pkg) FALSE)
r <- json_out(c("packages", "installed", "--json"))
rctl:::set_has_pkg(old)
expect_equal(r$code, 3L)
expect_false(r$doc$ok)
expect_equal(r$doc$error$class[[1L]], "rctl_environment_error")
expect_equal(r$doc$error$resource, "pkgstate")

# --- usage errors: exit 2 with an envelope, stdout never empty ---

r <- json_out(c("no", "such", "--json"))
expect_equal(r$code, 2L)
expect_equal(r$doc$error$class[[1L]], "rctl_usage_error")

r <- json_out("--json")
expect_equal(r$code, 2L)

r <- json_out(c("services", "journal", "--priority=high", "--json"))
expect_equal(r$code, 2L)

r <- json_out(c("capabilities", "--frobnicate", "--json"))
expect_equal(r$code, 2L)

# --- argv parsing details ---

p <- rctl:::parse_argv(c("services", "journal", "--unit=ssh.service",
    "--n=50", "--json"))
expect_equal(p$operation, "services.journal")
expect_true(p$json)
expect_equal(p$opts$unit, "ssh.service")
expect_equal(p$opts$n, "50")

p <- rctl:::parse_argv(c("packages.origins", "dpkg", "bash"))
expect_equal(p$operation, "packages.origins")
expect_equal(p$positional, c("dpkg", "bash"))
expect_false(p$json)

# --- positional-arg validation through dispatch ---

r <- json_out(c("packages", "policy", "--json"))
expect_equal(r$code, 2L)
r <- json_out(c("packages", "origins", "--json"))
expect_equal(r$code, 2L)

# --- Live smoke tests ---

if (at_home()) {
    if (!requireNamespace("pkgstate", quietly = TRUE) ||
        !requireNamespace("rsystemd", quietly = TRUE)) {
        exit_file("live smoke needs pkgstate + rsystemd installed")
    }
    r <- json_out(c("capabilities", "--json"))
    expect_equal(r$code, 0L)
    expect_true(r$doc$result$subsystems$pkgstate$present)
    expect_true(r$doc$result$subsystems$rsystemd$present)

    r <- json_out(c("packages", "installed", "--json"))
    expect_equal(r$code, 0L)
    expect_true(length(r$doc$result) > 100L)

    r <- json_out(c("services", "units", "--json"))
    expect_equal(r$code, 0L)
    units <- vapply(r$doc$result, function(x) x$unit, character(1))
    expect_true("-.mount" %in% units)

    r <- json_out(c("services", "journal", "--priority=3", "--n=25",
        "--json"))
    expect_equal(r$code, 0L)

    r <- json_out(c("packages", "policy", "no-such-pkg-xyzzy", "--json"))
    expect_equal(r$code, 1L)
    expect_equal(r$doc$error$class[[1L]], "pkgstate_unknown_package")
}
