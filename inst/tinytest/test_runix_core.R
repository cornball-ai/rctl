## Compatibility after adopting the runix core: rctl classifies retryability
## via the shared runix registry, populated through the normal subsystem load
## path (codex gate) rather than a hardcoded class table.

## rctl:::is_retryable delegates to runix::is_retryable for any condition
plain <- structure(
    class = c("rctl_usage_error", "rctl_error", "runix_error", "error",
              "condition"),
    list(message = "x"))
expect_equal(rctl:::is_retryable(plain), runix::is_retryable(plain))
expect_false(rctl:::is_retryable(plain))

## Codex gate: a pkgstate_cache_race is retryable once pkgstate has loaded the
## way dispatch loads it (requireNamespace -> .onLoad -> register_retryable),
## not only under a manual registry poke or a particular test-file order.
if (requireNamespace("pkgstate", quietly = TRUE)) {
    race <- structure(
        class = c("pkgstate_cache_race", "pkgstate_error", "runix_error",
                  "error", "condition"),
        list(message = "cache moved"))
    expect_true(rctl:::is_retryable(race))
    expect_equal(rctl:::is_retryable(race), runix::is_retryable(race))
}
