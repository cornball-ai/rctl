## Operation registry and dispatch. Subsystem packages are Suggests,
## detected at runtime through an injectable probe so tests can simulate
## absence; capabilities must succeed either way.

.rctl_state <- new.env(parent = emptyenv())

has_pkg <- function() {
    f <- .rctl_state$has_pkg
    if (is.null(f)) {
        function(pkg) requireNamespace(pkg, quietly = TRUE)
    } else {
        f
    }
}

set_has_pkg <- function(f = NULL) {
    old <- .rctl_state$has_pkg
    .rctl_state$has_pkg <- f
    invisible(old)
}

## Each operation: the subsystem package it needs (NA for none) and a
## handler taking (positional args, named options).
operations <- function() {
    list(
         "capabilities" = list(pkg = NA_character_, fn = op_capabilities),
         "packages.installed" = list(pkg = "rdpkg",
                                     fn = function(pos, opts) rdpkg::dpkg_installed()),
         "packages.upgradable" = list(pkg = "rdpkg",
                                      fn = function(pos, opts) rdpkg::apt_upgradable()),
         "packages.origins" = list(pkg = "rdpkg",
                                   fn = function(pos, opts) {
        if (length(pos) == 0L) {
            stop_rctl("packages.origins needs package names",
                      class = "rctl_usage_error")
        }
        rdpkg::apt_origins(pos)
    }),
         "packages.candidates" = list(pkg = "rdpkg",
                                      fn = function(pos, opts) {
        if (length(pos) == 0L) {
            stop_rctl("packages.candidates needs package names",
                      class = "rctl_usage_error")
        }
        rdpkg::apt_candidates(pos)
    }),
         "packages.policy" = list(pkg = "rdpkg",
                                  fn = function(pos, opts) {
        if (length(pos) != 1L) {
            stop_rctl("packages.policy needs exactly one package",
                      class = "rctl_usage_error")
        }
        rdpkg::apt_policy(pos)
    }),
         "packages.cache-timestamps" = list(pkg = "rdpkg",
            fn = function(pos, opts) rdpkg::apt_cache_timestamps()),
         "services.units" = list(pkg = "rsystemd",
                                 fn = function(pos, opts) {
        rsystemd::systemd_units(pattern = if (length(pos) > 0L) {
                pos[1L]
            })
    }),
         "services.info" = list(pkg = "rsystemd",
                                fn = function(pos, opts) {
        if (length(pos) != 1L) {
            stop_rctl("services.info needs exactly one unit",
                      class = "rctl_usage_error")
        }
        rsystemd::systemd_unit_info(pos)
    }),
         "services.timers" = list(pkg = "rsystemd",
                                  fn = function(pos, opts) rsystemd::systemd_timers()),
         "services.journal" = list(pkg = "rsystemd",
                                   fn = function(pos, opts) {
        n <- opt_int(opts, "n", 1000L)
        priority <- opt_int(opts, "priority", NULL)
        rsystemd::systemd_journal(unit = opts[["unit"]],
                                  priority = priority, since = opts[["since"]],
                                  until = opts[["until"]], n = n)
    }),
         "services.state" = list(pkg = "rsystemd",
                                 fn = function(pos, opts) rsystemd::systemd_state())
    )
}

opt_int <- function(opts, name, default) {
    v <- opts[[name]]
    if (is.null(v)) {
        return(default)
    }
    out <- suppressWarnings(as.integer(v))
    if (is.na(out)) {
        stop_rctl("--", name, " must be an integer, got: ", v,
                  class = "rctl_usage_error")
    }
    out
}

op_capabilities <- function(pos, opts) {
    probe <- has_pkg()
    subs <- lapply(c(rdpkg = "rdpkg", rsystemd = "rsystemd"), function(p) {
        if (probe(p)) {
            list(present = TRUE,
                 version = as.character(utils::packageVersion(p)))
        } else {
            list(present = FALSE)
        }
    })
    list(
         rctl_version = as.character(utils::packageVersion("rctl")),
         operations = names(operations()),
         subsystems = subs
    )
}

dispatch <- function(operation, pos, opts) {
    ops <- operations()
    if (!operation %in% names(ops)) {
        stop_rctl("unknown operation: ", operation, class = "rctl_usage_error")
    }
    op <- ops[[operation]]
    if (!is.na(op$pkg) && !has_pkg()(op$pkg)) {
        cond <- structure(
                          class = c("rctl_environment_error", "rctl_error", "runix_error",
                                    "error", "condition"),
                          list(message = paste0("subsystem package not installed: ",
                    op$pkg), call = NULL, resource = op$pkg)
        )
        stop(cond)
    }
    op$fn(pos, opts)
}
