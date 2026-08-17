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

## Each operation: the subsystem package it needs (NA for none), whether it
## mutates system state, and a handler taking (positional args, named
## options). Mutation handlers delegate wholesale to rsystemd and return its
## runix_result / raise its typed conditions unchanged — rctl adds no
## mutation semantics of its own.
operations <- function() {
    ro <- function(pkg, fn) list(pkg = pkg, mutates = FALSE, fn = fn)
    mut <- function(pkg, fn) list(pkg = pkg, mutates = TRUE, fn = fn)
    list(
         "capabilities" = ro(NA_character_, op_capabilities),
         "packages.installed" = ro("pkgstate",
                                   function(pos, opts) pkgstate::dpkg_installed()),
         "packages.upgradable" = ro("pkgstate",
                                    function(pos, opts) pkgstate::apt_upgradable()),
         "packages.origins" = ro("pkgstate", function(pos, opts) {
        if (length(pos) == 0L) {
            stop_rctl("packages.origins needs package names",
                      class = "rctl_usage_error")
        }
        pkgstate::apt_origins(pos)
    }),
         "packages.candidates" = ro("pkgstate", function(pos, opts) {
        if (length(pos) == 0L) {
            stop_rctl("packages.candidates needs package names",
                      class = "rctl_usage_error")
        }
        pkgstate::apt_candidates(pos)
    }),
         "packages.policy" = ro("pkgstate", function(pos, opts) {
        if (length(pos) != 1L) {
            stop_rctl("packages.policy needs exactly one package",
                      class = "rctl_usage_error")
        }
        pkgstate::apt_policy(pos)
    }),
         "packages.cache-timestamps" = ro("pkgstate",
            function(pos, opts) pkgstate::apt_cache_timestamps()),
         "services.units" = ro("rsystemd", function(pos, opts) {
        rsystemd::systemd_units(pattern = if (length(pos) > 0L) {
                pos[1L]
            })
    }),
         "services.info" = ro("rsystemd", function(pos, opts) {
        rsystemd::systemd_unit_info(one_unit(pos, "services.info"),
                                    scope = opt_scope(opts))
    }),
         "services.timers" = ro("rsystemd",
                                function(pos, opts) rsystemd::systemd_timers()),
         "services.journal" = ro("rsystemd", function(pos, opts) {
        rsystemd::systemd_journal(unit = opts[["unit"]],
                                  priority = opt_int(opts, "priority", NULL),
                                  since = opts[["since"]], until = opts[["until"]],
                                  n = opt_int(opts, "n", 1000L))
    }),
         "services.state" = ro("rsystemd",
                               function(pos, opts) rsystemd::systemd_state()),
         "services.start" = mut("rsystemd", mutation_handler(
                rsystemd::systemd_start, "services.start")),
         "services.stop" = mut("rsystemd", mutation_handler(
                rsystemd::systemd_stop, "services.stop")),
         "services.restart" = mut("rsystemd", mutation_handler(
                rsystemd::systemd_restart, "services.restart")),
         "services.enable" = mut("rsystemd", mutation_handler(
                rsystemd::systemd_enable, "services.enable")),
         "services.disable" = mut("rsystemd", mutation_handler(
                rsystemd::systemd_disable, "services.disable")),
         "host.memory" = ro("hwstate", function(pos, opts) hwstate::mem_info()),
         "host.load" = ro("hwstate",
                          function(pos, opts) hwstate::load_average()),
         "host.cpu" = ro("hwstate", function(pos, opts) hwstate::cpu_info()),
         "host.processes" = ro("hwstate",
                               function(pos, opts) hwstate::processes()),
         "host.cgroups" = ro("hwstate",
                             function(pos, opts) hwstate::cgroup_rss()),
         "host.security" = ro("hwstate",
                              function(pos, opts) hwstate::proc_security()),
         "host.disks" = ro("hwstate", function(pos, opts) hwstate::disks()),
         "host.disk-usage" = ro("hwstate",
                                function(pos, opts) hwstate::disk_usage()),
         "host.disk-health" = ro("hwstate",
                                 function(pos, opts) hwstate::disk_health()),
         "host.thermals" = ro("hwstate",
                              function(pos, opts) hwstate::thermals()),
         "host.gpus" = ro("hwstate", function(pos, opts) hwstate::gpus()),
         "host.conditions" = ro("hwstate",
                                function(pos, opts) hwstate::node_conditions())
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

## Numeric option (fractional allowed) — timeouts may be sub-second. Value
## validation (positivity) is left to rsystemd; a non-numeric spelling is a
## usage error here.
opt_num <- function(opts, name, default) {
    v <- opts[[name]]
    if (is.null(v)) {
        return(default)
    }
    out <- suppressWarnings(as.numeric(v))
    if (is.na(out)) {
        stop_rctl("--", name, " must be a number, got: ", v,
                  class = "rctl_usage_error")
    }
    out
}

## --scope option; validation of the value is left to rsystemd (single
## source of truth for the allowed set), but a wrong count is a usage error.
opt_scope <- function(opts) {
    v <- opts[["scope"]]
    if (is.null(v)) {
        "system"
    } else {
        v
    }
}

one_unit <- function(pos, op) {
    if (length(pos) != 1L) {
        stop_rctl(op, " needs exactly one unit", class = "rctl_usage_error")
    }
    pos
}

## A mutation handler is a thin closure over an rsystemd verb: it maps the
## CLI surface (positional unit, --scope, --timeout, --preview) onto the
## verb's arguments and returns whatever the verb returns / raises. No
## result reshaping, no error reclassification.
mutation_handler <- function(verb, op) {
    force(verb)
    force(op)
    function(pos, opts) {
        unit <- one_unit(pos, op)
        verb(unit, scope = opt_scope(opts),
             dry_run = isTRUE(opts[["preview"]]),
             timeout = opt_num(opts, "timeout", 90))
    }
}

op_capabilities <- function(pos, opts) {
    probe <- has_pkg()
    subs <- lapply(c(pkgstate = "pkgstate", rsystemd = "rsystemd",
                     hwstate = "hwstate"), function(p) {
        if (probe(p)) {
            list(present = TRUE,
                 version = as.character(utils::packageVersion(p)))
        } else {
            list(present = FALSE)
        }
    })
    ops <- operations()
    mutating <- names(ops)[vapply(ops, function(o) isTRUE(o$mutates),
                                  logical(1))]
    list(
         rctl_version = as.character(utils::packageVersion("rctl")),
         operations = names(ops),
         mutating_operations = mutating,
         subsystems = subs,
         audit = audit_capability()
    )
}

## Host audit capability, read-only: whether a mutation's audit can be made
## system-durable here, and the scope a system-scope mutation's record would be
## written under (runix authority matrix). A fleet policy reads this to refuse
## system-scope mutations that would only be caller-durably audited. No prompt,
## no mutation. Falls back gracefully if the runix helpers are unavailable.
audit_capability <- function() {
    tryCatch({
        ## Both fields come from ONE probe so they cannot contradict. If a
        ## system-scope mutation's audit would be system-durable here (root, or a
        ## reachable root-authenticated broker), its record is written under the
        ## "system" scope; otherwise report the honest fallback scope. Deriving
        ## audit_scope from the same snapshot rules out the nonsensical pair
        ## {system_durable_audit = TRUE, audit_scope = "caller"}.
        durable <- runix::system_durable_audit_available()
        scope <- if (isTRUE(durable)) "system" else runix::audit_scope_for("system")
        list(system_durable_audit = durable, audit_scope = scope)
    }, error = function(e) list(system_durable_audit = FALSE,
                                audit_scope = NA_character_))
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
