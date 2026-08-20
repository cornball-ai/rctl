# rctl 0.0.1.8

* Wired the **pkgops** apt package-state mutation surface into the CLI: nine
  read-only `apt.<verb>-preview` operations (an advisory `pkgops_preview`, opening
  no intent) and nine mutating `apt.<verb>` operations (`install`, `remove`,
  `purge`, `hold`, `unhold`, `update`, `upgrade`, `dist-upgrade`, `configure`),
  each previewing then committing through the pkgops issuer. Target verbs take
  package names; the whole-system verbs take none. `--preview` returns the
  advisory and opens no intent.
* Authorization is always machine mode: `apt.<verb>` commits with
  `interactive = FALSE`, so a polkit denial or approval challenge surfaces as a
  terminal exit-1 envelope carrying the condition class, never a prompt.
* pkgops is a runtime-detected Suggests like the other subsystems, so
  `capabilities` reports its presence and version, and error envelopes now carry
  pkgops's `verb`/`plan_hash`/`status`/`effect_issued`/`detail` fields.

# rctl 0.0.1.7

* Wired the **hwstate** subsystem into the CLI: 12 read-only `host.*` operations
  covering the host hardware/resource read surface -- `host.memory`, `host.load`,
  `host.cpu`, `host.processes`, `host.cgroups`, `host.security`, `host.disks`,
  `host.disk-usage`, `host.disk-health`, `host.thermals`, `host.gpus`, and
  `host.conditions`. hwstate is a runtime-detected Suggests like the other
  subsystems, so `capabilities` now reports its presence and version, and
  `host.*` operations return exit 3 with an `rctl_environment_error` naming
  `hwstate` when it is absent. No mutations: hwstate is report-only.

# rctl 0.0.1.6

* JSON backend changed from **yyjsonr to janssonr**, aligning rctl with the rest
  of the Runix stack on a single strict Jansson-based JSON dependency.
* **One intentional wire-format change:** a whole double is now written as a bare
  integer literal (`22671360`) rather than yyjsonr's `.0`-marked form
  (`22671360.0`). JSON has a single numeric type, so numeric *semantics* -- not
  lexical spelling -- remain contractual and `schema_version` stays `1`. See
  `docs/rctl-json-contract.md` in cornball-ai/runix.
* The envelope encoder is now **strict and fail-closed**: it refuses named
  atomic vectors, factors, `POSIXlt`, `AsIs` vectors, other classed atomics,
  non-finite numbers (`NaN`/`Inf`), and duplicate or empty object keys rather
  than silently reinterpreting them. Legitimate shapes are unchanged (data
  frames as row arrays, `POSIXct` as RFC 3339, `NA` as `null`, classed result
  objects serialized as their content, big whole doubles emitted exact).
* Added a Linux CI workflow (install, tests, `R CMD check`, and a real
  `rctl capabilities --json` envelope) with janssonr installed as the
  `r-cornball-janssonr` apt binary from the cornball apt repository -- no
  compiler or `libjansson-dev` needed at install time.
