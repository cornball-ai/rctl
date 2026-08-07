# rctl

The Runix CLI. **The contract is law**: `docs/rctl-json-contract.md` in
cornball-ai/runix — read it before changing anything here.

Non-negotiables:
- Machine mode (`--json`): exactly one JSON envelope on stdout, diagnostics
  to stderr, error envelope emitted even on usage/environment failures.
- Agents branch on error `class`, never `message`. Retryability comes from
  the class table in R/main.R.
- Deterministic encoding lives in R/encode.R and is fixture-tested: NA to
  null, RFC 3339 UTC timestamps, row-object data frames, no scientific
  notation (big whole scalars go verbatim; big values in data frame columns
  are a refusal, never corruption), guaranteed UTF-8.
- rdpkg/rsystemd are Suggests, detected at runtime (`has_pkg`, injectable
  for tests): `capabilities` must succeed and exit 0 with subsystems absent.
- All semantics in `main(argv)`; both launchers (inst/bin/rctl for littler,
  inst/bin/rctl-rscript for Rscript) are thin and byte-parity-tested from
  the runix integration suite.
- Human mode is unstable by contract — never let anything parse it.
- Tinyverse workflow: `tinyrox::document()`, `tinypkgr::install()`, tinytest.
