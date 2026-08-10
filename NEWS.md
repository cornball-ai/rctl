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
  `rctl capabilities --json` envelope) with the janssonr dependency installed
  from the cornball drat.
