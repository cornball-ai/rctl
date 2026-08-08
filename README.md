# rctl

The Runix command-line interface: a machine-drivable CLI over the Runix
read-only introspection packages ([rdpkg](https://github.com/cornball-ai/rdpkg),
[rsystemd](https://github.com/cornball-ai/rsystemd)).

**Status: experimental.** Version 0.0.1; the `--json` interface follows the
[machine-interface contract](https://github.com/cornball-ai/runix/blob/master/docs/rctl-json-contract.md)
and is the stable surface. Human mode (no `--json`) is explicitly unstable.

```console
$ rctl capabilities --json
{"schema_version":1,"operation":"capabilities","ok":true,"result":{...}}

$ rctl packages upgradable --json
$ rctl services units --json
$ rctl services journal --priority=3 --n=100 --json
```

Machine mode guarantees: exactly one JSON envelope on stdout (also on
failures), diagnostics on stderr, typed error classes with retryability,
exit codes 0/1/2/3, deterministic encoding (NA as null, RFC 3339 UTC
timestamps, row-object data frames, no scientific notation, guaranteed
UTF-8). Subsystem packages are detected at runtime — `capabilities`
succeeds even where they are absent.

Two launchers with byte-identical output: littler (`inst/bin/rctl`, the
fast path) and an Rscript fallback (`inst/bin/rctl-rscript`).

## Install

```r
remotes::install_github("cornball-ai/rctl")
```

Linux only (`OS_type: unix`); install rdpkg/rsystemd for the actual
operations.

## License

MIT © cornball.ai
