#' rctl entry point
#'
#' All rctl semantics live here; both launchers (littler and the Rscript
#' fallback) are thin wrappers that pass argv in and quit with the returned
#' status. In machine mode (\code{--json}) exactly one JSON envelope is
#' written to stdout — also on failures — and diagnostics go to stderr.
#'
#' @param argv Character vector of command-line arguments.
#' @return Invisibly, the exit code: 0 success, 1 operation failure,
#'   2 usage error, 3 environment failure.
#' @examples
#' \dontrun{
#' main(c("capabilities", "--json"))
#' }
#' @export
main <- function(argv = character()) {
    parsed <- parse_argv(argv)
    outcome <- if (is.null(parsed$error)) {
        tryCatch(
                 list(result = dispatch(parsed$operation, parsed$positional,
                                        parsed$opts), cond = NULL),
                 error = function(e) list(result = NULL, cond = e)
        )
    } else {
        list(result = NULL, cond = parsed$error)
    }

    code <- if (is.null(outcome$cond)) {
        0L
    } else {
        exit_code(outcome$cond)
    }

    if (parsed$json) {
        out <- if (is.null(outcome$cond)) {
            envelope_success(parsed$operation, outcome$result)
        } else {
            envelope_error(parsed$operation, outcome$cond)
        }
        cat(out)
    } else {
        if (is.null(outcome$cond)) {
            print(outcome$result)
        } else {
            message("rctl: ", conditionMessage(outcome$cond))
        }
    }
    invisible(code)
}

## Exit-code mapping from condition classes, per the contract.
exit_code <- function(cond) {
    cls <- class(cond)
    if ("rctl_usage_error" %in% cls) {
        return(2L)
    }
    if ("rctl_environment_error" %in% cls ||
        any(grepl("_missing_tool$", cls))) {
        return(3L)
    }
    1L
}

## Never throws: parse problems land in $error so machine mode can still
## emit an envelope. --json anywhere; --key=value options; leading
## non-flag words form the operation (two words joined with ".", or one,
## or a literal dotted name); the rest are positional.
parse_argv <- function(argv) {
    out <- list(operation = "unknown", json = FALSE,
                positional = character(), opts = list(), error = NULL)
    flags <- startsWith(argv, "--")
    out$json <- "--json" %in% argv
    for (f in argv[flags]) {
        if (f == "--json") {
            next
        }
        if (!grepl("^--[a-z][a-z-]*=", f)) {
            out$error <- structure(
                                   class = c("rctl_usage_error", "rctl_error", "runix_error",
                    "error", "condition"),
                                   list(message = paste0("unrecognized flag: ", f,
                        " (options use --key=value)"), call = NULL)
            )
            return(out)
        }
        key <- sub("^--([a-z][a-z-]*)=.*$", "\\1", f)
        out$opts[[key]] <- sub("^--[a-z][a-z-]*=", "", f)
    }
    words <- argv[!flags]
    known <- names(operations())
    if (length(words) >= 2L &&
        paste(words[1L], words[2L], sep = ".") %in% known) {
        out$operation <- paste(words[1L], words[2L], sep = ".")
        out$positional <- words[-(1:2)]
    } else if (length(words) >= 1L && words[1L] %in% known) {
        out$operation <- words[1L]
        out$positional <- words[-1L]
    } else {
        if (length(words) >= 1L) {
            out$operation <- paste(words[seq_len(min(2L, length(words)))],
                                   collapse = ".")
        }
        out$error <- structure(
                               class = c("rctl_usage_error", "rctl_error", "runix_error",
                "error", "condition"),
                               list(message = if (length(words) == 0L) {
                    "no operation given"
                } else {
                    paste0("unknown operation: ", out$operation)
                }, call = NULL)
        )
    }
    out
}
