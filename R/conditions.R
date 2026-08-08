## Typed conditions. rctl_error inherits runix_error; the two subclasses
## the exit-code mapping cares about are rctl_usage_error (exit 2) and
## rctl_environment_error (exit 3).
stop_rctl <- function(..., class = character(), call. = sys.call(-1)) {
    stop(structure(
                   class = c(class, "rctl_error", "runix_error", "error", "condition"),
                   list(message = paste0(...), call = call.)
        ))
}
