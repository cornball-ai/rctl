% tinyrox says don't edit this manually, but it can't stop you!
\name{main}
\alias{main}
\title{rctl entry point}
\usage{
main(argv = character())
}
\arguments{
\item{argv}{Character vector of command-line arguments.}
}
\value{
Invisibly, the exit code: 0 success, 1 operation failure,
  2 usage error, 3 environment failure.
}
\description{
All rctl semantics live here; both launchers (littler and the Rscript
fallback) are thin wrappers that pass argv in and quit with the returned
status. In machine mode (\code{--json}) exactly one JSON envelope is
written to stdout — also on failures — and diagnostics go to stderr.
}
\examples{
\dontrun{
main(c("capabilities", "--json"))
}
}
