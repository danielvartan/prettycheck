% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empty.R
\name{check_empty}
\alias{check_empty}
\alias{test_empty}
\alias{assert_empty}
\title{Check if an argument has zero length}
\usage{
check_empty(x, null_ok = FALSE, .names = deparse(substitute(x)))

test_empty(x, null_ok = FALSE, .names = deparse(substitute(x)))

assert_empty(x, null_ok = FALSE, .names = deparse(substitute(x)))
}
\arguments{
\item{x}{Any R object.}

\item{null_ok}{(Optional) A \code{\link[base:logical]{logical}} flag indicating if
\code{NULL} values are allowed (Default: \code{FALSE}).}

\item{.names}{(Optional) A \code{\link[base:character]{character}} vector specifying
the names of the objects being tested. This argument is used internally
and should not be set by the user.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\verb{*_empty()} check if an argument has zero length.
}
\examples{
test_empty(1)
#> [1] FALSE # Expected

test_empty(character())
#> [1] TRUE # Expected
}
