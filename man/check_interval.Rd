% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval.R
\name{check_interval}
\alias{check_interval}
\alias{test_interval}
\alias{assert_interval}
\alias{expect_interval}
\title{Check if an argument is an \code{\link[lubridate:interval]{Interval}} object}
\usage{
check_interval(
  x,
  lower = -Inf,
  upper = Inf,
  any_missing = TRUE,
  null_ok = FALSE,
  .names = deparse(substitute(x))
)

test_interval(
  x,
  lower = -Inf,
  upper = Inf,
  any_missing = TRUE,
  null_ok = FALSE,
  .names = deparse(substitute(x))
)

assert_interval(
  x,
  lower = -Inf,
  upper = Inf,
  any_missing = TRUE,
  null_ok = FALSE,
  .names = deparse(substitute(x)),
  .var.name = checkmate::vname(x),
  add = NULL
)

expect_interval(
  x,
  lower = -Inf,
  upper = Inf,
  any_missing = TRUE,
  null_ok = FALSE,
  .names = deparse(substitute(x)),
  info = NULL,
  label = checkmate::vname(x)
)
}
\arguments{
\item{x}{Any R object.}

\item{lower}{(Optional) A value of the same class as \code{x} indicating
the minimum value of \code{x} (Default: \code{-Inf}).}

\item{upper}{(Optional) A value of the same class as \code{x} indicating
the maximum value of \code{x} (Default: \code{Inf}).}

\item{any_missing}{(Optional) A \code{\link[base:logical]{logical}} flag indicating
if missing values are allowed (Default: \code{TRUE}).}

\item{null_ok}{(Optional) A \code{\link[base:logical]{logical}} flag indicating if
\code{NULL} values are allowed (Default: \code{FALSE}).}

\item{.names}{(Optional) A \code{\link[base:character]{character}} vector specifying
the names of the objects being tested. This argument is used internally
and should not be set by the user.}

\item{.var.name}{(Optional) A \code{\link[base:character]{character}} string
indicating the name of the checked object to print in assertions.
This argument is used internally and should not be set by the user.}

\item{add}{(Optional) A \code{\link[checkmate:AssertCollection]{AssertCollection}}
object indicating the collection to store assertion messages. This
argument is used internally and should not be set by the user.}

\item{info}{(Optional) A \code{\link[base:character]{character}} string with extra
information to be included in the message for the `testthat`` reporter.
This argument is used internally and should not be set by the user.}

\item{label}{(Optional) A \code{\link[base:character]{character}} string
indicating the name of the checked object to print in messages.
This argument is used internally and should not be set by the user.}
}
\value{
\itemize{
\item \verb{test_*}: \code{TRUE} if it passes the test; \code{FALSE} otherwise.
\item \verb{check_*}: \code{TRUE} if it passes the test; a string with a message otherwise.
\item \verb{assertion_*}: The same input (as \link[base:invisible]{invisible}) if it
passes the test; an error message otherwise.
\item \verb{expect_*}: The same input (as \link[base:invisible]{invisible}) if it
passes the test; an error message otherwise.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\verb{*_interval()} check if an argument is a
\href{https://lubridate.tidyverse.org/}{\code{lubridate}}
\code{\link[lubridate:interval]{Interval}} object.
}
\examples{
test_interval(1)
#> [1] FALSE # Expected

test_interval(lubridate::dhours())
#> [1] FALSE # Expected

int <- lubridate::interval(as.Date("2001-01-01"), as.Date("2002-01-01"))

test_interval(int)
#> [1] TRUE # Expected

test_interval(int, lower = lubridate::dyears(2))
#> [1] FALSE # Expected

test_interval(int, upper = lubridate::dyears(0.5))
#> [1] FALSE # Expected

test_interval(c(int, NA), any_missing = FALSE)
#> [1] FALSE # Expected

test_interval(NULL, null_ok = FALSE)
#> [1] FALSE # Expected
}
