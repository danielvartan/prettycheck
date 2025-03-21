% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scientific_notation.R
\name{check_scientific_notation}
\alias{check_scientific_notation}
\alias{test_scientific_notation}
\alias{assert_scientific_notation}
\alias{expect_scientific_notation}
\title{Check if a \code{numeric} vector is in scientific notation}
\usage{
check_scientific_notation(
  x,
  any_missing = TRUE,
  null_ok = FALSE,
  .names = deparse(substitute(x))
)

test_scientific_notation(
  x,
  any_missing = TRUE,
  null_ok = FALSE,
  .names = deparse(substitute(x))
)

assert_scientific_notation(
  x,
  any_missing = TRUE,
  null_ok = FALSE,
  .names = deparse(substitute(x)),
  .var.name = checkmate::vname(x),
  add = NULL
)

expect_scientific_notation(
  x,
  any_missing = TRUE,
  null_ok = FALSE,
  .names = deparse(substitute(x)),
  info = NULL,
  label = checkmate::vname(x)
)
}
\arguments{
\item{x}{(Optional) A \code{\link[base:numeric]{numeric}} vector.}

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

\verb{*_scientific_notation()} check if an argument is formatted in scientific
notation.
}
\examples{
opt <- getOption("scipen")

options(scipen = 0)
test_scientific_notation(0.00000000000000000000000001)
#> [1] TRUE # Expected

options(scipen = 999)
test_scientific_notation(0.00000000000000000000000001)
#> [1] FALSE # Expected

options(scipen = opt)
}
