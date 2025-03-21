% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identical.R
\name{check_identical}
\alias{check_identical}
\alias{assert_identical}
\alias{test_identical}
\title{Check if multiple objects are identical}
\usage{
check_identical(..., type = "value", .names = get_names(...))

assert_identical(..., type = "value", .names = get_names(...))

test_identical(..., type = "value", .names = get_names(...))
}
\arguments{
\item{...}{Objects to compare.}

\item{type}{(optional) a string corresponding to the type of comparison to
perform. Valid values: \code{"value"}, \code{"length"}, and \code{"class"} (default:
\code{"value"}).}

\item{.names}{(Optional) A \code{\link[base:character]{character}} vector specifying
the names of the objects being tested. This argument is used internally
and should not be set by the user.}
}
\value{
\itemize{
\item \verb{test_*}: \code{TRUE} if it passes the test; \code{FALSE} otherwise.
\item \verb{check_*}: \code{TRUE} if it passes the test; a string with a message otherwise.
\item \verb{assertion_*}: The same input (as \link[base:invisible]{invisible}) if it
passes the test; an error message otherwise.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
x <- 1; y <- 1
test_identical(x, y, type = "value")
#> [1] TRUE # Expected

x <- 1; y <- 2
check_identical(x, y, type = "value") |> cli::cli_alert_warning()
#> ! x and y must have identical values. # Expected

x <- letters; y <- 1:2
check_identical(x, y, type = "length") |> cli::cli_alert_warning()
#> ! x and y must have identical lengths. # Expected

x <- "a"; y <- 1
check_identical(x, y, type = "class") |> cli::cli_alert_warning()
#> ! x and y must belong to the same class. # Expected
}
