% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/length.R
\name{check_length}
\alias{check_length}
\alias{assert_length}
\alias{test_length}
\title{Check if an argument has a specific length}
\usage{
check_length(
  x,
  len = 1,
  min_len = NULL,
  max_len = NULL,
  null_ok = FALSE,
  .names = deparse(substitute(x))
)

assert_length(
  x,
  len = 1,
  min_len = NULL,
  max_len = NULL,
  null_ok = FALSE,
  .names = deparse(substitute(x))
)

test_length(
  x,
  len = 1,
  min_len = NULL,
  max_len = NULL,
  null_ok = FALSE,
  .names = deparse(substitute(x))
)
}
\arguments{
\item{x}{An R object.}

\item{len}{(optional) an integer number indicating the expected length of \code{x}
(default: \code{1}).}

\item{min_len}{(optional) an integer number indicating the minimum length of
\code{x} (default: \code{NULL}).}

\item{max_len}{(optional) an integer number indicating the maximum length of
\code{x} (default: \code{NULL}).}

\item{null_ok}{(optional) a \code{\link[base:logical]{logical}} flag indicating
if \code{x} can be \code{NULL} (default: \code{FALSE}).}

\item{.names}{(optional) a \link[base:character]{character} vector
containing names for each object in \code{...} (default:
\code{deparse(substitute(x))}). This argument is used internally and should
not be set by the user.}
}
\value{
\itemize{
\item \verb{test_*}: \code{TRUE} if it passes the test; \code{FALSE} otherwise.
\item \verb{check_*}: \code{TRUE} if it passes the test; a string with a message otherwise.
\item \verb{assertion_*}: The same input as \link[=invisible]{invisible} if it passes the
test; an error message otherwise.
\item \verb{expect_*}: The same input as \link[=invisible]{invisible} if it passes the
test; an error message otherwise.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
x <- 1:2

test_length(x, len = 2)
#> [1] TRUE # Expected

check_length(x, len = 1) |> cli::cli_alert_warning()
#> ! x must have 1 element. # Expected

check_length(x, min_len = 3) |> cli::cli_alert_warning()
#> ! x must have 3 or more elements. # Expected

check_length(x, max_len = 1) |> cli::cli_alert_warning()
#> ! x must have 1 or less elements. # Expected

x <- 1
check_length(x, min_len = 2, max_len = 3) |> cli::cli_alert_warning()
#> ! x must have a length between 2 and 3. # Expected
}
