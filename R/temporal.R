#' Check if an argument is a temporal object
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `*_temporal()` check if an argument is a temporal object.
#'
#' @details
#'
#' These functions check the following classes of temporal objects:
#'
#' ```
#' classes <- c(
#'   "Duration", "Period", "difftime", "hms", "Date", "POSIXct",
#'   "POSIXlt", "Interval", "ts", "tsibble", "zoo", "xts", "timeDate",
#'   "timeSeries", "circular", "yearmon", "yearqtr", "year", "quarter",
#'   "month", "week", "day", "hour", "minute", "second", "millisecond",
#'   "microsecond", "nanosecond"
#' )
#' ```
#'
#' @param rm (Optional) A [`character`][base::character] vector indicating
#'   the name of classes to remove from the check. See the `Details` section
#'  for more information (Default: `NULL`).
#'
#' @template param-x
#' @template param-any_missing
#' @template param-null_ok
#' @template param-.names
#' @template param-checkmate-minus-.var_name
#' @template return-default
#' @export
#'
#' @examples
#' test_temporal(1)
#' #> [1] FALSE # Expected
#'
#' test_temporal("a")
#' #> [1] FALSE # Expected
#'
#' test_temporal(lubridate::dhours())
#' #> [1] TRUE # Expected
#'
#' test_temporal(lubridate::hours())
#' #> [1] TRUE # Expected
#'
#' test_temporal(as.difftime(1, units = "hours"))
#' #> [1] TRUE # Expected
#'
#' test_temporal(hms::parse_hm("01:00"))
#' #> [1] TRUE # Expected
#'
#' test_temporal(as.Date("2020-01-01"))
#' #> [1] TRUE # Expected
#'
#' test_temporal(Sys.time())
#' #> [1] TRUE # Expected
#'
#' int <- lubridate::interval(as.Date("2001-01-01"), as.Date("2002-01-01"))
#' test_temporal(int)
#' #> [1] TRUE # Expected
#'
#' test_temporal(c(lubridate::dhours(), NA), any_missing = FALSE)
#' #> [1] FALSE # Expected
#'
#' test_temporal(NULL, null_ok = FALSE)
#' #> [1] FALSE # Expected
check_temporal <- function(
    x, #nolint
    rm = NULL,
    any_missing = TRUE,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_character(rm, any.missing = FALSE, null.ok = TRUE)
  assert_flag(any_missing)
  assert_flag(null_ok)
  assert_string(.names)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any_missing)) {
    paste0(glue::single_quote(.names), " cannot have missing values")
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(.names), " cannot be 'NULL'")
  } else if (!test_temporal(x, rm = rm)) {
    paste0(
      "Must be a temporal object (run '?test_temporal()'), ",
      "not ", class_collapse(x)
    )
  } else {
    TRUE
  }
}

#' @rdname check_temporal
#' @export
test_temporal <- function(
    x, #nolint
    rm = NULL,
    any_missing = TRUE,
    null_ok = FALSE,
    .names = deparse(substitute(x))
  ) {
  assert_character(rm, any.missing = FALSE, null.ok = TRUE)
  assert_flag(any_missing)
  assert_flag(null_ok)
  assert_string(.names)

  classes <- c(
    "Duration", "Period", "difftime", "hms", "Date", "POSIXct",
    "POSIXlt", "Interval", "ts", "tsibble", "zoo", "xts", "timeDate",
    "timeSeries", "circular", "yearmon", "yearqtr", "year", "quarter",
    "month", "week", "day", "hour", "minute", "second", "millisecond",
    "microsecond", "nanosecond"
  )

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (any(is.na(x)) && isFALSE(any_missing)) {
    FALSE
  } else {
    if (!is.null(rm)) {
      rm <- paste0("^", rm, "$", collapse = "|")
      classes <- stringr::str_subset(classes, rm, negate = TRUE)
    }

    test_subset(class(x)[1], classes)
  }
}

#' @rdname check_temporal
#' @export
assert_temporal <- checkmate::makeAssertionFunction(check_temporal)

#' @rdname check_temporal
#' @export
expect_temporal <- checkmate::makeExpectationFunction(
  check_temporal,
  use.namespace = TRUE
)
