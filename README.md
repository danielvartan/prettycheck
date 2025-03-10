# prettycheck

<!-- quarto render -->

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check.yaml](https://github.com/danielvartan/prettycheck/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/danielvartan/prettycheck/actions/workflows/check-standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/danielvartan/prettycheck/branch/main/graph/badge.svg)](https://app.codecov.io/gh/danielvartan/prettycheck?branch=main)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green.png)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)
<!-- badges: end -->

## Overview

Assertive programming doesn’t have to be ugly. If ultra-fast checks
aren’t your top priority, `prettycheck` is for you. This R package
provides a set of functions that produce pretty and informative error
messages, primarily using the [`cli`](https://cli.r-lib.org/) package
from [r-lib](https://github.com/r-lib).

`prettycheck` adheres to [tidyverse
principles](https://tidyverse.tidyverse.org/articles/manifesto.html) and
seamlessly integrates with the [tidyverse
ecosystem](https://www.tidyverse.org/).

Need faster assertive programming? Try
[`checkmate`](https://mllg.github.io/checkmate/). Since `prettycheck`
uses the same structural approach to checks, transitioning between
packages is straightforward. You can also use both packages together
without conflicts.

## Installation

You can install `prettycheck` using the
[`remotes`](https://github.com/r-lib/remotes) package:

``` r
remotes::install_github("danielvartan/prettycheck")
```

## Usage

Like [`checkmate`](https://mllg.github.io/checkmate/), `prettycheck`
includes four family of functions: `test_*()`, `check_*()`,
`assert_*()`, and `expect_*()`.

The `test_*()` functions return a logical flag; the `check_*()`
functions throw a warning message as a string if the condition is not
met; and the `assert_*()` and `expect_*()` functions throw an error if
the condition is not met. If the condition is met, `check_*()` returns a
logical flag; `assert_*()` and `expect_*()` functions return the input
object invisibly.

Here are some of the functions available:

- [`assert_color()`](https://danielvartan.github.io/prettycheck/reference/assert_color.html):
  Assert if an argument is a valid color
- [`check_duration()`](https://danielvartan.github.io/prettycheck/reference/check_duration.html)
  `test_duration()` `assert_duration()`: Check if an argument is a
  [`Duration`](https://lubridate.tidyverse.org/reference/duration.html)
  object
- [`check_hms()`](https://danielvartan.github.io/prettycheck/reference/check_hms.html)
  `test_hms()` `assert_hms()`: Check if an argument is a
  [`hms`](https://hms.tidyverse.org/reference/hms.html) object
- [`check_identical()`](https://danielvartan.github.io/prettycheck/reference/check_identical.html)
  `test_identical()` `assert_identical()`: Check if multiple objects are
  identical
- [`check_interval()`](https://danielvartan.github.io/prettycheck/reference/check_interval.html)
  `test_interval()` `assert_interval()`: Check if an argument is an
  [`Interval`](https://lubridate.tidyverse.org/reference/interval.html)
  object
- [`check_length()`](https://danielvartan.github.io/prettycheck/reference/check_length.html)
  `test_length()` `assert_length()`: Check if an argument has a specific
  length
- [`check_period()`](https://danielvartan.github.io/prettycheck/reference/check_period.html)
  `test_period()` `assert_period()`: Check if an argument is a
  [`Period`](https://lubridate.tidyverse.org/reference/period.html)
  object
- [`check_pick()`](https://danielvartan.github.io/prettycheck/reference/check_pick.html)
  `test_pick()` `assert_pick()`: Check how many arguments were picked
- [`check_posixt()`](https://danielvartan.github.io/prettycheck/reference/check_posixt.html)
  `test_period()` `assert_period()`: Check if an argument is a `POSIXt`
  object
- [`check_temporal`](https://danielvartan.github.io/prettycheck/reference/check_temporal.html)
  `test_temporal()` `assert_temporal()`: Check if an argument is a
  temporal object

Click [here](https://danielvartan.github.io/prettycheck/) to see the
full list of functions.

## License

[![License:
MIT](https://img.shields.io/badge/license-MIT-green.png)](https://opensource.org/license/mit/)

`prettycheck` code is released under the [MIT
license](https://opensource.org/license/mit/).

## Contributing

Contributions are welcome, including bug reports. Take a moment to
review the [Guidelines for
Contributing](https://danielvartan.github.io/prettycheck/CONTRIBUTING.html).

<br>

Become an `prettycheck` supporter!

Click [here](https://github.com/sponsors/danielvartan) to make a
donation. Please indicate the `prettycheck` package in your donation
message.
