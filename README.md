

# prettycheck

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/danielvartan/prettycheck/workflows/R-CMD-check/badge.svg)](https://github.com/danielvartan/prettycheck/actions)
[![Codecov test
coverage](https://codecov.io/gh/danielvartan/prettycheck/branch/main/graph/badge.svg)](https://app.codecov.io/gh/danielvartan/prettycheck?branch=main)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green.png)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
<!-- badges: end -->

## Overview

Assertive programming doesn’t have to be ugly. If ultra-fast checks are
not your top priority, `prettycheck` is for you. It provides a set of
functions that produce pretty and informative error messages, primarily
relying on the [`cli`](https://cli.r-lib.org/) package from
[r-lib](https://github.com/r-lib). Inspired by the
[`checkmate`](https://mllg.github.io/checkmate/) package, it places a
strong emphasis on aesthetics and user experience.

`prettycheck` also adheres to the [tidyverse
principles](https://tidyverse.tidyverse.org/articles/manifesto.html) and
integrates well with the [tidyverse
ecosystem](https://www.tidyverse.org/).

## Installation

You can install `prettycheck` using the
[`remotes`](https://github.com/r-lib/remotes) package:

``` r
remotes::install_github("danielvartan/prettycheck")
```

## Usage

Like [`checkmate`](https://mllg.github.io/checkmate/), `prettycheck`
includes four main types of functions: `test_*()`, `check_*()`,
`assert_*()`, and `expect_*()`. The `test_*()` functions return a
logical flag; the `check_*()` functions return a logical flag and throw
a warning message if the condition is not met; and the`assert_*()` and
`expect_*()` functions return a logical flag and throw an error if the
condition is not met.

Here are some of the functions available:

> **Not yet implemented!**

- `assert_character()`: Assert that an object is a character vector.
- `assert_complex()`: Assert that an object is a complex vector.
- `assert_date()`: Assert that an object is a date vector.
- `assert_double()`: Assert that an object is a double vector.
- `assert_factor()`: Assert that an object is a factor vector.
- `assert_integer()`: Assert that an object is an integer vector.
- `assert_list()`: Assert that an object is a list.
- `assert_logical()`: Assert that an object is a logical vector.
- `assert_matrix()`: Assert that an object is a matrix.
- `assert_numeric()`: Assert that an object is a numeric vector.
- `assert_vector()`: Assert that an object is a vector.
- `assert_positive()`: Assert that an object is a positive number.
- `assert_negative()`: Assert that an object is a negative number.
- `assert_infinite()`: Assert that an object is infinite.
- `assert_finite()`: Assert that an object is finite.
- `assert_na()`: Assert that an object is `NA`.
- `assert_null()`: Assert that an object is `NULL`.
- `assert_empty()`: Assert that an object is empty.
- `assert_not_empty()`: Assert that an object is not empty.
- `assert_equal()`: Assert that two objects are equal.
- `assert_not_equal()`: Assert that two objects are not equal.
- `assert_identical()`: Assert that two objects are identical.
- `assert_not_identical()`: Assert that two objects are not identical.
- `assert_in()`: Assert that an object is in a set.
- `assert_not_in()`: Assert that an object is not in a set.
- `assert_between()`: Assert that an object is between two values.
- `assert_not_between()`: Assert that an object is not between two
  values.

Click [here](https://danielvartan.github.io/prettycheck/) to see the
full list of functions.

## Contributing

We welcome contributions, including bug reports. Take a moment to review
our [Guidelines for
Contributing](https://danielvartan.github.io/prettycheck/CONTRIBUTING.html).

## License

[![License:
MIT](https://img.shields.io/badge/license-MIT-green.png)](https://opensource.org/license/mit/)

`prettycheck` code is released under the [MIT
license](https://opensource.org/license/mit/).

<br>

Become an `prettycheck` supporter!

Click [here](https://github.com/sponsors/danielvartan) to make a
donation. Please indicate the `actverse` package in your donation
message.
