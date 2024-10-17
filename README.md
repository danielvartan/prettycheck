# prettycheck

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
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

If you need a faster system for assertive programming, we recommend the
[`checkmate`](https://mllg.github.io/checkmate/) package. Since
`prettycheck` uses the same structure for checks, transitioning to
`checkmate` is straightforward. You can also use both packages together
seamlessly, since `prettycheck` also uses `checkmate` under the hood.

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
functions throw a warning message if the condition is not met; and the
`assert_*()` and `expect_*()` functions throw an error if the condition
is not met. If the condition is met, `check_*()`, `assert_*()`, and
`expect_*()` functions return the input object invisibly.

Here are some of the functions available for now:

- [`test_identical()`](https://danielvartan.github.io/prettycheck/reference/test_identical.html)
  `check_identical()` `assert_identical()` `expect_identical()`: Check
  if multiple objects are identical.
- [`test_length()`](https://danielvartan.github.io/prettycheck/reference/test_length.html)
  `check_length()` `assert_length()` `expect_length()`: Check if an
  argument has a specific length.
- [`test_pick()`](https://danielvartan.github.io/prettycheck/reference/test_pick.html)
  `check_pick()` `assert_pick()`: Check how many arguments were picked.

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
donation. Please indicate the `prettycheck` package in your donation
message.
