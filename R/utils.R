# library(cli)

else_error <- function() {
  cli::cli_abort(
    paste0(
      "Something has gone {.strong wrong}. ",
      "Please report this issue at <",
      pkg_vars$url_issues,
      ">."
    )
  )

  invisible(NULL)
}

# TO DO: Move the following to `rutils` package -----

# library(magrittr)

get_names <- function(...) {
  list(...) |>
    substitute() |>
    magrittr::extract(-1) |>
    lapply(deparse1) |>
    vapply(unlist, character(1))
}

get_values <- function(...) {
  list(...) |>
    lapply(function(x) ifelse(is.null(x), "NULL", x)) |>
    unlist()
}

# library(glue)

class_collapse <- function(x) {
  glue::single_quote(paste0(class(x), collapse = "/"))
}

# library(cli)
# library(glue)

collapse_names <- function(
    ..., #nolint
    deparsed = FALSE,
    color = "red",
    last = "or",
    names = NULL
  ) {
  assert_flag(deparsed)
  assert_choice(color, cli_color_choices())
  assert_choice(last, c("and", "or"))
  assert_character(names, null.ok = TRUE)

  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  . <- NULL
  # nolint end

  last <- paste0(" ", last, " ")

  if (isTRUE(deparsed)) {
    fun <- get_names
  } else {
    fun <- get_values
  }

  if (is.null(names)) {
    if (length(fun(...)) > 5) {
      names <- utils::head(fun(...), 5)
    } else {
      names <- fun(...)
    }
  }

  glue::glue_collapse(
    names |>
      glue::single_quote() %>%
      paste0(
        "{.strong {",
        cli_color_function(color),
        "(", ., ")}}"
      ),
    sep = ", ",
    last = last
  )
}

env_get_list <- function(env = parent.frame()) {
  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  . <- NULL
  # nolint end

  env |>
    ls(envir = _) |>
    mget(envir = env)
}

# library(cli)

cli_color_function <- function(color, type = "col", as_string = TRUE) {
  fun_name <- switch(
    color,
    black = paste0(type, "_black"),
    blue = paste0(type, "_blue"),
    cyan = paste0(type, "_cyan"),
    green = paste0(type, "_green"),
    magenta = paste0(type, "_magenta"),
    red = paste0(type, "_red"),
    white = paste0(type, "_white"),
    yellow = paste0(type, "_yellow")
  )

  if (isTRUE(as_string)) {
    paste0("cli::", fun_name)
  } else {
    envir <- loadNamespace("cli")

    fun_name |> get(envir = envir)
  }
}

cli_color_choices <- function() {
  c(
    "black", "blue", "cyan", "green", "magenta", "red",
    "white", "yellow"
  )
}
