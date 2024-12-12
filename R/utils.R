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

class_collapse <- function(x) {
  glue::single_quote(paste0(class(x), collapse = "/"))
}

collapse_names <- function(
    ...,
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

equalize_values <- function(x, y) {
  x <- eval(x)
  y <- eval(y)

  if (!is.null(x) && is.null(y)) {
    x
  } else if (is.null(x) && !is.null(y)) {
    y
  } else {
    x
  }
}

remove_last <- function(x) {
  # assert_length(x, 2)

  x[-length(x)]
}

get_fun_name <- function() {
  rlang::caller_call() |>
    as.character() |>
    magrittr::extract(1)
}

get_trace_back <- function() {
  rlang::trace_back() |>
    utils::capture.output() |>
    stringr::str_extract("(?<=\\u2514\\u2500).*(?=\\()") |>
    magrittr::inset(1, "Global Enviroment") |>
    remove_last()
}

test_first_check_family <- function() {
  trace_back <- get_trace_back()

  check_family <- rlang::caller_call() |>
    as.character() |>
    magrittr::extract(1) |>
    extract_check_family()

  test <- which(
    stringr::str_detect(
      trace_back,
      paste0(
        "^prettycheck::[:]?test", "|",
        "^prettycheck::[:]?check", "|",
        "^prettycheck::[:]?assert"
      )
    )
  )

  if (length(test) == 0) {
    TRUE
  } else {
    trace_back[test[1]] |>
      extract_check_family() |>
      magrittr::equals(check_family)
  }
}

is_type_expected <- function(caller_call) {
  check_type <- caller_call |>
    as.character() |>
    magrittr::extract(1) |>
    extract_check_type()

  if (check_type == "expect") {
    TRUE
  } else {
    FALSE
  }
}

# Credits: B. Christian Kamgang
# Source: Adapted from
# <https://stackoverflow.com/questions/66329835/
# how-to-get-all-parameters-passed-into-a-function-with-their-values>

grab_fun_par <- function() {
  args_names <- ls(envir = parent.frame(), all.names = TRUE, sorted = FALSE)

  if ("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = parent.frame())
  } else {
    dots = list()
  }

  args_names <- sapply(setdiff(args_names, "..."), as.name)

  if(length(args_names)) {
    not_dots <- lapply(args_names, eval, envir = parent.frame()) 
  } else {
    not_dots <- list()
  }

  out <- c(not_dots, dots)
  
  out[names(out) != ""] 
}  

env_get_list <- function(env = parent.frame()) {
  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  . <- NULL
  # nolint end

  env %>%
    ls(envir = .) %>%
    mget(envir = env)
}

match_args <- function(fun, list) {
  match_list(formals(fun), list)
}

match_list <- function(list_1, list_2) {
  assert_multi_class(list_1, c("list", "pairlist"))
  assert_multi_class(list_2, c("list", "pairlist"))
  assert_list(as.list(list_1), names = "named")
  assert_list(as.list(list_2), names = "named")

  list_1 <- nullify_list(list_1)
  list_2 <- nullify_list(list_2)

  for (i in names(list_1)) {
    if (i %in% names(list_2)) {
      if (is.null(list_2[[i]])) {
        list_1[i] <- list(NULL)
      } else {
        list_1[[i]] <- list_2[[i]]
      }
    }
  }

  list_1 %>% magrittr::inset("...", NULL)
}

nullify_list <- function(list) {
  assert_multi_class(list, c("list", "pairlist"))
  assert_list(as.list(list), names = "named")

  for (i in names(list)) {
    if (!is.null(list[[i]]) && is.atomic(list[[i]])) {
      if (any(list[[i]] == "", na.rm = TRUE)) {
        list[i] <- list(NULL)
      }
    }
  }

  list
}

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
  c("black", "blue", "cyan", "green", "magenta", "red", "white", "yellow")
}
