make_check <- function(type, family, arg_names = NULL) {
  assert_check_funs(family)

  base_formals <- formals(paste0("test_", family))

  dots <- "..." %in% names(base_formals)
  sep <- ", "

  fun_exp <- str2expression(
    glue::glue(
      "
      function({ifelse(dots, '...', '')}) {{
        obj_list <- env_get_list()

        {
          ifelse(
            isTRUE(dots),
            'dots_list <- list(...)',
            ''
          )
        }

        test <- do.call(
          '{paste0('test_', family)}',
          {ifelse(dots, 'c(dots_list, obj_list)', 'obj_list')}
        )

        if (isFALSE(test)) {{
          do.call(
            '{paste0('message_', family)}',
            {
              ifelse(
                isTRUE(dots),
                'c(dots_list, obj_list, list(names = get_names(...)))',
                paste0(
                  'c(',
                    'obj_list, ',
                    'list(names = ',
                      'c(',
                        #paste0(
                        # glue::double_quote(arg_names),
                        #  collapse = ', '
                        #),
                        paste0(
                          'paste0(',
                            'deparse(substitute(',
                            arg_names,
                            ')), ',
                            'collapse = ',
                            glue::double_quote(sep),
                          ')'
                        ),
                      ')',
                    ')',
                  ')'
                )
              )
            }
          ) |>
            {
              ifelse(
                type == 'assert' || type == 'expect',
                'cli::cli_abort()',
                'cli::cli_alert_warning()'
              )
            }
        }}
      }}
      "
    )
  )

  out <- eval(fun_exp)

  formals(out) <- formals(get(paste0("test_", family)))

  out
}

assert_check_funs <- function(family) {
  test_fun_name <- paste0("test_", family)
  message_fun_name <- paste0("message_", family)

  for (i in c(test_fun_name, message_fun_name)) {
    if (!exists(i)) {
      cli::cli_abort(
        paste0(
          "Function ",
          "{.strong {cli::col_red(i)}} ",
          "does not exist."
        )
      )
    }
  }
}
