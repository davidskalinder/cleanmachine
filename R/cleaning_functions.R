clean_column <- function(dataset, name, type_fn_working,
                         oldexpr1, newexpr1,
                         oldexpr2, newexpr2,
                         oldexpr3, newexpr3,
                         oldexpr4, newexpr4,
                         oldexpr5, newexpr5, ...) {
  name <- rlang::sym(name)
  # parse_expr business allows either values or expressions in sheet
  old1 <- rlang::expr(eval(rlang::parse_expr(as.character(oldexpr1))))
  old2 <- rlang::expr(eval(rlang::parse_expr(as.character(oldexpr2))))
  old3 <- rlang::expr(eval(rlang::parse_expr(as.character(oldexpr3))))
  old4 <- rlang::expr(eval(rlang::parse_expr(as.character(oldexpr4))))
  old5 <- rlang::expr(eval(rlang::parse_expr(as.character(oldexpr5))))
  new1 <- rlang::expr(rlang::exec(type_fn_working,
                                  eval(rlang::parse_expr(as.character(newexpr1)))))
  new2 <- rlang::expr(rlang::exec(type_fn_working,
                                  eval(rlang::parse_expr(as.character(newexpr2)))))
  new3 <- rlang::expr(rlang::exec(type_fn_working,
                                  eval(rlang::parse_expr(as.character(newexpr3)))))
  new4 <- rlang::expr(rlang::exec(type_fn_working,
                                  eval(rlang::parse_expr(as.character(newexpr4)))))
  new5 <- rlang::expr(rlang::exec(type_fn_working,
                                  eval(rlang::parse_expr(as.character(newexpr5)))))

  dplyr::transmute(dataset, !!name := dplyr::case_when(!!old1 ~ !!new1,
                                                       !!old2 ~ !!new2,
                                                       !!old3 ~ !!new3,
                                                       !!old4 ~ !!new4,
                                                       !!old5 ~ !!new5,
                                                       TRUE ~ !!name))
}

apply_cleaning_rules <- function(dataset, varinfo) {
  # Clean up varinfo data ----
  varinfo <- dplyr::filter(varinfo,!is.na(name))
  varinfo <-
    dplyr::mutate(varinfo,
                  type_fn_final = stringr::str_c("as.", final_data_type))
  varinfo <-
    dplyr::mutate(varinfo,
                  type_fn_working = stringr::str_c("as.", cleaning_data_type))
  # mutate(type_fn_working = case_when(data_type == "factor" ~ "as.integer",
  #                                    TRUE                  ~ type_fn_final))

  # Rename and trim ----
  dataset <-
    purrr::pmap_dfc(
      .l = varinfo,
      .f = function(source, name, ...)
        dplyr::select(dataset, !!name := !!source)
    )

  # Convert columns to correct types for cleaning ----
  dataset <-
    purrr::pmap_dfc(
      .l = varinfo,
      .f = function(name, type_fn_working, ...)
        dplyr::transmute(dataset, !!name := rlang::exec(
          type_fn_working, !!rlang::sym(name)
        ))
    )

  # Clean using specified operation and old and new values ----
  dataset <- purrr::pmap_dfc(.l = varinfo,
                             .f = clean_column,
                             dataset = dataset)


  # Convert columns to correct final types ----
  dataset <-
    purrr::pmap_dfc(
      .l = varinfo,
      .f = function(name, type_fn_final, ...)
        dplyr::transmute(dataset, !!name := rlang::exec(
          type_fn_final, !!rlang::sym(name)
        ))
    )

  dataset
}
