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

prefix_str_to_type_fns <- function(varinfo, str = "as.") {
  varinfo |>
    dplyr::mutate(
      type_fn_final = stringr::str_c("as.", final_data_type),
      type_fn_working = stringr::str_c("as.", cleaning_data_type)
    )
}

rename_sources <- function(dataset, varinfo) {
  # Rename and trim
  purrr::pmap(
    .l = varinfo,
    .f = function(source, name, ...) dplyr::select(dataset, !!name := !!source)
  ) |>
    purrr::list_cbind()
}

convert_to_working_type <- function(dataset, varinfo) {
  # Convert columns to correct types for cleaning
  purrr::pmap(
    .l = varinfo,
    .f = function(name, type_fn_working, ...) {
      dataset |>
        dplyr::mutate(
          !!name := rlang::exec(type_fn_working, !!rlang::sym(name)),
          .keep = "none"
        )
    }
  ) |>
    purrr::list_cbind()
}

clean_columns <- function(dataset, varinfo) {
  # Clean using specified operation and old and new values
  purrr::pmap(.l = varinfo, .f = clean_column, dataset = dataset) |>
    purrr::list_cbind()
}

convert_to_final_type <- function(dataset, varinfo) {
  # Convert columns to correct final types
  purrr::pmap_dfc(
    .l = varinfo,
    .f = function(name, type_fn_final, ...) {
      dataset |>
        dplyr::mutate(
          !!name := rlang::exec(type_fn_final, !!rlang::sym(name)),
          .keep = "none"
        )
    }
  )
}

#' Apply cleaning rules
#'
#' Note that \code{cleaning_data_type} is applied to each column before *and*
#' after cleaning.  Then \code{final_data_type} is applied just before output.
#' This behavior should be restructured in future versions.
#'
#' @param dataset A dataset
#' @param varinfo Variable info for the dataset
#'
#' @return The cleaned dataset.
#' @export
apply_cleaning_rules <- function(dataset, varinfo, type_fn_prefix = "as.") {
  # Clean up varinfo data ----
  varinfo <-
    varinfo |>
    dplyr::filter(!is.na(name)) |>
    prefix_str_to_type_fns(type_fn_prefix)

  dataset <-
    dataset |>
    rename_sources(varinfo) |>
    convert_to_working_type(varinfo) |>
    clean_columns(varinfo) |>
    convert_to_final_type(varinfo)

  dataset
}
