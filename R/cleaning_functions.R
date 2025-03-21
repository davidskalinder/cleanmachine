parse_expr_via_str <- function(metadata_expr) {
  # allows either values or expressions in metadata spreadsheet
  rlang::parse_expr(as.character(metadata_expr))
}

parse_to_type <- function(metadata_expr, type_fn) {
  # evaluate these args to make an expr with their values (a str and an expr)
  rlang::expr(rlang::exec(!!type_fn, !!parse_expr_via_str(metadata_expr)))
}

clean_column <- function(dataset, name, type_fn_working,
                         oldexpr1, newexpr1,
                         oldexpr2, newexpr2,
                         oldexpr3, newexpr3,
                         oldexpr4, newexpr4,
                         oldexpr5, newexpr5, ...) {
  name <- rlang::sym(name)

  old1 <- parse_expr_via_str(oldexpr1)
  old2 <- parse_expr_via_str(oldexpr2)
  old3 <- parse_expr_via_str(oldexpr3)
  old4 <- parse_expr_via_str(oldexpr4)
  old5 <- parse_expr_via_str(oldexpr5)
  new1 <- parse_to_type(newexpr1, type_fn_working)
  new2 <- parse_to_type(newexpr2, type_fn_working)
  new3 <- parse_to_type(newexpr3, type_fn_working)
  new4 <- parse_to_type(newexpr4, type_fn_working)
  new5 <- parse_to_type(newexpr5, type_fn_working)

  fmlas <-
    list(c(old1, old2, old3, old4, old5), c(new1, new2, new3, new4, new5)) |>
    purrr::pmap(\(x, y) rlang::expr(!!x ~ !!y))

  dplyr::mutate(
    dataset,
    !!name := dplyr::case_when(!!!fmlas, TRUE ~ !!name),
    .keep = "none"
  )
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
#' Note that `cleaning_data_type` is applied to each column before *and*
#' after cleaning.  Then `final_data_type` is applied just before output.
#' This behavior should be restructured in future versions.
#'
#' @param dataset A dataset
#' @param varinfo Variable info for the dataset
#' @param type_fn_prefix `r lifecycle::badge("experimental")` Prefix for cleaning_data_type and final_data_type cols
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
