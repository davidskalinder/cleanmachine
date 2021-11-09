# write_metadata_skeleton ----

#' Write a skeleton metadata file
#'
#' @param source_vars A data frame with one row per source variable.
#' @param outfile Destination of the skeleton file.
#' @param source_varname_col Tidy-select column in \code{source_vars} with the
#'   name of the variable.
#' @param num_exprs Number of new/old expression cols to include in file.
#'
#' @return Nothing, called only for side effects.
#' @export
#'
#' @examples
#' write_metadata_skeleton(
#'   source_vars = tibble::tibble(source_varname = names(gss_demo)),
#'   outfile = tempfile(),
#'   source_varname_col = source_varname,
#'   num_exprs = 5
#' )
write_metadata_skeleton <-
  function(
    source_vars,
    outfile,
    source_varname_col,
    num_exprs = 5
  ) {
    if (file.exists(outfile)) {
      return(invisible(source_vars))
    }
    source_vars %>%
      dplyr::rename(source_varname = {{ source_varname_col }}) %>%
      dplyr::mutate(name = "",
                    cleaning_data_type = "",
                    final_data_type = "",
                    oldexpr1 = "",
                    newexpr1 = "",
                    oldexpr2 = "",
                    newexpr2 = "",
                    oldexpr3 = "",
                    newexpr3 = "",
                    oldexpr4 = "",
                    newexpr4 = "",
                    oldexpr5 = "",
                    newexpr5 = "",
      ) %>%
      readr::write_csv(outfile)
    invisible(source_vars)
  }

