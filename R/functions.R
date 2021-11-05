# write_metadata_skeleton ----

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

