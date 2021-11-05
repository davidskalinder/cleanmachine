source_vars <- gss_demo %>%
  names() %>%
  tibble::tibble(source_varname = .) %>%
  dplyr::mutate(sv_lens = stringr::str_length(source_varname))

tmp <- tempfile()

write_metadata_skeleton(
  source_vars = source_vars,
  outfile = tmp,
  source_varname_col = source_varname,
  num_exprs = 5
)

var_metadata_skeleton <- readr::read_csv(tmp)

test_that("skeleton is good", {
  expect_equal(
    var_metadata_skeleton %>% dplyr::pull(source_varname),
    gss_demo %>% names()
  )
})
