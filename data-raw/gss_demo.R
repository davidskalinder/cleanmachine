library(magrittr)

tmp <- tempfile(fileext = ".zip")
download.file("https://gss.norc.org/documents/stata/2004_stata.zip", tmp)
dtafile <- unz(tmp, filename = "GSS2004.dta")
gss_big <- haven::read_dta(dtafile)
unlink(tmp)
set.seed(20211104)
gss_demo <-
  gss_big %>%
  dplyr::select(seq.int(from = 1, to = length(.), by = 100)) %>%
  dplyr::slice_sample(n = 100)

usethis::use_data(gss_demo, overwrite = TRUE)
