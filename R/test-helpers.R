expect_snapshot_skeleton <-
  function(
    name
  ) {
    path <-
      write_metadata_skeleton(
        source_vars = tibble::tibble(source_varname = names(gss_demo)),
        outfile = tempfile(),
        source_varname_col = source_varname,
        num_exprs = 5
      )
    announce_snapshot_file(name = name)
    expect_snapshot_file(path, name)
  }
