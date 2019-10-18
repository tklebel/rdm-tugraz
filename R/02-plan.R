plan <- drake_plan(
  raw_data = read_csv(
    file_in("data/results-survey896447.csv"),
    col_types = cols(
      .default = col_character(),
      id = col_double(),
      submitdate = col_datetime(format = ""),
      lastpage = col_double(),
      seed = col_double(),
      `DT03[other]` = col_logical()
    )),
  data = raw_data %>% 
    clean_data(),
  rda_fig = create_rda_fig(data, file_out("figs/data_sharing.png")),
  test_fig = create_test_fig(data, file_out("figs/test.png"))
)
