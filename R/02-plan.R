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
  # this could then be generalized by using `crossing`, and using paste to 
  # create out files. so we would have a single function that generalizes 
  # `create_test_fig` to a range of figures. Problematic though: this will
  # not allow for the necessary degree of customization. 
  # For printing, I will probably end up with single functions per single fig.
  # Maybe then not to spend too much time on getting this first version right, 
  # as it is only for exploratory purposes
  test_fig = create_test_fig(data, file_out("figs/test.png"))
)
