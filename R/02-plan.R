plan <- drake_plan(
  raw_data = read_csv(
    file_in("data/results-survey896447.csv"),
    col_types = cols(
      .default = col_character(),
      id = col_double(),
      submitdate = col_datetime(format = ""),
      lastpage = col_double(),
      seed = col_double()
    )),
  data = raw_data %>% 
    clean_data(),
  labels = make_labels(
    file_in("data/label_basis.csv")
  ),
  rda_fig = create_rda_fig(data, labels, file_out("figs/final/data_sharing.png")),
  descriptive_graphs = target(
    save_univ_fig(data, labels, var, sort_string),
    transform = map(data = data, labels = labels, 
                    var = c("DHRP05", "DHRP03b"),
                    sort_string = c("Sometimes", "Sometimes"))
  )
)
