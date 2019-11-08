descriptives_grid <- tribble(
  ~var,        ~sort_string,
  "DQ03",      "",
  "ORDM02",    "large|some",
  "DHRP02",     "Always|Most|Sometimes",
  "DHRP03b",    "Always|Most|Sometimes",
  "DHRP03c",    "Always|Most|Sometimes",
  "DHRP04",    "Always|Most|Sometimes",
  "DHRP05",    "Sometimes",
  "DHRP06",     "\\sagree",
) %>% 
  mutate(out_path = file.path("figs/descriptive", paste0(var, ".png")))



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
    clean_data() %>% 
    set_factors(),
  labels = make_labels(
    file_in("data/label_basis.csv")
  ),
  report = rmarkdown::render(
    knitr_in("general_exploration.Rmd"),
    output_file = file_out("general_exploration.html"),
    quiet = TRUE
  ),
  report_mca = rmarkdown::render(
    knitr_in("MCA.Rmd"),
    output_file = file_out("MCA.html"),
    quiet = TRUE
  ),
  rda_fig = create_rda_fig(data, labels, file_out("figs/final/data_sharing.png")),
  data_sharing_cat = create_data_sharing_cat(data, labels, file_out("figs/final/data_sharing_cat.png")),
  descriptive_graphs = target(
    make_univ_fig(data, labels, var, sort_string, file_out(out_path)),
    transform = map(data = data, labels = labels, .data = !!descriptives_grid)
  ),
  data_type = create_data_type(data, labels, file_out("figs/final/data_type.png")),
  data_type2 = create_data_type2(data, labels, file_out("figs/final/data_type_2.png")),
  data_size = create_data_size(data, labels, file_out("figs/final/data_size.png")),
  data_per_year = create_data_per_year(data, labels, file_out("figs/final/data_per_year.png")),
  data_per_year_cat = create_data_per_year_cat(data, labels, file_out("figs/final/data_per_year_cat.png")),
  data_reuse_1a = create_data_reuse(data, labels, D06, out_path = file_out("figs/final/data_reuse_1a.png")),
  data_reuse_2a = create_data_reuse2(data, labels, D06, out_path = file_out("figs/final/data_reuse_2a.png")),
  data_reuse_1b = create_data_reuse(data, labels, DQ04, order_string = "", wrap = F,
                                    file_out("figs/final/data_reuse_1b.png")),
  data_reuse_2b = create_data_reuse2(data, labels, DQ04, sort_y = FALSE, 
                                     file_out("figs/final/data_reuse_2b.png")),
  data_reuse_sharing = create_reuse_sharing(data, labels, out_path = file_out("figs/final/reuse_sharing.png"))
)
