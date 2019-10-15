clean_data <- function(raw_data) {
  raw_data %>% 
    set_names(., str_replace_all(names(.), "\\[|\\]", "_"))
}


make_proportion <- function(df, var, group, order_string = NA_character_,
                            .drop_na = FALSE) {
  counts <- df %>%
    group_by({{group}}) %>%
    count({{var}})
  
  if (.drop_na) {
    counts <- drop_na(counts)
  }
    
  counts %>% 
    mutate(
      prop = n/sum(n),
      order = case_when(str_detect({{var}}, order_string) ~ prop,
                        TRUE ~ 0),
      order = sum(order)
    )
}

create_rda_fig <- function(data, out_path, width = 10, height = 5) {
  pdata <- data %>% 
    pivot_longer(cols = starts_with("DHRP05"),
                 names_to = "var", values_to = "val") %>% 
    mutate(var_recoded = case_when(
      str_detect(var, "01") ~ "...share data to an institutional repository or data center (provided by TU Graz)",
      str_detect(var, "02") ~ "...share data to a non-institutional repository or data center (e.g. arXiv, GitHub)",
      str_detect(var, "03") ~ "...share data as a supplement or appendix to a publication",
      str_detect(var, "04") ~ "...share data through a stand-alone data publication",
    ) %>% str_wrap(50)) %>%
    # dropping missing values is ok, since there are few of them and they are
    # almost equally distributed among the three questions
    drop_na(val) %>% 
    mutate(val = factor(val, levels = c(
      "Always, or almost always", "Most of the time", "Sometimes", 
      "Rarely", "Never, or almost never", "Do not know/cannot answer"))) %>% 
    make_proportion(val, var_recoded, order_string = "Sometimes")
  
  
  p <- pdata %>% 
    ggplot(aes(fct_reorder(var_recoded, order), prop, fill = val)) +
    ggchicklet::geom_chicklet(width = .7) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    hrbrthemes::theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = "How frequently do you/does your group...") +
    theme(legend.position = "top")
  
  ggsave(out_path, p, width = width, height = height, dpi = 400)
}

