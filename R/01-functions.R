# Helper functions ------
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

make_univ_fig <- function(data, var, .drop_na, order_string,
                         title) {
    pdata <- data %>% 
      pivot_longer(cols = starts_with(var),
                   names_to = "var", values_to = "val") %>% 
      select(var, val) 
    
    if (.drop_na) pdata <- drop_na(pdata)
    
    
    # determine type of response category and recode accordingly
    if (str_detect(pdata$val, "Always, or")) {
      pdata <- pdata %>% 
        mutate(val = factor(val, levels = c(
          "Always, or almost always", "Most of the time", "Sometimes",
          "Rarely", "Never, or almost never", "Do not know/cannot answer")))
    } else if (str_detect(pdata$val, "Completely agree")) {
      pdata <- pdata %>% 
        mutate(val = factor(val, levels = c(
          "Completely agree", "Tend to agree", "Tend to disagree",
          "Completely disagree", "Do not know/cannot answer")))
    } else if (str_detect(pdata$val, "To a ")) {
      pdata <- pdata %>% 
        mutate(val = factor(val, levels = c(
          "To a very large extent", "To a large extent", "To some extent",
          "To little or no extent at all", "Do not know/cannot answer")))
    }
  
    
    pdata <- pdata %>% 
      make_proportion(val, var, order_string = order_string)
    
    pdata %>% 
      ggplot(aes(fct_reorder(var, order), prop, fill = val)) +
      ggchicklet::geom_chicklet(width = .7) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_brewer(palette = "Dark2") +
      coord_flip() +
      hrbrthemes::theme_ipsum(base_family = "Hind") +
      labs(x = NULL, y = NULL, fill = NULL, 
           title = title) +
      theme(legend.position = "top")
    
  
}





# Functions for drake plan -----

clean_data <- function(raw_data) {
  raw_data %>% 
    set_names(., str_replace_all(names(.), "\\[|\\]", "_"))
}

create_rda_fig <- function(data, out_path, width = 10, height = 7) {
  pdata <- data %>% 
    pivot_longer(cols = starts_with("DHRP05"),
                 names_to = "var", values_to = "val") %>% 
    select(var, val) %>% 
    mutate(var_recoded = case_when(
      str_detect(var, "01") ~ "...to an institutional repository or data center (provided by TU Graz)",
      str_detect(var, "02") ~ "...to a non-institutional repository or data center (e.g. arXiv, GitHub)",
      str_detect(var, "03") ~ "...as a supplement or appendix to a publication",
      str_detect(var, "04") ~ "...through a stand-alone data publication",
    ) %>% str_wrap(25)) %>%
    # dropping missing values is ok, since there are few of them and they are
    # almost equally distributed among the three questions
    drop_na(val) %>% 
    mutate(val = factor(val, levels = c(
      "Always, or almost always", "Most of the time", "Sometimes", 
      "Rarely", "Never, or almost never", "Do not know/cannot answer"))) %>% 
    make_proportion(val, var_recoded, order_string = "Sometimes")
  
  
  p <- pdata %>% 
    ggplot(aes(fct_reorder(var_recoded, order), prop, fill = val)) +
    ggchicklet::geom_chicklet(width = .6) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    hrbrthemes::theme_ipsum(base_family = "Hind", base_size = 15,
                            plot_title_size = 25, plot_margin = margin()) +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = "How frequently do you/does your group share data...") +
    theme(legend.position = "top", plot.title.position = "plot")
  
  ggsave(out_path, p, width = width, height = height, dpi = 400)
}

create_test_fig <- function(data, out_path) {
  p <- make_univ_fig(data, "DHRP05", .drop_na = T, "Sometimes", "test-title")
  
  ggsave(out_path, p, width = 10, height = 5, dpi = 400)
}




