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

make_univ_fig <- function(data, labels, var, .drop_na, order_string) {
    pdata <- data %>% 
      pivot_longer(cols = starts_with(var),
                   names_to = "var", values_to = "val") %>% 
      select(var, val) %>% 
      left_join(labels, by = "var") %>% 
      mutate(label = str_wrap(label, 25))
    
    
    title <- unique(pdata$question)
    
    # stop if we have multiple titles for unknown reason
    stopifnot(identical(length(title), 1L))
    

    # determine type of response category and recode accordingly
    if (any(str_detect(pdata$val, "Always, or"), na.rm = T)) {
      pdata <- pdata %>% 
        mutate(val = factor(val, levels = c(
          "Always, or almost always", "Most of the time", "Sometimes",
          "Rarely", "Never, or almost never", "Do not know/cannot answer")))
    } else if (any(str_detect(pdata$val, "Completely agree"), na.rm = T)) {
      pdata <- pdata %>% 
        mutate(val = factor(val, levels = c(
          "Completely agree", "Tend to agree", "Tend to disagree",
          "Completely disagree", "Do not know/cannot answer")))
    } else if (any(str_detect(pdata$val, "To a "), na.rm = T)) {
      pdata <- pdata %>% 
        mutate(val = factor(val, levels = c(
          "To a very large extent", "To a large extent", "To some extent",
          "To little or no extent at all", "Do not know/cannot answer")))
    }
    
    
    
    if (.drop_na) {
      pdata <- drop_na(pdata)
    } else {
      pdata$val <- fct_explicit_na(pdata$val)
    }
  
    
    pdata <- pdata %>% 
      make_proportion(val, label, order_string = order_string)
    
    pdata %>% 
      ggplot(aes(fct_reorder(label, order), prop, fill = val)) +
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

make_labels <- function(labels) {
  read_csv(labels, n_max = 1,
           col_types = cols(
             .default = col_character()
           )) %>% 
    pivot_longer(everything(), names_to = c("var", "question", "label"), 
                 names_pattern = "(.*)_(.*)\\s\\[(.*)\\]") %>% 
    mutate(var = str_replace_all(var,  "\\[|\\]", "_")) %>% 
    select(-value)
}

create_rda_fig <- function(data, labels, out_path, width = 10, height = 7) {
  base_data <- data %>% 
    pivot_longer(cols = starts_with("DHRP05"),
                 names_to = "var", values_to = "val") %>% 
    select(var, val) %>% 
    left_join(labels, by = "var") %>% 
    mutate(label = str_remove_all(label, "share data "),
           label = str_wrap(label, 25)) %>% 
    # dropping missing values is ok, since there are few of them and they are
    # almost equally distributed among the three questions
    drop_na(val) %>% 
    mutate(val = factor(val, levels = c(
      "Always, or almost always", "Most of the time", "Sometimes", 
      "Rarely", "Never, or almost never", "Do not know/cannot answer")))
  
  pdata <- base_data %>% 
    make_proportion(val, label, order_string = "Sometimes")
  
  
  p <- pdata %>% 
    ggplot(aes(fct_reorder(label, order), prop, fill = val)) +
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

save_univ_fig <- function(data, labels, var, sort_string, out_path, 
                          .drop_na = F) {
  p <- make_univ_fig(data, labels, var, .drop_na = .drop_na, sort_string)
  # p <- make_univ_fig(data, labels, "DHRP05", .drop_na = T, "Sometimes")
  
  ggsave(file_out(file.path("figs/descriptive", paste0(var, ".png"))), p, 
         width = 10, height = 7, dpi = 300)
}




