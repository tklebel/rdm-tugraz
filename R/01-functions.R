# Helper functions ------
make_proportion <- function(df, var, ..., order_string = NA_character_,
                            .drop_na = FALSE) {
  counts <- df %>%
    group_by(...) %>%
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


data_sizes_storage <- c("< 10 GB", "10 GB - 99 GB", "100 GB - 999 GB", 
                        "1 TB - 10 TB", "10 TB - 100 TB", "> 100 TB", 
                        "No Answer")

data_sizes_production <- c("< 1 MB", "Up to 99 MB", "100 MB - 999 MB", 
                           "1 GB - 9 GB", "10 GB - 99 GB", "100 GB - 499 GB", 
                           "> 500 GB")

data_size_per_year <- c("0 - 99 GB", "100 GB - 499 GB", "500 GB - 900 GB", 
                   "1 TB - 5 TB", "> 5 TB")

make_univ_fig <- function(data, labels, var, sort_string, out_path, 
                          .drop_na = FALSE) {
    pdata <- data %>% 
      pivot_longer(cols = starts_with(var),
                   names_to = "var", values_to = "val") %>% 
      select(var, val) %>% 
      left_join(labels, by = "var") %>% 
      mutate(label = str_wrap(label, 45))
    
    
    title <- unique(pdata$question) %>% str_wrap()
    
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
    } else if (any(str_detect(pdata$val, "500 GB"), na.rm = T)) {
      pdata <- pdata %>% 
        mutate(val = factor(val, levels = data_sizes_storage))
    } else if (any(str_detect(pdata$val, "99 MB"), na.rm = T)) {
      pdata <- pdata %>% 
        mutate(val = factor(val, levels = data_sizes_production))
    }
    
    
    
    if (.drop_na) {
      pdata <- drop_na(pdata)
    } else {
      pdata$val <- fct_explicit_na(pdata$val)
    }
  
    
    pdata <- pdata %>% 
      make_proportion(val, label, order_string = sort_string)
    
    p <- pdata %>% 
      ggplot(aes(fct_reorder(label, order), prop, fill = val)) +
      geom_chicklet(width = .7) +
      scale_y_continuous(labels = percent) +
      scale_fill_brewer(palette = "Dark2") +
      coord_flip() +
      theme_ipsum(base_family = "Hind") +
      labs(x = NULL, y = NULL, fill = NULL, 
           title = title) +
      theme(legend.position = "top", plot.title.position = "plot")
    
    
    n_vars <- pdata$label %>% unique() %>% length()

    ggsave(out_path, p, width = 10, height = 1.5 + sqrt(n_vars * 4))
    
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
    geom_chicklet(width = .6) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_ipsum(base_family = "Hind", base_size = 15,
                plot_title_size = 25, plot_margin = margin()) +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = "How frequently do you/does your group share data...") +
    theme(legend.position = "top", plot.title.position = "plot")
  
  ggsave(out_path, p, width = width, height = height, dpi = 400)
}

create_data_sharing_cat <- function(data, labels, out_path) {
  base_data <- data %>% 
    pivot_longer(cols = starts_with("DHRP05"),
                 names_to = "var", values_to = "val") %>% 
    select(var, val) %>% 
    left_join(labels, by = "var") %>% 
    mutate(label = str_remove_all(label, "share data "),
           label = str_wrap(label, 25)) %>% 
    mutate(val = case_when(
      str_detect(val, "Never") ~ "No",
      str_detect(val, "know") ~ NA_character_,
      TRUE ~ "Yes"
    ) %>% factor(levels = c("Yes", "No"))) %>% 
    # dropping missing values is ok, since there are few of them and they are
    # almost equally distributed among the three questions
    drop_na(val)
    
  
  pdata <- base_data %>% 
    make_proportion(val, label, order_string = "Yes")
  
  
  p <- pdata %>% 
    ggplot(aes(fct_reorder(label, order), prop, fill = val)) +
    geom_chicklet(width = .6) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_ipsum(base_family = "Hind", base_size = 15,
                plot_title_size = 25) +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = "Do you/does your group share data...") +
    theme(legend.position = "top", plot.title.position = "plot")
  
  ggsave(out_path, p, width = 10, height = 7)
}


create_data_type <- function(data, labels, out_path) {
  pdata <- data %>% 
    pivot_longer(cols = starts_with("DT01"),
                 names_to = "var", values_to = "val") %>% 
    select(var, val, D06) %>% 
    filter(val %in% c("Yes", "No")) %>% 
    make_proportion(val, D06, var, order_string = "Yes", .drop_na = T) %>% 
    left_join(labels, by = "var") 
  
  title <- unique(pdata$question) %>% str_wrap()
  
  p <- pdata %>% 
    ggplot(aes(fct_rev(str_wrap(D06, 40)), prop, fill = fct_rev(val))) +
    geom_chicklet(width = .7) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    facet_wrap(vars(label), ncol = 3) +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = title) +
    theme(legend.position = "top", plot.title.position = "plot")
  
  
  ggsave(out_path, p, width = 12, height = 16)
  
}

create_data_type2 <- function(data, labels, out_path) {
  pdata <- data %>% 
    pivot_longer(cols = starts_with("DT02"),
                 names_to = "var", values_to = "val") %>% 
    select(var, val, D06) %>% 
    filter(val %in% c("Yes", "No")) %>% 
    make_proportion(val, D06, var, order_string = "Yes", .drop_na = T) %>% 
    left_join(labels, by = "var") 
  
  title <- unique(pdata$question) %>% str_wrap()
  
  p <- pdata %>% 
    ggplot(aes(fct_rev(str_wrap(D06, 40)), prop, fill = fct_rev(val))) +
    geom_chicklet(width = .7) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    facet_wrap(vars(label), ncol = 3) +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = title) +
    theme(legend.position = "top", plot.title.position = "plot")
  
  
  ggsave(out_path, p, width = 12, height = 10)
  
}

create_data_size <- function(data, labels, out_path) {
  pdata <- data %>% 
    select(DQ01, D06) %>% 
    make_proportion(DQ01, D06, order_string = "MB", .drop_na = F) %>% 
    mutate(DQ01 = factor(DQ01, levels = data_sizes_production)) %>% 
    filter(!is.na(D06))
  
  title <- "How big are the data sets you work with in a typical research project?"
  
  p <- pdata %>% 
    ggplot(aes(fct_reorder(str_wrap(D06, 40), order), prop, fill = DQ01)) +
    geom_chicklet(width = .7) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = title) +
    theme(legend.position = "top", plot.title.position = "plot")
  
  
  ggsave(out_path, p, width = 9, height = 6)
  
}


create_data_per_year <- function(data, labels, out_path) {
  pdata <- data %>% 
    select(DQ02, D06) %>% 
    make_proportion(DQ02, D06, order_string = "99", .drop_na = T) %>% 
    mutate(DQ02 = factor(DQ02, levels = data_size_per_year)) %>%
    filter(!is.na(D06))
  
  title <- "How much data do you handle in the course of your research,\non average, per year?"
  
  p <- pdata %>% 
    ggplot(aes(fct_reorder(str_wrap(D06, 40), order), prop, fill = DQ02)) +
    geom_chicklet(width = .7) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = title, subtitle = "This includes all data used as input to or output of your research.") +
    theme(legend.position = "top", plot.title.position = "plot")
  
  
  ggsave(out_path, p, width = 9, height = 6)
  
}

create_data_per_year_cat <- function(data, labels, out_path) {
  pdata <- data %>% 
    select(DQ02, D06) %>% 
    mutate(DQ02 = case_when(
      str_detect(DQ02, "99 GB") ~ "< 500 GB",
      TRUE ~ ">= 500 GB"
    )) %>% 
    make_proportion(DQ02, D06, order_string = "<", .drop_na = T) %>% 
    # mutate(DQ02 = factor(DQ02, levels = data_size_per_year)) %>% 
    filter(!is.na(D06))
  
  title <- "How much data do you handle in the course of your research,\non average, per year?"
  
  p <- pdata %>% 
    ggplot(aes(fct_reorder(str_wrap(D06, 40), order), prop, fill = DQ02)) +
    geom_chicklet(width = .7) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = title) +
    theme(legend.position = "top", plot.title.position = "plot")
  
  
  ggsave(out_path, p, width = 9, height = 6)
  
}


