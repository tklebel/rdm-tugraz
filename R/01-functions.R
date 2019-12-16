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

propensity <- c(
  "Always, or almost always", "Most of the time", "Sometimes",
  "Rarely", "Never, or almost never", "Do not know/cannot answer"
)


group_size <- c(
  "Fewer than 5 people", paste(c("5-9", "10-19", "20-29"), "people"),
  "30 or more people"
)

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

set_factors <- function(data) {
  data %>% 
    mutate(DQ04 = factor(DQ04, levels = c(
      "Fewer than 5 people", paste(c("5-9", "10-19", "20-29"), "people"),
      "30 or more people")))
}

make_labels <- function(labels) {
  read_csv(labels, n_max = 1,
           col_types = cols(
             .default = col_character()
           )) %>% 
    # using pivot_longer with the following spec does not work, since unmatched
    # patterns lead to NA. Maybe a better regex could solve this. for now, 
    # explicitely using extract fixes the problem.
    #   # pivot_longer(everything(), names_to = c("var", "question", "label"), 
    #                  names_pattern = "(.*)_(.*)\\s\\[(.*)\\]") %>% 
    pivot_longer(everything()) %>% 
    extract(name, into = c("var", "question"),
            regex = "(.*)_(.*)") %>% 
    extract(question, into = c("new", "label"),
            regex = "(.*)\\s\\[(.*)\\]", remove = FALSE) %>% 
    mutate(question = coalesce(new, question)) %>% 
    mutate(var = str_replace_all(var,  "\\[|\\]", "_")) %>% 
    select(-value, -new) %>% 
    # clean a few labels
    mutate(label = str_replace(label, "&lt;", "<"),
           label = str_replace(label, "&gt;", ">"))
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

create_data_reuse <- function(data, labels, by, order_string = "Alwa|Most|Some", 
                              wrap = T, out_path) {
  pdata <- data %>% 
    select(DHRP03b_SQ003_, {{by}}) %>% 
    make_proportion(DHRP03b_SQ003_, {{by}}, order_string = order_string, .drop_na = F) %>% 
    mutate(DHRP03b_SQ003_ = forcats::fct_explicit_na(DHRP03b_SQ003_) %>% 
             factor(levels = c(propensity, "(Missing)"))) %>%
    filter(!is.na({{by}})) %>% 
    ungroup()
  
  if (wrap) {
    pdata <- pdata %>% 
      mutate_at(vars({{by}}), str_wrap, 40)
  }
  
  if (str_length(order_string) > 0) {
    pdata <- pdata %>% 
      mutate({{by}} := fct_reorder({{by}}, order))
    
    caption <- "Faculties ordered by 'Always, or almost always' & 'Most of the time' & 'Sometimes'"
  } else {
    caption <- NULL
  }
  
  
  title <- "During a project, how frequently do you/does your group\nreuse data from third parties?"
  
  p <- pdata %>% 
    ggplot(aes({{by}}, prop, fill = DHRP03b_SQ003_)) +
    geom_chicklet(width = .7) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = title, 
         caption = caption) +
    theme(legend.position = "top", plot.title.position = "plot")
  
  
  ggsave(out_path, p, width = 9, height = 6)
  
}

create_data_reuse2 <- function(data, labels, by, sort_y = TRUE, out_path) {
  pdata <- data %>% 
    filter(!DHRP03b_SQ003_ %in% c("Never, or almost never", 
                                  "Do not know/cannot answer",
                                  NA_character_)) %>% 
    pivot_longer(cols = starts_with("DHRP03c"),
                 names_to = "var", values_to = "val") %>% 
    select(var, val, {{by}}) %>% 
    make_proportion(val, {{by}}, var, order_string = "Alwa|Most|Some", .drop_na = F) %>% 
    left_join(labels, by = "var") %>% 
    ungroup() %>% 
    mutate(val = val %>% 
             forcats::fct_explicit_na() %>% 
             factor(levels = c(propensity, "(Missing)"))) %>% 
    drop_na()
    
    
  
  title <- unique(pdata$question) %>% str_wrap()
  
  if (sort_y) {
    p <- pdata %>% 
      ggplot(aes(tidytext::reorder_within(str_wrap({{by}}, 40), order, label), prop, 
                 fill = val)) 
    
    caption <- "Y-axis ordered by 'Always, or almost always' & 'Most of the time' & 'Sometimes'"
  } else {
    p <- pdata %>% 
      ggplot(aes({{by}}, prop, fill = val)) 
    
    caption <- NULL
  }
  
  p <- p + 
    geom_chicklet(width = .7) +
    scale_y_continuous(labels = percent) +
    tidytext::scale_x_reordered() + 
    scale_fill_brewer(palette = "Dark2") +
    facet_wrap(vars(label), ncol = 1, scales = "free_y") +
    coord_flip() +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = title,
         caption = caption) +
    theme(legend.position = "top", plot.title.position = "plot")
  
  
  ggsave(out_path, p, width = 12, height = 16)
  
}

create_reuse_sharing <- function(data, labels, lines = TRUE, out_path) {
  pdata <- data %>% 
    select(DHRP03b_SQ003_, contains("DHRP06"), D06) %>% 
    mutate(id = seq_along(DHRP03b_SQ003_)) %>% 
    tidyr::gather(var, val, -id, -D06) %>% 
    left_join(labels, by = "var") %>% 
    mutate(val_new = paste(label, "_", val),
           val_new = str_replace(val_new, "...sharing research data", "sharing")) %>% 
    select(id, var, val_new, D06) %>% 
    pivot_wider(names_from = var, values_from = val_new) %>% 
    select(-id) %>% 
    filter_all(all_vars(!str_detect(., "NA|answer"))) %>% 
    select(starts_with("DHR"), D06)
  
  p <- pdata %>% 
    ca::mjca(supcol = 6) %>% 
    plot_ca(lines = lines) +
    labs(title = "Opinions on data sharing & data reuse")
  
  ggsave(out_path, p, width = 14, height = 10)
  
}


create_amount_storage <- function(data, labels, lines = TRUE, out_path) {
  pdata <- data %>% 
    select(DQ01, DQ02, contains("DQ03"), D06) %>% 
    mutate(id = seq_along(DQ01)) %>% 
    tidyr::gather(var, val, -id, -D06) %>% 
    left_join(labels, by = "var") %>% 
    mutate(val_new = case_when(
             str_detect(question, "How big") ~ paste("Data size", "_", val),
             str_detect(question, "How much") ~ paste("Data produced per year", "_", val),
             TRUE ~ paste(label, "_", val)
           )) %>% 
    select(id, var, val_new, D06) %>% 
    pivot_wider(names_from = var, values_from = val_new) %>% 
    select(-id) %>% 
    filter_all(all_vars(!str_detect(., "NA"))) %>% 
    select(starts_with("DQ0"), D06) 
  
  p <- pdata %>% 
    ca::mjca(supcol = 5) %>% 
    plot_ca(lines = lines) +
    labs(title = "Opinions on data production and storage")
  
  ggsave(out_path, p, width = 16, height = 10)
  
}

create_data_amount2 <- function(data, labels, by, sort_y = TRUE, out_path) {
  pdata <- data %>% 
    pivot_longer(cols = starts_with("DQ03"),
                 names_to = "var", values_to = "val") %>% 
    select(var, val, {{by}}) %>% 
    make_proportion(val, {{by}}, var, order_string = "GB", .drop_na = F) %>% 
    left_join(labels, by = "var") %>% 
    ungroup() %>% 
    mutate(val = val %>% 
             forcats::fct_explicit_na() %>% 
             factor(levels = c(data_sizes_storage, "(Missing)"))) %>% 
    drop_na()
  
  
  
  title <- unique(pdata$question) %>% str_wrap()
  
  if (sort_y) {
    p <- pdata %>% 
      ggplot(aes(tidytext::reorder_within(str_wrap({{by}}, 40), order, label), prop, 
                 fill = val)) 
    
    caption <- "Y-axis ordered by GB vs. TB"
  } else {
    p <- pdata %>% 
      ggplot(aes({{by}}, prop, fill = val)) 
    
    caption <- NULL
  }
  
  p <- p + 
    geom_chicklet(width = .7) +
    scale_y_continuous(labels = percent) +
    tidytext::scale_x_reordered() + 
    scale_fill_brewer(palette = "Dark2") +
    facet_wrap(vars(label), ncol = 1, scales = "free_y") +
    coord_flip() +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = title,
         caption = caption) +
    theme(legend.position = "top", plot.title.position = "plot")
  
  
  ggsave(out_path, p, width = 12, height = 10)
  
}


create_data_amount <- function(data, labels, out_path) {
  base_data <- data %>% 
    pivot_longer(cols = starts_with("DQ03"),
                 names_to = "var", values_to = "val") %>% 
    select(var, val) %>% 
    left_join(labels, by = "var") %>% 
    mutate(label = str_wrap(label, 25),
           val = fct_explicit_na(val)) %>% 
    # # dropping missing values is ok, since there are few of them and they are
    # # almost equally distributed among the three questions
    # drop_na(val) %>% 
    mutate(val = factor(val, levels = c(data_sizes_storage, "(Missing)")))
  
  pdata <- base_data %>% 
    make_proportion(val, label, order_string = "GB")
  
  
  p <- pdata %>% 
    ggplot(aes(fct_reorder(label, order), prop, fill = val)) +
    geom_chicklet(width = .6) +
    scale_y_continuous(labels = percent) +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    theme_ipsum(base_family = "Hind") +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = str_wrap("Sometimes, data need to be stored only for the duration of a project, sometimes data need to be kept longer. Of the data you generate annually, how much would you say requires...")) +
    theme(legend.position = "top", plot.title.position = "plot")
  
  ggsave(out_path, p, width = 10, height = 5)
}

create_sample_overview <- function(data, out_path) {
  
  total_sample <- tribble(
    ~D06,                                                                ~n_total,
    "Architecture",                                                        116,
    "Civil Engineering Sciences",                                          200,
    "Computer Science and Biomedical Engineering",                         287,
    "Electrical and Information Engineering",                              244,
    "Mathematics, Physics, and Geodesy",                                   219,
    "Mechanical Engineering and Economic Sciences",                        392,
    "Technical Chemistry, Chemical and Process Engineering, Biotechnology",298
  )
  
  p <- data %>% 
    count(D06) %>% 
    drop_na() %>% 
    left_join(total_sample, by = "D06") %>% 
    mutate(prop = n/n_total) %>% 
    ggplot(aes(fct_reorder(str_wrap(D06, 40), prop), prop)) +
    geom_col(width = .7) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    labs(y = NULL, x = NULL, title = "Sample Coverage") +
    theme_ipsum(base_family = "Hind") 
  
  ggsave(out_path, p, width = 7, height = 5)  
}
