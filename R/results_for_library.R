library(tidyverse)
source("R/01-functions.R")

my_data <- read_csv("data/results-survey896447.csv", 
                    col_types = cols(
                      .default = col_character(),
                      id = col_double(),
                      submitdate = col_datetime(format = ""),
                      lastpage = col_double(),
                      seed = col_double(),
                      `DT03[other]` = col_logical()
                    )) %>% 
  clean_data()

my_data <- my_data %>% 
  select(DQ03_SQ001_, DQ03_SQ002_) %>% 
  mutate_all(fct_explicit_na, na_level = "No Answer")

plot_levels <- c("< 10 GB", "10 GB - 99 GB", "100 GB - 999 GB",
                 "1 TB - 10 TB", "10 TB - 100 TB", "> 100 TB",
                 "No Answer")

plot_labels <- str_remove_all(plot_levels, "\\s(?=G|TB)")


my_data %>% 
  tidyr::gather(var, val) %>% 
  # group_by(var) %>% 
  count(var, val) %>% 
  complete(var, val, fill = list(n = 0)) %>% 
  mutate(var = case_when(str_detect(var, "SQ001") ~ "... short term storage (< 3 years)",
                         TRUE ~ "... long term storage (> 3 years)")) %>% 
  mutate(val = factor(val, levels = plot_levels, labels = plot_labels)) %>% 
  ggplot(aes(val, n)) +
  geom_col(width = .7) +
  facet_wrap(~fct_rev(var)) +
  hrbrthemes::theme_ipsum(base_family = "Hind") +
  labs(title = "Annual data that needs ...", y = NULL, x = NULL)

ggsave("survey_data_storage_question.png", width = 16, height = 6)


