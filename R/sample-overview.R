df <- readd(data)



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

p1 <- df %>% 
  count(D06) %>% 
  drop_na() %>% 
  left_join(total_sample) %>% 
  mutate(prop = n/n_total) %>% 
  ggplot(aes(fct_reorder(str_wrap(D06, 40), prop), prop)) +
  geom_col(width = .7) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = NULL, x = NULL, title = "Sample Coverage") +
  theme_ipsum(base_family = "Hind") 

ggsave("figs/final/sample_coverage.png", p1, width = 7, height = 5)  


