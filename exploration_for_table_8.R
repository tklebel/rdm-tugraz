df <- readd(data)
df %>% 
  select(DHRP06_SQ004_, ORDM02_SQ007_) %>% 
  mutate_all(as.factor) %>% 
  filter(!str_detect(DHRP06_SQ004_, "Don't"), !str_detect(ORDM02_SQ007_, "Do n")) %>% 
  drop_na() %>% 
  count(DHRP06_SQ004_, ORDM02_SQ007_) %>% 
  spread(ORDM02_SQ007_, n)


new_res <- df %>% 
  select(DHRP06_SQ004_, ORDM02_SQ007_) %>% 
  mutate_all(as.factor) %>% 
  filter(!str_detect(DHRP06_SQ004_, "Don't"), !str_detect(ORDM02_SQ007_, "Don")) %>% 
  drop_na() %>% 
  summarise(res = list(chisq.test(DHRP06_SQ004_, ORDM02_SQ007_))) 


  ggplot(aes(ORDM02_SQ007_, DHRP06_SQ004_)) +
  geom_jitter(width = .1, height = .1)
  summarise(corr = cor(DHRP06_SQ004_, ORDM02_SQ007_, method = "spearman", 
                       use = "pairwise.complete.obs"))

  
  
table(df$ORDM02_SQ007_, df$DHRP06_SQ004_)

# try with collapsing categories
recoded_data <- df %>% 
  select(sharing = DHRP06_SQ004_, time = ORDM02_SQ007_) %>% 
  mutate(sharing = case_when(str_detect(sharing, "\\sagree") ~ "agree",
                             str_detect(sharing, "\\sdisag") ~ "disagree",
                             TRUE ~ NA_character_),
         time = case_when(str_detect(time, "large") ~ "large extent",
                          str_detect(time, "some|little") ~ "some or little or no",
                          TRUE ~ NA_character_))

recoded_data %>% 
  count(sharing, time) %>% 
  drop_na() %>% 
  spread(time, n)
  

recoded_data %>% 
  summarise(p = chisq.test(time, sharing)$p.value)
