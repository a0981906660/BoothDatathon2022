library(tidyverse)

df_states <- read_csv("./data/raw/blue_red_states.csv")
df_states %>% str()
df_states$status_new %>% unique()
df_states$last_change

df_states <- df_states %>% 
  mutate(red = ifelse(status_new == "Republican trifecta", 1, 0),
         blue = ifelse(status_new == "Democratic trifecta", 1, 0),
         purple = ifelse(status_new == "Divided government", 1, 0)
         )

df_states %>% write_csv("./data/cleaned/blue_red_states.csv")
