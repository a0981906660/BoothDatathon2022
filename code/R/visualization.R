library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

getwd()
# setwd("/Users/boyie/Programming/BoothDatathon2022")

# Load cleaned data
df_covid <- read_csv("./data/cleaned/covid_cases_by_day.csv")
df_news <- read_csv("./data/cleaned/news_loc_clean.csv")
df_mask <- read_csv("./data/cleaned/weekly_mask_intensity.csv")[, -1]
df_state <- read_csv("./data/cleaned/blue_red_states.csv")

df_mask <- df_mask %>% 
  filter(!stringr::str_detect(weeks, "No data")) %>% 
  filter(!stringr::str_detect(state, "No data")) %>% 
  filter(!stringr::str_detect(mask_intensity, "No data")) %>% 
  mutate(weeks = lubridate::ymd_hms(weeks),
         mask_intensity = as.numeric(mask_intensity)) %>% 
  filter(weeks >= "2020-01-21")

df_covid <- df_covid %>% filter(date >= "2020-01-27")

# Merge
## daily data -> weekly data
df <- df_covid %>% 
  left_join(df_mask, by = c("date" = "weeks", "state" = "state")) %>% 
  drop_na() %>% 
  left_join(df_state %>% select(state, red, blue, purple), by = "state") %>% 
  mutate(party = ifelse(red == 1, "red", 
                        ifelse(blue == 1, "blue", "purple")))

write_feather(df, "./data/cleaned/df.feather")

# sketch
## CA
df %>% 
  filter(state == "CA") %>%
  mutate(cases_per_m = (cases_per_m - mean(cases_per_m))/sd(cases_per_m) ,
         mask_intensity = (mask_intensity-mean(mask_intensity))/sd(mask_intensity) ) %>%
  mutate(cases_per_m = (cases_per_m - min(cases_per_m)),
         mask_intensity = mask_intensity - min(mask_intensity)) %>%
  ggplot() + 
  geom_rect(data = subset(df %>% filter(state == "CA"), outbreak == 1),
            aes(ymin = -Inf, ymax = Inf, xmin = date-86400*3, xmax = date+86400*3),
            alpha = 0.2, color = 'grey')+
  geom_line(aes(date, cases_per_m ), col = 'blue')+
  geom_line(aes(date, mask_intensity), col = 'black')+
  ylim(c(0, 4))

## TX
df %>% 
  filter(state == "TX") %>%
  mutate(cases_per_m = (cases_per_m - mean(cases_per_m))/sd(cases_per_m) ,
         mask_intensity = (mask_intensity-mean(mask_intensity))/sd(mask_intensity) ) %>%
  mutate(cases_per_m = (cases_per_m - min(cases_per_m)),
         mask_intensity = mask_intensity - min(mask_intensity)) %>%
  ggplot() + 
  geom_rect(data = subset(df %>% filter(state == "TX"), outbreak == 1),
            aes(ymin = -Inf, ymax = Inf, xmin = date-86400*3, xmax = date+86400*3),
            alpha = 0.2, color = 'grey')+
  geom_line(aes(date, cases_per_m ), col = 'blue')+
  geom_line(aes(date, mask_intensity), col = 'black')+
  ylim(c(0, 4))




# function
drawPlot <- function(state_abb) {
  p <- df %>% 
    filter(state == state_abb) %>%
    mutate(cases_per_m = (cases_per_m - mean(cases_per_m))/sd(cases_per_m) ,
           mask_intensity = (mask_intensity-mean(mask_intensity))/sd(mask_intensity) ) %>%
    mutate(cases_per_m = (cases_per_m - min(cases_per_m)),
           mask_intensity = mask_intensity - min(mask_intensity)) %>%
    ggplot() + 
    geom_rect(data = subset(df %>% filter(state == state_abb), outbreak == 1),
              aes(ymin = -Inf, ymax = Inf, xmin = date-86400*3, xmax = date+86400*3),
              alpha = 0.2, color = 'grey')+
    geom_line(aes(date, cases_per_m ), col = 'blue')+
    geom_line(aes(date, mask_intensity), col = 'black')+
    ylim(c(0, 4))
  
  return(p)
}

for (state_abb in df_state$state) {
  filename <- paste0("./plot/", state_abb, ".png")
  try(
    ggsave(filename = filename,
           plot = drawPlot(state_abb),
           device = "png",
           width = 6,
           height = 4,
           units = "in",
           dpi = 300
    )
  )

}

# # red states and blue states
# df_agg <- df %>% 
#   # standardize 
#   mutate(z_mask_intensity = (mask_intensity-mean(mask_intensity))/sd(mask_intensity),
#          LNcases_per_m = log(cases_per_m),
#   ) %>% 
#   group_by(date, party) %>% 
#   summarize(z_mask_intensity = mean(z_mask_intensity),
#             LNcases_per_m = mean(LNcases_per_m),
#             outbreak = mean(outbreak))
# 
# df_agg %>% 
#   filter(party == "blue") %>% 
#   ggplot() + 
#   geom_line(aes(date, z_mask_intensity), col = 'black')+
#   # geom_line(aes(date, (cases-mean(cases))/sd(cases) ), col = 'blue')+
#   geom_line(aes(date, LNcases_per_m ), col = 'blue')+
#   geom_line(aes(date, outbreak), col = 'red')
# 
# 
# df_agg %>% 
#   filter(party == "red") %>% 
#   ggplot() + 
#   geom_line(aes(date, z_mask_intensity), col = 'black')+
#   # geom_line(aes(date, (cases-mean(cases))/sd(cases) ), col = 'blue')+
#   geom_line(aes(date, LNcases_per_m ), col = 'blue')+
#   geom_line(aes(date, outbreak), col = 'red')
