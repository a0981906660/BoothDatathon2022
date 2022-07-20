library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

getwd()
# setwd("/Users/boyie/Programming/BoothDatathon2022")

df <- read_feather("./data/cleaned/df.feather")


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

drawPlot("CA")
drawPlot("TX")

# loop
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