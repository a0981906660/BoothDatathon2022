setwd("C:/Users/boyuchen/Documents/GitHub/BoothDatathon2022/")
data <- "C:/Users/boyuchen/Documents/datathon/"

library(data.table)
library(tidyverse)
library(magrittr)
library(ggsci)
library(feather)
library(jsonlite)
library(lubridate)

options(fread.data.table = T)

## MSA dictionary ----

msa_dict <- jsonlite::fromJSON("data/cleaned/msa_dict.json")
msa_dict <- msa_dict %>% unlist() %>% as.data.frame()
msa_dict <- bind_cols(msa = msa_dict %>% rownames,
          dict_name = msa_dict$.) %>% as.data.table()
msa_dict[, state := str_extract(dict_name, "(?<=, )[A-Z]{2}")]
msa_dict[, area := str_extract(msa, ".*(?=, )")]

# files <- list.files('Proquest News Articles/Proquest News Bag-of-Words/')
# files %>% str_subset('NoStop.', negate = T)

## county state name
county <- fread("data/interm/county_description.csv")
state <- county %>% unique(by = 'state_name') %>%
  select(state_name, state_abrev)



## Covid cases ----
case <- fread(paste0(data, '/The NYT Covid Cases Report/msa_cumulative_cases.csv'), header = T)
setnames(case, "V1", "msa")
case %<>% melt(id.var = 'msa', 
              variable.name = 'date', 
              value.name = 'cases')
case[, msa := as.character(msa)]
case <- merge(case, msa_dict, by = 'msa')
case_state <- case %>% dcast.data.table(date+state~., fun.aggregate = sum, 
                                        value.var = 'cases')
setnames(case_state, '.', 'cases')
case_state[, date := as.Date(date)]

## read pop ----
census_files <- list.files(paste0(data, 'Urban Region Census/Region_Demographics/'),
                           full.names = T)

read_census <- function(file){
  msa_file <- tstrsplit(file, '/')[[length(tstrsplit(file, '/'))]] %>% str_remove_all("\\.csv")
  dt <- file %>% 
    fread(select = 'population')
  msa_pop <- dt[,.(pop = sum(population))] %>% 
    mutate(msa_dict = msa_file, 
           state = str_extract(msa_dict, "(?<=, )[A-Z]{2}"))
  return(msa_pop)
}

msa_pop <- lapply(census_files, read_census) %>% rbindlist() 
state_pop <- msa_pop[, .(pop = sum(pop)), by = state]
case_state <- merge(state_pop, case_state, by = 'state')
case_state[, cases_per_m := 1000000*cases/pop]

case_state %>% fwrite("data/cleaned/covid_cases_by_day.csv")

case_state_week <- case_state %>%
  dcast.data.table(state+week~., fun.aggregate = max, 
                   value.var = 'cases_per_m')

setnames(case_state_week, '.', 'cases_per_m')
case_state_week[, outbreak := ifelse(cases_per_m > 5000, 1, 0)]
case_state_week[outbreak == 1][, min(ymd(date)), by = state]
case_state_week[outbreak == 1]

# 
# case_state %>% 
#   ggplot() + 
#   geom_line(aes(x = date, y = cases_per_m, color = state)) +
#   theme_bw()

## news
# news <- fread('Proquest News Articles/Proquest News Meta Data.csv')[,-1]
# newvar <- c('id', 'pub_loc', 'pub_name', 'pub_title', 'title',
#             'author','date', 'doc_type')
# setnames(news, names(news), newvar)
# news[,.N, by=pub_title] %>% 
#   arrange(-N) %>% 
#   head(30)
# 
# news %>% 
#   select(pub_loc, pub_title) %>%
#   unique(by = 'pub_loc') %>%
#   fwrite("data/interm/news_loc.csv")
# 
# news_state <- fread("data/interm/news_loc_clean.csv") %>%
#   select(pub_loc, state)
# 
# news_scale <- read_feather("data/cleaned/newsMedia.feather") %>% as.data.table()
# news_scale %<>% merge(news_state, by = 'pub_loc')
# 
# news_scale[, .(MajorMedia, state)] %>% table
# 
# ## Business 
# busi <- fread(paste0(data, "/Meta Business Activities/ic2s2_fb_bat_2020_verticals.csv"))
# busi[, week := week(ymd(ds))]
# 
# busi_state <- busi %>%
#   dcast.data.table(gadm_state_name+business_vertical+week~. , fun.aggregate = mean,
#                    value.var = c('activity_percentage','activity_quantile'))
# 
# busi_state_week <- busi %>%
#   dcast.data.table(gadm_state_name+business_vertical+week~. , fun.aggregate = mean,
#                    value.var = c('activity_percentage','activity_quantile'))
# 
# busi_state_week %<>% merge(state, by.x = "gadm_state_name", by.y = "state_name", all.x = T)
# setnames(busi_state_week, 'state_abrev', 'state')
# 
# 
# ### merge 
# dt <- merge(busi_state_week, case_state_week, by = c('week','state'))
# setnames(dt, 'gadm_state_name', 'state_name')
# 
# dt %>% melt(id.var = c('week', 'state', 'business_vertical', 'state_name'),
#             value.var = c('activity_percentage', 'cases_per_m'))
# 
# dt %>% 
#   ggplot(aes(x = week, color = variable, y = value)) +
#    geom_line()
