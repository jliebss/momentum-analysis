library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

head(data)
sum(!is.na(data$td_team))
data <- data %>% mutate(td = !is.na(td_team))

drive_touchdowns <- data %>% 
  group_by(game_id, drive) %>% 
  summarize(sum(td))
drive_touchdowns <- drive_touchdowns[complete.cases(drive_touchdowns), ]

td_percent <- sum(drive_touchdowns$`sum(td)`) / nrow(drive_touchdowns)
