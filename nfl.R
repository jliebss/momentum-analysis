library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

head(data)
sum(!is.na(data$td_team))
data <- data %>% mutate(td = !is.na(td_team))

data$turnover <- 0
data$turnover[which(data$interception == 1 | data$fumble_lost == 1)] <- 1
data$drive_after_turnover <- lag(data$turnover, n = 1)

drive_touchdowns <- data[data$special == 0, ] %>% 
  group_by(game_id, drive) %>%
  mutate(starting_position    = factor(ceiling(first(yardline_100) / 10), levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")),
         drive_after_turnover = first(drive_after_turnover)) %>% 
  group_by(game_id, drive, starting_position, drive_after_turnover) %>%
  summarize(touchdown = sum(td))
drive_touchdowns <- drive_touchdowns[complete.cases(drive_touchdowns), ]

baseline_drives <- drive_touchdowns %>% 
  group_by(starting_position) %>% 
  summarize(td_percentage = mean(touchdown))

after_turnover_drives <- drive_touchdowns[which(drive_touchdowns$drive_after_turnover == 1), ] %>% 
  group_by(starting_position) %>% 
  summarize(td_percentage = mean(touchdown))

ggplot(baseline_drives) +
  geom_col(aes(starting_position, td_percentage), fill = "#5778a4") +
  geom_col(data = after_turnover_drives, aes(starting_position, td_percentage), fill = "#e49444", width = 0.5) +
  theme_classic() +
  labs(x = "Starting 10-yard line", y = "Proportion of drives ending in touchdowns") +
  ggtitle("Touchdown drives after turnovers vs baseline") +
  coord_cartesian(expand = FALSE)
                  