library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)

# data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

season_txt <- "2010 - 2019"
seasons <- 2010:2019
data <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

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
  geom_col(aes(starting_position, td_percentage, fill = "#5778a4")) +
  geom_col(data = after_turnover_drives, aes(starting_position, td_percentage, fill = "#e49444"), width = 0.5) +
  ggtitle(paste0("Touchdown drives following forced turnovers vs. all drives, ", season_txt)) +
  labs(x = "Starting yard line", y = "Proportion of drives ending in touchdowns") +
  scale_fill_manual(name="Touchdown Percentage", 
                    values=c("#5778a4", "#e49444"), 
                    labels=c("All drives", "Drives following forced turnover")) + 
  scale_x_discrete(labels = c('1-10','11-20','21-30','31-40','41-50','51-60','61-70','71-80','81-90','91-100')) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.85)) +
  coord_cartesian(expand = FALSE)
  