library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(scales)

# Load data from one season
nfl_data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

# Load data from multiple seasons
# season_txt <- "2014 - 2019"
# seasons <- 2014:2019
# data <- map_df(seasons, function(x) {
#   readRDS(
#     url(
#       paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
#     )
#   )
# })

# 1) First-quarter drives
nfl_q1_data <- nfl_data[which(nfl_data$game_half == "Half1" &
                                nfl_data$play_type != "kickoff" &
                                nfl_data$play_type != "no play"), ] %>% 
  group_by(game_id, drive) %>% 
  mutate(drive_start_quarter = first(qtr),
         starting_position   = factor(ceiling(first(yardline_100) / 10),
                                      levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                      labels = c('1-10','11-20','21-30','31-40','41-50','51-60','61-70','71-80','81-90','91-100'))) %>% 
  filter(drive_start_quarter == 1)
# 2) Scored differential fewer than 10 points
nfl_q1_data <- nfl_q1_data[which(abs(nfl_q1_data$score_differential) < 10), ]


# Drive after big defensive plays
nfl_q1_data$lagged_td <- lag(nfl_q1_data$touchdown, default = 0)

# 1) Drives after interceptions (remove pick-sixes)
nfl_q1_data$lagged_interception <- lag(nfl_q1_data$interception, default = 0)
nfl_q1_data <- nfl_q1_data %>% 
  mutate(drive_after_interception = if_else(lagged_interception == 1 & lagged_td == 0, 1, 0))

# 2) Fumble recovered by the defense
nfl_q1_data$lagged_fumble_lost <- lag(nfl_q1_data$fumble_lost, default = 0)
nfl_q1_data <- nfl_q1_data %>% 
  mutate(drive_after_fumble_lost = if_else(lagged_fumble_lost == 1 & lagged_td == 0, 1, 0))

# 3) 4th down stop (turnover on downs)
nfl_q1_data$lagged_fourth_down_failed <- lag(nfl_q1_data$fourth_down_failed, default = 0)
nfl_q1_data <- nfl_q1_data %>% 
  mutate(drive_after_fourth_down_failed = if_else(lagged_fourth_down_failed == 1 & lagged_td == 0, 1, 0))

# 4) Safety
nfl_q1_data$lagged_safety <- lag(nfl_q1_data$safety, default = 0)
nfl_q1_data <- nfl_q1_data %>% 
  mutate(drive_after_safety = if_else(lagged_safety == 1 & lagged_td == 0, 1, 0))

# 5) Blocked field goal or punt
nfl_q1_data$blocked_field_goal <- 0
nfl_q1_data[which(nfl_q1_data$field_goal_result == "blocked"), "blocked_field_goal"] <- 1
nfl_q1_data$lagged_blocked_field_goal <- lag(nfl_q1_data$blocked_field_goal, default = 0)
nfl_q1_data$lagged_punt_blocked       <- lag(nfl_q1_data$punt_blocked,       default = 0)
nfl_q1_data <- nfl_q1_data %>% 
  mutate(drive_after_blocked_kick = if_else((lagged_blocked_field_goal == 1 | lagged_punt_blocked == 1) & lagged_td == 0, 1, 0))


nfl_drives <- nfl_q1_data %>% 
  group_by(game_id, drive, drive_start_quarter, starting_position) %>% 
  mutate(first_play_result = first(yards_gained)) %>% 
  summarize(drive_after_interception       = sum(drive_after_interception),
            drive_after_fumble_lost        = sum(drive_after_fumble_lost),
            drive_after_fourth_down_failed = sum(drive_after_fourth_down_failed),
            drive_after_safety             = sum(drive_after_safety),
            drive_after_blocked_kick       = sum(drive_after_blocked_kick),
            first_play_yards_gained        = first(yards_gained),
            first_play_touchdown           = first(touchdown),
            first_play_safety              = first(safety),
            first_play_interception        = first(interception),
            first_play_fumble_lost         = first(fumble_lost)) %>%
  mutate(BD_drive = factor(case_when(drive_after_interception       == 1 ~ "BD Drive",
                                     drive_after_fumble_lost        == 1 ~ "BD Drive",
                                     drive_after_fourth_down_failed == 1 ~ "BD Drive",
                                     drive_after_safety             == 1 ~ "BD Drive",
                                     drive_after_blocked_kick       == 1 ~ "BD Drive",
                                     TRUE                                ~ "NBD Drive")),
         # BD_drive = factor(BD_drive),
         first_play = factor(case_when(first_play_safety       == 1  ~ "safety",
                                       first_play_interception == 1  | first_play_fumble_lost == 1 ~ "turnover",
                                       first_play_interception == 0  & first_play_fumble_lost == 0 & first_play_touchdown == 1 ~ "touchdown",
                                       first_play_yards_gained <  0  ~ "loss of yardage",
                                       first_play_yards_gained <= 3  ~ "0-3 yards gained",
                                       first_play_yards_gained <= 6  ~ "4-6 yards gained",
                                       first_play_yards_gained <= 9  ~ "7-9 yards gained",
                                       first_play_yards_gained <= 29 ~ "10-29 yards gained",
                                       first_play_yards_gained <= 49 ~ "30-49 yards gained",
                                       first_play_yards_gained >= 50 ~ "50+ yards gained"),
                             levels = c("safety", "turnover", "loss of yardage", "0-3 yards gained", "4-6 yards gained", "7-9 yards gained", "10-29 yards gained", "30-49 yards gained", "50+ yards gained", "touchdown")))
nfl_drives <- nfl_drives[complete.cases(nfl_drives), ]
summary(nfl_drives)
str(nfl_drives)

ggplot(nfl_drives, aes(BD_drive, fill = first_play)) +
  geom_bar(position = "fill")
options(scipen = 9999)

proportions <- nfl_drives %>%
  group_by(first_play, BD_drive) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count),
         count = NULL)
proportions <- spread(proportions, BD_drive, prop)
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(proportions)
