# I'm aware that this code is messy and not documented great and inefficient. I'm learning :)

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(scales)
options(scipen = 9999)

# Load data from one season
# nfl_data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

# Load data from multiple seasons
season_txt <- "1999 - 2019"
seasons <- 1999:2019
nfl_data <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

# 1) First-quarter drives
labels <- c('1-10','11-20','21-30','31-40','41-50','51-60','61-70','71-80','81-90','91-100')
nfl_q1_data <- nfl_data[which(nfl_data$game_half   == "Half1" &
                                nfl_data$play_type != "kickoff" &
                                nfl_data$play_type != "extra_point" &
                                nfl_data$play_type != "no play"), ] %>% 
  group_by(game_id, drive) %>% 
  mutate(drive_start_quarter = first(qtr),
         starting_position   = factor(ceiling(first(yardline_100) / 10),
                                      levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                      labels = labels)) %>% 
  filter(drive_start_quarter == 1)

# 2) Score differential fewer than 10 points
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
nfl_q1_data$drive_after_safety <- lag(nfl_q1_data$safety, default = 0)

# 5) Blocked field goal or punt
nfl_q1_data$blocked_field_goal <- 0
nfl_q1_data[which(nfl_q1_data$field_goal_result == "blocked"), "blocked_field_goal"] <- 1
nfl_q1_data$lagged_blocked_field_goal <- lag(nfl_q1_data$blocked_field_goal, default = 0)
nfl_q1_data$lagged_punt_blocked       <- lag(nfl_q1_data$punt_blocked,       default = 0)
nfl_q1_data <- nfl_q1_data %>% 
  mutate(drive_after_blocked_kick = if_else((lagged_blocked_field_goal == 1 | lagged_punt_blocked == 1) & lagged_td == 0, 1, 0))


nfl_drives <- nfl_q1_data %>% 
  mutate(made_field_goal = case_when(field_goal_result == "made" ~ 1,
                                     TRUE                        ~ 0),
         turnover = case_when(interception == 1 ~ 1,
                              fumble_lost  == 1 ~ 1,
                              TRUE              ~ 0),
         defensive_touchdown = if_else(touchdown == 1 & turnover == 1, 1, 0)) %>% 
  group_by(game_id, drive, drive_start_quarter, starting_position, drive_first_downs, drive_ended_with_score) %>% 
  summarize(drive_after_interception       = sum(drive_after_interception,       na.rm = TRUE),
            drive_after_fumble_lost        = sum(drive_after_fumble_lost,        na.rm = TRUE),
            drive_after_fourth_down_failed = sum(drive_after_fourth_down_failed, na.rm = TRUE),
            drive_after_safety             = sum(drive_after_safety,             na.rm = TRUE),
            drive_after_blocked_kick       = sum(drive_after_blocked_kick,       na.rm = TRUE),
            first_play_yards_gained        = first(yards_gained),
            first_play_touchdown           = first(touchdown),
            first_play_safety              = first(safety),
            first_play_interception        = first(interception),
            first_play_fumble_lost         = first(fumble_lost),
            defensive_touchdown_on_drive   = sum(defensive_touchdown, na.rm = TRUE),
            safety_on_drive                = sum(safety,              na.rm = TRUE),
            made_field_goal_on_drive       = sum(made_field_goal,     na.rm = TRUE),
            touchdown_on_drive             = sum(touchdown,           na.rm = TRUE),
            touchdown_on_drive             =if_else(touchdown_on_drive == 1 & defensive_touchdown_on_drive == 0, 1, 0)) %>%
  mutate(first_series_success = factor(if_else(drive_first_downs > 0 | drive_ended_with_score > 0, 1, 0)),
         BD_drive             = factor(case_when(drive_after_interception       == 1 ~ "BD_Drive",
                                                 drive_after_fumble_lost        == 1 ~ "BD_Drive",
                                                 drive_after_fourth_down_failed == 1 ~ "BD_Drive",
                                                 drive_after_safety             == 1 ~ "BD_Drive",
                                                 drive_after_blocked_kick       == 1 ~ "BD_Drive",
                                                 TRUE                                ~ "NBD_Drive")),
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
                             levels = c("safety", "turnover", "loss of yardage", "0-3 yards gained", "4-6 yards gained", "7-9 yards gained", "10-29 yards gained", "30-49 yards gained", "50+ yards gained", "touchdown")),
         drive_result = factor(case_when(defensive_touchdown_on_drive == 1 ~ "defensive touchdown",
                                         safety_on_drive              == 1 ~ "safety",
                                         made_field_goal_on_drive     == 1 ~ "field goal",
                                         touchdown_on_drive           == 1 ~ "touchdown",
                                         TRUE                              ~ "no points"
         )))
nfl_drives <- nfl_drives[complete.cases(nfl_drives), ]
summary(nfl_drives)
str(nfl_drives)


# ANALYSIS ONE: The first play after a NBD and BD drives

# horizontal bar plot
first_play_plot <- ggplot(nfl_drives, aes(first_play, group = BD_drive)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop..), stat = "count", vjust = .5, hjust = -0.1) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(~BD_drive) +
  coord_flip() +
  theme_classic() +
  labs(y = "", x = "First Play", title = "First Play Following NBD and BD Drives") +
  theme(legend.position = "none")
first_play_plot
ggsave(filename = "./plots/first_play_plot.png", first_play_plot)

# breakdown by starting field position
group_by_first_play <- function(yard_decile, data) {
  proportions <- nfl_drives[which(nfl_drives$starting_position == yard_decile), ] %>%
    group_by(first_play, BD_drive) %>% 
    summarize(count = n()) %>% spread(BD_drive, count) %>% 
    mutate_all(~replace(., is.na(.), 0))
  proportions[, -1] <- apply(proportions[, -1], 2, function(x) round(x/sum(x), 4))
  return(proportions)
}
lapply(labels, group_by_first_play, nfl_drives)


# ANALYSIS TWO: The first set of downs after a BD

# prepare data
percentages <- nfl_drives %>% 
  group_by(starting_position, BD_drive, first_series_success) %>% 
  summarize(count = n()) %>% 
  mutate(success_rate = count/sum(count)) %>% 
  filter(first_series_success == 1)
percentages

# bar chart of successful first downs
first_downs_plot <- ggplot(percentages, aes(BD_drive, success_rate, fill = BD_drive)) +
  geom_bar(stat = "identity") +
  facet_grid(~starting_position, switch = "x") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.position = c(.9, .9)) +
  labs(x = "", y = "Success Rate", title = "First Set of Downs Success Rate") +
  coord_cartesian(expand = 0)
first_downs_plot
ggsave(filename = "./plots/first_downs_plot.png", first_downs_plot)

# ANALYSIS THREE: Points of drive

drive_results_percentages <- nfl_drives %>% 
  group_by(BD_drive, drive_result) %>% 
  summarize(count = n()) %>% 
  mutate(success_rate = count/sum(count))

drive_result_plot <-
  ggplot(drive_results_percentages, aes(drive_result, success_rate, fill = BD_drive)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  labs(x = "Drive Result", y = "Percentage of Total Drives", title = "Result of Drives") +
  theme(legend.title = element_blank(),
        legend.position = c(.9, .9)) +
  coord_cartesian(expand = 0)
drive_result_plot
ggsave(filename = "./plots/drive_result_plot.png", drive_result_plot)

# 
# write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
#   write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
# }
# 
# write.excel(proportions)
