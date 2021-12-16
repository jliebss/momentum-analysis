library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(scales)

# Load data from one season
# data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

# Load data from multiple seasons
season_txt <- "2010 - 2019"
seasons <- 2010:2019
data <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

# Add column indicating if a play is a touchdown
data <- data %>% mutate(td = !is.na(td_team))

# Add column indicating is a play is a turnover, then column indicating if previous play is a turnover
# This allows us to zero in on drives following forced turnovers
data$turnover <- 0
data$turnover[which(data$interception == 1 | data$fumble_lost == 1)] <- 1
data$drive_after_turnover <- lag(data$turnover, n = 1)

# TODO: Verify starting field position works correctly
# Groups each drive and provides its starting field position (in ten-yard increments),
# whether it follows a forced turnover, and if it results in a touchdown
drive_touchdowns <- data[data$special == 0, ] %>% 
  group_by(game_id, drive) %>%
  mutate(starting_position    = factor(ceiling(first(yardline_100) / 10), levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")),
         drive_after_turnover = first(drive_after_turnover)) %>% 
  group_by(game_id, drive, starting_position, drive_after_turnover) %>%
  summarize(touchdown = sum(td))
drive_touchdowns <- drive_touchdowns[complete.cases(drive_touchdowns), ]

# Gets the mean amount of drives that end in touchdowns for each ten-yard increment
baseline_drives <- drive_touchdowns %>% 
  group_by(starting_position) %>% 
  summarize(td_percentage = mean(touchdown))
after_turnover_drives <- drive_touchdowns[which(drive_touchdowns$drive_after_turnover == 1), ] %>% 
  group_by(starting_position) %>% 
  summarize(td_percentage = mean(touchdown))

# Build the plot: Are drives following forced turnovers more likely to score touchdowns?
ggplot(baseline_drives) +
  geom_col(aes(starting_position, td_percentage, fill = "#5778a4")) +
  geom_col(data = after_turnover_drives, aes(starting_position, td_percentage, fill = "#e49444"), width = 0.5) +
  labs(title   = "Are drives following forced turnovers more likely to score touchdowns?",
       x       = "Starting yard line",
       y       = "Percentage of drives ending in touchdowns",
       caption = paste0("Data from ", season_txt, " NFL seasons")) +
  scale_fill_manual(name    = "", 
                    values  = c("#5778a4", "#e49444"), 
                    labels  = c("All drives", "Drives following forced turnover")) +
  scale_x_discrete(labels   = c('1-10','11-20','21-30','31-40','41-50','51-60','61-70','71-80','81-90','91-100')) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(legend.position  = c(0.8, 0.85)) +
  coord_cartesian(expand = FALSE)
  
# dummy line - delete this