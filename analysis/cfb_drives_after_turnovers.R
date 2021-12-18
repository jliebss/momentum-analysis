pacman::p_load(tidyverse, cfbfastR, zoo, ggimage, gt)

season_txt <- "2014 - 2019"
seasons <- 2014:2019

pbp <- data.frame()
future::plan("multisession")
pbp <- cfbfastR::load_cfb_pbp(seasons)
# pbp <- cfbfastR::load_cfb_pbp(2019)

pbp$drive_after_turnover = factor(lag(pbp$turnover, n = 1),
                                  levels = c(0, 1),
                                  labels = c("no", "yes"))

cfb_drive_touchdowns <- pbp[,c("drive_id","drive_start_yards_to_goal","turnover","touchdown")] %>% 
  mutate(starting_position = factor(ceiling(drive_start_yards_to_goal / 10), levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")),
         drive_after_turnover = lag(turnover)) %>% 
  group_by(drive_id, starting_position) %>%
  summarize(drive_after_turnover = sum(drive_after_turnover),
            touchdown = sum(touchdown))
cfb_drive_touchdowns <- cfb_drive_touchdowns[complete.cases(cfb_drive_touchdowns), ]

cfb_baseline_drives <- cfb_drive_touchdowns[which(cfb_drive_touchdowns$drive_after_turnover == 0), ] %>% 
  group_by(starting_position) %>% 
  summarize(td_percentage = mean(touchdown))
cfb_after_turnover_drives <- cfb_drive_touchdowns[which(cfb_drive_touchdowns$drive_after_turnover == 1), ] %>% 
  group_by(starting_position) %>% 
  summarize(td_percentage = mean(touchdown))

ggplot(cfb_baseline_drives) +
  geom_col(aes(starting_position, td_percentage, fill = "#5778a4")) +
  geom_col(data = cfb_after_turnover_drives, aes(starting_position, td_percentage, fill = "#e49444"), width = 0.5) +
  labs(title   = "Are CFB drives following forced turnovers more likely to score touchdowns?",
       x       = "Starting yard line",
       y       = "Percentage of drives ending in touchdowns",
       caption = paste0("Data from ", season_txt, " CFB seasons")) +
  scale_fill_manual(name    = "", 
                    values  = c("#5778a4", "#e49444"), 
                    labels  = c("Other drives", "Drives following forced turnover")) +
  scale_x_discrete(labels   = c('1-10','11-20','21-30','31-40','41-50','51-60','61-70','71-80','81-90','91-100')) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(legend.position  = c(0.8, 0.85)) +
  coord_cartesian(expand = FALSE)
