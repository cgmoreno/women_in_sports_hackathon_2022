library(tidyverse)
comb_dat <- readRDS(paste0(getwd(), "/data/raw/combined_events_360.rds"))

## assign high-level position-line bins for DEF, MID, FOR
player_recode <- comb_dat %>% distinct(position.name, position.id) %>% 
  filter(!position.name %in% c(NA, "Goalkeeper")) %>% 
  mutate(recode_position = ifelse(position.id %in% c(2:6), "DEF", NA),
         recode_position = ifelse(position.id %in% c(7:21), "MID", recode_position),
         recode_position = ifelse(position.id %in% c(22:25), "FOR", recode_position))

## for visualization purposes, estimate pitch position for position.id's 1-25 per StatsBomb event data documentation on position.id
pos_lookup <- data.frame(position.id = c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 8, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 25, 22, 23, 24),
                         x = c(15, 
                               30, 30, 30, 30, 30,
                               45, 45, 45, 45, 45,
                               60, 60, 60, 60, 60,
                               75, 75, 75, 75, 75, 
                               90,
                               105, 105, 105),
                         y = c(40, 
                               80/6*1, 80/6 *2, 80/6*3, 80/6*4, 80/6*5,
                               80/6*1, 80/6*2, 80/6*3, 80/6*4, 80/6*5,
                               80/6*1, 80/6*2, 80/6*3, 80/6*4, 80/6*5,
                               80/6*1, 80/6*2, 80/6*3, 80/6*4, 80/6*5,
                               40, 
                               80/6*2, 80/6*3, 80/6*4))

player_recode %>% left_join(pos_lookup, by = "position.id") %>%  saveRDS(paste0(getwd(), "/data/processed/player_recode_position.rds"))
