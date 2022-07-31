library(shiny)
library(tidyverse)

## load data
match_info_display <- readRDS("app_data/match_info_display_table.rds")
player_recode_position <- readRDS("app_data/player_recode_position.rds")

match_info_display <- match_info_display %>% 
  left_join(match_info_display %>% 
              select(match_id, type, score) %>% 
              pivot_wider(names_from = type, values_from = score) %>% 
              mutate(home_relative = home - away, 
                     away_relative = away - home) %>% 
              select(match_id, home_relative, away_relative) %>% 
              pivot_longer(!match_id) %>% 
              mutate(type = str_extract(name, "^[:alpha:]+")) %>% 
              select(-name),
            by = c("match_id", "type")) %>% 
  mutate(end_status = ifelse(sign(value) == 0, "tie", ifelse(sign(value) == 1, "win", "loss")))

comb_select <- readRDS("app_data/comb_dat_select.rds")

## helper function / info
set_step_size <- function(step_in_min) {
  res <- tibble(start_step = seq(0, (90 + step_in_min) *60, by = step_in_min*60)) %>% 
    mutate(end_step = lead(start_step)) %>% 
    drop_na() %>% 
    mutate(step_group = 1:n())
  
  return(res)
}

data_source_snippet <- "This web app is for the 2022 Women in Sports Data hackathon and uses the StatsBomb event and 360 data from the men's UEFA Euro 2020 competition via the StatsBombR package."

plot_description <-  "The fill gradient represents the spatial density of events for this position-line / time-segment combination (the more yellow, the more events in that space). 
The red point represents the average x-y location and the size of point relates to user-selected metric (e.g. # events). "

time_seg_lookup <- tibble(key = c("full game", "period", "15", "10", "5"),
                          value = c("Full Game", "Period", "15 min interval", "10 min interval", "5 min interval"))

metric_lookup <- tibble(key = c("total_dur", "n_events", "rho_opponent"),
                        value = c("Total duration events [min]", "Total # events", "# opponents in freeze-frame"))
