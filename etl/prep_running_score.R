library(tidyverse)
source(paste0(getwd(), "/src/etl/utils.R"))

## read in match info
match_info_display <- readRDS(paste0(getwd(), "/data/processed/match_info_display_table.rds")) 

## read in comb dat and add position info
comb_dat <- readRDS(paste0(getwd(), "/data/raw/combined_events_360.rds")) %>% 
  # drop_na(position.id) %>% 
  select(id, match_id, team.name, position.id, position.id, 
         player.name, freeze_frame, visible_area, location.x, location.y,  
         type.name, shot.outcome.name, duration, ElapsedTime, period) %>% 
  filter(period %in% c(1, 2))

# event_select <- readRDS(paste0(getwd(),"/data/processed/comb_dat_select.rds")) %>% select(-freeze_frame, -visible_area)
## create example time segments
time_segments <- vector("list", length = 5)
time_segments[[1]] <- "full"
time_segments[[2]] <- "period"
time_segments[[3]] <- 15
time_segments[[4]] <- 10
time_segments[[5]] <- 5

for(seg in 1:length(time_segments)){
  # track_match_sum_dat <- vector("list", length = nrow(match_info_display))
  # track_match_position_info <- vector("list", length = nrow(match_info_display))
  track_match_score <- vector("list", length = nrow(match_info_display))
  
  for(i in 1:nrow(match_info_display)) {
    
    events_filtered <- comb_dat %>% 
      inner_join(match_info_display %>% slice(i) %>% 
                   select(match_id, team.name), by = c("match_id", "team.name"))
    
    reg_game_end_actual <- match_info_display$end_time_in_min_period_2[i]
    
    if(seg == 1) {
      events_filtered <- events_filtered %>% mutate(step_group = 1,
                                                    start_step = NA, 
                                                    end_step = NA)
    }
    else if(seg == 2) {
      events_filtered <- events_filtered %>% mutate(step_group = period, start_step = NA, end_step = NA)
    }
    else {
      this_step <- set_step_size(step_in_min = time_segments[[seg]]) 
      this_step <- this_step %>% mutate(end_step = ifelse(step_group == max(this_step$step_group), reg_game_end_actual, end_step))
      events_filtered <- events_filtered %>% 
        fuzzyjoin::fuzzy_left_join(this_step, 
                                   by = c("ElapsedTime" = "start_step",
                                          "ElapsedTime" = "end_step"),
                                   match_fun = list(`>=`, `<`)) 
    }
    
    track_match_score[[i]] <-  events_filtered %>% 
      group_by(match_id, team.name, step_group, start_step, end_step) %>% 
      summarise(.groups = 'drop',
                num_goals = sum(shot.outcome.name %in% c("Goal") | type.name %in% c("Own Goal For")))
    
    
  }
 
  track_match_score %>% bind_rows() %>% mutate(seg = time_segments[[seg]]) %>% saveRDS(paste0(getwd(), "/data/processed/dat_", time_segments[[seg]], "_running_score.rds"))
}
