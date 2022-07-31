library(tidyverse)
source(paste0(getwd(), "/src/etl/utils.R"))

## read in match info
match_info_display <- readRDS(paste0(getwd(), "/data/processed/match_info_display_table.rds")) 

event_select <- readRDS(paste0(getwd(),"/data/processed/comb_dat_select.rds")) %>% select(-freeze_frame, -visible_area)
## create example time segments
time_segments <- vector("list", length = 5)
time_segments[[1]] <- "full"
time_segments[[2]] <- "period"
time_segments[[3]] <- 15
time_segments[[4]] <- 10
time_segments[[5]] <- 5

pos_distinct <- c("DEF", "FOR", "MID")
for(seg in 1:length(time_segments)){
  track_match_sum_dat <- vector("list", length = nrow(match_info_display))
  track_match_position_info <- vector("list", length = nrow(match_info_display))
  # track_match_score <- vector("list", length = nrow(match_info_display))
  
  for(i in 1:nrow(match_info_display)) {

    events_filtered <- event_select %>% 
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
    
    # track_match_score[[i]] <-  events_filtered %>% 
    #   group_by(match_id, team.name, step_group, start_step, end_step) %>% 
    #   summarise(.groups = 'drop',
    #             num_goals = sum(shot.outcome.name %in% c("Goal") | type.name %in% c("Own Goal For", "Own Goal Against")))
    # 
    track_pos_sum_dat <- vector("list", length = length(pos_distinct))
    track_pos_position_info <- vector("list", length = length(pos_distinct))
    for(p in 1:length(pos_distinct)) {
      events_filtered_pos <- events_filtered %>% filter(recode_position %in% pos_distinct[p])
      
      ## save summary dat
      track_pos_sum_dat[[p]] <- events_filtered_pos %>% 
        group_by(match_id, team.name, recode_position, step_group, start_step, end_step) %>%
        summarise(.groups = 'drop',
                  n_events = n(),
                  total_dur = sum(duration, na.rm = TRUE),
                  # n_pass = sum[which(type.name == "Pass" & is.na(pass.outcome.name)],
                  # avg_pass_len = ,
                  avg_x = mean(x_final, na.rm = T),
                  avg_y = mean(y_final, na.rm = T),
                  sd_x = sd(x_final, na.rm = T),
                  sd_y = sd(y_final, na.rm = T), 
                  rho_opponent = mean(n_opponent/vis_area_calc, na.rm = T),
                  rho_non_actor = mean(n_other/vis_area_calc, na.rm = T))
      
      track_pos_position_info[[p]] <- events_filtered_pos %>% 
        group_by(match_id, team.name, recode_position, step_group, start_step, end_step, position.id) %>% 
        arrange(ElapsedTime) %>% 
        summarise(.groups = 'drop',
                  first_player = first(player.name),
                  n_players = n_distinct(player.name))
      
    }
    track_match_sum_dat[[i]] <- track_pos_sum_dat %>% bind_rows()
    track_match_position_info[[i]] <- track_pos_position_info %>% bind_rows()
  }
  track_match_sum_dat %>% bind_rows() %>% mutate(seg = time_segments[[seg]]) %>% saveRDS(paste0(getwd(), "/data/processed/dat_", time_segments[[seg]], "_summary_dat.rds"))
  track_match_position_info %>% bind_rows() %>% mutate(seg = time_segments[[seg]]) %>% saveRDS(paste0(getwd(), "/data/processed/dat_", time_segments[[seg]], "_position_info.rds"))
  # track_match_score %>% bind_rows() %>% mutate(seg = time_segments[[seg]]) %>% saveRDS(paste0(getwd(), "/data/processed/dat_", time_segments[[seg]], "_running_score.rds"))
}
