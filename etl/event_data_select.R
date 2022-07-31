library(tidyverse)
source(paste0(getwd(), "/src/etl/utils.R"))

## read in match info
match_info_display <- readRDS(paste0(getwd(), "/data/processed/match_info_display_table.rds")) 

## read in position info
pos <- readRDS(paste0(getwd(), "/data/processed/player_recode_position.rds"))
pos_distinct <- pos %>% distinct(recode_position) %>% pull()

## read in comb dat and add position info
comb_dat <- readRDS(paste0(getwd(), "/data/raw/combined_events_360.rds")) %>% 
  drop_na(position.id) %>%
  left_join(pos %>% select(position.id, recode_position), by = "position.id") %>% 
  select(id, match_id, team.name, recode_position, position.id, position.id, 
         player.name, freeze_frame, visible_area, location.x, location.y,  
         type.name, shot.outcome.name, duration, ElapsedTime, period) 

## select important features, add new features and write out 
track_match_event_select <- vector("list", length = nrow(match_info_display))
for(i in 1:nrow(match_info_display)) {
  this_match_team <- match_info_display %>% filter(match_id == match_info_display$match_id[i], 
                                                   team.name == match_info_display$team.name[i])
  
  
  events_filtered <- comb_dat %>% 
    inner_join(this_match_team %>% 
                 select(match_id, team.name), 
               by = c("match_id", "team.name")) %>% 
    filter(period %in% c(1, 2)) 
  
  ## get xy info of actor
  res_x <- tibble(x_360 = events_filtered %>% 
                    pull(freeze_frame) %>% purrr::map_dbl(function(x) {get_loc_info(x, coord_info = 1)}),
                  x_orig = events_filtered %>% pull(location.x)) %>% 
    mutate(x_final = ifelse(is.na(x_360), x_orig, x_360)) %>% select(x_final)
  res_y <- tibble(y_360 = events_filtered %>% 
                    pull(freeze_frame) %>% purrr::map_dbl(function(y) {get_loc_info(y, coord_info = 2)}),
                  y_orig = events_filtered %>% pull(location.y)) %>% 
    mutate(y_final = ifelse(is.na(y_360), y_orig, y_360)) %>% select(y_final)
  
  ## get num non actor
  res_non_actor <- tibble(n_other = events_filtered %>% 
                            pull(freeze_frame) %>% 
                            purrr::map_dbl(function(x) {calc_n_other(x)}))
  ## get num opponent
  res_opponent <- tibble(n_opponent = events_filtered %>% 
                           pull(freeze_frame) %>% 
                           purrr::map_dbl(function(x) {calc_n_opponent(x)}))
  ## get freeze frame polygon area
  res_vis_area <- tibble(vis_area_calc = events_filtered %>% 
                           pull(visible_area) %>% purrr::map_dbl(function(x) {calc_visible_area(x)}))
  
  track_match_event_select[[i]] <- events_filtered %>% 
    bind_cols(res_x) %>%
    bind_cols(res_y) %>%
    bind_cols(res_non_actor) %>% 
    bind_cols(res_opponent) %>% 
    bind_cols(res_vis_area)
}

track_match_event_select %>% bind_rows() %>% 
  saveRDS(paste0(getwd(), "/data/processed/comb_dat_select.rds"))


