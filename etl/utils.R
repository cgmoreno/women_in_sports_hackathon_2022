create_match_table <- function(this_id) {
  # browser()
  score_info <- match_info %>% filter(match_id == this_id) %>% 
    select(match_id, home_score, away_score) %>% 
    pivot_longer(contains("score")) %>% 
    mutate(type = str_extract(name, "home|away")) %>% 
    rename(score = value) %>% select(-name) 
  
  name_info <- match_info %>% filter(match_id == this_id) %>% 
    select(match_id, home_team.home_team_name, away_team.away_team_name) %>% 
    pivot_longer(contains("team_name")) %>% 
    mutate(type = str_extract(name, "home|away")) %>% rename(team.name = value) %>%
    select(-name)
  
  this_match <- score_info %>% 
    inner_join(name_info, by = c("match_id", "type")) %>% 
    # pivot_wider(id_cols = match_id, values_from = score, names_from = team.name) %>% 
    ## bring in other pertinent match meta data
    left_join(match_info %>% 
                select(match_id, kick_off, match_date, 
                       competition_stage.name, stadium.name, referee.name), 
              by = "match_id") %>% 
    ## bring in number of periods in game
    mutate(num_periods_in_match =  comb_dat %>% 
             filter(match_id == this_id) %>% 
             pull(period) %>% max(na.rm = T))
  
  ## bring in period 1, 2 info
  reg_play_period_times <- comb_dat %>% 
    filter(match_id == this_id, period %in% c(1,2)) %>% 
    group_by(match_id, period) %>% 
    summarise(max_time = max(ElapsedTime, na.rm = T)) %>% 
    ungroup() %>% 
    pivot_wider(names_prefix = "end_time_in_min_period_", 
                values_from = max_time, names_from = period, id_cols= match_id)
  ## return final 
  this_match %>% left_join(reg_play_period_times, by = "match_id")
  
}

get_loc_info <- function(x, coord_info) {
  if(is.null(x)) {
    res <- NA
  }
  else {
    loc_info <- x %>% filter(actor) 
    if(nrow(loc_info) > 0) {
      res <- loc_info %>% pull(location) %>% .[[1]] %>% .[coord_info]
    }
    else {
      res <- NA
    }
  }
}

calc_visible_area <- function(visible_area) {
  # browser()
  if(is.null(visible_area) | length(visible_area) == 0){
    res <- NA
  }
  else {
    df <- data.frame(x = visible_area[seq(1,length(visible_area), by = 2)],
                     y = visible_area[seq(2, length(visible_area), by = 2)]) 
    
    res <- pracma::polyarea(x = df$x, y = df$y)
  }
  return(res)
}

# calc_density <- function(freeze_frame) {
#   # vis_area_res <- calc_visible_area(visible_area[[1]])
#   if(is.na(freeze_frame)){
#     res <- NA
#   }
#   else {
#     n_all <- freeze_frame[[1]]  %>% filter(!actor) %>% nrow()
#     n_opponent <- freeze_frame[[1]] %>% filter(!actor, !teammate) %>% nrow()
#   }
#   return(n_opponent)
# }

set_step_size <- function(step_in_min) {
  ## convert min --> sec to match ElapsedTime 
  ## allow over 90 min to handle extra time in first two periods
  res <- tibble(start_step = seq(0, (90 + step_in_min) *60, by = step_in_min*60)) %>% 
    mutate(end_step = lead(start_step)) %>% 
    drop_na() %>% 
    mutate(step_group = 1:n())
  
  return(res)
}

calc_n_other <- function(df) {
  if(is.null(df)) {
    res <- NA
  }
  else {
    res <- df %>% filter(!teammate) %>% nrow()
  }
}
calc_n_opponent <- function(df) {
  if(is.null(df)) {
    res <- NA
  }
  else {
    res <- df %>% filter(!actor, !teammate) %>% nrow()
  }
}