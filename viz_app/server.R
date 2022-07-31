shinyServer(function(input, output) {
  output$return_matches <- renderUI({
    this_teams_match_list <- match_info_display %>% 
      filter(team.name %in% input$team) %>% 
      mutate(stage_date = paste0(competition_stage.name, " (", match_id, ")" )) %>% 
      arrange(desc(match_date)) %>% 
      select(stage_date) %>% pull()
    selectInput("match_id", "Select Match", choices = this_teams_match_list)
  })
  
  output$match_info <- renderTable({
    if(!is.null(input$match_id)) {
      match_filtered <- match_info_display %>% 
        mutate(stage_date = paste0(competition_stage.name, " (", match_id, ")" )) %>% 
        filter(stage_date %in% input$match_id)
      
      country_other <- match_filtered %>% filter(!team.name %in% input$team) %>% pull(team.name)
     
      match_filtered %>% 
        select(-type, -end_status, -value) %>% 
        pivot_wider(names_from = team.name, values_from = score) %>% 
        select(-stage_date, -match_id) %>% 
        mutate_at(.vars = c("end_time_in_min_period_1", "end_time_in_min_period_2"), .funs = function(x) {round(x/60, 1)}) %>% 
        select(input$team, country_other, Stage = competition_stage.name, Date = match_date, Stadium = stadium.name, Kickoff = kick_off, Referee = referee.name)
      }
    
  })
  
  output$match_periods_total_text <- renderText({
    if(!is.null(input$match_id)) {
      match_filtered <- match_info_display %>% 
        mutate(stage_date = paste0(competition_stage.name, " (", match_id, ")" )) %>% 
        filter(stage_date %in% input$match_id)
      
      total_periods <- match_filtered %>% distinct(num_periods_in_match) %>% pull()
      end_time_period2 <- match_filtered %>% distinct(end_time_in_min_period_2) %>% pull()
      
      if(total_periods > 2) {
        paste("Total number of periods in match = ", total_periods, ";", "Period 2 ended at ", round(end_time_period2 / 60, 1), "minutes") 
      }
      else {
        paste("Period 2 ended at ", round(end_time_period2 / 60, 1), "minutes") 
        
      }
    }
  })
  
  output$field_view_eda <- renderPlot({

    if(!is.null(input$match_id)) {
      # browser()
      this_team <- input$team 
      
      match_filtered <- match_info_display %>% 
        mutate(stage_date = paste0(competition_stage.name, " (", match_id, ")" )) %>% 
        filter(stage_date %in% input$match_id)
      
      country_other <- match_filtered %>% filter(!team.name %in% input$team) %>% pull(team.name)
      
      this_match <- match_filtered %>% distinct(match_id) %>% pull()
      reg_game_end_actual <- match_filtered %>% pull(end_time_in_min_period_2)
      events_filtered <- comb_select %>% 
          filter(team.name == this_team,
                 match_id == this_match, 
                 recode_position %in% input$pos) 
      
      time_slice_key <- time_seg_lookup %>% filter(value %in% input$time_slice) %>% pull(key)
      size_var_key <- metric_lookup %>% filter(value %in% input$size_var) %>% pull(key)
      
      if(time_slice_key %in% "full game") {
        
        summary_dat <- readRDS("app_data/dat_full_summary_dat.rds") %>%  filter(team.name == this_team,
                                                                       match_id == this_match,
                                                                       recode_position %in% input$pos) %>% 
          mutate(total_dur = round(total_dur/60))
        
        running_score <- readRDS("app_data/dat_full_running_score.rds") %>%  filter(match_id == this_match) %>% 
          pivot_wider(names_from = team.name, values_from = num_goals) %>% 
          mutate_at(.vars = c(input$team, country_other), .funs = list(~cumsum(.))) %>% 
          mutate(display_score = paste(input$team, !!as.name(input$team), "-", country_other, !!as.name(country_other)),
                 status = sign(!!as.name(input$team) - !!as.name(country_other)),
                 display_status = ifelse(status == -1, "down", ifelse(status == 0, "tie", "up")))
        
          
        events_filtered <- events_filtered %>% mutate(facet_var = match_id) 
      } 
      else if(time_slice_key %in% "period") {
        summary_dat <- readRDS("app_data/dat_period_summary_dat.rds") %>%  filter(team.name == this_team,
                                                                       match_id == this_match,
                                                                       recode_position %in% input$pos) %>% 
          mutate(facet_var = step_group,
                 total_dur = round(total_dur/60))
        
        running_score <- readRDS("app_data/dat_period_running_score.rds") %>%  filter(match_id == this_match) %>% 
          pivot_wider(names_from = team.name, values_from = num_goals) %>% 
          mutate_at(.vars = c(input$team, country_other), .funs = list(~cumsum(.))) %>% 
          mutate(display_score = paste(input$team, !!as.name(input$team), "-", country_other, !!as.name(country_other)),
                 status = sign(!!as.name(input$team) - !!as.name(country_other)),
                 display_status = ifelse(status == -1, "down", ifelse(status == 0, "tie", "up")),
                 facet_var = step_group)
        
        events_filtered <- events_filtered %>% mutate(facet_var = period)
      }
      else {
        summary_dat <- readRDS(paste0("app_data/dat_", time_slice_key, "_summary_dat.rds")) %>%  filter(team.name == this_team,
                                                                         match_id == this_match,
                                                                         recode_position %in% input$pos) %>% 
          mutate(facet_var = paste0("[", start_step/60, "-", round(end_step/60), "]"),
                 total_dur = round(total_dur/60))
        
        running_score <- readRDS(paste0("app_data/dat_", time_slice_key,"_running_score.rds")) %>% 
          filter(match_id == this_match) %>% drop_na(step_group) %>% 
          pivot_wider(names_from = team.name, values_from = num_goals) %>% 
          mutate_at(.vars = c(input$team, country_other), .funs = list(~cumsum(.))) %>% 
          mutate(display_score = paste(input$team, !!as.name(input$team), "-", country_other, !!as.name(country_other)),
                 status = sign(!!as.name(input$team) - !!as.name(country_other)),
                 display_status = ifelse(status == -1, "down", ifelse(status == 0, "tie", "up")),
                 facet_var = paste0("[", start_step/60, "-", round(end_step/60), "]"))
        
        events_filtered <- events_filtered %>% mutate(facet_var = period)
        
        this_step <- set_step_size(step_in_min = as.numeric(time_slice_key)) 
        this_step <- this_step %>% mutate(end_step = ifelse(step_group == max(this_step$step_group), reg_game_end_actual, end_step))
        events_filtered <- events_filtered %>% 
          fuzzyjoin::fuzzy_left_join(this_step, 
                                     by = c("ElapsedTime" = "start_step",
                                            "ElapsedTime" = "end_step"),
                                     match_fun = list(`>=`, `<`)) %>% 
          mutate(facet_var = paste0("[", start_step/60, "-", round(end_step/60), "]"))
      }
      
      p1 <- events_filtered %>% 
        mutate(recode_position = factor(recode_position, levels = c("FOR", "MID", "DEF"))) %>% 
        ggplot() + 
        geom_density_2d_filled(na.rm = T, contour_var = "ndensity", show.legend = FALSE, mapping = aes(x = x_final, y = y_final)) +
        coord_fixed(ratio = 1, xlim = c(0, 120), ylim = c(0, 80)) +
        geom_point(data = summary_dat %>% 
                     mutate(recode_position  = factor(recode_position, levels = c("FOR", "MID", "DEF"))), 
                   mapping = aes(x = avg_x, y = avg_y, size = !!as.name(size_var_key) ), color = "red") +
        theme_bw() + 
        theme(panel.grid = element_blank(),legend.position = "top",
              axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
        ## add pitch view to figure using StatsBomb example
        annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, color = "white", size = 0.4)+
        annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, color = "white", size = 0.4) +
        annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, size = 0.4, color = "white") +
        annotate("rect", xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, size = 0.4, color = "white") +
        annotate("path", size = 0.4, color = "white", x = 60 + 10*cos(seq(0, 2*pi, length.out = 2000)),
                 y = 40+10*sin(seq(0, 2*pi, length.out = 2000))) +
        geom_text(data = running_score, mapping = aes(x = 60, y = 70, label = display_score),color = "white", size = 3) +
        # scale_color_manual(values = c("down" = "red", "tie" = "darkgray", "up" = "blue")) +
        facet_grid(recode_position ~ facet_var) +
        labs(color = "", size = input$size_var)

      if(time_slice_key %in% c("full game")) {
        p1 <- p1 + facet_grid(recode_position ~ facet_var) + 
          theme(strip.text = element_blank())
      }
     
      else {
        p1 <- p1 + facet_grid(recode_position ~ facet_var)
      }
      
      # if(input$match_id == "Final (3795506)") {
      #   p1 <- p1 + labs(caption = "EXAMPLE interpretation: note change in FOR event density from period 1 (scored 1 goal) --> period 2 (no goals)") +
      #     theme(plot.caption = element_text(color = "red", face = "bold"))
      # }
      print(p1)
    }
    

  })
  
  output$tactical_info <- renderDataTable({
    
    if(!is.null(input$match_id)) {
      match_filtered <- match_info_display %>% 
        mutate(stage_date = paste0(competition_stage.name, " (", match_id, ")" )) %>% 
        filter(stage_date %in% input$match_id)
      
      this_match <- match_filtered %>% distinct(match_id) %>% pull()
      
      time_slice_key <- time_seg_lookup %>% filter(value %in% input$time_slice) %>% pull(key)
      
      if(time_slice_key %in% "full game") {
      pos_dat <- readRDS("app_data/dat_full_position_info.rds") %>%  
        filter(team.name == input$team,
               match_id == this_match,
               recode_position %in% input$pos) %>% 
        mutate(facet_var = match_id) 
      } 
      else if(time_slice_key %in% "period") {
      pos_dat <- readRDS("app_data/dat_period_position_info.rds") %>%  filter(team.name == input$team,
                                                                   match_id == this_match,
                                                                   recode_position %in% input$pos) %>% 
        mutate(facet_var = step_group) 
      }
      else {
      pos_dat <- readRDS(paste0("app_data/dat_", time_slice_key,"_position_info.rds")) %>%  filter(team.name == input$team,
                                                                   match_id == this_match,
                                                                   recode_position %in% input$pos) %>% 
        mutate(facet_var = paste0("[", start_step/60, "-", round(end_step/60), "]"))
      }
      
      pos_dat %>% 
        left_join(player_recode_position %>%
                    select(x, y, position.name, position.id), by= "position.id") %>% 
        mutate(recode_position = factor(recode_position, levels = c("FOR", "MID", "DEF"))) %>% 
        arrange(step_group, recode_position) %>%
        select(`Time Segment` = step_group, `Position` = position.name, `First Player in Time Segment` = first_player, `Total # Players in Time Segment` = n_players)
    }
    
  })

})