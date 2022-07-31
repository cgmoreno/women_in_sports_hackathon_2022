shiny::shinyUI(fluidPage(
  titlePanel("Evaluating Gameplay Patterns in the UEFA Euro 2020 Matches"),
  span(tags$h6(data_source_snippet),style="color:gray; font-size: 8px;font-style: italic;"),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("time_slice", "Segment Game By:", choices = time_seg_lookup$value, selected = "Period"),
      selectInput("size_var", "Summarize Gameplay Info For:", choices = metric_lookup$value, selected = "Total # events"),
      selectInput("pos", "Include Position Lines:", choices = c("FOR", "MID", "DEF"), multiple = TRUE, selected = c("FOR", "MID", "DEF"))
    ),
    mainPanel(
      fluidRow(
        column(4, 
               selectInput("team", "Select Team", choices = match_info_display %>% distinct(team.name) %>% pull(), selected = "England")
               ),
        column(4, 
               uiOutput("return_matches")
               )
        ),
      tableOutput("match_info"),
      span(textOutput("match_periods_total_text"), style="color:gray; font-size: 10px;font-style: italic;"),
      plotOutput("field_view_eda"),
      span(plot_description, style="color:gray; font-size: 10px;font-style: italic;"),
      br(),
      checkboxInput("display_position_info", "Show Lineup Info"),
      conditionalPanel(
        condition= "input.display_position_info == true",
        dataTableOutput("tactical_info")
        )
      )
    )
  )
  )