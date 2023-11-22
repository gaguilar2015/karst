server <- function(input, output, session) {
  
  # myLogger(uinfo$email, 'login')
  
  output$ctv_plot <- renderPlot({
    
    base |> 
      ggplot(aes(x = ctv)) +
      geom_bar(fill = 'cadetblue3') +
      geom_text(
        aes(y = ..count.., label = ..count..),
        stat = 'count', 
        size = 5, 
        color = 'white',
        fontface = "bold",
        position = position_stack(0.5), 
        vjust = 0.5
      ) +
      coord_flip() +
      labs(
        title = '',
        x = 'Community',
        y = 'Count'
      ) +
      gg_blank_x +
      gg_text_charts
  })
  

  
  output$result_plot <- renderPlot({
    
    base |> 
      ggplot(aes(x = result)) +
      geom_bar(fill = 'cadetblue3') +
      geom_text(
        aes(y = ..count.., label = ..count..),
        stat = 'count', 
        size = 5, 
        color = 'white',
        fontface = "bold",
        position = position_stack(0.5), 
        vjust = 0.5
      ) +
      coord_flip() +
      labs(
        title = '',
        x = 'Result',
        y = 'Count'
      ) +
      gg_blank_x +
      gg_text_charts
  })
  
  output$progress <- renderReactable({
    df <- ksample_join |> 
      group_by(ctv_official) |> 
      summarise(
        `Sampled HHs` = sum(!is.na(interview_key)),
        `Visited HHs` = sum(!is.na(interview__key)),
        `Visited %` = paste0((`Visited HHs` * 100/`Sampled HHs`) |> round(1), '%'),
        `Completed HHs` = sum(result == 'Completed', na.rm = T),
        `Completed %` = paste0((`Completed HHs` * 100/`Sampled HHs`) |> round(1), '%'),
      ) |>
      rename(
        Community = ctv_official
      )
    
    dfn <- ksample_join |> 
      mutate(ctv_official = 'Total') |> 
      group_by(ctv_official) |> 
      summarise(
        `Sampled HHs` = sum(!is.na(interview_key)),
        `Visited HHs` = sum(!is.na(interview__key)),
        `Visited %` = paste0((`Visited HHs` * 100/`Sampled HHs`) |> round(1), '%'),
        `Completed HHs` = sum(result == 'Completed', na.rm = T),
        `Completed %` = paste0((`Completed HHs` * 100/`Sampled HHs`) |> round(1), '%'),
      ) |>
      rename(
        Community = ctv_official
      )
    
    df <- rbind(df,dfn)
    
    reactable(df)
  })
  
  output$response <- renderReactable({
    df <- base |> 
      tabyl(ctv, result, show_missing_levels = F) |> 
      adorn_totals() |> 
      adorn_percentages(c('row')) |> 
      adorn_pct_formatting(digits = 0) |> 
      adorn_ns()
    
    reactable(df)
  })
  
  output$map <- renderLeaflet({
    dfs <- ksample_join |> filter(ctv_official == input$community)
    
    getColor <- function(df) {
      sapply(df$result, function(result) {
        if(result == 'Completed' & !is.na(result)) {
          "green"
        } else if(!is.na(result)) {
          "orange"
        } else {
          "white"
        } })
    }
  
    
    icons <- awesomeIcons(
      icon = 'home',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(dfs)
    )
    

    
    leaflet(data = dfs) %>% 
      addProviderTiles(input$basemap) %>%
      addAwesomeMarkers(~GPS_Building_Navig__Longitude, 
                 ~GPS_Building_Navig__Latitude, 
                 icon = icons,
                 popup = if_else(is.na(dfs$interview__key), 
                                 paste0(
                                   "Head Name: ", dfs$head_name.x, "<br />",
                                   "No questionnaire available"
                                 ), 
                                 paste0(
                                   "Head Name: ", dfs$head_name.x, "<br />",
                                   "Result: ", dfs$result, "<br />",
                                   "<a href='http://surveys.belizeanalytics.com/bkhc/Interview/Review/",
                                   dfs$interview__id,
                                   "' target='_blank'>Questionnaire Link</a><br />"
                                 )), 
                 label = ~as.character(head_name.x)
                 )
  })
  
  output$nonrespondents <- renderReactable({
    df <- base |> 
      filter(result != 'Completed') |> 
      select(interview__key, interview__id, head_name, ctv, result, Interviewer)
    
    reactable(df |> select(-interview__id),
              columns = list(
                interview__key = colDef(cell = function(value, index) {
                  # Render as a link
                  url <- paste0("http://surveys.belizeanalytics.com/bkhc/Interview/Review/",
                                df[index, 'interview__id'])
                  htmltools::tags$a(href = url, target = "_blank", as.character(value))
                })
              )
              )
  })
  
  output$missing_result <- renderReactable({
    df <- base |> 
      filter(is.na(result)) |> 
      select(interview__key, interview__id, head_name, ctv, result, Interviewer)
    
    reactable(df |> select(-interview__id),
              columns = list(
                interview__key = colDef(cell = function(value, index) {
                  # Render as a link
                  url <- paste0("http://surveys.belizeanalytics.com/bkhc/Interview/Review/",
                                df[index, 'interview__id'])
                  htmltools::tags$a(href = url, target = "_blank", as.character(value))
                })
              )
              )
  })
  
  output$int_chart <- renderPlot({
    base |> 
      ggplot(aes(x = Interviewer)) +
      geom_bar(fill = 'cadetblue2')  +
      geom_text(
        aes(y = ..count.., label = ..count..),
        stat = 'count', 
        size = 5, 
        color = 'white',
        fontface = "bold",
        position = position_stack(0.5), 
        vjust = 0.5
      ) +
      labs(x = 'Interviewer') +
      coord_flip() +
      gg_blank_x +
      gg_text_charts
  })
  
  output$tmnr_chart <- renderPlot({
    
    base |> 
      ggplot(aes(x = k3)) +
      geom_bar(fill = 'cadetblue2')  +
      geom_text(
        aes(y = ..count.., label = ..count..),
        stat = 'count', 
        size = 5, 
        color = 'white',
        fontface = "bold",
        position = position_stack(0.5), 
        vjust = 0.5
      ) +
      labs(x = 'Ever heard of TMNR') +
      gg_blank_y +
      gg_text_charts
    
  })
  
}
