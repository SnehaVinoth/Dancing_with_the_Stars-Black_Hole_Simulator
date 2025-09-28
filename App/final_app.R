library(shiny)
library(plotly)
library(dplyr)

td <- read.csv("Data/td_data.csv")

td$scaled_td <- td$time_dilation * 1e16
min_td <- min(td$scaled_td)
max_td <- max(td$scaled_td)
td$scaled_td <- (td$scaled_td - min_td) / (max_td - min_td)

top10 <- td %>% arrange(desc(scaled_td)) %>% slice(1:10)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: black; color: white; }
    .well { background-color: black !important; border-color: white; }
    .form-control, .selectize-input { background-color: #222 !important; color: white !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-line { background: white; }
    .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: white; color: black; }
  "))),
  
  titlePanel("Time Dilation Near Sagittarius A*"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_star", "Select a Star:", 
                  choices = top10$simbad_main_id),
      sliderInput("td_slider", "Time Dilation Range (0 to 1):", 
                  min = 0, max = 1,
                  value = c(0, 1), step = 0.01),
      h4("Earth Time:"),
      uiOutput("earth_time"),
      h4("Time on Selected Star:"),
      uiOutput("star_time")
      
    ),
    
    mainPanel(
      plotlyOutput("plot3d", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  start_time <- Sys.time()
  timer <- reactiveTimer(1000)  # refresh every 1000ms = 1s
  
  filtered_data <- reactive({
    td %>% filter(scaled_td >= input$td_slider[1], scaled_td <= input$td_slider[2])
  })
  
  output$plot3d <- renderPlotly({
    plot_data <- filtered_data()
    selected_star_data <- td %>% filter(simbad_main_id == input$selected_star)
    
    plot_ly() %>%
      add_trace(
        data = plot_data,
        x = ~simbad_ra, y = ~simbad_dec, z = ~gaia_dist_to_sgra,
        type = 'scatter3d', mode = 'markers',
        marker = list(
          size = 3,
          opacity = 0.7,
          color = ~scaled_td,
          colorscale = list(c(0, 1), c("blue", "red")),
          colorbar = list(
            title = "Time Dilation",
            x = 0.5,
            xanchor = "center",
            y = -0.3,
            yanchor = "top",
            orientation = "h"  # horizontal colorbar
          )
        ),
        text = ~simbad_main_id,
        hoverinfo = 'text',
        name = "Stars"
      ) %>%
      add_trace(
        data = selected_star_data,
        x = ~simbad_ra, y = ~simbad_dec, z = ~gaia_dist_to_sgra,
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 6, color = 'lime'),
        text = ~simbad_main_id,
        hoverinfo = 'text',
        name = "Selected Star"
      ) %>%
      add_trace(
        x = 0, y = 0, z = 0,
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 10, color = 'white'),
        text = "Sagittarius A*",
        hoverinfo = "text",
        name = "Sagittarius A*"
      ) %>%
      layout(
        scene = list(
          xaxis = list(title = "RA", backgroundcolor = "black", color = "white"),
          yaxis = list(title = "Dec", backgroundcolor = "black", color = "white"),
          zaxis = list(title = "Distance", backgroundcolor = "black", color = "white"),
          bgcolor = "black"
        ),
        paper_bgcolor = "black",
        plot_bgcolor = "black",
        font = list(color = "white")
      )
  })
  
  output$earth_time <- renderUI({
    timer()
    current_time <- format(Sys.time(), "%H:%M:%S")
    tags$div(style = "font-size: 24px; font-family: monospace; color: white;", current_time)
  })
  
  output$star_time <- renderUI({
    timer()
    td_now <- td %>% filter(simbad_main_id == input$selected_star)
    td_factor <- td_now$time_dilation * 10.5  # exaggerated factor
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    star_time <- start_time + elapsed * td_factor
    formatted <- format(star_time, "%H:%M:%S")
    tags$div(style = "font-size: 24px; font-family: monospace; color: red;", formatted)
  })
  
}


shinyApp(ui, server)
