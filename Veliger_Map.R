library(shiny)
library(tidyverse)
library(shiny.fluent)
library(sf)
library(spData)
library(leaflet)
library(DT)
library(plotly)

data <- read_csv("final_mussel_data.csv")
shapes_2 <- st_read("HUC2.shp")
shapes_4 <- st_read("HUC4.shp")
shapes_6 <- st_read("HUC6.shp")
shapes_8 <- st_read("HUC8.shp")

ui <- fluidPage(
  tags$style(HTML(".welcome-label {font-size: 24px; 
                  color: #2C3E50; 
                  font-weight: bold; 
                  text-align: center; 
                  margin-top: 20px;}",
                  ".names {font-size: 18px; 
                  color: #000000; 
                  font-weight: bold; 
                  text-align: center; 
                  margin-top: 0px;}")),
  
  tags$div(class = "welcome-label", "Welcome to the Veliger Interactive Website!"),
  tags$div(class = "names", "Teddy Wagner and Anand Achit-Erdene"),
  
  fluidRow(
    column(6, leafletOutput(outputId = "map", height = "400px", width = "100%")),
    column(6, selectizeInput("state", "Choose a State to Explore",
                             choices = unique(data$state),
                             options = list(search = FALSE)),
           dataTableOutput(outputId = "state_table"))),
  
  fluidRow(
    column(4, selectInput("change_huc", "Choose a HUC to Display", 
                          choices = c("No HUC Data", "HUC2", "HUC4", "HUC6", "HUC8"))),
    column(4, textInput("waterbody_name", "Search for a Waterbody")),
    column(4, dateRangeInput("date_range", "Select a Date Range",
                             start = min(data$sample_date), end = max(data$sample_date),
                             format = "yyyy-mm-dd"))),
  
  fluidRow(
    column(12, plotlyOutput("bar_chart"), height = "250px")))

server <- function(input, output, session) {
  
  huc_choice <- reactiveVal("huc0")
  data$color <- ifelse(data$veliger_detect == 1, "red", "green")
  
  filtered_data <- reactive({
    filtered <- data
    
    if (input$waterbody_name != "") {
      filtered <- filtered |> 
        filter(str_detect(waterbody_name, regex(input$waterbody_name, ignore_case = TRUE)))}
    
    if (!is.null(input$date_range)) {
      filtered <- filtered |> 
        filter(sample_date >= input$date_range[1] & sample_date <= input$date_range[2])}
    
    return(filtered)})
  
  output$map <- renderLeaflet({
    leaflet(filtered_data()) |> 
      addTiles() |> 
      addAwesomeMarkers(lat = ~latitude,
                        lng = ~longitude,
                        label = ~waterbody_name,
                        icon = ~awesomeIcons(icon = ifelse(filtered_data()$veliger_detect == 1, 
                                                           "times", "check"),
                                             library = "fa",
                                             markerColor = color),
                        layerId = ~waterbody_name) |> 
      addLegend(position = "bottomright",
                colors = c("red", "green"),
                labels = c("Yes", "No"),
                title = "Veliger Detected?",
                opacity = 1)})
  
  output$state_table <- renderDT({
    filtered_data() |> 
      filter(state == input$state)
  }, options = list(dom = 't',
                    scrollY = "285px",
                    scroller = TRUE))
  
  observeEvent(input$change_huc, {
    if (input$change_huc == "No HUC Data") {
      leafletProxy("map") |> 
        clearShapes()
    } else if (input$change_huc == "HUC2") {
      huc_choice("huc2")
      leafletProxy("map") |> 
        clearShapes() |> 
        addPolygons(data = shapes_2, label = ~name, 
                    weight = 1, fillOpacity = 0.1)
    } else if (input$change_huc == "HUC4") {
      huc_choice("huc4")
      leafletProxy("map") |> 
        clearShapes() |> 
        addPolygons(data = shapes_4, label = ~name, 
                    weight = 1, fillOpacity = 0.1)
    } else if (input$change_huc == "HUC6") {
      huc_choice("huc6")
      leafletProxy("map") |> 
        clearShapes() |> 
        addPolygons(data = shapes_6, label = ~name, 
                    weight = 1, fillOpacity = 0.1)
    } else if (input$change_huc == "HUC8") {
      huc_choice("huc8")
      leafletProxy("map") |> 
        clearShapes() |> 
        addPolygons(data = shapes_8, label = ~name, 
                    weight = 1, fillOpacity = 0.1)}})
  
  observeEvent(input$map_marker_click, {
    clicked_waterbody <- input$map_marker_click$id
    
    output$state_table <- renderDT({
      data |> 
        filter(waterbody_name == clicked_waterbody)
    }, options = list(dom = 't',
                      scrollY = "285px",
                      scroller = TRUE))
    
    output$bar_chart <- renderPlotly({
      filtered <- filtered_data() |> 
        filter(waterbody_name == clicked_waterbody)
      
      chart_data <- filtered |>
        mutate(water_year = as.factor(format(sample_date, "%Y"))) |>
        count(water_year, veliger_detect) |>
        mutate(veliger_detect = ifelse(veliger_detect == 1, "Yes", "No"))
      
      plot_ly(chart_data, x = ~water_year, y = ~n, type = 'bar', color = ~veliger_detect, 
              colors = c("No" = "#008000", "Yes" = "red"), barmode = "stack") |>
        layout(title = paste("Veliger Detection for", clicked_waterbody),
               xaxis = list(title = "Water Year"),
               yaxis = list(title = "Count"))})})}


shinyApp(ui, server)
