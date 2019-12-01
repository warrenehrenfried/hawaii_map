#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Read in packages, probably don't need all of these
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinydashboard)
library(dataRetrieval)

# # import data
# data <- cname
# 
# # Set variables for max & min of data range to call for range adjuster
# min.chem <- min(data$avg)
# max.chem <- max(data$avg)

# Uses vectors from data table to use to create choices in select input 
axis_vars <- names(data)

# Define UI for application 
ui <- fluidPage(
  
  # import data
  data <- cname,
  
  # Set variables for max & min of data range to call for range adjuster
  min.chem <- min(data$avg),
  max.chem <- max(data$avg),
  
  
  titlePanel("Water Chemistry Heatmap"),
  
  sidebarPanel(
    
    h3("Map Input"),
    
    # uiOutput("sliderUI"),
    
    # ui for chem range adjustment, takes variables made above
    sliderInput("chemrange",
                "Range of measurement",
                min = min.chem,
                max = max.chem,
                value = c(min.chem, max.chem),
    ),
    
    sliderInput("multi",
                "Multiplier",
                min = 1,
                max = 2000,
                value = 250,
    ),
    
    # Provides choices based on vectors from data table
    
    selectInput(inputId = "char",
                label = "Variable: ",
                choices = unique(data$CharacteristicName),
                selected = "Sulfate"),
    
    # Graph not built yet
    
    h3("Graph Input"),
    
    selectInput(inputId = "x",
                label = "X-Value: ",
                choices = unique(data$CharacteristicName),
                selected = "Chloride"),
    
    selectInput(inputId = "y",
                label = "Y-Value: ",
                choices = unique(data$CharacteristicName),
                selected = "Magnesium")
  ),
  
  # Creates map itself, absolutePanel to zoom it over Hawaii, plot not built yet
  mainPanel(
    leafletOutput(outputId = "mymap"),
    
    absolutePanel(top = 60, left = 20),
    
    plotOutput("plot")
  ),
  
  titlePanel(
    title = "Data Read-In"
  ),
  
  sidebarPanel(
    
    textInput("state", "Enter state (ex: 'Tennessee'):"),
    
    textInput("county", "Enter county (ex: 'Knox County'):"),
    
    actionButton("go", 
                 "Get Data",
                 icon = icon("thumbs-up"))
    
  )
)


server <- function(input, output, session) {
  
  # output$slider <- renderUI({
  #   sliderInput("chemrange", "Slider",
  #               min = input$min_val, 
  #               max = input$max_val, 
  #               value = 2000)
  # })
  
  filt_chem <- reactive({
    
    data %>%
      filter(CharacteristicName == input$char) %>%
      filter(avg >= min(input$chemrange)) %>%
      filter(avg <= max(input$chemrange))
  })
  
  # output$sliderUI <- renderUI({
  #   sliderInput("slider", "Slider",
  #               min = min(filt_chem()),
  #               max = max(filt_chem()),
  #               value = 1)
  # })


  
  # m_chem <- eventReactive(input$go, {
  #   ggplot(filt_dia(), aes_string(x = input$xvar, y = input$yvar, colour = input$color)) + 
  #     geom_point()
  # })
  
  # define color pallette
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$avg)
  
  # create map
  output$mymap <- renderLeaflet({
    leaflet(filt_chem()) %>%
      # set view over Hawaii
      setView(lng = -155.5, lat = 19.2, zoom = 7) %>%
      addTiles() 
  })
  
  # output$plot <- renderPlot({

  # next we use the observe function to make the checkboxes dynamic. If you leave 
  # this part out you will see that the checkboxes, when clicked on the first 
  # time, display our filters...But if you then uncheck them they stay on. So we
  # need to tell the server to update the map when the checkboxes are unchecked.
  
  observe({
    
    leafletProxy("mymap", data = filt_chem()) %>%
      addCircles(lat = ~ LatitudeMeasure, lng = ~ LongitudeMeasure,
                 weight = 1, radius = ~sqrt(filt_chem()$avg)*input$multi, 
                 popup = ~as.character(avg),
                 label = ~as.character(paste0('Chem: ', avg)),
                 color = ~pal(avg), fillOpacity = 0.5)
      
  })
  
  # output$plot <- renderPlot({
  #   
  #   # d_filt needs () to call it for some reason (needs to execute)
  #   ggplot(data, aes(x = input$x, y = input$y)) +
  #     geom_point()
    
 # })
  
  # read in desired data: Major inorganic ions in stream and well water, 
  # on the Big Island, Hawaii
  
  
  eventReactive(input$go, {
    siteData <- readWQPdata(statecode = str(input$state),
                            countycode = str(input$county),
                            SiteType = c("Stream", "Spring", "Well"),
                            SampleMedia = "Water",
                            CharacteristicType = c("Inorganics, Major, Metals",
                                                   "Inorganics, Major, Non-metals"))
    
    # data frame with site info: name, lat/long, drainage area, etc
    siteInfo <- attr(siteData, "siteInfo")
    
    # combine both tables for one comprehensive table of data + site ID
    Hawaii_water_data <- siteData %>%
      left_join(siteInfo, by = "MonitoringLocationIdentifier") %>%
      select("MonitoringLocationIdentifier",
             "MonitoringLocationName",
             "MonitoringLocationTypeName",
             "ActivityStartDate",
             "ActivityStartTime.Time",
             "CharacteristicName",
             "ResultSampleFractionText",
             "ResultMeasureValue",
             "ResultMeasure.MeasureUnitCode",
             "USGSPCode",
             "WellDepthMeasure.MeasureValue",
             "WellDepthMeasure.MeasureUnitCode",
             "LatitudeMeasure",
             "LongitudeMeasure") %>%
      filter(ResultMeasure.MeasureUnitCode %in% c("mg/l", 'mg/l CaCO3', 'mg/l as Na'),
             ResultSampleFractionText != "Total") %>% 
      distinct() %>%
      group_by(MonitoringLocationIdentifier)
    
    cname <- Hawaii_water_data %>%
      group_by(CharacteristicName, MonitoringLocationIdentifier,
               LatitudeMeasure, LongitudeMeasure) %>%
      summarise(avg = mean(ResultMeasureValue))
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

