#####
# Shiny app, uses USGS water chemistry data to create heatmap
#####

# Read in packages
library(shiny)
library(leaflet) # Package to create basemap
library(tidyverse)
library(leaflet.extras) 
library(shinydashboard)
library(dataRetrieval) # Package developed by USGS to download data from their website

# Define UI for application 
ui <- fluidPage(
  
  # Import data, you have to have run the loader.R script first or the app will crash unfortunately,
  # this is kind of unnacceptable and I wan to fix it
  data <- cname,
  
  # Set variables for max & min of data range to call for range adjuster
  min.chem <- min(data$avg),
  max.chem <- max(data$avg),
  
  titlePanel("Water Chemistry Heatmap"),
  
  sidebarLayout(
    sidebarPanel(
      
      h3("Data Read-In"),
      
      # To enter your desired state & county
      textInput("state", "Enter state (ex: 'California'):"),
      
      textInput("county", "Enter county (ex: 'Orange County'):"),
      
      # Executes code in server to download desired USGS data 
      actionButton("searcher", 
                   "Get Data",
                   icon = icon("thumbs-up")),
      
      h3("Map Input"),
      
      # ui for mg/l chemistry range adjustment
      sliderInput("chemrange",
                  "Range of measurement",
                  min = 0,
                  max = 500,
                  value = c(0, 500),
      ),
      
      # The multiplier is useful for humans to uniformly edit the size of the circles of the 
      # heatmap to most easily understand what they are looking at
      sliderInput("multi",
                  "Multiplier",
                  min = 1,
                  max = 2000,
                  value = 250,
      ),
      
      # Provides choices based on vectors from data table
      selectInput(inputId = "char",
                  label = "Variable: ",
                  choices = unique(data$CharacteristicName), # Takes unique values from vector (ex: Sulfate)
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
    
    # Creates map itself
    mainPanel(
      
      # Height set by trial and error to make it fill the screen and be much more prettier
      leafletOutput(outputId = "mymap", height = 600)
    ),
  )
)


server <- function(input, output, session) {
  
  # This filters the raw data using choices selected in the ui to give you the desired range
  # of the chemical selected
  filt_chem <- reactive({
    
    data %>%
      filter(CharacteristicName == input$char) %>%
      filter(avg >= min(input$chemrange)) %>%
      filter(avg <= max(input$chemrange))
  })
  
  # Create map
  output$mymap <- renderLeaflet({
    
    # Use leaflet to make map using data filtered with filt_chem variable above
    leaflet(filt_chem()) %>%
      # Use generic basemap 'tiles'
      addTiles() %>%
      addFullscreenControl() %>%
      # Really useful as it dynamically resizes the map window based on min and max lat/long
      # coordinates in your filtered data
      fitBounds(~min(filt_chem()$LongitudeMeasure),
                ~min(filt_chem()$LatitudeMeasure),
                ~max(filt_chem()$LongitudeMeasure),
                ~max(filt_chem()$LatitudeMeasure))
  })
  
  observe({
    
    pal <- colorNumeric(
      # Pallette needs to be in observe function or else the colors won't be resized to
      # changing variable selection (ie: if you had a bunch of low values the whole map would
      # have gold circles)
      palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
      # Creates bins by dividing data based on value and colors them with one of these six colors
      # with gold being the lowest values and dark red those with the highest values of filtered 
      # data, this filtered data range is set by the domain
      domain = filt_chem()$avg)
    
    # Leaflet proxy is useful as the entire map is not redrawn after changing the sliders and 
    # just the individual points change
    leafletProxy("mymap", data = filt_chem()) %>%
      # Without clearShapes() circles will be drawn on top of the same circles and will not 
      # disappear creating a huge messy pile of colors
      clearShapes() %>%
      # You could use many different shapes for your heatmap
      addCircles(lat = ~ LatitudeMeasure, lng = ~ LongitudeMeasure, # Tells it where to put the points on the map
                 weight = 1, radius = ~sqrt(filt_chem()$avg)*input$multi, # Multiplier function, just multiplies the avg by an integer you slide to
                 popup = ~as.character(avg), # When you move your cursor over circle it tells you its value
                 label = ~as.character(paste0('Chem: ', avg)),
                 color = ~pal(avg), fillOpacity = 0.5) # Applies color pallette and makes the circles 50% transparent
    
  })
  
  observeEvent(input$searcher, {
    print("Getting your data...")
    
    # Don't know why but this is the only way I could get it to work, put observe inside of 
    # observeEvent to get the code to actually execute
    observe({
      
      # A simple try function allows the app not to crash when you enter nonsense state or county,
      # this is the part where we use the USGS package to download their data
      try(siteData <<- readWQPdata(statecode = input$state,
                               countycode = input$county,
                               SiteType = c("Stream", "Spring", "Well"),
                               SampleMedia = "Water",
                               CharacteristicType = c("Inorganics, Major, Metals",
                                                      "Inorganics, Major, Non-metals")))
      
      
      # Data frame with site info: name, lat/long, drainage area, etc
      siteInfo <<- attr(siteData, "siteInfo")
      
      # Combine both tables for one comprehensive table of data + site ID
      water_data <<- siteData %>%
        left_join(siteInfo, by = "MonitoringLocationIdentifier") %>%
        select("MonitoringLocationIdentifier",
               "MonitoringLocationName",
               "CharacteristicName",
               "ResultSampleFractionText",
               "ResultMeasureValue",
               "ResultMeasure.MeasureUnitCode",
               "LatitudeMeasure",
               "LongitudeMeasure") %>%
        filter(ResultMeasure.MeasureUnitCode %in% c("mg/l", 'mg/l CaCO3', 'mg/l as Na'),
               ResultSampleFractionText != "Total") %>%
        distinct() %>%
        group_by(MonitoringLocationIdentifier)
      
      # Takes all the essential data we need for the app
      cname <<- water_data %>%
        group_by(CharacteristicName, MonitoringLocationIdentifier,
                 LatitudeMeasure, LongitudeMeasure) %>%
        summarise(avg = mean(ResultMeasureValue)) # Averages all of the meaurements from the same
                                                  # site, in the future we could make the map have 
                                                  # a time element which would be pretty cool
      
      print("Data retrieved")
    
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)