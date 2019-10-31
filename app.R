#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

# import data
data <- read.csv('Data_Files/tidydata/kil_haw_data.csv')

# Define UI for application 
ui <- fluidPage(
  
  mainPanel(
    leafletOutput(outputId = "mymap"),
    
    absolutePanel(top = 60, left = 20,
                  checkboxInput('heat', 'Heatmap', FALSE)
                )
  )
)

server <- function(input, output, session) {
  
  # define color pallette
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$sulfate
  )
  
  # create map
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      # set view over Hawaii
      setView(lng = -155.5, lat = 19.2, zoom = 7) %>%
      addTiles() %>%
      addCircles(data = data, lat = ~ latitude, lng = ~ longitude,
                 weight = 1, radius = ~sqrt(sulfate)*500, 
                 popup = ~as.character(sulfate),
                 label = ~as.character(paste0('Sulfate: ', sulfate)),
                 color = ~pal(sulfate), fillOpacity = 0.5)
  })
  
  # next we use the observe function to make the checkboxes dynamic. If you leave 
  # this part out you will see that the checkboxes, when clicked on the first 
  # time, display our filters...But if you then uncheck them they stay on. So we
  # need to tell the server to update the map when the checkboxes are unchecked.
  
}

# Run the application 
shinyApp(ui = ui, server = server)

