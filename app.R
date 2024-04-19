#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a map
ui <- fluidPage(

    # Application title
    titlePanel("EV Locations Data"),
    
    sidebarLayout(
      sidebarPanel(
        textInput("state", label = h3("State"), value = "CT"),
        ## Take text input for network
        textInput("network", label = h3("Network"), value = "All Networks"),
        ## Select from level 2 or level 3
        selectInput("level", label = h3("Level"), choices = c("Level 2", "Level 3")),
        dateRangeInput("dates", label = h3("Date range"), start = "2015-01-01", end = "2024-12-31")
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
)


# Define server logic required to draw a map
server <- function(input, output) {
  output$value <- renderPrint({ input$text })
  df <- read.csv('EVstations.csv')
  ## Constrain df to only rows where state is the input
  df1 <- reactive({
    state <- input$state
    network <- input$network
    dates <- input$dates
    level <- input$level
    filtered <- df
    filtered <- filtered[filtered$state == state,]
    if(network == "All Networks"){
      filtered <- filtered
    } else {
      filtered <- filtered[filtered$network == network,]
    filtered <- filtered[filtered$open.date >= dates[1] & filtered$open.date <= dates[2],]
    if(level == "Level 2"){
      filtered <- filtered[filtered$lev2 != 0,]
    } else {
      filtered <- filtered[filtered$lev3 != 0,]
    }
    }
    return(filtered)
  })
  
  output$map <- renderLeaflet({
    leaflet(data = df1(), 
            height = 480, 
            width = 800) %>% 
      addTiles() %>%                       
      addCircleMarkers(lng = ~lon,         
                       lat = ~lat,         
                       radius = ~lev2 + lev3,          
                       color = 'gray', 
                       stroke = FALSE,     
                       fillOpacity = 0.5,
                  
                       popup = ~label,       
                       label = ~label %>% lapply(HTML)
      )
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
