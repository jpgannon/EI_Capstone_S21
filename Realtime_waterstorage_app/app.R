#-------------------------------
#Description: This is a shiny web app for Real time water visaulization of 
# Hubbard Brook Watershed sites 3 and 9 - snow, well, and other specific watershed data 
#
#
# Author: Sam Lausten, 
#
#-------------------------------
#install.packages('waterData')

#load libraries
library(shiny)
library(leaflet)
library(waterData)
library()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Realtime Watershed Data - Hubbard Brook"),

    # Sidebar with daterange 
    sidebarLayout(
        sidebarPanel(
            dateInput("startdate", label = "Startdate", val=""),
            dateInput("enddate", label= "Enddate", value=Sys.Date(), max=Sys.Date())
        ),

        #main panel/tabs
        mainPanel(
           tabsetPanel(
               tabPanel('Time-series', plotlyOutput)
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
