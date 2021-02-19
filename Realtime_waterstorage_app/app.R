#-------------------------------
#Description: This is a shiny web app for Real time water visaulization of 
# Hubbard Brook Watershed sites 3 and 9 - snow, well, and other specific watershed data 
#
#
# Author: Sam Lausten, 
#
#-------------------------------


#load libraries
library(shiny)
library(leaflet)
library(waterData)
library(plotly)

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
               tabPanel('Watershed Visualizations', plotlyOutput),
               tabPanel('Table view' ,tableOutput("table")),
               tabPanel('Map of Stations', leafletOutput("map",width = '100%')),
               tabPanel('About',h4("This app visualizes real-time data from Hubbard Brook research Watershed sites, 
                                   as well as related snowpack and weather conditions, for a date range selected to compare the two sites
                                   or by certain perameters")
      
           )
        ) #mainPanel
    )#sidebarLayout
)#fluidPage

# Define server
server <- function(input, output) {
    #----------------
    # read in cleaned watershed data
    # ---------------
    
    


# Run the application 
shinyApp(ui = ui, server = server)
