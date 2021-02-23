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
library(DT) #MU: Helpful for displaying data tables.
library(tidyverse) #MU: I added tidyverse because it has ggplot2 and other good functions 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Realtime Watershed Data - Hubbard Brook"),

    # Sidebar with daterange 
    sidebarLayout(
        sidebarPanel(
            dateInput("startdate", label = "Start Date", val=""), #MU: Should we make the default start value the first data present in the data we read in?
            dateInput("enddate", label= "End Date", value=Sys.Date(), max=Sys.Date()),
            width = 3
        ),

        #main panel/tabs
        mainPanel(
           tabsetPanel(
               tabPanel('About',h4("This app visualizes real-time data from Hubbard Brook research watershed sites 3 and 9, 
                                   as well as related snowpack and weather conditions, for a date range selected to compare the two sites
                                 or by certain perameters."),
               #tabPanel('Watershed Visualizations', plotlyOutput),
               tabPanel('Table' ,tableOutput("table"))#,
               #tabPanel('Map of Stations', leafletOutput("map",width = '100%'))
           )
        ) #mainPanel
    )#sidebarLayout
))#fluidPage

# Define server
server <- function(input, output) {
    #----------------
    # read in cleaned watershed data
    # ---------------
    output$table <- renderDT(ws3_upper_snowdat_hr, #MU: The table won't show yet because this package requires the data to be read into a reactive datatable format "reactiveFileReader()"
                             class = "display nowrap compact", #MU: this is the style of the table
                             filter = "top") #MU: This places the filter at the top of the table
    #MU: This is a placeholder table for when we finish cleaning the data and can input summarized values
    
}

# Run the application 
shinyApp(ui = ui, server = server)
