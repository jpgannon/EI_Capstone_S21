library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)


Litterfall <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/Litterfall.csv")
SoilRespiration <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/SoilResp.csv")
StandLocations <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/StandLocations.csv")

CleanSoilResp <- select(SoilRespiration, date, stand, flux, treatment)

CleanSoilResp <- filter(CleanSoilResp, flux < 500, flux >= 0)

#CleanLit <- select()

#FixedLocations <-



ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("Home Page", tabName = "Home_Page", icon = icon("home")),
        menuItem("Map", tabName = "Map", icon = icon("globe")),
        menuItem("Litterfall", tabName = "Litterfall", icon = icon("leaf")),
        menuItem("Soil Respiration", tabName = "Soil_Respiration", icon = icon("tint"))
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "Home_Page",
                h1("Home Page, desciption of app and how to use will be placed here")),
        tabItem(tabName = "Map",
                box(width = 8, sliderInput("Year", "Year:", 
                                           min = min(Litterfall$Year),
                                           max = max(Litterfall$Year),
                                           value = c(min(Litterfall$Year), max(Litterfall$Year)),
                                           sep = "",
                                           step = 1)),
                box(width = 4, selectInput("Stand", "Stand:", StandLocations$Site)),
                fluidRow(box(width = 12, leaflet()%>% addTiles())),
                h1("Map")),
        tabItem(tabName = "Litterfall",
                box(
                    #plot code goes here
                ),
                h1("Litterfall")),
        tabItem(tabName = "Soil_Respiration",
                box(plotOutput("correlation_plot"), width = 8),
                box(
                    selectInput("Flux", "Flux", 
                                c("flux","stand"))
                ),
                h1("Soil Respiration"))
    ))
)

server <- function(input, output) {
    output$correlation_plot <- renderPlot({
        plot(CleanSoilResp$treatment, CleanSoilResp[[input$Flux]],
             xlab = "Treatment", ylab = "Flux")
    })
    
}


shinyApp(ui, server)