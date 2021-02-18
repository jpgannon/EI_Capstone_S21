library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)


Litterfall <-
  read.csv("C:/Users/Jake Kwak/Downloads/LitterFall.csv")
SoilRespiration <-
  read.csv("C:/Users/Jake Kwak/Downloads/SoilResp.csv")
StandLocations <-
  read.csv("C:/Users/Jake Kwak/Downloads/StandLocations.csv")


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
                            min = min(Litterfall$year),
                            max = max(Litterfall$year),
                            value = c(min(Litterfall$year), max(Litterfall$year)),
                            sep = "",
                            step = 1)),
            box(width = 4, selectInput("Stand", "Stand:", StandLocations$Site)),
            fluidRow(box(width = 12, leaflet()%>% addTiles())),
            h1("Map")),
    tabItem(tabName = "Litterfall",
            h1("Litterfall")),
    tabItem(tabName = "Soil_Respiration",
            h1("Soil Respiration"))
  ))
)

server <- function(input, output) {
  
}


shinyApp(ui, server)
