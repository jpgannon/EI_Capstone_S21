library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)


Litterfall <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/Litterfall.csv")
SoilRespiration <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/SoilResp.csv")
StandLocations <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/StandLocations.csv")
lat_long <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/lat_long.csv")

CleanSoilResp <- select(SoilRespiration, date, stand, flux, treatment)

CleanSoilResp <- filter(CleanSoilResp, flux < 500, flux >= 0)

lat_long <- lat_long%>%mutate(popup_info = paste("Stand:",Site))

colors <- c("green", "blue")
pal <- colorFactor(colors, lat_long$Site)

#lat_long<- lat_long%>%mutate(popup_info = paste("Stand",stand,"<br/>",
#                                                           treatment,"<br/>",
#                                                          "Flux:", flux, "<br/>",
#                                                         date))

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
            h1("Home Page, desciption of app and
               how to use will be placed here")),
    tabItem(tabName = "Map",
            box(width = 8, sliderInput("Year", "Year:", 
                                       min = min(Litterfall$Year),
                                       max = max(Litterfall$Year),
                                       value = c(min(Litterfall$Year), max(Litterfall$Year)),
                                       sep = "",
                                       step = 1)),
            #Stand selection box
            box(width = 4, selectInput("Stand", "Stand:", lat_long$Site)),
            #Mapping the stands with circle markers
            fluidRow(box(width = 12, leaflet()%>% 
                           addTiles()%>% 
                           addCircleMarkers(data = lat_long,
                                            lat = ~Lat, 
                                            lng = ~Long, 
                                            radius = 3,
                                            popup = ~popup_info,
                                            color = ~pal(Site)))),
            h1("Map")),
    tabItem(tabName = "Litterfall",
            box(
              #plot code goes here
            ),
            h1("Litterfall")),
    tabItem(tabName = "Soil_Respiration",
            box(plotOutput("correlation_plot"), width = 8),
            box(selectInput("Flux", "Flux", 
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
  
  output$sites <- ({})
  
}


shinyApp(ui, server)
