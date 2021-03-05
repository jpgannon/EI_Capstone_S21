library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(ggthemes)


Litterfall <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/Litterfall.csv")
SoilRespiration <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/SoilResp.csv")
StandLocations <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/StandLocations.csv")
lat_long <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/lat_long.csv")

CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, treatment) 
CleanLitter <- Litterfall %>% select(Year, whole.mass, Stand, Treatment)

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
            h1("Soil Respiration"),
            box(width = 12, dateRangeInput("date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-07-25",
                                           min = "2008-07-01",
                                           max = "2020-07-25")),
            box(plotOutput("flux_ts_plot"), width = 12))
  ))
)

server <- function(input, output) {
  output$timeseries_plot <- renderPlot({
    min <- input$Year[1]
    max <- input$Year[2]
    Treatmentselection <- input$Treatment
    Standselection <- input$Stand
    
    Litterfall %>%
      filter(Year >= min & Year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      
      ggplot(aes(x = Year, group = interaction(Treatment, Year), y = whole.mass, color = Treatment)) +
      geom_boxplot(size = 1) +
      geom_point(size = 1) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(title ="Time vs. Litterfall Mass Time Series",
           x = "Year",
           y = "Mass (g litter /m2)") +
      facet_wrap(facets = "Stand", ncol = 5)
  })
  
  output$litterfall_box <- renderPlot({
    min <- input$Year[1]
    max <- input$Year[2]
    Treatmentselection <- input$Treatment
    Standselection <- input$Stand
    
    Litterfall %>%
      filter(Year >= min & Year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      ggplot(aes(x=Treatment, y=whole.mass, color = Treatment)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 5, lwd = 1.5) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme_bw() +
      labs(title ="Litterfall Mass vs. Treatment Type Boxplot",
           x = "Treatment",
           y = "Mass (g litter /m2)") +
      facet_wrap(facets = "Stand", ncol = 5)
    
  })
  
}


shinyApp(ui, server)
