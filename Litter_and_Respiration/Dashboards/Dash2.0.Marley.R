library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(shinyWidgets)

Litterfall <-
    read_csv("C:/Users/marle/Desktop/EI Capstone/EI_Capstone_S21/Litter_and_Respiration/Litterfall.csv") %>%
  mutate(Treat = paste(Stand, Treatment))

SoilRespiration <-
    read.csv("C:/Users/marle/Desktop/EI Capstone/EI_Capstone_S21/Litter_and_Respiration/SoilResp.csv")
StandLocations <-
    read.csv("C:/Users/marle/Desktop/EI Capstone/EI_Capstone_S21/Litter_and_Respiration/StandLocations.csv")

CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, treatment) %>%
    mutate(date = mdy(date))
  

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
                h1("Map")),
        
        tabItem(tabName = "Litterfall",
                h1("Litterfall"),
                box(width = 3,
                    sliderInput("Year", label = em("Date Range:",
                            style = "text-align:center;color black;font-size:100%"),
                            min = min(Litterfall$Year),
                            max = max(Litterfall$Year),
                            value = c(min(Litterfall$Year), max(Litterfall$Year)),
                            format = "yyyy",
                            sep = "",
                           step = 1)),
                box(width = 3, 
                    selectInput("Treatment", label = em("Select Treatment:",
                    style = "text-align:center;color black;font-size:100%"),
                    unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P", "NP", "Ca", "C"))),
                box(width = 3, 
                    selectizeInput("Stand", label = em("Select Stand:",
                                 style = "text-align:center;color black;font-size:100%"),
                                choices = unique(Litterfall$Stand), multiple = TRUE, selected = "C1"),
        
                    ),
                box(plotOutput("timeseries_plot"), width = 12),

                ),
        
        tabItem(tabName = "Soil_Respiration",
                h1("Soil Respiration"),
                box(width = 12, dateRangeInput("date", "Date Range:",
                                               start = "2008-07-01",
                                               end = "2020-07-25",
                                               min = "2008-07-01",
                                               max = "2020-07-25")
                ),
                box(plotOutput("flux_ts_plot"), width = 5),
        )
    )),
)

server <- function(input, output) {
    output$flux_ts_plot <- renderPlot({
        startdate <- input$date[1]
        enddate <- input$date[2]
        
        CleanSoilResp %>%
            filter(date >= startdate & date <= enddate)%>%
            ggplot(aes(x = date, y = flux))+
            geom_line(color = "black")+
            labs(title = "Soil Respiration Flux", 
                 x = "Date", 
                 y = "CO2 efflux per unit area (μg CO2/m2/s)") +
            geom_smooth(method = "lm")
    })
    
    output$timeseries_plot <- renderPlot({
        min <- input$Year[1]
        max <- input$Year[2]
        Treatment <- input$Treatment
        Stand <- input$Stand

        
        Litterfall %>%
            filter(Year >= min & Year <= max) %>%
            filter(Treatment == input$Treatment) %>%
            filter(Stand == input$Stand) %>%
            ggplot(aes(x=Year, y=whole.mass, color = Treat)) +
            geom_line()+
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
            labs(title ="Time Series Litterfall",
                 x = "Year",
                 y = "Mass (g litter /m2)") +
          facet_wrap(facets = "Stand", ncol = 1)
          }) 

}

shinyApp(ui, server)