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
library(DT)

Litterfall <-
    read_csv("C:/Users/marle/Desktop/EI Capstone/EI_Capstone_S21/Litter_and_Respiration/Litterfall.csv") %>%
  mutate(Treatment = paste(Treatment)) 


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
        menuItem("Soil Respiration", tabName = "Soil_Respiration", icon = icon("tint")),
        menuItem("Litterfall Data", tabName = "Litterfall_Data", icon = icon("table"))
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
                box(plotOutput("litterfall_box"), width = 12)
                ),
        
        tabItem(tabName = "Soil_Respiration",
                h1("Soil Respiration"),
                box(width = 12, dateRangeInput("date", "Date Range:",
                                               start = "2008-07-01",
                                               end = "2020-07-25",
                                               min = "2008-07-01",
                                               max = "2020-07-25")
                ),
                box(plotOutput("flux_ts_plot"), width = 4)),
        tabItem(tabName = "Litterfall_Data",
                        h1("Litterfall Data"),
                DT:: dataTableOutput("litterfalltable"))
    )),
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
            mutate(Year = as.factor(Year)) %>%
          ggplot(aes(x = Year, y = whole.mass)) +
            geom_boxplot(aes(x = Year, y = whole.mass, color = Treatment), position = position_dodge(0.8))+
            geom_dotplot(aes(x = Year, y = whole.mass, color = Treatment), position = position_dodge(0.8), 
                       binaxis = "y")+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
            labs(title ="Time vs. Litterfall Mass Time Series",
                 x = "Year",
                 y = "Mass (g litter /m2)") +
          facet_wrap(facets = "Stand", ncol = 4)
          })
    
    output$litterfall_box <- renderPlot({
      min <- input$Year[1]
      max <- input$Year[2]
      Treatmentselection <- input$Treatment
      Standselection <- input$Stand
      
        Litterfall %>%
          filter(Year >= min & Year <= max) %>%
          filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
          ggplot(aes(x=Treatment, y=whole.mass, fill = Treatment)) +
          geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                       outlier.size = 5, lwd = 1.5)+
          geom_line()+
          theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
          theme_bw() +
          labs(title ="Litterfall Mass vs. Treatment Type Boxplot",
             x = "Treatment",
             y = "Mass (g litter /m2)") +
        facet_wrap(facets = "Stand", ncol = 4)
      
    })
    
    output$litterfalltable = DT::renderDataTable({
      Litterfall
    })
}

shinyApp(ui, server)