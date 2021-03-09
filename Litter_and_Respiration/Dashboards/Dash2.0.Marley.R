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
<<<<<<< HEAD
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/Litterfall.csv")
=======
    read_csv("C:/Users/marle/Desktop/EI Capstone/EI_Capstone_S21/Litter_and_Respiration/Litterfall.csv") %>%
  mutate(Treatment = paste(Treatment))

>>>>>>> 846588b93726bc745418fbcc46a26eaf7ab043d1
SoilRespiration <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/SoilResp.csv")
StandLocations <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/StandLocations.csv")
lat_long <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/lat_long.csv")

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
<<<<<<< HEAD
                box(plotOutput("timeseries_plot"), width = 8),
                box(
                    selectInput("Treatment", "Treatment Type",
                                c("N", "P", "NP", "C"))
                ),
                h1("Litterfall")),
=======
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
>>>>>>> 846588b93726bc745418fbcc46a26eaf7ab043d1
        
        tabItem(tabName = "Soil_Respiration",
                h1("Soil Respiration"),
                box(width = 12, dateRangeInput("date", "Date Range:",
                                               start = "2008-07-01",
                                               end = "2020-07-25",
                                               min = "2008-07-01",
                                               max = "2020-07-25")
                ),
<<<<<<< HEAD
                box(plotOutput("flux_ts_plot"), width = 4)),
        tabItem(tabName = "Litterfall_Data",
                        h1("Litterfall Data"),
                DT:: dataTableOutput("litterfalltable"))
=======
                box(plotOutput("flux_ts_plot"), width = 5),
        )
>>>>>>> a16b2c10ca4383640b3deae012df10b9e313c9c7
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
<<<<<<< HEAD
        ggplot(data = Litterfall,aes(x=Year, y=whole.mass)) +
            geom_line( color = "black") +
            
            xlab("") +
            theme_ipsum() +
=======
        min <- input$Year[1]
        max <- input$Year[2]
        Treatment <- input$Treatment
        Stand <- input$Stand
#
        
        Litterfall %>%
            filter(Year >= min & Year <= max) %>%
<<<<<<< HEAD
            filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
            mutate(Year = as.factor(Year)) %>%
          ggplot(aes(x = Year, y = whole.mass)) +
            geom_boxplot(aes(x = Year, y = whole.mass, color = Treatment), position = position_dodge(0.8))+
            geom_dotplot(aes(x = Year, y = whole.mass, color = Treatment), position = position_dodge(0.8), 
                       binaxis = "y")+
            theme_bw() +
=======
            filter(Treatment == input$Treatment) %>%
            filter(Stand == input$Stand) %>%
            ggplot(aes(x=Year, y=whole.mass, color = Treatment)) +
            geom_point(size = 3) +
            #geom_line(size = 1.5) +
>>>>>>> 846588b93726bc745418fbcc46a26eaf7ab043d1
>>>>>>> a16b2c10ca4383640b3deae012df10b9e313c9c7
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
            labs(title ="Time Series Litterfall",
                 x = "Year",
                 y = "Mass (g litter /m2)") +
          facet_wrap(facets = "Stand", ncol = 4)
<<<<<<< HEAD
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
=======
          }) 

>>>>>>> a16b2c10ca4383640b3deae012df10b9e313c9c7
}

shinyApp(ui, server)