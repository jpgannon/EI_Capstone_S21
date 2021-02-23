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
#library(waterData)
#library(plotly)
library(DT) #MU: Helpful for displaying data tables.
library(tidyverse) #MU: I added tidyverse because it has ggplot2 and other good functions 

#reading in WS3 well data
setwd("/Volumes/GoogleDrive/My Drive/CLASSES/EI Capstone/EI_Capstone_S21")
ws3_upper_wells <- read_csv("RealTime_Water_Storage/RealTime_Water_Storage/Water_table_WS3upper_WS_3Up_wells.dat",
                            skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                                    X4= "ptemp_Max" , X5= "WS3_N1_psi",X6="WS3_N1_rawdepth",
                                                    X7="WS3_N1_depth_corr",X8="WS3_N1_corr_depth",
                                                    X9="WS3_N1_welltemp",X10="WS3_N2_psi",
                                                    X11="WS3_N2_rawdepth",X12="WS3_N2_depth_corr",
                                                X13="WS3_N2_corr_depth",X14="WS3_N2_welltemp",
                                                X15="WS3_42_4_d2_psi",X16="WS3_42_4_d2_rawdepth",
                                                X17="WS3_42_4_d2_depth_corr",X18="WS3_42_4_d2_corr_depth",
                                                X19="WS3_42_4_d2_welltemp"))%>%
    select(TIMESTAMP, WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth) %>%
    pivot_longer(cols = c(WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Realtime Watershed Data - Hubbard Brook"),

    # Sidebar with daterange 
    sidebarLayout(
        sidebarPanel(
            dateInput("startdate", label = "Start Date", val=""), #MU: Should we make the default start value the first data present in the data we read in?
            dateInput("enddate", label= "End Date", value=Sys.Date(), max=Sys.Date()),
            selectInput(inputId = "toview", label = "Select dataset to view:", 
                        choices = unique(ws3_upper_wells$name), 
                        selected = unique(ws3_upper_wells$name)[1]),
            width = 3
        ),

        #main panel/tabs
        mainPanel(
           tabsetPanel(
               tabPanel('About',h4("This app visualizes real-time data from Hubbard Brook research watershed sites 3 and 9, 
                                   as well as related snowpack and weather conditions, for a date range selected to compare the two sites
                                 or by certain perameters."),
               tabPanel('Watershed Visualizations', plotOutput("plot1")),
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
    output$plot1 <- renderPlot({
        ws3_upper_wells %>% filter(name == input$toview) %>%
            ggplot(aes(x = TIMESTAMP, y = value))+
            geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
