#-------------------------------
#Description: This is a shiny web app for Real time water visaulization of 
# Hubbard Brook Watershed sites 3 and 9 - snow, well, and other specific watershed data 
#
#
# Author: Sam Lausten, Michelle Uchitel, Alison Walters
#
#-------------------------------

#load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(lubridate)
library(DT) #MU: Helpful for displaying data tables.
library(tidyverse) #MU: I added tidyverse because it has ggplot2 and other good functions 
library(grid)

#reading in WS3 well data
#setwd("/Volumes/GoogleDrive/My Drive/CLASSES/EI Capstone/EI_Capstone_S21")
ws3_upper_wells <- read_csv("Realtime_waterstorage_app/Water_Storage_Data/Water_table_WS3upper_WS_3Up_wells.dat",
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


#reading in WS9 well data

ws9_upper_wells <- read_csv("Realtime_waterstorage_app/Water_Storage_Data/Water_table_WS9_WS_9_wells.dat",
                            skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD",
                                                    X3 ="Batt_Volt", X4= "ptemp_Max" , 
                                                    X5= "HB156_psi", X6= "HB156_rawdepth", 
                                                    X7= "HB156_depth_corr", X8= "HB156_corr_depth",
                                                    X9=  "HB156_welltemp" , X10= "HB179s_psi",
                                                    X11= "HB179s_rawdepth", X12=  "HB179s_depth_corr" , 
                                                    X13= "HB179s_corr_depth" , X14= "HB179s_welltemp" ,
                                                    X15= "HB176d_psi" , X16= "HB176d_rawdepth", 
                                                    X17= "HB176d_depth_corr" , X18= "HB176d_corr_depth", 
                                                    X19 = "HB176d_welltemp"))%>%
  select(TIMESTAMP, HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth) %>%
  pivot_longer(cols = c(HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth))




#reading in WS3 Snow 15 mins 

ws3_upper_snowdat15mins <- read_csv("Realtime_waterstorage_app/Water_Storage_Data/Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                                    skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", 
                                                            X3 ="Batt_Volt", X4= "ptemp" , 
                                                            X5= "H2O_Content_1", X6= "H2O_Content_2", 
                                                            X7= "Avg_Period_1", X8= "Avg_Period_2", 
                                                            X9=  "RTD(1)" , X10= "RTD(2)", X11= "RTD(3)", 
                                                            X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , 
                                                            X15= "RTD(7)" , X16= "RTD(8)", X17= "RTD(9)" , 
                                                            X18= "Air_TempC_Avg", X19 = "Depthraw", 
                                                            X20= "Depthscaled"))%>%
  select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) 


#reading in WS3 Snow hourly data 

ws3_upper_snowdat_hr <- read_csv("Realtime_waterstorage_app/Water_Storage_Data/Water_table_WS3upper_WS_3Up_snowdat_hr.dat",
                                 skip = 4, col_names = c(X1 = "TIMESTAMP" , 
                                                         X2 = "RECORD", X3 ="H2O_Content_1_Avg", 
                                                         X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", 
                                                         X6= "Avg_Period_2_Avg", X7="RTD_Avg(1)", 
                                                         X8= "RTD_Avg(2)", X9=  "RTD_Avg(3)" , 
                                                         X10= "RTD_Avg(4)", X11= "RTD_Avg(5)", 
                                                         X12=  "RTD_Avg(6)" , X13= "RTD_Avg(7)" , 
                                                         X14= "RTD_Avg(8)" , X15= "RTD_Avg(9)" , 
                                                         X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , 
                                                         X18= "Depthscaled_Avg"))%>%
  select(TIMESTAMP, H2O_Content_1_Avg, H2O_Content_2_Avg, Depthscaled_Avg) 

#reading in WS9 Snow 15 mins 

ws9_upper_snowdat15mins <- read_csv("Realtime_waterstorage_app/Water_Storage_Data/Water_table_WS9_WS_9_snowdat_15min.dat",
                                    skip = 4, col_names = c(X1 = "TIMESTAMP" ,
                                                            X2 = "RECORD", X3 ="Batt_Volt", 
                                                            X4= "ptemp" , X5= "H2O_Content_1", 
                                                            X6= "H2O_Content_2", X7= "Avg_Period_1", 
                                                            X8= "Avg_Period_2a", X9=  "RTD(1)" , 
                                                            X10= "RTD(2)", X11= "RTD(3)", X12=  "RTD(4)" , 
                                                            X13= "RTD(5)" , X14= "RTD(6)" , X15= "RTD(7)" , 
                                                            X16= "RTD(8)", X17= "RTD(9)" , 
                                                            X18= "Air_TempC_Avg", X19 = "Depthraw", 
                                                            X20= "Depthscaled"))%>%
  select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) 


#reading in WS9 Snow hourly data 

ws9_upper_snowdat_hr <- read_csv("Realtime_waterstorage_app/Water_Storage_Data/Water_table_WS9_WS_9_snowdat_hr.dat",
                                 skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", 
                                                         X3 ="H2O_Content_1_Avg", X4= "H2O_Content_2_Avg", 
                                                         X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg", 
                                                         X7="RTD_Avg(1)", X8= "RTD_Avg(2)", 
                                                         X9=  "RTD_Avg(3)" , X10= "RTD_Avg(4)", 
                                                         X11= "RTD_Avg(5)", X12=  "RTD_Avg(6)" , 
                                                         X13= "RTD_Avg(7)" , X14= "RTD_Avg(8)" , 
                                                         X15= "RTD_Avg(9)" , X16= "Air_TempC_Avg", 
                                                         X17= "Depthraw_Avg" , X18= "Depthscaled_Avg"))%>%
  select(TIMESTAMP, H2O_Content_1_Avg, H2O_Content_2_Avg, Depthscaled_Avg) 

# Define UI for application
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
               tabPanel('About'),
               tabPanel('Watershed Visualizations', plotOutput("plot1")),
               tabPanel('Table' ,DTOutput("table")),
               tabPanel('Map of Stations', leafletOutput("map",width = '100%'))
           )
        ) #mainPanel
    )#sidebarLayout
)#fluidPage

# Define server
server <- function(input, output) {
    #----------------
    # read in cleaned watershed data
    # ---------------
    output$table <- renderDT(ws3_upper_snowdat_hr, #MU: When we do the calculations we can put them in one dataset and output that.
                             class = "display", #MU: this is the style of the table
                             caption = 'Table 1: This table shows x.', #MU: adds a caption to the table
                             filter = "top") #MU: This places the filter at the top of the table
    #MU: This is a placeholder table for when we finish cleaning the data and can input summarized values
    output$plot1 <- renderPlot({
        ws3_upper_wells %>% filter(name == input$toview) %>%
            ggplot(aes(x = TIMESTAMP, y = value))+
            geom_line()
    })



#---------------------------------------------
# Plot map of station locations using leaflet
#---------------------------------------------

m <-leaflet() %>% 
    addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>% 
    addMarkers(lng= -71.7185, lat = 43.9403, popup = "Hubbard Brook Experimental Forest")

output$map <- renderLeaflet(
    m
  )

#---------------------------------------------
} # END Server function
#---------------------------------------------
#---------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)
