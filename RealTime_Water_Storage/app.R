#-------------------------------
#Description: This is a shiny web app for Real time water visaulization of 
# Hubbard Brook Watershed sites 3 and 9 - snow, well, and other specific watershed data 
#
#
# Authors: Sam Lausten, Michelle Uchitel, Alison Walters
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
library(shinythemes)
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
    select(TIMESTAMP, WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth)  

ws3_upper_wells <-  ws3_upper_wells %>% 
                    aggregate(list(TIME = cut(ws3_upper_wells$TIMESTAMP, breaks="hour")), 
                            mean, na.rm = TRUE) %>%  #MU: I think I aggregated this correctly so it shows hourly data but not positive. 
                    select(TIME, WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth)

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
  select(TIMESTAMP, HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth) 

ws9_upper_wells <-  ws9_upper_wells %>% 
  aggregate(list(TIME = cut(ws9_upper_wells$TIMESTAMP, breaks="hour")), 
            mean, na.rm = TRUE) %>%  #MU: I think I aggregated this correctly so it shows hourly data but not positive. 
  select(TIME, HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth)




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
  select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) %>% 
  mutate(H2O_Content_1 = replace(H2O_Content_1, which(H2O_Content_1 < 0), NA)) %>%   #AW - change the two H20 contents to NA for the negatives 
  mutate(H2O_Content_2 = replace(H2O_Content_2, which(H2O_Content_2 < 0), NA)) 

#AW - adds a column with the average ignoring the NA 
ws3_upper_snowdat15mins <- ws3_upper_snowdat15mins %>% 
  mutate(VWC_average = rowMeans(ws3_upper_snowdat15mins[,c('H2O_Content_1','H2O_Content_2')],
                                na.rm = TRUE ))


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
  select(TIMESTAMP, H2O_Content_1_Avg, H2O_Content_2_Avg, Depthscaled_Avg) %>% 
  mutate(H2O_Content_1_Avg = replace(H2O_Content_1_Avg, which(H2O_Content_1_Avg < 0), NA)) %>%   #AW - change the two H20 contents to NA for the negatives 
  mutate(H2O_Content_2_Avg = replace(H2O_Content_2_Avg, which(H2O_Content_2_Avg < 0), NA)) 

#AW - adds a column with the average ignoring the NA 
ws3_upper_snowdat_hr <- ws3_upper_snowdat_hr %>% 
  mutate(VWC_average = rowMeans(ws3_upper_snowdat_hr[,c('H2O_Content_1_Avg','H2O_Content_2_Avg')],
                                na.rm = TRUE ))

#reading in WS9 Snow 15 mins 
# AW - currently the VWC is either 0 or NA for all entries 

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
  select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) %>% 
  mutate(H2O_Content_1 = replace(H2O_Content_1, which(H2O_Content_1 < 0), NA)) %>%   #AW - change the two H20 contents to NA for the negatives 
  mutate(H2O_Content_2 = replace(H2O_Content_2, which(H2O_Content_2 < 0), NA)) 

#AW - adds a column with the average ignoring the NA 
ws9_upper_snowdat15mins <- ws9_upper_snowdat15mins %>% 
  mutate(VWC_average = rowMeans(ws9_upper_snowdat15mins[,c('H2O_Content_1','H2O_Content_2')],
                                na.rm = TRUE ))


#reading in WS9 Snow hourly data 
# AW - currently the VWC is either 0 or NA for all entries 

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
  select(TIMESTAMP, H2O_Content_1_Avg, H2O_Content_2_Avg, Depthscaled_Avg) %>% 
  mutate(H2O_Content_1_Avg = replace(H2O_Content_1_Avg, which(H2O_Content_1_Avg < 0), NA)) %>%   #AW - change the two H20 contents to NA for the negatives 
  mutate(H2O_Content_2_Avg = replace(H2O_Content_2_Avg, which(H2O_Content_2_Avg < 0), NA)) 

#AW - adds a column with the average ignoring the NA 
ws9_upper_snowdat_hr <- ws9_upper_snowdat_hr %>% 
  mutate(VWC_average = rowMeans(ws9_upper_snowdat_hr[,c('H2O_Content_1_Avg','H2O_Content_2_Avg')],
                                na.rm = TRUE ))


# Define UI for application
ui <- fluidPage(navbarPage("Hubbard Brook - Realtime Watershed Data Explorer",
                           theme = shinytheme('cosmo'),
  

        
#define tabs to be used in the app
tabPanel('About',
         fluidRow(
           column(1, tags$h3("Watershed 3", align = "left")), #MU: Watershed 3 Label
           column(10, tags$h3("Watershed 9", align = "right"))), #MU: Watershed 9 Label
         fluidRow(
           column(1, tags$img(src = "WS3map.png", align = "left", width = 340 , height = 230)), #MU: Watershed 3 Map
           column(10, tags$img(src = "WS9map.png", align = "right", width = 340 , height = 230))), #MU: Watershed 9 Map
         fluidRow(
           tags$h4("This app visualizes data from Watershed 3 and 9 of the Hubbard
                                    Brook Experimental Forest through graphs, a map showing where the data was collected,
                   and a table. The data can also be filtered using the various filters found in each tab.")),
        fluidRow(
          tags$p("Map Credit: Hubbard Brook Experimental Forest"),
          tags$p("This application will attempt to:
                    - Visualize Realtime and Past Watershed Data.
                    - Create a user-friendly dashboard that allows for data exploration.
                    - Assist Hubbard Brook Scientists in testing hypothetical data and results.")
        )),
tabPanel('Watershed Visualizations',
         sidebarLayout(
           sidebarPanel(width = 3,
                        dateInput("startdate", label = "Start Date", val= "2020-12-14"), #MU: Should we make the default start value the first data present in the data we read in?
                        dateInput("enddate", label= "End Date", value=Sys.Date(), max=Sys.Date()),
                        selectInput(inputId = "toview", label = "Select dataset to view:", 
                                    choices = unique(ws3_upper_wells$name), 
                                    selected = unique(ws3_upper_wells$name)[1]),
                        numericInput("poros","Porosity:",
                                   0.1, step = 0.1, min = 0, max = 1),
                        verbatimTextOutput("value"),
                        fluid = TRUE),
           mainPanel(
             plotOutput("plot1")
           )
         ) 
      ),

  
tabPanel('Table View' ,DTOutput("table")),

         
tabPanel('Map', leafletOutput("map",width = '100%'))



))



# Define server
server <- function(input, output) {
    #----------------
    # read in cleaned watershed data
    # ---------------

  #MU: standardized well ws3 data to mm H2O
 # standardized_Well_WS3 <-  ws3_upper_wells %>% 
  #  mutate(standardized_well_1 = ((WS3_N1_corr_depth * 10) * input$poros)) %>% 
  #  mutate(standardized_well_2 = ((WS3_N2_corr_depth * 10) * input$poros)) %>% 
  #  mutate(standardized_deep_well = ((WS3_42_4_d2_corr_depth * 10) * input$poros)) %>%
   # select(TIME, standardized_well_1, standardized_well_2, standardized_deep_well)
  
  
  #Mu: standardized well ws9 data to mm H2O
  standardized_Well_WS9 <-  reactive({
    ws9_upper_wells %>% 
    mutate(standardized_well_1 = ((HB156_corr_depth * 10) * input$poros)) %>% 
    mutate(standardized_well_2 = ((HB179s_corr_depth * 10) * input$poros)) %>% 
    mutate(standardized_deep_well = ((HB176d_corr_depth * 10) * input$poros)) %>%
    select(TIME, standardized_well_1, standardized_well_2, standardized_deep_well)
  })
  #MU: standardized snow ws3 data to mm H2O
  standardized_SnowHr_WS3 <-  reactive({
    ws3_upper_snowdat_hr %>% 
    mutate(standardized_snow = (((H2O_Content_1_Avg + H2O_Content_2_Avg) / 2) * (Depthscaled_Avg * 10)))
  })
  
  #MU: standardized snow ws9 data to mm H2O
  standardized_SnowHr_WS9 <- reactive({
    ws9_upper_snowdat_hr %>% 
    mutate(standardized_snow = (((H2O_Content_1_Avg + H2O_Content_2_Avg) / 2) * (Depthscaled_Avg * 10)))
 })
  
  #MU: standardized precip data ws3 to mm H2O
  
  
  output$table <- DT::renderDataTable({DT::datatable(standardized_Well_WS9(), #MU: When we do the calculations we can put them in one dataset and output that.
                  class = "display", #MU: this is the style of the table
                  caption = 'Table 1: This table shows x.', #MU: adds a caption to the table
                  filter = "top")
      })#MU: This places the filter at the top of the table
    #MU: This is a placeholder table for when we finish cleaning the data and can input summarized values
    output$plot1 <- renderPlot({
        ws3_upper_wells %>% 
        filter(name == input$toview & TIMESTAMP > input$startdate & TIMESTAMP < input$enddate) %>%
            ggplot(aes(x = TIMESTAMP, y = value))+
            geom_line()
      })
      
    output$porosPlot <- renderPlot({
      x <- seq(from = 0, to = 100, by = 0.1)
      y <- x*input$poros + input$change
      plot(x,y)
    })


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
