install.packages("shineythemes")
install.packages("googledrive")
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(googledrive)

setwd("~/EI Capstone")
#Reading in Datasets

install.packages('rsconnect')

well9 <- read_csv("Water_table_WS9_WS_9_wells.dat",
                  skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                          X4= "ptemp_Max" , X5= "HB156_psi", X6= "HB156_rawdepth", 
                                          X7= "HB156_depth_corr", X8= "HB156_corr_depth", X9=  "HB156_welltemp" , 
                                          X10= "HB179s_psi", X11= "HB179s_rawdepth", X12=  "HB179s_depth_corr" , 
                                          X13= "HB179s_corr_depth" , X14= "HB179s_welltemp" , 
                                          X15= "HB176d_psi" , X16= "HB176d_rawdepth", 
                                          X17= "HB176d_depth_corr" , X18= "HB176d_corr_depth", X19 = "HB176d_welltemp")) %>% 
  select(TIMESTAMP, HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth) %>% 
  pivot_longer(cols = c(HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth))

well3 <- read_csv("Water_table_WS3upper_WS_3Up_wells.dat",
                  skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp_Max" ,
                                          X5= "WS3_N1_psi",X6="WS3_N1_rawdepth",X7="WS3_N1_depth_corr",X8="WS3_N1_corr_depth",
                                          X9="WS3_N1_welltemp",X10="WS3_N2_psi",X11="WS3_N2_rawdepth",X12="WS3_N2_depth_corr",
                                          X13="WS3_N2_corr_depth",X14="WS3_N2_welltemp",X15="WS3_42_4_d2_psi",X16="WS3_42_4_d2_rawdepth",
                                          X17="WS3_42_4_d2_depth_corr",X18="WS3_42_4_d2_corr_depth",X19="WS3_42_4_d2_welltemp")) %>% 
  select(TIMESTAMP, WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth) %>% 
  pivot_longer(cols = c(WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth))

well9_snowdat_15m <- read_csv("Water_table_WS9_WS_9_snowdat_15min.dat",
                              skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                                      X4= "ptemp" , X5= "H2O_Content_1", X6= "H2O_Content_2", 
                                                      X7= "Avg_Period_1", X8= "Avg_Period_2", X9=  "RTD(1)" , X10= "RTD(2)",
                                                      X11= "RTD(3)", X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , X15= "RTD(7)" , 
                                                      X16= "RTD(8)", X17= "RTD(9)" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) %>% 
  pivot_longer(cols = c(H2O_Content_1, H2O_Content_2, Depthscaled))

well9_snowdat_hr <- read_csv("Water_table_WS9_WS_9_snowdat_hr.dat",
                             skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg", 
                                                     X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg", X7="RTD_Avg(1)", 
                                                     X8= "RTD_Avg(2)", X9=  "RTD_Avg(3)" , X10= "RTD_Avg(4)", X11= "RTD_Avg(5)", 
                                                     X12=  "RTD_Avg(6)" , X13= "RTD_Avg(7)" , X14= "RTD_Avg(8)" , X15= "RTD_Avg(9)" ,
                                                     X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, H2O_Content_2_Avg, Depthscaled_Avg) %>% 
  pivot_longer(cols = c(H2O_Content_2_Avg, Depthscaled_Avg))




well3_snowdat_15m <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                              skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp" ,
                                                      X5= "H2O_Content_1", X6= "H2O_Content_2", X7= "Avg_Period_1",
                                                      X8= "Avg_Period_2", X9=  "RTD(1)" , X10= "RTD(2)", X11= "RTD(3)",
                                                      X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , X15= "RTD(7)" , X16= "RTD(8)",
                                                      X17= "RTD(9)" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) %>% 
  pivot_longer(cols = c(H2O_Content_1, H2O_Content_2, Depthscaled))



well3_snowdat_hr <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_hr.dat",
                             skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg",
                                                     X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg",
                                                     X7="RTD_Avg(1)", X8= "RTD_Avg(2)", X9=  "RTD_Avg(3)" , X10= "RTD_Avg(4)",
                                                     X11= "RTD_Avg(5)", X12=  "RTD_Avg(6)" , X13= "RTD_Avg(7)" , X14= "RTD_Avg(8)" ,
                                                     X15= "RTD_Avg(9)" , X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, H2O_Content_2_Avg, Depthscaled_Avg) %>% 
  pivot_longer(cols = c(H2O_Content_2_Avg, Depthscaled_Avg))

#binding common well/snow data
well_data <- bind_rows(well3, well9, .id = "well")
snowdat_15m <- bind_rows(well3_snowdat_15m, well9_snowdat_15m, .id = "well")
snowdat_hr <- bind_rows(well3_snowdat_hr, well9_snowdat_hr, .id = "well")

#seperating the depth of sensor (RTD) from the rest of the data so we can use this for the heat map
well9_15_RTD <- read_csv("Water_table_WS9_WS_9_snowdat_15min.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                                 X4= "ptemp" , X5= "H2O_Content_1", X6= "H2O_Content_2", 
                                                 X7= "Avg_Period_1", X8= "Avg_Period_2", X9=  "RTD1" , X10= "RTD2",
                                                 X11= "RTD3", X12=  "RTD4" , X13= "RTD5" , X14= "RTD6" , X15= "RTD7" , 
                                                 X16= "RTD8", X17= "RTD9" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, RTD1 , RTD2 , RTD3 , RTD4 , RTD5 ,
         RTD6 , RTD7 , RTD8, RTD9) %>% 
  pivot_longer(cols = c(RTD1 , RTD2 , RTD3 , RTD4 , RTD5 ,
                        RTD6 , RTD7 , RTD8 , RTD9))

well9_hr_RTD <- read_csv("Water_table_WS9_WS_9_snowdat_hr.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg", 
                                                 X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg", X7="RTD_Avg1", 
                                                 X8= "RTD_Avg2", X9=  "RTD_Avg3" , X10= "RTD_Avg4", X11= "RTD_Avg5", 
                                                 X12=  "RTD_Avg6" , X13= "RTD_Avg7" , X14= "RTD_Avg8" , X15= "RTD_Avg9" ,
                                                 X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, RTD_Avg1 , RTD_Avg2 , RTD_Avg3 , RTD_Avg4 , RTD_Avg5 ,
         RTD_Avg6 , RTD_Avg7 , RTD_Avg8 , RTD_Avg9) %>% 
  pivot_longer(cols = c(RTD_Avg1 , RTD_Avg2 , RTD_Avg3 , RTD_Avg4 , RTD_Avg5 ,
                        RTD_Avg6 , RTD_Avg7 , RTD_Avg8 , RTD_Avg9))

well3_15_RTD <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp" ,
                                                 X5= "H2O_Content_1", X6= "H2O_Content_2", X7= "Avg_Period_1",
                                                 X8= "Avg_Period_2", X9=  "RTD1" , X10= "RTD2", X11= "RTD3",
                                                 X12=  "RTD4" , X13= "RTD5" , X14= "RTD6" , X15= "RTD7" , X16= "RTD8",
                                                 X17= "RTD9" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled")) %>% 
  select(TIMESTAMP, RTD1 , RTD2 , RTD3 , RTD4 , RTD5 ,
         RTD6 , RTD7 , RTD8, RTD9) %>% 
  pivot_longer(cols = c(RTD1 , RTD2 , RTD3 , RTD4 , RTD5 ,
                        RTD6 , RTD7 , RTD8 , RTD9))

well3_hr_RTD <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_hr.dat",
                         skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg",
                                                 X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg",
                                                 X7="RTD_Avg1", X8= "RTD_Avg2", X9=  "RTD_Avg3" , X10= "RTD_Avg4",
                                                 X11= "RTD_Avg5", X12=  "RTD_Avg6" , X13= "RTD_Avg7" , X14= "RTD_Avg8" ,
                                                 X15= "RTD_Avg9" , X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, RTD_Avg1 , RTD_Avg2 , RTD_Avg3 , RTD_Avg4 , RTD_Avg5 ,
         RTD_Avg6 , RTD_Avg7 , RTD_Avg8 , RTD_Avg9) %>% 
  pivot_longer(cols = c(RTD_Avg1 , RTD_Avg2 , RTD_Avg3 , RTD_Avg4 , RTD_Avg5 ,
                        RTD_Avg6 , RTD_Avg7 , RTD_Avg8 , RTD_Avg9))

discharge_3 <- read_csv("weir3_Ws_3b.dat",
                        skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt",
                                                X4= "Ptemp", X5= "WLOptical_median", X6= "Optical_WL_max",
                                                X7="Optical_WL_min", X8= "flow_equation", x9 = "Q",
                                                x10 = "Specific_Discharge", x11 = "Streamtemp")) %>% 
  select(TIMESTAMP, Specific_Discharge) %>% 
  pivot_longer(cols = c(Specific_Discharge))


discharge_9 <- read_csv("weir9_Ws_9b.dat",
                        skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt",
                                                X4= "Ptemp", X5= "WLOptical_median", X6= "Optical_WL_max",
                                                X7="Optical_WL_min", X8= "flow_equation", x9 = "Q",
                                                x10 = "Specific_Discharge", x11 = "Streamtemp")) %>% 
  select(TIMESTAMP, Specific_Discharge) %>% 
  pivot_longer(cols = c(Specific_Discharge))

discharge_data <- bind_rows(discharge_3, discharge_9, .id = "watershed")

precip <- read_csv("wxsta1_Wx_1_rain.dat",
                   skip = 4, col_names = c(x1 = "TIMESTAMP", x2 = "RECORD", x3 = "GageMinV",
                                           x4 = "ActTemp", x5 = "ActDepth", x6 = "ReportPCP", 
                                           x7 = "ODPCounts", x8 = "blockedSec", x9 = "Scan10",
                                           x10 = "ActDepthRA")) %>% 
  select(TIMESTAMP, ReportPCP) %>% 
  pivot_longer(cols = c(ReportPCP))


#filtering dates from the DoS data we just selected 
#so there is more continuity with the data, 
#if not the heat map would be mostly empty

RTD_15 <- bind_rows(well3_15_RTD, well9_15_RTD, .id = "well")
RTD_hr <- bind_rows(well3_hr_RTD, well9_hr_RTD, .id = "well")

ui <- fluidPage(theme = shinytheme("slate"),
                tabsetPanel(
                  tabPanel("Home Page",
                           sidebarLayout(
                             sidebarPanel(width = 1
                             ),
                             mainPanel()),
                           #Creates App home page tab
                           titlePanel("Hubbard Brook Watershed Vizualization")),
                  tabPanel("User Guide", 
                           sidebarLayout(
                             SidebarPanel(width = 1)
                           ),  mainPanel("Welcome to our app! You will find that there are several
                                 options located on the top of your screen.
                                They include times series analysis, a bivariate analysis, snow temperature heat map for wells 3 and 9.
                                In the time series tab you will be able to visualize well depth, watershed data, snow depth for every 15 mins and hourly data and discharge data
                                The bivariate analysis tab you will be able to visualize wells 3 and 9 simutanieously. The snow temperature heat tab 
                                you can visualize different temperature levels of the snow over the time with color coordination. Each plot includes a legend and axis descriptions.
                                . Click on the respective tabs you're 
                                interested in and there will be multiple drop down options on your left.
                                Select the type of plot you'd like to see and the wells. Option 1 is well 3 
                                and option 2 is well 9. For more information you can highlight and click a section of 
                                plot in order to get a closer look at specific timestamps of the timeseries
                                data. Double click to clear out the zoomed in look. I hope you have found 
                                this guide useful in navigating our app!")
                           )
                           ),
                  tabPanel("Timeseries analysis",
                           sidebarLayout(
                             sidebarPanel(
                               dateInput("startdate", label = "Start Date", val= "2020-12-14"), 
                               dateInput("enddate", label= "End Date", value= "2021-03-26"),
                               selectInput("var1", "What well would you like to plot over time?", 
                                           choices = unique(well_data$name), selected = unique(well_data$name)[1], multiple = TRUE),
                               checkboxInput("checkbox1", "Show plot?", TRUE),
                               selectInput("var2", "What snow data (15m) would you like to plot over time?", 
                                           choices = unique(snowdat_15m$name), selected = unique(snowdat_15m$name)[1], multiple = TRUE),
                               checkboxInput("checkbox2", "Show plot?", TRUE),
                               selectInput("var3", "What snow data (hr) would you like to plot over time?", 
                                           choices = unique(snowdat_hr$name), selected = unique(snowdat_hr$name)[1], multiple = TRUE),
                               checkboxInput("checkbox3", "Show plot?", TRUE),
                               selectInput("var_dis", "What discharge data would you like to plot over time?",
                                           choices = unique(discharge_data$name), selected=unique(discharge_data$name)[1], multiple = FALSE),
                               checkboxInput("checkboxTogDis", "Toggle Watersheds (Off: 3, On: 9)", FALSE),
                               checkboxInput("checkbox4", "Show plot?", TRUE),
                               actionButton("dataDL", "Download most recent data"),
                             ),
                             mainPanel(conditionalPanel(condition = "input.checkbox1 == 1",plotOutput("var1", width="100%", height = "215px",
                                                                                                      dblclick = "plot1_dblclick",
                                                                                                      brush = brushOpts(
                                                                                                        id = "plot1_brush",
                                                                                                        resetOnNew = TRUE
                                                                                                      ))), 
                                       conditionalPanel(condition = "input.checkbox2 == 1",plotOutput("var2", width="100%", height = "300px",
                                                                                                      dblclick = "plot2_dblclick",
                                                                                                      brush = brushOpts(
                                                                                                        id = "plot2_brush",
                                                                                                        resetOnNew = TRUE
                                                                                                      ))),
                                       conditionalPanel(condition = "input.checkbox3 == 1",plotOutput("var3", width="100%", height = "215px",
                                                                                                      dblclick = "plot3_dblclick",
                                                                                                      brush = brushOpts(
                                                                                                        id = "plot3_brush",
                                                                                                        resetOnNew = TRUE))),
                                       conditionalPanel(condition = "input.checkbox4 == 1",plotOutput("var_dis", width="100%", height = "215px", 
                                       )))
                           )
                  ),
                  ###Creates tab and tab settings for Watershed 3
                  tabPanel("Bivariate",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("var_x", "What variable would you like to plot on the x axis?", 
                                           choices = unique(well_data$name), selected = unique(well_data$name)[1], multiple = FALSE),
                               selectInput("var_y", "What variable would you like to plot on the y axis?", 
                                           choices = unique(well_data$name), selected = unique(well_data$name)[2], multiple = FALSE),
                               selectInput("var_dis2", "What discharge data would you like to plot over time?",
                                           choices = unique(discharge_data$name), selected=unique(discharge_data$name)[1], multiple = FALSE),
                             ),
                             mainPanel(plotOutput("var_x"), 
                                       plotOutput("var_dis2"))
                           )
                           
                  ),
                  
                  tabPanel("Depth of Snow Heat Map", 
                           sidebarLayout(
                             sidebarPanel(
                               dateInput("startdate2", label = "Start Date", val= "2020-12-14"), 
                               dateInput("enddate2", label= "End Date", value=Sys.Date(), max=Sys.Date()),
                               selectInput("var4", "Which well depth of sensor heat map would you like to see (15min interval)? (Well 3(1) / Well 9(2))",
                                           choices = unique(RTD_15$well), selected = unique(RTD_15$well[1]), multiple = FALSE),
                               selectInput("var5", "Which well depth of sensor heat map would you like to see (hr interval)? (Well 3(1) / Well 9(2))",
                                           choices = unique(RTD_hr$well), selected = unique(RTD_hr$well[1]), multiple = FALSE)
                             ),
                             mainPanel(plotOutput("var4"),
                                       plotOutput("var5"))
                           ))
                )
)

server <- function(input, output, sessions) {
  
  filterdate <- reactiveValues(x = ymd(c("2020-12-14", "2021-03-26")))
  
  output$var1 <- renderPlot({
    well_data %>%  filter(name %in% input$var1 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line() +
      labs(title = "Timeseries Analysis of Well Data",
           x = "Time", 
           y = "Depth (cm)",
           fill = "Wells") +
      theme_bw()
    
  })
  
  output$var2 <- renderPlot({
    snowdat_15m %>%  filter(name %in% input$var2 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line() +
      labs(title = "Timeseries Analysis of Snow Data (15m)",
           x = "Time", 
           y = "Depth (cm)") +
      theme_bw()
    
  })
  
  output$var3 <- renderPlot({
    snowdat_hr %>%  filter(name %in% input$var3 & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = name)) +
      geom_line() +
      labs(title = "Timeseries Analysis of Snow Data (hr)",
           x = "Time", 
           y = "Depth (cm)") +
      theme_bw()
    
  })
  
  output$var_dis <- renderPlot({
    if(input$checkboxTogDis == FALSE)
      discharge_3 %>%  filter(name == input$var_dis & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = TIMESTAMP)) +
      geom_line() +
      scale_color_gradientn(colours = rainbow(5)) +
      labs(title = "Timeseries Analysis of Discharge data",
           x = "Time", 
           y = "mm/Hr") +
      theme_bw()
    else
      discharge_9 %>%  filter(name == input$var_dis & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = TIMESTAMP)) +
      geom_line() +
      scale_color_gradientn(colours = rainbow(5)) +
      labs(title = "Timeseries Analysis of Discharge data",
           x = "Time", 
           y = "mm/Hr)") +
      theme_bw()
  })
  
  output$var_dis2 <- renderPlot({
    if(input$checkboxTogDis == FALSE)
      discharge_3 %>%  filter(name == input$var_dis & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = TIMESTAMP)) +
      geom_line() +
      scale_color_gradientn(colours = rainbow(5)) +
      labs(title = "Timeseries Analysis of Discharge data",
           x = "Time", 
           y = "mm/Hr") +
      theme_bw()
    else
      discharge_9 %>%  filter(name == input$var_dis & TIMESTAMP > filterdate$x[1] & TIMESTAMP < filterdate$x[2]) %>% 
      ggplot(aes(x = TIMESTAMP, y = value, color = TIMESTAMP)) +
      geom_line() +
      scale_color_gradientn(colours = rainbow(5)) +
      labs(title = "Timeseries Analysis of Discharge data",
           x = "Time", 
           y = "mm/Hr)") +
      theme_bw()
  })
  
  
  output$var_x <- renderPlot({
    
    varY <- well_data %>% 
      filter(name == input$var_y)
    
    varX <- well_data %>% 
      filter(name == input$var_x)
    
    toPlot <- inner_join(varY, varX, by = "TIMESTAMP")
    
    toPlot %>% 
      ggplot(aes(x = value.x, y = value.y, color = TIMESTAMP)) +
      geom_point() +
      scale_color_gradientn(colours = rainbow(5)) +
      labs(title = "Bivariate Analysis of Well Data",
           x = unique(toPlot$name.x),
           y = unique(toPlot$name.y)) +
      theme_bw()
  })
  
  output$var4 <- renderPlot({
    RTD_15 %>% filter(well == input$var4 & TIMESTAMP > input$startdate2 & TIMESTAMP < input$enddate2) %>% 
      ggplot(aes(x = TIMESTAMP, y = name, fill = value)) +
      geom_tile() +  
      scale_fill_gradient(low = "#1fddff",
                          high = "#ff4b1f",
                          space = "Lab",
                          na.value = "grey50",
                          guide = "colourbar",
                          aesthetics = "fill") +
      labs(x = "Date",
           y = "Depth of Sensor (cm)", 
           title = "RTD Sensor Heat map") +
      scale_y_discrete(labels = c(("RTD1" = "0"), ("RTD2" = "5"), ("RTD3" = "10"), 
                                  ("RTD4" = "15"), ("RTD5" = "20"), ("RTD6" = "25"), 
                                  ("RTD7" = "30"), ("RTD8" = "35"), ("RTD9" = "40")))+
      theme_bw()
  })
  
  output$var5 <- renderPlot({
    RTD_hr %>% filter(well == input$var5 & TIMESTAMP > input$startdate2 & TIMESTAMP < input$enddate2) %>% 
      ggplot(aes(x = TIMESTAMP, y = name, fill = value)) +
      geom_tile() +  
      scale_fill_gradient(low = "#1fddff",
                          high = "#ff4b1f",
                          space = "Lab",
                          na.value = "grey50",
                          guide = "colourbar",
                          aesthetics = "fill") +
      scale_y_discrete(labels = c(("RTD1" = "0"), ("RTD2" = "5"), ("RTD3" = "10"), 
                                  ("RTD4" = "15"), ("RTD5" = "20"), ("RTD6" = "25"), 
                                  ("RTD7" = "30"), ("RTD8" = "35"), ("RTD9" = "40")))+
      labs(x = "Date",
           y = "Depth of Sensor(cm)", 
           title = "RTD Sensor Heat map")
  })
  
  observeEvent(input$dataDL, {
    drive_deauth()
    
    drive_download(as_id("1yhfJ7zJdumI0cMxQ7i7Zzmebo5pdEV_E"), overwrite = TRUE)
    drive_download(as_id("1wsvu3CXHj81n81eS4wbLE1dj3kpSrOqH"), overwrite = TRUE)
    drive_download(as_id("175Ai1DzFq13ut2J1JD43HExgh7n15HVS"), overwrite = TRUE)
    drive_download(as_id("1eq62MIa6n0tpeLTG-4R64q8iuhOHDDAi"), overwrite = TRUE)
    drive_download(as_id("1g3iBLteoLn_ipqhbY25UahJd--bWlwHf"), overwrite = TRUE)
    drive_download(as_id("1Ckb2L41xRAopHM3y4nnv8JmFEY6JN599"), overwrite = TRUE)
    drive_download(as_id("12CQ5lF-dU9B950eaEOYWp27slikcyNS0"), overwrite = TRUE)
    drive_download(as_id("12CVHTfrD9Qef9GB_CTn0qX-EExo-HgNw"), overwrite = TRUE)
    
  })
  
  observeEvent(input$startdate, {filterdate$x[1] <- ymd(input$startdate)}) 
  observeEvent(input$enddate, {filterdate$x[2] <- ymd(input$enddate)}) 
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
  observeEvent(input$plot2_dblclick, {
    brush <- input$plot2_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
  observeEvent(input$plot3_dblclick, {
    brush <- input$plot3_brush
    
    
    if (!is.null(brush)) {
      filterdate$x <- c(brush$xmin, brush$xmax)
    } 
    
    
    else {
      filterdate$x <- c(ymd(input$startdate), ymd(input$enddate))
    }
  }
  )
  
}

shinyApp(ui, server)