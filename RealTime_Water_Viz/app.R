library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)

ui <- fluidPage(
  plotOutput("example")
)

server <- function(input, output, sessions) {
  
  well9 <- read_csv("Water_table_WS9_WS_9_wells.dat",
                    skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp_Max" , X5= "HB156_psi", X6= "HB156_rawdepth", X7= "HB156_depth_corr", X8= "HB156_corr_depth", X9=  "HB156_welltemp" , X10= "HB179s_psi", X11= "HB179s_rawdepth", X12=  "HB179s_depth_corr" , X13= "HB179s_corr_depth" , X14= "HB179s_welltemp" , X15= "HB176d_psi" , X16= "HB176d_rawdepth", X17= "HB176d_depth_corr" , X18= "HB176d_corr_depth", X19 = "HB176d_welltemp"))
  
  well9_snowdat_15m <- read_csv("Water_table_WS9_WS_9_snowdat_15min.dat",
                                skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp" , X5= "H2O_Content_1", X6= "H2O_Content_2", X7= "Avg_Period_1", X8= "Avg_Period_2", X9=  "RTD(1)" , X10= "RTD(2)", X11= "RTD(3)", X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , X15= "RTD(7)" , X16= "RTD(8)", X17= "RTD(9)" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled"))
  
  well9_snowdat_hr <- read_csv("Water_table_WS9_WS_9_snowdat_hr.dat",
                               skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg", X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg", X7="RTD_Avg(1)", X8= "RTD_Avg(2)", X9=  "RTD_Avg(3)" , X10= "RTD_Avg(4)", X11= "RTD_Avg(5)", X12=  "RTD_Avg(6)" , X13= "RTD_Avg(7)" , X14= "RTD_Avg(8)" , X15= "RTD_Avg(9)" , X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg"))
  
  
  well3 <- read_csv("Water_table_WS3upper_WS_3Up_wells.dat",
                    skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp_Max" , X5= "WS3_N1_psi",X6="WS3_N1_rawdepth",X7="WS3_N1_depth_corr",X8="WS3_N1_corr_depth",X9="WS3_N1_welltemp",X10="WS3_N2_psi",X11="WS3_N2_rawdepth",X12="WS3_N2_depth_corr",X13="WS3_N2_corr_depth",X14="WS3_N2_welltemp",X15="WS3_42_4_d2_psi",X16="WS3_42_4_d2_rawdepth",X17="WS3_42_4_d2_depth_corr",X18="WS3_42_4_d2_corr_depth",X19="WS3_42_4_d2_welltemp"))
  
  well3_snowdat_15m <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                                skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", X4= "ptemp" , X5= "H2O_Content_1", X6= "H2O_Content_2", X7= "Avg_Period_1", X8= "Avg_Period_2", X9=  "RTD(1)" , X10= "RTD(2)", X11= "RTD(3)", X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , X15= "RTD(7)" , X16= "RTD(8)", X17= "RTD(9)" , X18= "Air_TempC_Avg", X19 = "Depthraw", X20= "Depthscaled"))
  
  well3_snowdat_hr <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_hr.dat",
                               skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="H2O_Content_1_Avg", X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg", X7="RTD_Avg(1)", X8= "RTD_Avg(2)", X9=  "RTD_Avg(3)" , X10= "RTD_Avg(4)", X11= "RTD_Avg(5)", X12=  "RTD_Avg(6)" , X13= "RTD_Avg(7)" , X14= "RTD_Avg(8)" , X15= "RTD_Avg(9)" , X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , X18= "Depthscaled_Avg"))
  
  output$example <- renderPlot({
    ggplot(data = well9, aes(x = WS_9_wells, y = CR1000))+
      geom_point()
  })
}
shinyApp(ui, server)
