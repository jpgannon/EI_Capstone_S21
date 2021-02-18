library(shiny)
library(ggplot2)


ui <- fluidPage(
  plotOutput("plot"), #MU: This is the plot input for the first plot
  plotOutput("plot2") #MU: Plot input for the second plot
)

server <- function(input, output, session) {
  output$plot <- renderPlot({ #MU: This is the output where the data is specified and plotted. 
    ggplot(data = ws9_upper_snowdat_hr,
           mapping = aes (x = Air_TempC_Avg,
                          y = Depthscaled_Avg)) +
      geom_point()+
      labs(title = "Average Temperature by Scaled Depth",
           x = "Avg Air Temperature (C)",
           y = "Scaled Depth (cm")})
  output$plot2 <- renderPlot({ #MU: This is the output where the data is specified and plotted.
    ggplot(data = ws9_upper_wells,
           mapping = aes (x = HB176d_welltemp,
                          y = HB176d_psi)) +
      geom_point()+
      labs(title = "Well Temp (C) by Pressure (PSi)",
           x = "Well Temp (C)",
           y = "Pressure (Psi)")})
}

shinyApp(ui, server)