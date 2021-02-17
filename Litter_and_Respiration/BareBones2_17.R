library(tidyverse)
library(shiny)

#LL <- read_csv("litter data")
#SR <- read_csv("Soil Resp")

#Define Treatment Groups and Species Types
treatment <- c("Phosphorus", "Nitrogen", "Nitrogen and Phosphorus","Control")
species <- c("ASH", "ASH",	"BASP",	"BASS",	"BE",	"HB",	"OAK")

#Define UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel("User Guide",
             #Application Title
             titlePanel("Explore the Co-Limitation of Nitrogen and Phosphorus")),
    tabPanel("Litterfall",
             titlePanel("Litterfall Visualization"),
             selectInput("stand", label = "Choose a Stand:", choices = c("C1", "C2", "C3", "C4", "C5")),
             radioButtons("treatment", "Choose a Treatment Type:", treatment),
             sliderInput("date", "Choose a Timeframe:", value = c(2005,2006), min = 2005, max = 2021),
             radioButtons("species", "Choose a Species:", species),
             plotOutput("timeseries", width = "700px", height = "300px"),
             plotOutput("boxplot")
    ),
    tabPanel("Soil Respiration"),
    tabPanel("Heatmap")
  )
)

#Define Server
server <- function(input, output, sessions) {
  stand <- reactive({get(input$stand, "package:ggplot2")})
  treatment <- reactive({get(input$treatment, "package:ggplot2")})
  output$product <- renderText({date})
  species<- reactive({get(input$species, "package:ggplot2")})
  output$timeseries <- renderPlot(plot(1:5), res = 96)
  output$boxplot <- renderPlot(boxplot(1:5))
}
#Run the Application
shinyApp(ui = ui, server = server)