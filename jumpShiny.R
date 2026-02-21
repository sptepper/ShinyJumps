install.packages(c('ggplot2', 'shiny'))

## This script runs after jumpsR

library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Long Jump Attempts"),
  plotlyOutput("jumpPlot")
)

server <- function(input, output) {
  output$jumpPlot <- renderPlotly({
    interactive_plot
  })
}

shinyApp(ui, server)