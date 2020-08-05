library(plotly)
library(dplyr)

source("drunkFuncs.R")


server <- shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  
  output$walkChart <- renderPlotly({
    
    steps <- input$nSteps
    seed <- input$seed
    height <- input$chartHeight
    
    path <- makeDrunkPath(steps)
    
    chart <- makeChart(path, height)
    
    chart
    
  })
  
})