library(plotly)
library(dplyr)

source("drunkFuncs.R")


server <- shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  
  output$walkChart <- renderPlotly({
    
    steps <- input$nSteps
    seed <- input$seed
    height <- input$chartHeight
    
    policeMin <- input$policeMin
    policeConcentration <- input$policeConcentration
    
    
    path <- makeDrunkPath(steps)
    police <- makePolice(path, policeMin, policeConcentration)
    
    chart <- makeChart(path, height, police)
    
    chart
    
  })
  
})