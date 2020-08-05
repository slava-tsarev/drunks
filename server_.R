library(plotly)
library(dplyr)

source("drunkFuncs.R")


server <- shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  
  output$walkChart <- renderPlotly({
    
    drunks <- input$nDrunks
    steps <- input$nSteps
    seed <- input$seed
    height <- input$chartHeight
    policeAreBlind <- input$policeAreBlind
    policeMin <- input$policeMin
    policeConcentration <- input$policeConcentration
    
    paths <- nDrunkPaths(drunks, steps)
    
    police <- makePolice(paths, policeMin, policeConcentration)
    
    split <- catchDrunk(paths, police, policeAreBlind)

    chart <- makeChart(
        split$uncaught, 
        split$residuals,
        height, 
        police
    )
    
    chart
    
  })
  
})