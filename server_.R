library(plotly)
library(dplyr)

source("drunkFuncs.R")

server <- shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  
  paths <- reactive({
    drunks <- input$nDrunks
    steps <- input$nSteps
    seed <- input$seed
    
    p <- nDrunkPaths(drunks, steps)
    
    p
  })
  
  pathsWithPolice <- reactive({
    walks <- paths()
    
    policeMin <- input$policeMin

    policeConcentration <- input$policeConcentration
    
    police <- makePolice(walks, policeMin, policeConcentration)
    
    list(paths = walks, police = police)
  })
  
  pathsWithPoliceAction <- reactive({
    pwp <- pathsWithPolice()
    
    policeAreBlind <- input$policeAreBlind
    
    split <- catchDrunk(pwp$paths, pwp$police, policeAreBlind)
    
    split
  })
  
  
  output$walkChart <- renderPlotly({
    
    split <- pathsWithPoliceAction()
    
    height <- input$chartHeight
    
    finalOnly <- input$finalPositionOnly
    
    isolate({
      pwp <- pathsWithPolice()
      
      chart <- makeChart(
        split$uncaught, 
        split$residuals,
        height, 
        pwp$police, 
        finalOnly
      )
      
      chart
    })
    
  })
  
})