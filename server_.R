library(plotly)
library(dplyr)

source("drunkFuncs.R")

server <- shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  
  paths <- reactive({
    drunks <- input$nDrunks
    steps <- input$nSteps
    seed <- input$seed
    
    p <- nDrunkPaths(steps, drunks)
    
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
  
  statsPaths <- reactive({
    steps <- input$statsNSteps
    drunks <- input$statsNDrunks
    isolate({
      nDrunkPaths(steps, drunks)
    })
  })
  
  statsPolice <- reactive({
    paths <- statsPaths()
    
    policeCncs <- input$statsPoliceCnc
    policeMin <- input$statsPoliceMin
    
    isolate({
      cncs <- seq(policeCncs[1], policeCncs[2], length.out = 30)
      pool <- makePolice(paths, policeMin, max(cncs)) 
      
      list(
        cncs = cncs,
        pool = pool
      )
      
    })
  })
  
  statsParam <- reactive({
    constructParam(
      steps = input$statsNSteps, 
      drunks = input$statsNDrunks, 
      policeMin = input$statsPoliceMin,
      policeCnc = input$statsPoliceCnc[1],
      policeAreBlind = FALSE
    )
  })
  
  statsData <- reactive({
    paths <- statsPaths()
    police <- statsPolice() 
    
    isolate({
      
      param <- statsParam()
      
      area <- getPathArea(getPathRange(paths))
      
      stats <- doMakeMultStats(paths, police$pool, area, param, police$cncs)
      
      title <- makeTitle(param$drunks, param$steps)
      
      list(paths = paths, 
           police = police, 
           param = param, 
           area = area, 
           stats = stats, 
           title = title)
      
    })
  })
  
  output$statsChart <- renderPlotly({
    
    data <- statsData()
    height <- input$chartHeight
    
    isolate({
      makeStatsChart(data$stats, data$title, height = height)
    })
  })
  
  output$stats3DChart <- renderPlotly({
    
    data <- statsData()
    height <- input$chartHeight
    
    isolate({
      makeStatsChart3D(data$stats, data$title, height = height)
    })
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