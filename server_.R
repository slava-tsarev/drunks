library(plotly)
library(dplyr)

source("drunkFuncs.R")

server <- shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  
  seedValue <- reactive({
    input$seed
  })
  
  observe({
    s <- seedValue()
    set.seed(s)
  })
  
  paths <- reactive({
    drunks <- input$nDrunks
    steps <- input$nSteps
    seed <- seedValue() # taking dependency
    
    isolate({
      p <- nDrunkPaths(steps, drunks)
      
      p
    })

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
    seed <- seedValue() # taking dependency
    
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
  
  statsChartPlotly <- reactive({
    data <- statsData()
    height <- input$chartHeight
    isolate({
      makeStatsChart(data$stats, data$title, height = height)
    })
  })
  
  output$statsChart <- renderPlotly({
    
    webGl <- input$useWebGl
    chart <- statsChartPlotly()
    
    isolate({
      if (webGl) chart %>% toWebGL() else chart
    })
  })
  
  stats3DChartPlotly <- reactive({
    data <- statsData()
    height <- input$chartHeight
    makeStatsChart3D(data$stats, data$title, height = height)
  })
  
  output$stats3DChart <- renderPlotly({
    
    # currently webGL doesn't support scatter3d type of trace
    # perhaps it can be migrated to surface3d 
    
    # webGl <- input$useWebGl
    chart <- stats3DChartPlotly()
    
    isolate({
      chart 
      #if (webGl) chart %>% toWebGL() else chart
    })
  })
  
  walkChartPlotly <- reactive({
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
  
  output$walkChart <- renderPlotly({
    
    webGl <- input$useWebGl
    chart <- walkChartPlotly()
    
    isolate({
      
      if (webGl) chart %>% toWebGL() else chart
      
    })
  })
  
})