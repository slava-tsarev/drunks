nDrunkPaths <- function(drunks, steps) {
  result <- bind_rows(
    lapply(seq(1, drunks),
           FUN = function(i) {
             makeDrunkPath(nSteps = steps, drunkNum = i)
           }
    )
  )
  result
}

# returns tibble(x, y) where n-th line is the drunk's location after n-th step
makeDrunkPath <- function(nSteps, drunkNum = 1) {
  
  # uniformly distributed between 0 and 100
  s0 <- runif(nSteps, 0, 100)
  
  # 0 = right, 1 = left, 2 = up, 3 = down
  s1 <- ifelse(s0 < 25, 0, 
               ifelse(s0 >= 25 & s0 < 50, 1, 
                      ifelse(s0 >= 50 & s0 < 75, 2, 3)))
  
  # coordinate increments
  x0 <- ifelse(s1 == 0, 1, 
               ifelse(s1 == 1, -1, 0))
  
  y0 <- ifelse(s1 == 2, 1, 
               ifelse(s1 == 3, -1, 0))
  
  # absolute coordinates
  x1 <- cumsum(x0)
  y1 <- cumsum(y0)
  
  dataset <- tibble(
    n = drunkNum, 
    step = seq(1, nSteps), 
    x = x1, 
    y = y1
  )
  
  dataset  
}

makeChart <- function(uncaughtPaths, 
                      residualPaths = NULL, 
                      chartHeight = 600, 
                      police = NULL, 
                      finalOnly = FALSE) {

  
  p0 <- plot_ly(type = "scatter", mode = "lines", height = chartHeight) 
  
  if (finalOnly) {
    
    finalPositions <- uncaughtPaths %>% 
      group_by(n) %>%
      filter(step == max(step)) %>%
      ungroup()
    
    for (fin in unique(finalPositions$n)) {
      
      path = finalPositions %>% filter(n == fin)
      
      p0 <- p0 %>% add_trace(
        data = path, 
        name = paste0("drunk ", fin), 
        x = ~x,  y = ~y, 
        mode = "markers", 
        marker = list(size = 10)
      )
    }
    
  } else {
  
    for (up in unique(uncaughtPaths$n)) {
      
      path = uncaughtPaths %>% filter(n == up)
      
      p0 <- p0 %>% add_trace(
        data = path, 
        name = paste0("drunk ", up), 
        x = ~x,  y = ~y, mode = "lines"
      )
    }
    
    for (re in unique(residualPaths$n)) {
      
      path = residualPaths %>% filter(n == re)
      
      p0 <- p0 %>% add_trace(
        data = path, 
        name = paste0("drunk ", re), 
        x = ~x,  y = ~y, mode = "lines",
        line = list(
          width = 1, 
          color = "lightgray"
        )
      )
    }
  }
  
  p1 <- if (!is.null(police)) {
    p0 %>% 
      add_trace(
        data = police, 
        name = "police", 
        x = ~x, 
        y = ~y, 
        mode = "markers", 
        marker = list(size = 10, color = "gold", 
                      symbol = "hexagram",
                      line = list(width = 2, color = "DarkBlue")))
  } else p0
  
  p1
}

makePolice <- function(drunkPath, policeMin, policeConcentration) {

  rX <- range(drunkPath$x) # array of min and max values
  rY <- range(drunkPath$y)
  
  pathArea = (rX[2] - rX[1]) * (rY[2] - rY[1])
  
  nPolice <- round(max(policeMin, policeConcentration * pathArea))
  
  police <- tibble(
    x = round(runif(nPolice, 
              min = min(drunkPath$x),
              max = max(drunkPath$x)
    )),
    y = round(runif(nPolice, 
              min = min(drunkPath$y),
              max = max(drunkPath$y)
    ))
  )
  
  police

}

# police is a tibble(x, y) 
# paths is a tibble(n, step, x, y), where n is the number of drunk
# returns a list of three tibbles (uncaught, residuals, catchStatus)
catchDrunk <- function(paths, police, policeAreBlind = FALSE) {
  
  all_drunks <- unique(paths$n)
  no_catch <- tibble(n = all_drunks,  stepCaught = 0) 
  
  if (policeAreBlind) return(
    list(
      uncaught = paths,
      residuals = paths %>% head(0),
      catchStatus = no_catch
    )
  )
  
  # tibble(n, stepCaught)
  caught <- paths %>% 
    group_by(n) %>% 
    inner_join(police, by = c("x", "y")) %>%
    summarize(stepCaught = min(step)) %>%
    select(n, stepCaught) %>%
    ungroup()
  
  uncaught <- no_catch %>% filter(!(n %in% unique(caught$n)))
  
  catchStatus <- bind_rows(caught, uncaught)
  
  total <- paths %>% inner_join(catchStatus, by = "n") 
  
  uncaughtPaths <- total %>% 
    filter(stepCaught == 0 | step <= stepCaught) %>%
    select(n, step, x, y)
  
  residualPaths <- total %>% 
    filter(stepCaught > 0 & step > stepCaught) %>%
    select(n, step, x, y)
  
  result <- list(
    uncaught = uncaughtPaths,
    residuals = residualPaths,
    catchStatus = catchStatus
  )
  
  result
}


makeAll <- function(nSteps = 1000, seed = Sys.time()) {
  set.seed(seed)
  p0 <- makeDrunkPath(nSteps)
  makeChart(p0)
}