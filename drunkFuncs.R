# returns tibble(x, y) where n-th line is the drunk's location after n-th step
makeDrunkPath <- function(nSteps) {
  
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
  
  dataset <- tibble(x = x1, y = y1)
  
  dataset  
}

makeChart <- function(drunkPath, chartHeight = 600, police = NULL) {

  p0 <- plot_ly(type = "scatter", mode = "lines", height = chartHeight) %>%
    add_trace(data = drunkPath, name = "drunk", x = ~x,  y = ~y, mode = "lines") 
  
  p1 <- if (!is.null(police)) {
    p0 %>% 
      add_trace(
        data = police, 
        name = "police", 
        x = ~x, 
        y = ~y, 
        mode = "markers", 
        marker = list(size = 10))
  } else p0
  
  p1
}

makePolice <- function(drunkPath, policeMin, policeConcentration) {

  rX <- range(drunkPath$x) # array of min and max values
  rY <- range(drunkPath$y)
  
  pathArea = (rX[2] - rX[1]) * (rY[2] - rY[1])
  
  nPolice <- round(max(policeMin, policeConcentration * pathArea))
  
  police <- tibble(
    x = runif(nPolice, 
              min = min(drunkPath$x),
              max = max(drunkPath$x)
    ),
    y = runif(nPolice, 
              min = min(drunkPath$y),
              max = max(drunkPath$y)
    )
  )
  
  police

}




makeAll <- function(nSteps = 1000, seed = Sys.time()) {
  set.seed(seed)
  p0 <- makeDrunkPath(nSteps)
  makeChart(p0)
}