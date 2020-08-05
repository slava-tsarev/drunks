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

makeChart <- function(drunkPath, chartHeight) {

  fig <- plot_ly(data = drunkPath, 
                 x = ~x, 
                 y = ~y,
                 mode = "lines", 
                 type = "scatter", 
                 height = chartHeight
        ) 
  fig
}

makeAll <- function(nSteps = 1000, seed = Sys.time()) {
  set.seed(seed)
  p0 <- makeDrunkPath(nSteps)
  makeChart(p0)
}