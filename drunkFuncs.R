library(dplyr)
library(tidyr)
library(plotly)
library(data.table)
library(futile.logger)

constructParam <- function(steps, drunks, policeMin, policeCnc, policeAreBlind) {
  param <- list(
    steps = steps, 
    drunks = drunks,
    policeMin = policeMin, 
    policeCnc = policeCnc,
    policeAreBlind = policeAreBlind
  )
  param
}

# returns tibble(x, y) where n-th line is the drunk's location after n-th step
 makeDrunkPath <- function(nSteps, nDrunks = 1L) {
  
   s0 = as.integer(round(runif(nDrunks * nSteps, min = -0.5, max = 3.5)))
   
   x0 = rep(0L, length(s0))
   y0 = rep(0L, length(s0))
   
   x0[s0 == 0L] <-  1L
   x0[s0 == 1L] <- -1L
   y0[s0 == 2L] <-  1L
   y0[s0 == 3L] <- -1L
   
   f1 <- 1L + (0L:(nDrunks-1L)) * nSteps
   
   x0[f1] <- 0L
   y0[f1] <- 0L

   d0 <- tidyr::expand_grid (
     n = 1:nDrunks,
     step = 1:nSteps,
   ) 

   # cumsum is faster with data.table
   dt0 <- data.table(n = d0$n, x1 = x0, y1 = y0)
   dt0[, x := cumsum(x1), by = n]
   dt0[, y := cumsum(y1), by = n]
  
   d0 %>% mutate(x = dt0$x, y = dt0$y)
}

makeTitle <- function(drunks, steps) {
  paste0(
    drunks, " drunks, ", 
    steps, " steps"
  )
}

makeStatsTitle <- function(run, police = TRUE) {
  t <- makeTitle(run$param$drunks, run$param$steps)
  if (police) {
    paste0(
      t, ", ",
      nrow(run$police), " police (", 
      run$param$policeCnc, " cnc)"
    )
  } else t
}

makeStats <- function(run)  {
  
  catchStatus <- run$split$catchStatus
  
  d0 <- catchStatus %>% 
    mutate(caught = stepCaught > 0) %>%
    arrange(stepCaught) %>%
    mutate(caughtTotal = cumsum(caught)) %>%
    mutate(uncaughtTotal = nrow(catchStatus) - caughtTotal) %>%
    select(stepCaught, caughtTotal, uncaughtTotal) %>%
    rename(step = stepCaught) %>%
    unique()
  
  d0 %>% mutate(
    runId = run$id, 
    policeNum = nrow(run$police), 
    policeCnc = run$param$policeCnc
  )
  
}

constructRun <- function(id, param, dp, police, split) {
  run <- list(
    id = id, 
    param = param,
    paths = dp,
    police = police,
    split = split
  )
  run  
}

makeRun <- function(param, id = 1) {
  dp <- makeDrunkPath(param$steps, param$drunks)
  police <- makePolice(dp, param$policeMin, param$policeCnc)
  split <- catchDrunk(dp, police, param$policeAreBlind)
  
  constructRun(id, param, dp, police, split)
}

makeRunChart <- function(run, ...) {
  makeChart(run$split$uncaught, run$split$residuals, police = run$police, ...)
}

makeStatsChart3D <- function(stats, title = NULL, height = 600) {
  plot_ly(data = stats, 
          z = ~caughtTotal, 
          x = ~step,
          y = ~policeCnc,
          marker = list(
            size = 3,
            color = ~caughtTotal, 
            colorscale = "reds", 
            showscale = TRUE
          ), 
          mode = "markers",
          type = "scatter3d",
          height = height
  ) %>% 
    layout(title = title)
}

colorscale = c('#FFE1A1', '#683531')

makeStatsChart <- function(stats, title = NULL, caught = TRUE, survived = FALSE, logX = FALSE, height = 600) {
  
  p <- plot_ly(type = "scatter", mode = "lines+markers", height = height) 
  
  for (id in unique(stats$runId)) {
    
    d <- stats %>% filter(runId == id)
    
    r <- head(d, 1)
    cnc <- format(r$policeCnc, scientific = 1)
    name <- paste0(r$policeNum, " (", cnc, ")")
    
    p <- p %>%
      add_trace(data = d, 
                name = name, 
                legendgroup = "caught",
                visible = if (caught) TRUE else "legendonly",
                x = ~step, 
                y = ~caughtTotal) %>%
      add_trace(data = d, 
                name = name, 
                legendgroup = "survived",
                visible = if (survived) TRUE else "legendonly",
                x = ~step, 
                y = ~uncaughtTotal)
  }
  
  if (logX) {
    p <- p %>% layout(xaxis = list(type = "log"))  
  } 
  
  p <- p %>% layout(title = title)
  
  p
}

makeStatsAndChart <- function(run, ...) {
  stats <- makeStats(run)
  title <- makeStatsTitle(run)
  
  makeStatsChart(stats, title, ...)
}

addJitter <- function(d, jt) {
  d %>% 
    left_join(jt, by = "n") %>%
    mutate(x = x + jt, y = y + jt) %>%
    select(-jt)
}

applyJitter <- function(u, r) {
  
  n0 <- unique(u$n)
  
  n1 <- if (!is.null(r)) unique(r$n)
  
  n <- unique(c(n0, n1))
  
  jt <- tibble(n = n, jt = runif(length(n), -0.45, +0.45))

  ur <- u %>% addJitter(jt)
  
  rr <- if (!is.null(r)) r %>% addJitter(jt) else NULL
  
  list(u = ur, r = rr)
}

makeChart <- function(uncaughtPaths, 
                      residualPaths = NULL, 
                      chartHeight = 600, 
                      police = NULL, 
                      finalOnly = FALSE, 
                      displayResiduals = TRUE, 
                      jitter = FALSE) {
  
  if (jitter) {
    r <- applyJitter(uncaughtPaths, residualPaths)
    uncaughtPaths <- r$u
    residualPaths <- r$r
  }
  
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
    
    if (displayResiduals) {
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
  
    for (up in unique(uncaughtPaths$n)) {
      
      path = uncaughtPaths %>% filter(n == up)
      
      p0 <- p0 %>% add_trace(
        data = path, 
        name = paste0("drunk ", up), 
        x = ~x,  y = ~y, mode = "lines",
        line = list(
          width = 1
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
                      line = list(width = 1, color = "DarkBlue")))
  } else p0
  
  p1
}

getPathRange <- function(drunkPath) {
  rX <- range(drunkPath$x) # array of min and max values
  rY <- range(drunkPath$y)
  
  list(rX = rX, rY = rY)
}

getPathArea <- function(pathRange) {
  pathArea = (pathRange$rX[2] - pathRange$rX[1]) * 
             (pathRange$rY[2] - pathRange$rY[1])
  pathArea
}

calcNPolice <- function(area, policeMin, policeCnc) {
  round(max(policeMin, policeCnc * area))
}

# draws first N police from pool
drawPoliceFromPool <- function(policePool, nPolice) {
  
  policePool %>% head(n = nPolice)
  
}

makePolice <- function(drunkPath, policeMin, policeCnc) {

  range <- getPathRange(drunkPath)
  area <- getPathArea(range)
  nPolice <- calcNPolice(area, policeMin, policeCnc)
  
  police <- tibble(
    x = as.integer(
        round(
          runif(nPolice, 
              min = range$rX[1],
              max = range$rX[2]
          )
        )
    ),
    y = as.integer(
        round(
          runif(nPolice, 
            min = min(range$rY[1]),
            max = max(range$rY[2])
          )
        )
    )
  )
  
  police

}

# police is a tibble(x, y) 
# paths is a tibble(n, step, x, y), where n is the number of drunk
# returns a list of three tibbles (uncaught, residuals, catchStatus)
catchDrunk <- function(paths, police, policeAreBlind = FALSE, survivorsOnly = FALSE) {
  
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
    filter(stepCaught == 0 | (!survivorsOnly & stepCaught > 0 & step <= stepCaught)) %>%
    select(n, step, x, y)
  
  residualPaths <- total %>% 
    filter(stepCaught > 0 & (survivorsOnly | step > stepCaught)) %>%
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

doMakeMultStats <- function(dp, policePool, area, param, policeCncs) {
  multiple_run_stats <- bind_rows(
    lapply(1:length(policeCncs), FUN = function(i) {
      
      p <- param
      p$policeCnc <- policeCncs[i]
      
      nPolice <- calcNPolice(area, p$policeMin, p$policeCnc)
      police <- drawPoliceFromPool(policePool, nPolice)
      split <- catchDrunk(dp, police)
      
      run <- constructRun(id = i, param = p, dp = dp, police = police, split = split)
      
      makeStats(run)
      
    })
  )
  multiple_run_stats
}

makeMultStats <- function(param, policeCncs = c(0.01, 0.05)) {
  
  dp <- makeDrunkPath(param$steps, param$drunks)
  
  policePool <- makePolice(dp, param$policeMin, max(policeCncs)) # police pool 
  
  area <- getPathArea(getPathRange(dp))
  
  doMakeMultStats(dp, policePool, area, param, policeCncs)
}
