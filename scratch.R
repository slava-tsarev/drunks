
drunkPath <- tibble(
  x = c(0, 5, -3, 2), 
  y = c(0, 10, 14, -20),
  transparency = c(0.25, 0.5, 0.75, 1)
)


policeConcentration <- 0.01
policeMin <- 1

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


plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(data = drunkPath, name = "drunk", x = ~x,  y = ~y, mode = "lines") %>%
  add_trace(data = police, name = "police", x = ~x, y = ~y, mode = "markers", marker = list(size = 10))
  