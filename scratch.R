
d1 <- tibble(
  x = c(0, 5, -3, 2), 
  y = c(0, 10, 14, -20),
  transparency = c(0.25, 0.5, 0.75, 1)
)
colorfunc <- colorRampPalette(c("black", "white"))

plot_ly(
  data = d1, 
  x = ~x, 
  y = ~y, 
  mode = "lines", 
  type = "scatter",
  line = list(color = c('rgba(100, 50, 50, 0.1)', 'rgba(100, 50, 50, 0.5)', 'rgba(100, 50, 50, 0.7)', 'rgba(100, 50, 50, 1)')) 
)