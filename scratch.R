library(dplyr)

source("drunkFuncs.R")

param <- constructParam(
  steps = 10000, 
  drunks = 100,
  policeMin = 1, 
  policeCnc = 0.005,
  policeAreBlind = FALSE
)

run <- makeRun(param)

makeStatsAndChart(run, logX = FALSE) # police concentration vs percentage of survival

makeRunChart(run)

mult_stats <- makeMultStats(param, policeCncs = seq(0.0001, 0.003, 0.0001))

title = makeTitle(param$drunks, param$steps)

makeStatsChart(mult_stats, title)

makeStatsChart3D(mult_stats, title)

  