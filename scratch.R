library(dplyr)

source("drunkFuncs.R")

param <- constructParam(
  steps = 1000, 
  drunks = 100000,
  policeMin = 1, 
  policeCnc = 0.005,
  policeAreBlind = FALSE
)

start.time = Sys.time()
dp <- makeDrunkPath(nSteps = 1000, nDrunks = 100000)
duration = Sys.time() - start.time
duration


makeDrunkPath(nSteps = 2, nDrunks = 3)

run <- makeRun(param)




makeStatsAndChart(run, logX = FALSE) # police concentration vs percentage of survival

makeRunChart(run)

mult_stats <- makeMultStats(param, policeCncs = seq(0.0001, 0.003, 0.0001))

title = makeTitle(param$drunks, param$steps)

makeStatsChart(mult_stats, title)

makeStatsChart3D(mult_stats, title)

  