library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(shinyjs)


ui <- dashboardPage(
  dashboardHeader(
    title = "Drunks"
  ),
  dashboardSidebar(
     numericInput(inputId = "nDrunks", label = "Drunks", value = 1, min = 0, step = 1),
     numericInput(inputId = "nSteps", label = "Steps", value = 10000, min = 1, step = 1),
     numericInput(inputId = "seed", label = "Seed", value = 19, min = 1, step = 1),
     numericInput(inputId = "policeMin", label = "Police Min", value = 1, min = 0, step = 1),
     numericInput(inputId = "policeConcentration", label = "Police Concentration", value = 0.001, min = 0, max = 1),
     numericInput(inputId = "chartHeight", label = "Chart Height, px", value = 600, min = 200, max = 5000, step = 50),
     materialSwitch(inputId = "finalPositionOnly", label = "Show Final Position Only", right = TRUE, value = FALSE, inline = FALSE),
     materialSwitch(inputId = "policeAreBlind", label = "Police Are Blind", right = TRUE, value = FALSE, inline = FALSE)
  ),
  dashboardBody(
    plotlyOutput("walkChart")
  )
)
