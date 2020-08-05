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
     numericInput(inputId = "nSteps", label = "Steps", value = 10000, min = 1, step = 1),
     numericInput(inputId = "seed", label = "Seed", value = 19, min = 1, step = 1),
     numericInput(inputId = "chartHeight", label = "Chart Height, px", value = 600, min = 200, max = 5000, step = 50)
  ),
  dashboardBody(
    plotlyOutput("walkChart")
  )
)
