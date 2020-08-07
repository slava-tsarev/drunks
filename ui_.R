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
    sidebarMenu(id = "menu",
      menuItem("Plot the Walk", tabName = "walkTab", icon = icon("random"), startExpanded = TRUE),
      menuItem("Plot Stats", icon = icon("chart-line"), tabName = "statsTab", badgeLabel = "new", badgeColor = "green"),
      menuItem("Plot Stats 3D", icon = icon("cube"), tabName = "stats3DTab", badgeLabel = "new", badgeColor = "green")
    ),
    conditionalPanel(condition = "input.menu  == 'walkTab'",
      numericInput(inputId = "nDrunks", label = "Drunks", value = 10, min = 0, step = 1),
      numericInput(inputId = "nSteps", label = "Steps", value = 5000, min = 1, step = 1),
      numericInput(inputId = "seed", label = "Seed", value = 19, min = 1, step = 1),
      numericInput(inputId = "policeMin", label = "Police Min", value = 1, min = 0, step = 1),
      numericInput(inputId = "policeConcentration", label = "Police Concentration", value = 0.0005, min = 0, max = 1),
      materialSwitch(inputId = "finalPositionOnly", label = "Show Final Position Only", right = TRUE, value = FALSE, inline = FALSE),
      materialSwitch(inputId = "policeAreBlind", label = "Police Are Blind", right = TRUE, value = FALSE, inline = FALSE)
    ),
    conditionalPanel(condition = "(input.menu == 'statsTab') || (input.menu == 'stats3DTab')",
      numericInput(inputId = "statsNDrunks", label = "Drunks", value = 100, min = 0, step = 1),
      numericInput(inputId = "statsNSteps", label = "Steps", value = 10000, min = 1, step = 1),
      numericInput(inputId = "statsSeed", label = "Seed", value = 19, min = 1, step = 1),
      numericInput(inputId = "statsPoliceMin", label = "Police Min", value = 1, min = 0, step = 1),
      sliderInput(inputId = "statsPoliceCnc", label = "Police Concentration", min = 0, max = 0.05, value = c(0.0001, 0.003), step = 0.0001)
    ),
    numericInput(inputId = "chartHeight", label = "Chart Height, px", value = 630, min = 200, max = 5000, step = 50)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "walkTab",
              plotlyOutput("walkChart")
      ),
      
      tabItem(tabName = "statsTab",
              plotlyOutput("statsChart")
      ),
      
      tabItem(tabName = "stats3DTab",
              plotlyOutput("stats3DChart")
      )
    )
  )
)
