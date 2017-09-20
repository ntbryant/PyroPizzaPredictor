
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Pyro Pizza Dough Predictor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Spreadsheet", tabName = "spreadsheet", icon = icon("th")),
      menuItem(actionButton("save","Save Table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Dashboard
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Enter Inventory",
                  solidHeader = TRUE,
                  status = "success",
                  helpText(HTML(paste("Enter the current dough inventory for",
                                      "<br>",
                                  lubridate::wday(Sys.Date(),
                                                  label=TRUE,
                                                  abbr=FALSE),", ",
                                  lubridate::month(Sys.Date(),
                                                   label=TRUE,
                                                   abbr=FALSE)," ",
                                  day(Sys.Date()),", ",
                                  year(Sys.Date())))),
                  
                  wellPanel(
                    numericInput("inventory",NULL,300,min=0,max=600),
                    actionButton("submit" ,"Submit")
                  )
                ),
                valueBoxOutput("bigaBox")
              ),
              
              fluidRow(
                box(
                  title = "Weather Forecast",
                  solidHeader = TRUE,
                  status = "warning",
                  htmlOutput("frame"))
              )
      ),
      
      # Spreadsheet
      tabItem(tabName = "spreadsheet",
              fluidPage(
                  rHandsontableOutput("hot")
              )
      )
    )
  )
)