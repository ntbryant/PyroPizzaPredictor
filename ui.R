
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(dtplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Pyro Pizza Dough Predictor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Spreadsheet", tabName = "spreadsheet", icon = icon("th")),
      menuItem(actionButton("save","Save Spreadsheet"))
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
                  helpText(HTML(paste0("Enter the current dough inventory for",
                                      "<br>",
                                  lubridate::wday(Sys.Date(),
                                                  label=TRUE,
                                                  abbr=FALSE),", ",
                                  lubridate::month(Sys.Date(),
                                                   label=TRUE,
                                                   abbr=FALSE)," ",
                                  day(Sys.Date()),", ",
                                  year(Sys.Date())))),
                  
                  
                  numericInput("inventory",NULL,value=NA),
                  actionButton("submitInventory" ,"Submit")
                ),
                valueBoxOutput("bigaBox")
              ),
              
              fluidRow(
                box(
                  title = "Recommended Prep for Today",
                  solidHeader = TRUE,
                  status = "danger",
                  helpText(HTML(paste0("Expected use for next ",
                                      lubridate::wday((Sys.Date()+4),
                                                      label=TRUE,
                                                      abbr=FALSE)," is ",
                                      dt[date==(Sys.Date()+4),use_predicted],
                                      " pizzas. Given current inventory and the expected
                                      use of ",dt[date==Sys.Date(),par],
                                      " doughs over the next four days, recommended prep
                                      for today is:"))),
                  h1(textOutput("expected_use"), align = "center")
                )
              ),
              
              fluidRow(
                box(
                  title = "Actual Prep and Waste",
                  solidHeader = TRUE,
                  status = "success",
                  helpText("Amount prepped: "),
                  numericInput("prep_actual",NULL,value=NA),
                  helpText("Amount wasted: "),
                  numericInput("waste",NULL,value=NA),
                  actionButton("submitPrep" ,"Submit")
                )
              ),
              
              fluidRow(
                tags$iframe(
                  seamless = "seamless", 
                  src = "https://forecast.io/embed/#lat=45.5121&lon=-122.6535&name=Downtown Portland",
                  height = 800, width = 1400
                )
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