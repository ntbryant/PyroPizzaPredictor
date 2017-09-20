
library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)

shinyServer(function(input, output) {
  
  output$frame <- renderUI({
    
    tags$iframe(id = 'app',
                src = url("//forecast.io/embed/#lat=42.3583&lon=-71.0603&name=Downtown Boston",
                          method = "wininet"),
                width = '100%')
    })
  
  output$bigaBox <- renderValueBox({
    valueBox(
      HTML("62.8&deg;F"),
      "Biga Water Temperature",
      icon = icon("thermometer-half",lib = "font-awesome"),
      color = "blue"
    )
  })
  
  DF = copy(dt)
  DF[order(-date)]
  outdir=getwd()
  outfilename="table"
  
  values <- reactiveValues()
  
  observeEvent(input$inventory, {
    DF[date==Sys.Date(),initial_inventory:=input$inventory]
  })
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    DF <- DF[rev(order(DF$date))]
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "all",
                    width = "100%",
                    height = 800) %>%
      hot_cols(fixedColumnsLeft = 1,
               columnSorting = TRUE) %>%
      hot_table(highlightCol=TRUE,highlightRow=TRUE)
  })
  
  ## Save 
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
  })
  
})