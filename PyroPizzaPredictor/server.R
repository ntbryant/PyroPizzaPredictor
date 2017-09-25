
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dtplyr)

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
  
  DF = dt[,.(date,day,initial_inventory,par,prep_rec,prep_actual,waste,
             final_inventory,short_long,scale,use_expected,use_actual,
             use_predicted,temp_max,temp_min,conditions,rain,snow,
             humidity,wind,season,holiday,
             use7,use3,use1,avgUse,medUse,quart1Use,quart3Use)]
  DF[order(-date)]
  outdir=getwd()
  outfilename="table"
  
  values <- reactiveValues(hot = DF)
  
  # update dough prediction box
  observeEvent(input$submitInventory, {

    output$expected_use = renderText({
      
      (round((dt[date==Sys.Date(),par] - input$inventory) / 15) * 15)
      
    })

  })
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      
      # update table based on initial inventory
      if (input$submitInventory) {
        
        DF[date==Sys.Date(),initial_inventory:=input$inventory]
        DF[date==Sys.Date(),prep_rec:=round((par - initial_inventory)
                                            / 15) * 15]
        
        DF[is.na(final_inventory),
           final_inventory:=sum(initial_inventory,
                                prep_actual,
                                -waste,na.rm = TRUE),by=date]

        DF[,use_actual:=ifelse(date==Sys.Date()-1,sum(final_inventory,
                                                    -(.SD[match(date + 1,.SD[,date]),initial_inventory]),
                                                    na.rm=TRUE),
                                use_actual)]
        
        DF[date==Sys.Date(),short_long:=final_inventory - par]
      }
      
      # update table based on actual prep and waste
      if (input$submitPrep) {
        
        DF[date==Sys.Date(),prep_actual:=input$prep_actual]
        DF[date==Sys.Date(),waste:=input$waste]
        DF[date==Sys.Date(),
           final_inventory:=sum(initial_inventory,
                                prep_actual,
                                -waste,na.rm = TRUE),by=date]
        DF[date==Sys.Date(),scale:=(round((par - (final_inventory - use_predicted))
                                          / 15) * 15) + ifelse(prep_rec > prep_actual,
                                                              round((prep_rec - prep_actual)
                                                                    / 15) * 15,0)]
      }
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
                    height = 800,
                    readOnly = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1,
               columnSorting = TRUE) %>%
      hot_table(highlightCol=TRUE,highlightRow=TRUE) %>%
      hot_col(c("initial_inventory","par","prep_rec","prep_actual","waste",
                "final_inventory","short_long","scale","use_expected","use_actual",
                "temp_max","temp_min","humidity","wind","holiday",
                "use7","use3","use1","avgUse","medUse","quart1Use","quart3Use"),
              format = "0") %>%
      hot_col(c("initial_inventory","prep_actual","waste","scale"),
              readOnly = FALSE)
  })
  
  ## Save 
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
  })
  
})