
library(rhandsontable)
library(shiny)
library(lubridate)

editTable <- function(DF, outdir=getwd(), outfilename="table"){
  ui <- shinyUI(fluidPage(
    
    titlePanel("Pyro Pizza Dough Predictor"),
    sidebarLayout(
      sidebarPanel(
        helpText(paste0("Enter the current dough inventory for ",
                 wday(Sys.Date(),label=TRUE,abbr=FALSE),", ",
                 month(Sys.Date(),label=TRUE,abbr=FALSE)," ",
                 day(Sys.Date()),", ",
                 year(Sys.Date()))),
        
        wellPanel(
          h3("Current Inventory"),
          numericInput("inventory",NULL,300,min=0,max=600),
          submitButton("Submit")
        ),
        br(), 
        
        wellPanel(
          h3("Save"), 
          actionButton("save", "Save table")
        )        
        
      ),
      
      mainPanel(
        
        rHandsontableOutput("hot")
        
      )
    )
  ))
  
  server <- shinyServer(function(input, output) {
    
    values <- reactiveValues()
    
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
      if (!is.null(DF))
        rhandsontable(DF, stretchH = "all")
    })
    
    ## Save 
    observeEvent(input$save, {
      finalDF <- isolate(values[["DF"]])
      saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
    })
    
  })
  

  ## run app 
  runApp(list(ui=ui, server=server))
  return(invisible())
}

dt[order(-date)]


editTable(dt)
