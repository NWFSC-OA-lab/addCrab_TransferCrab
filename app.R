#####
## Plate transfer form generator
####
library(shiny)

source("CrabTransferForApp.R", local = TRUE)

# Define UI ----
ui <- fluidPage(
  titlePanel("Well Plate Transfer Form Generator"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 fileInput("inputFile", h4("Input File"), multiple = FALSE, accept = c(".csv")),
                 dateInput("lastDate", h4("Last Transfer Date")),
                 dateInput("transferDate", h4("New Transfer Date")),
                 checkboxInput("isSameChamber", "Crabs returning to same chamber", value = TRUE),
                  radioButtons("isSameWellPostion", h4("Well Postion"),
                               choices = list("Keep well postion", "Condense wellplates"), selected = "Keep well postion"),
                 checkboxInput("isLastStage", "Include last stage", value = FALSE),
                 fileInput("wellAnalysisFile", h4("Well Analysis File"), multiple = FALSE, accept = c(".csv")),
                 downloadButton("downloadData", "Download")),
    mainPanel(
      width = 10,
      # tags$style(type="text/css",
      #            ".shiny-output-error { visibility: hidden; }",
      #            ".shiny-output-error:before { visibility: hidden; }"
      # ),
      dataTableOutput("inputTable"),
      dataTableOutput("outputTable")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
    values <- reactiveValues(isSameWellPosition = NULL, inputData = NULL, wellAnalysisData = NULL, outputData = NULL)
    
    observeEvent(input$isSameWellPostion,{
      if(input$isSameWellPostion =="Keep well postion" ){
        values$isSameWellPosition <- TRUE
      }
      else{
        values$isSameWellPosition <- FALSE
      }
    })
    
    observeEvent(input$wellAnalysisFile,{
      values$wellAnalysisData <- getLastStageDF(input$wellAnalysisFile$datapath)
    })
    
    observeEvent(input$inputFile, {
      values$inputData <- getLastTransferFile(input$inputFile$datapath, values$isSameWellPosition)
      output$inputTable <- renderDataTable(values$inputData)
      values$outputData <-  getNewForm(values$inputData, values$isSameWellPosition, input$isSameChamber,
                                       input$isLastStage, values$wellAnalysisData)
      output$outputTable  <- renderDataTable(values$outputData)
    })
   # Downloadable csv of selected dataset ----
   output$downloadData <- downloadHandler(
     filename = function() {
       getOutfileName(input$inputFile$name, input$transferDate)
       #paste("test", ".csv", sep = "")
     },
     content = function(file) {
       writeHeader(file, input$lastDate, input$transferDate)
       writeNewForm(values$outputData, file)
       #write.csv(values$crabData, file, row.names = FALSE)
   })
}

# Run the app ----
shinyApp(ui = ui, server = server)