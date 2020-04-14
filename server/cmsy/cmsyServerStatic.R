output$selectStockInFile <- renderUI({
  inFileCmsy <- input$fileCmsy
  if (is.null(inFileCmsy)) {
    return(NULL)
  }
  a <- read.csv(inFileCmsy$datapath)
  
  selectInput("stock", "Select a stock", sort(unique(a$Stock)))
})