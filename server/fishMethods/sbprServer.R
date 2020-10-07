sbprModule <- function(input, output, session) {
  
  sbprExec <- reactiveValues()
  sbprUploadVreResult <- reactiveValues()
  
  inputSbprData <- reactiveValues()
  
  sbprFileData <- reactive({
    contents <- validateFishMethodsFile(input$fileSbpr$datapath)
    if (is.null(contents$contents)) {
      shinyjs::disable("go_sbpr")
      showModal(modalDialog(
        title = "Error",
        if(!is.null(contents$checkDec)){
          if(contents$checkDec=="not point"){"Please ensure your separate decimals using points ‘.’ or you don't have non numeric value"
          }else if(contents$checkName=="colname error"){
            text<-"Please ensure your columns names exactly match the guidelines, i.e."
            text<-paste0(text, "<ul>")
            text <- paste0(text, "<li>age</li>")
            text <- paste0(text, "<li>ssbwgt</li>")
            text <- paste0(text, "<li>partial</li>")
            text <- paste0(text, "<li>pmat</li>")
            text <- paste0(text, "</ul>")
            HTML(text)
          } else{"Input file seems invalid"}},
        easyClose = TRUE,
        footer = NULL
      ))
      return (NULL)
    } else {
      shinyjs::enable("go_sbpr")
      return (contents$contents)  
    }
    
  })
  
  observeEvent(input$fileSbpr, {
    inputSbprData$data <- sbprFileData()
  })
  
  observeEvent(input$go_sbpr, {
    
    result = tryCatch({
      js$showComputing()
      js$removeBox("box_sbpr_results")
      dat <- inputSbprData$data
      
      flog.info("Starting SBPR computation")
      res <- sbpr_shinyApp(age=dat$age,ssbwgt=dat$ssbwgt,partial=dat$partial,pmat=dat$pmat,M=input$SBPR_M,pF=input$SBPR_pF, pM=input$SBPR_pM,MSP=input$SBPR_MSP,plus=FALSE,maxF=input$SBPR_maxF,incrF=input$SBPR_incrF, graph=FALSE)
      js$hideComputing()
      if ('error' %in% names(res)) {
        showModal(modalDialog(
          title = "Error",
          res$error,
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        sbprExec$results <- res
        js$showBox("box_sbpr_results")
        sbprUploadVreResult$res <- FALSE
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
            flog.info("Uploading SBPR report to i-Marine workspace")
            reportFileName <- paste(tempdir(),"/","Sbpr_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
            createSbprPDFReport(reportFileName, sbprExec, input)
            sbprUploadVreResult$res <- FALSE
            
            basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")
            tryCatch({
              uploadToIMarineFolder(reportFileName, basePath, uploadFolderName)
              sbprUploadVreResult$res <- TRUE
            }, error = function(err) {
              flog.error("Error uploading SBPR report to the i-Marine Workspace: %s", err)
              sbprUploadVreResult$res <- FALSE
            }, finally = {})
        }
      }}, error = function(err) {
        flog.error("Error in SBPR: %s ",err)
        showModal(modalDialog(
          title = "Error",
          HTML(sprintf(getErrorMessage("SBPR"), err)),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      },
      finally = {
        js$hideComputing()
        js$enableAllButtons()
      })})
  
  observeEvent(input$reset_sbpr, {
    resetSBPRInputValues()
  })
  
  output$sbprOutPlot1 <- renderPlot({
    if ('results' %in% names(sbprExec)) {
      plot(sbprExec$results$F_vs_SPR[,2]~sbprExec$results$F_vs_SPR[,1],ylab="SPR",xlab="Fishing Mortality (F)",type="l")
      abline(h=sbprExec$results$Reference_Point[1,2], col = "red", lty = 2)
      legend(1.4, 8, legend=c("SSB_per_recruit"),col=c("red"), lty=2, cex=0.9)
    }
  })
  output$sbprOutPlot2 <- renderPlot({
    if ('results' %in% names(sbprExec)) {
      plot(sbprExec$results$F_vs_SPR[,3]~sbprExec$results$F_vs_SPR[,1],ylab="% Max SPR",xlab="Fishing Mortality (F)",type="l")
      abline(h=input$SBPR_MSP, v = sbprExec$results$Reference_Point[1,1], col = "red", lty = 2)
      leg <- paste0(input$SBPR_MSP, "% MSP")
      legend(1.5, 85, legend=c(leg),col=c("red"), lty=2, cex=0.9)
    }
  })
  output$sbprMSPTableTitle <- renderText({
    if ('results' %in% names(sbprExec)) {
      title <- ""
      #title <- paste0("&nbsp;&nbsp;&nbsp;&nbsp;<b>F", input$SBPR_MSP, "% MSP</b>")
      title
    }
  })
  output$sbprOutTable <- renderTable({
    if ('results' %in% names(sbprExec)) {
      df <- as.data.frame(sbprExec$results$Reference_Point)
      if (!is.na(session$userData$fishingMortality$Fcurr)) {
        df$Fcurr <- session$userData$fishingMortality$Fcurr
      }
      else if (!is.na(session$userData$fishingMortality$FcurrGA)) {
        df$Fcurr <- session$userData$fishingMortality$FcurrGA
      }
      else if (!is.na(session$userData$fishingMortality$FcurrSA)) {
        df$Fcurr <- session$userData$fishingMortality$FcurrSA
      } else {
        df$Fcurr <- "You need to estimate Fcurrent before calculating F30%MSPR, using ELEFAN method if you have length frequency data."  
      }
      colnames(df) <- c("F30%MSPR", "SPR at F30%MSPR", "Fcurrent")
      df
    }
  }, options = list(
    paging = FALSE, searching = FALSE
  ))
  output$downloadSbprReport <- renderUI({
    if ("results" %in% names(sbprExec)) {
      downloadButton(session$ns('createSbprReport'), 'Download Report')
    }
  })
  output$SBPRVREUpload <- renderText(
    {
      text <- ""
      if ("results" %in% names(sbprExec)) {
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
          if (isTRUE(sbprUploadVreResult$res)) {
            text <- paste0(text, VREUploadText)
          }
        }
      }
      text
    }
  )
  output$createSbprReport <- downloadHandler(
    filename = paste("Sbpr_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
    content = function(file) {
      createSbprPDFReport(file, sbprExec, input)
    }
  )
  output$SBPRDataConsiderationsText <- renderText({
    fishMethodsDataConsiderationText()
  })
  
  output$sbprTitle <- renderText({
    session$userData$page("sbpr")
    text <- "<span><h3><b>Spawning stock biomass-per-recruit (SBPR)</b></h3></span>"
    text
  })
}