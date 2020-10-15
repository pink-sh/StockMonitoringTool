yprModule <- function(input, output, session) {
  
  yprExec <- reactiveValues()
  yprUploadVreResult <- reactiveValues()
  
  inputYprData <- reactiveValues()
  
  yprFileData <- reactive({
    contents <- validateFishMethodsFile(input$fileYpr$datapath)
    
      if (is.null(contents$contents)) {
        shinyjs::disable("go_YPR")
        showModal(modalDialog(
          title = "Error",
          if(!is.null(contents$checkDelim)){
          if(contents$checkDelim=="not ok"){"Please ensure that your .csv file delimiter is a comma ','"}  
          }else if(!is.null(contents$checkDec)){
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
      shinyjs::enable("go_YPR")
      return (contents$contents)  
    }
    
  })
  
  observeEvent(input$fileYpr, {
    inputYprData$data <- yprFileData()
  })
  
  observeEvent(input$go_YPR, {
    result = tryCatch({
      js$showComputing()
      js$removeBox("box_ypr_results")
      dat <- inputYprData$data
    
      flog.info("Starting YPR computation")
      res <- ypr_shinyApp(age=dat$age,wgt=dat$ssbwgt,partial=dat$partial,M=input$YPR_M,plus=input$YPR_Plus,oldest=input$YPR_oldest,maxF=input$YPR_maxF,incrF=input$YPR_incrF, graph = FALSE)
      js$hideComputing()
      if ('error' %in% names(res)) {
        showModal(modalDialog(
          title = "Error",
          res$error,
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        yprExec$results <- res
        js$showBox("box_ypr_results")
      
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
          flog.info("Uploading YPR report to i-Marine workspace")
          reportFileName <- paste(tempdir(),"/","Ypr_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
          createYprPDFReport(reportFileName, yprExec, input)
          yprUploadVreResult$res <- FALSE
        
          basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")
          tryCatch({
            uploadToIMarineFolder(reportFileName, basePath, uploadFolderName)
            yprUploadVreResult$res <- TRUE
          }, error = function(err) {
            flog.error("Error uploading YPR report to the i-Marine Workspace: %s", err)
            yprUploadVreResult$res <- FALSE
          }, finally = {})
        }
      }}, error = function(err) {
        flog.error("Error in YPR: %s ",err)
        showModal(modalDialog(
          title = "Error",
          HTML(sprintf(getErrorMessage("YPR"), err)),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      },
      finally = {
        js$hideComputing()
        js$enableAllButtons()
      })})
  
  observeEvent(input$reset_ypr, {
    resetYPRInputValues()
  })
  
  output$yprFishingMortality <- renderText({
    if (is.na(session$userData$fishingMortality$FcurrGA) && is.na(session$userData$fishingMortality$FcurrSA) && is.na(session$userData$fishingMortality$Fcurr)) {
      text <- "<strong>You need to estimate Fcurrent before calculating YPR, using ELEFAN method if you have lengnth frequency data. The data used for ELEFAN and YPR analysis should come from the same fish stock.</strong>"
    } else {
      text <- ""
      if (!is.na(session$userData$fishingMortality$Fcurr)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN method: </strong>", session$userData$fishingMortality$Fcurr, "<br/>")
      }
      if (!is.na(session$userData$fishingMortality$FcurrGA)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN GA method: </strong>", session$userData$fishingMortality$FcurrGA, "<br/>")
      }
      if (!is.na(session$userData$fishingMortality$FcurrSA)) {
        text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN SA method: </strong>", session$userData$fishingMortality$FcurrSA, "<br/>")
      }
    }
    text
  })
  output$yprOutPlot <- renderPlot({
    if ('results' %in% names(yprExec)) {
      YPR <- yprExec$results$F_vs_YPR
      plot(YPR[,2]~YPR[,1],ylab="Yield-Per-Recruit",xlab="Fishing Mortality (F)",type="l")
      abline(h = yprExec$results$Reference_Points[2,2], v = yprExec$results$Reference_Points[2,1], col = "black", lty = 2)
      abline(h = yprExec$results$Reference_Points[1,2], v = yprExec$results$Reference_Points[1,1], col = "red", lty = 2)
      legend(1.7, 0.09, legend=c("F-0.1", "F-Max"),col=c("red", "black"), lty=2:2, cex=0.9)
    }
  })
  output$yprOutTable <- renderTable({
    if ('results' %in% names(yprExec)) {
      #yprExec$results$Reference_Points
      df <- as.data.frame(yprExec$results$Reference_Points)
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
      #colnames(yprExec$results$Reference_Points) <- c("F", "Yield Per Recruit")
      colnames(df) <- c("F", "Yield Per Recruit", "Fcurrent")
      df
    }
  }, 
  include.rownames=TRUE, align="c")
  output$yprDifference <- renderText({
    if ('results' %in% names(yprExec)) {
      differenceinYPR = round(yprExec$results$Reference_Points[2,2] - yprExec$results$Reference_Points[1,2], 6)
      text <- paste0("<b>Difference in YPR: </b>",round(differenceinYPR, 4))
      text
    }
  })
  output$downloadYprReport <- renderUI({
    if ("results" %in% names(yprExec)) {
      colnames(yprExec$results$Reference_Points) <- c("F", "Yield Per Recruit")
      downloadButton(session$ns('createYprReport'), 'Download Report')
    }
  })
  output$YPRVREUpload <- renderText(
    {
      text <- ""
      if ("results" %in% names(yprExec)) {
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
          if (isTRUE(yprUploadVreResult$res)) {
            text <- paste0(text, VREUploadText)
          }
        }
      }
      text
    }
  )
  output$createYprReport <- downloadHandler(
    filename = paste("Ypr_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
    content = function(file) {
      createYprPDFReport(file, yprExec, input)
    }
  )
  
  output$YPRDataConsiderationsText <- renderText({
    fishMethodsDataConsiderationText()
  })
  
  output$yprTitle <- renderText({
    session$userData$page("ypr")
    text <- "<span><h3><b>Yield-per-recruit (YPR)</b></h3></span>"
    text
  })
}