observeEvent(input$go_YPR, {
  infile <- input$fileYpr
  
  if (is.null(infile)) {
    showModal(modalDialog(
      title = "Error",
      "No input file selected",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  }
  js$showComputing()
  js$removeBox("box_ypr_results")
  inputCsvFile <- infile$datapath
  dat <- read.csv(inputCsvFile)
  
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
    
    if (!is.null(sessionMode()) && sessionMode()=="GCUBE") {
      print("uploading to VRE")
      reportFileName <- paste("/tmp/","Ypr_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
      createYprPDFReport(reportFileName, yprExec, input)
      yprUploadVreResult$res <- FALSE
      tryCatch({
        if (fileFolderExistsInPath(sessionUsername(),sessionToken(),paste0("/Home/",sessionUsername(),"/Workspace/"), uploadFolderName) == FALSE) {
          print("Creating folder")
          createFolderWs(
            sessionUsername(), sessionToken(),
            paste0("/Home/",sessionUsername(),"/Workspace/"),
            uploadFolderName, 
            uploadFolderDescription)
        }
        uploadToVREFolder(
          username = sessionUsername(), 
          token = sessionToken(), 
          relativePath = paste0("/Home/",sessionUsername(),"/Workspace/", uploadFolderName, "/"), 
          file = reportFileName,
          overwrite = TRUE,
          archive = FALSE
        )
        yprUploadVreResult$res <- TRUE
      }, error = function(err) {
        print(paste0("Error uploading SBPR report to the Workspace: ", err))
        yprUploadVreResult$res <- FALSE
      }, finally = {})
    }
  }
})
output$yprFishingMortality <- renderText({
  if (is.na(fishingMortality$FcurrGA) && is.na(fishingMortality$FcurrSA) && is.na(fishingMortality$Fcurr)) {
    text <- "<strong>You need to estimate Fcurrent before calculating YPR, using ELEFAN method if you have lengnth frequency data. The data used for ELEFAN and YPR analysis should come from the same fish stock.</strong>"
  } else {
    text <- ""
    if (!is.na(fishingMortality$Fcurr)) {
      text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN method: </strong>", fishingMortality$Fcurr, "<br/>")
    }
    if (!is.na(fishingMortality$FcurrGA)) {
      text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN GA method: </strong>", fishingMortality$FcurrGA, "<br/>")
    }
    if (!is.na(fishingMortality$FcurrSA)) {
      text <- paste0(text, "<strong>Fishing moratlity calculated with ELEFAN SA method: </strong>", fishingMortality$FcurrSA, "<br/>")
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
    if (!is.na(fishingMortality$Fcurr)) {
      df$Fcurr <- fishingMortality$Fcurr
    }
    else if (!is.na(fishingMortality$FcurrGA)) {
      df$Fcurr <- fishingMortality$FcurrGA
    }
    else if (!is.na(fishingMortality$FcurrSA)) {
      df$Fcurr <- fishingMortality$FcurrSA
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
    downloadButton('createYprReport', 'Download Report')
  }
})
output$YPRVREUpload <- renderText(
  {
    text <- ""
    if ("results" %in% names(yprExec)) {
      if (!is.null(sessionMode()) && sessionMode() == "GCUBE") {
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
  text <- "<h5><b>Ensure that spawning stock weight-at-age data is representative of the full population, i.e., are all age groups sampled?</b></h5>"
  text <- paste0(text, "<h5>", "**If desired, the life history parameters pulled from FishBase.org in the Supporting Tools: 'Natural Mortality Estimators' tool could be used to provide estimates of M in the Optional Parameters section.", "</h5>")
  text
})