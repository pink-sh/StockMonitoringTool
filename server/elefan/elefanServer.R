observeEvent(input$go, {
  infile <- input$fileElefan
  
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
  inputCsvFile <- infile$datapath
  js$removeBox("box_elefan_results")
  js$disableAllButtons()
  result = tryCatch({
    dataset <- read_elefan_csv(inputCsvFile)
    #ds <- lfqModify(lfqRestructure(dataset), bin_size = 4)
    
    #ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    
    elefan_linf_range <- NA
    if (!is.na(input$ELEFAN_Linf_range_from) && !is.na(input$ELEFAN_Linf_range_to)) {
      elefan_linf_range <- seq(from = input$ELEFAN_Linf_range_from, to = input$ELEFAN_Linf_range_to, by = input$ELEFAN_Linf_range_by)
    }
    
    elefan_k_range <- exp(seq(log(0.1), log(10), length.out=100))
    if (!is.na(input$ELEFAN_K_Range_from) && !is.na(input$ELEFAN_K_range_to)) {
      elefan_linf_range <- seq(from = input$ELEFAN_K_Range_from, to = input$ELEFAN_K_range_to, by = input$ELEFAN_K_range_by)
    }
    
    
    elefan_agemax <- input$ELEFAN_agemax 
    if (is.na(input$ELEFAN_agemax)) {
      elefan_agemax <- NULL
    }
    res <- run_elefan(dataset, binSize = 4, Linf_fix = input$ELEFAN_Linf_fix, Linf_range = elefan_linf_range, K_range = elefan_k_range,
                      C = input$ELEFAN_C, ts = input$ELEFAN_ts, MA = input$ELEFAN_MA, addl.sqrt = input$ELEFAN_addl.sqrt,
                      agemax = elefan_agemax, contour = input$ELEFAN_contour, plus_group = input$ELEFAN_PLUS_GROUP)
    js$hideComputing()
    js$enableAllButtons()
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      js$showBox("box_elefan_results")
      elefan$results <- res
      fishingMortality$Fcurr <- round(elefan$results$plot3$currents[4]$curr.F, 2)
      
      if (!is.null(sessionMode()) && sessionMode()=="GCUBE") {
        print("uploading to VRE")
        reportFileName <- paste("/tmp/","Elefan_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
        createElefanPDFReport(reportFileName,elefan,input)
        elefanUploadVreResult$res <- FALSE
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
          elefanUploadVreResult$res <- TRUE
        }, error = function(err) {
          print(paste0("Error uploading SBPR report to the Workspace: ", err))
          elefanUploadVreResult$res <- FALSE
        }, finally = {})
      }
    }
  } , error = function(err) {
    print(paste0("Error in Elefan ",err))
    showModal(modalDialog(
      title = "Error",
      "General error, please check your input file",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  },
  finally = {
    js$hideComputing()
    js$enableAllButtons()
  })
})
output$plot_1 <- renderPlot({
  if ('results' %in% names(elefan)) {
    plot(elefan$results$plot1, Fname = "catch", date.axis = "modern")
  }
})
output$plot_2 <- renderPlot({
  if ('results' %in% names(elefan)) {
    plot(elefan$results$plot2, Fname = "rcounts", date.axis = "modern")
  }
})
output$plot_3 <- renderPlot({
  if ('results' %in% names(elefan)) {
    plot(elefan$results$plot3, mark = TRUE)
    mtext("(a)", side = 3, at = -1, line = 0.6)
  }
})
output$plot_4 <- renderPlot({
  if ('results' %in% names(elefan)) {
    plot(elefan$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
    mtext("(b)", side = 3, at = -0.1, line = 0.6)
  }
})
output$plot_5 <- renderPlot({
  if ('results' %in% names(elefan)) {
    plot(elefan$results$data)
  }
})
output$tbl1_e <- renderTable({
  if ('results' %in% names(elefan)) {
    elefan$results$plot3$df_Es
  }
}, 
include.rownames=TRUE)
output$tbl2_e <- renderTable({
  if ('results' %in% names(elefan)) {
    CURR<-elefan$results$plot3$currents
    CURR<-CURR[,-7]
    names(CURR)<-c("Length-at-1st-capture (Lc)", "Age-at-1st-capture (tc)", "Effort","Fishing mortality", "Catch", "Yield", "Biomass")
    CURR
  }
}, 
include.rownames=TRUE)
output$downloadReport <- renderUI({
  if ("results" %in% names(elefan)) {
    downloadButton('createElefanReport', 'Download Report')
  }
})
output$ElefanVREUpload <- renderText(
  {
    text <- ""
    if ("results" %in% names(elefan)) {
      if (!is.null(sessionMode()) && sessionMode() == "GCUBE") {
        if (isTRUE(elefanUploadVreResult$res)) {
          text <- paste0(text, VREUploadText)
        }
      }
    }
    text
  }
)
output$createElefanReport <- downloadHandler(
  filename = paste("Elefan_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
  content = function(file) {
    createElefanPDFReport(file,elefan, input)
  }
)

output$par <- renderText({
  if ("results" %in% names(elefan)) {
    title <- "<hr>"
    title <- paste0(title, "<strong>Length infinity (", withMathJax("\\(L_\\infty\\)"), "in cm):</strong>&nbsp;", elefan$results$data$par$Linf)
    title <- paste0(title, "<br/>")
    title <- paste0(title, "<strong>Curving coefficient (K):</strong>&nbsp;", elefan$results$data$par$K)
    title <- paste0(title, "<br/>")
    title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor):</strong>&nbsp;", round(elefan$results$data$par$t_anchor, 2))
    title <- paste0(title, "<br/>")
    title <- paste0(title, "<strong>Amplitude of growth oscillation (C):</strong>&nbsp;", elefan$results$data$par$C)
    title <- paste0(title, "<br/>")
    title <- paste0(title, "<strong>Winter point of oscillation (</strong>&nbsp;", withMathJax("\\(t_w\\)") , "<strong>)</strong>&nbsp;")
    title <- paste0(title, "<br/>")
    title <- paste0(title, "<strong>Summer point of oscillation (", withMathJax("\\(ts\\)"),"=", withMathJax("\\(t_w\\)"), "- 0.5):</strong>&nbsp;", round(elefan$results$data$par$ts, 2))
    title <- paste0(title, "<br/>")
    title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", round(elefan$results$data$par$phiL, 2))
    title <- paste0(title, "<br/>")
    title <- paste0(title, "<br>")
    title
  } else {  "" }
})
output$elefanDataConsiderationsText <- renderText({
  text <- gsub("%%ELEFAN%%", "ELEFAN", getDataConsiderationTextForElefan())
  text
})