elefanModule <- function(input, output, session) {
  
  elefan <- reactiveValues()
  elefanUploadVreResult <- reactiveValues()
  
  inputElefanData <- reactiveValues()
  fileState <- reactiveValues(
    upload = NULL
  )
  
  elefanFileData <- reactive({
    if (is.null(input$fileElefan) || is.null(fileState$upload)) {
      return(NA)
    }
    contents <- read_elefan_csv(input$fileElefan$datapath, input$elefanDateFormat)
    
    if (is.null(contents$catch)) {
      shinyjs::disable("go")
      showModal(modalDialog(
        title = "Error",
        if(!is.null(contents$checkDec)){
        if(contents$checkDec=="not point"){"Please ensure your separate decimals using points ‘.’ or you don't have non numeric value"
        }else if(contents$checkName=="colname error"){"Please ensure your first column name is : 'midLength'"
        } else{"Input file seems invalid"}},
        easyClose = TRUE,
        footer = NULL
      ))
      return (NULL)
      } else {
       if(is.Date(contents$dates)&&is.unsorted(contents$dates)){
        shinyjs::disable("go")
        showModal(modalDialog(
          title = "Error",
          "Please ensure that your dates are input in chronological order from left to right.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
    } else {
      shinyjs::enable("go")
      return (contents)  
    }
      }
   })
  
  observeEvent(input$fileElefan, {
    fileState$upload <- 'uploaded'
    inputElefanData$data <- elefanFileData()
  })
  
  observeEvent(input$elefanDateFormat, {
    inputElefanData$data <- elefanFileData()
  })
  
  observeEvent(input$go, {
    js$showComputing()
    #inputCsvFile <- infile$datapath
    js$removeBox("box_elefan_results")
    js$disableAllButtons()
    result = tryCatch({
      #dataset <- read_elefan_csv(inputCsvFile)
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
      flog.info("Starting Elegan computation")
      maxtime=as.numeric(object.size(inputElefanData$data))*0.145
      td<-seconds_to_period(round(maxtime,0))
      td<-sprintf('%02d:%02d:%02d', td@hour, minute(td), second(td))
      cat(sprintf("Time out fixed to %s",td),"\n")
      res<-withTimeout(run_elefan(inputElefanData$data, binSize = 4, Linf_fix = input$ELEFAN_Linf_fix, Linf_range = elefan_linf_range, K_range = elefan_k_range,
                             C = input$ELEFAN_C, ts = input$ELEFAN_ts, MA = input$ELEFAN_MA, addl.sqrt = input$ELEFAN_addl.sqrt,
                             agemax = elefan_agemax, contour = input$ELEFAN_contour, plus_group = input$ELEFAN_PLUS_GROUP),timeout = maxtime, onTimeout = "warning")
      #res<-run_elefan(inputElefanData$data, binSize = 4, Linf_fix = input$ELEFAN_Linf_fix, Linf_range = elefan_linf_range, K_range = elefan_k_range,
      #                            C = input$ELEFAN_C, ts = input$ELEFAN_ts, MA = input$ELEFAN_MA, addl.sqrt = input$ELEFAN_addl.sqrt,
      #                            agemax = elefan_agemax, contour = input$ELEFAN_contour, plus_group = input$ELEFAN_PLUS_GROUP)
      
            
      js$hideComputing()
      js$enableAllButtons()
      if ('error' %in% names(res)) {
        showModal(modalDialog(
          title = "Error",
          if(!is.null(res$error)){if (!is.null(grep("POSIXlt",res$error))) {
            HTML(sprintf("Please check that the chosen date format matches the date format in your data file.<hr/> <b>%s</b>",res$error))
            
       }else  if (!is.null(grep("reached elapsed time limit",res$error))){
         HTML(sprintf("Maximum time (%s) overpassed, the process of the calculations is abnormally long.", td))
       }else{res$error}},
          easyClose = TRUE,
          footer = NULL
        ))
      } else  {
        js$showBox("box_elefan_results")
        elefan$results <- res
        session$userData$fishingMortality$Fcurr <- round(elefan$results$plot3$currents[4]$curr.F, 2)
        
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
          flog.info("Uploading Elefan report to i-Marine workspace")
          reportFileName <- paste("/tmp/","Elefan_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
          createElefanPDFReport(reportFileName,elefan,input)
          
          basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")
          tryCatch({
            uploadToIMarineFolder(reportFileName, basePath, uploadFolderName)
            elefanUploadVreResult$res <- TRUE
          }, error = function(err) {
            flog.error("Error uploading Elefan report to the i-Marine Workspace: %s", err)
            elefanUploadVreResult$res <- FALSE
          }, finally = {})
        }
      }
    } , error = function(err) {
      flog.error("Error in Elefan: %s ",err)
      showModal(modalDialog(
        title = "Error",
        HTML(sprintf(getErrorMessage("ELEFAN"), err)),
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
  
  observeEvent(input$reset_elefan, {
    fileState$upload <- NULL
    resetElefanInputValues()
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
      downloadButton(session$ns('createElefanReport'), 'Download Report')
    }
  })
  output$ElefanVREUpload <- renderText(
    {
      text <- ""
      if ("results" %in% names(elefan)) {
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
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
  
  output$elefanTitle <- renderText({
    session$userData$page("elefan")
    text <- "<span><h3><b>Elefan</b></h3></span>"
    text
  })
}