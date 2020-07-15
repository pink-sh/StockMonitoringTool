elefanGaModule <- function(input, output, session) {
  
  elefan_ga <- reactiveValues()
  elefanGaUploadVreResult <- reactiveValues()
  
  inputElefanGaData <- reactiveValues()
  fileGaState <- reactiveValues(
    upload = NULL
  )
  
  elefanGaFileData <- reactive({
    if (is.null(input$fileGa) || is.null(fileGaState$upload)) {
      return(NA)
    }
    contents <- read_elefan_csv(input$fileGa$datapath, input$elefanGaDateFormat)
    print(input$fileGa)
    if (is.null(contents)) {
      shinyjs::disable("go_ga")
      showModal(modalDialog(
        title = "Error",
        "Input file seems invalid",
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
      shinyjs::enable("go_ga")
      return (contents)  
    }
    }
  })
  
  observeEvent(input$fileGa, {
    fileGaState$upload <- 'uploaded'
    inputElefanGaData$data <- elefanGaFileData()
  })
  
  observeEvent(input$elefanGaDateFormat, {
    inputElefanGaData$data <- elefanGaFileData()
  })
  
  observeEvent(input$go_ga, {

    js$showComputing()
    js$removeBox("box_elefan_ga_results")
    js$disableAllButtons()
    result = tryCatch({

      ds <- lfqModify(lfqRestructure(inputElefanGaData$data), bin_size = 4)
      flog.info("Starting Elegan GA computation")
      res <- run_elefan_ga(ds,binSize =  4, seasonalised = input$ELEFAN_GA_seasonalised, 
                           low_par = list(Linf = input$ELEFAN_GA_lowPar_Linf, K = input$ELEFAN_GA_lowPar_K, t_anchor = input$ELEFAN_GA_lowPar_t_anchor, C = input$ELEFAN_GA_lowPar_C, ts = input$ELEFAN_GA_lowPar_ts),
                           up_par = list(Linf = input$ELEFAN_GA_upPar_Linf, K = input$ELEFAN_GA_upPar_K, t_anchor = input$ELEFAN_GA_upPar_t_anchor, C = input$ELEFAN_GA_upPar_C, ts = input$ELEFAN_GA_upPar_ts),
                           popSize = input$ELEFAN_GA_popSize, maxiter = input$ELEFAN_GA_maxiter, run = input$ELEFAN_GA_run, pmutation = input$ELEFAN_GA_pmutation, pcrossover = input$ELEFAN_GA_pcrossover,
                           elitism = input$ELEFAN_GA_elitism, MA = input$ELEFAN_GA_MA, addl.sqrt = input$ELEFAN_GA_addl.sqrt, plus_group = input$ELEFAN_GA_PLUS_GROUP)
      
      js$hideComputing()
      js$enableAllButtons()
      if ('error' %in% names(res)) {
        showModal(modalDialog(
          title = "Error",
          if(grep("POSIXlt",res$error)==1) {
            HTML(sprintf("Please check that the chosen date format matches the date format in your data file.<hr/> <b>%s</b>",res$error)) 
          }else{res$error},
          easyClose = TRUE,
          footer = NULL
        ))
      # } else if(is.unsorted(contents$dates, na.rm = FALSE, strictly = FALSE)){
      #   showModal(modalDialog(
      #     title = "Error",
      #     HTML(sprintf("Please ensure that your dates are input in chronological order from left to right.")), 
      #     easyClose = TRUE,
      #     footer = NULL
      #   ))
      } else {
        js$showBox("box_elefan_ga_results")
        elefan_ga$results <- res
        session$userData$fishingMortality$FcurrGA <- round(elefan_ga$results$plot3$currents[4]$curr.F, 2)
        
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
          flog.info("Uploading Elefan GA report to i-Marine workspace")
          reportFileName <- paste("/tmp/","ElefanGA_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
          createElefanGaPDFReport(reportFileName,elefan_ga,input)
          elefanGaUploadVreResult$res <- FALSE
          
          basePath <- paste0("/Home/",session$userData$sessionUsername(),"/Workspace/")
          
          tryCatch({
            uploadToIMarineFolder(reportFileName, basePath, uploadFolderName)
            elefanGaUploadVreResult$res <- TRUE
          }, error = function(err) {
            flog.error("Error uploading Elefan GA report to the i-Marine Workspace: %s", err)
            elefanGaUploadVreResult$res <- FALSE
          }, finally = {})
        }
      }
    }, error = function(err) {
      flog.error("Error in Elefan GA: %s ",err)
      showModal(modalDialog(
        title = "Error",
        HTML(sprintf(getErrorMessage("ELEFAN GA"), err)),
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
  
  observeEvent(input$reset_ga, {
    fileGaState$upload <- NULL
    resetElefanGaInputValues()
  })
  
  output$plot_ga_1 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot1, Fname = "catch", date.axis = "modern")
    }
  })
  output$plot_ga_2 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot2, Fname = "rcounts", date.axis = "modern")
    }
  })
  output$plot_ga_3 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot3, mark = TRUE)
      mtext("(a)", side = 3, at = -1, line = 0.6)
    }
  })
  output$plot_ga_4 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
      mtext("(b)", side = 3, at = -0.1, line = 0.6)
    }
  })
  output$plot_ga_5 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$data)
    }
  })
  output$par_ga <- renderText({
    if ("results" %in% names(elefan_ga)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity (", withMathJax("\\(L_\\infty\\)"), "in cm):</strong>&nbsp;", round(elefan_ga$results$data$par$Linf, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient (K):</strong>&nbsp;", round(elefan_ga$results$data$par$K, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor):</strong>&nbsp;", round(elefan_ga$results$data$par$t_anchor, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation (NOTE: only if 'Seasonalized' is checked; C):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$data$par$C), NA, round(elefan_ga$results$data$par$C, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Winter point of oscillation (</strong>&nbsp;", withMathJax("\\(t_w\\)") , "<strong>)</strong>&nbsp;")
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (NOTE: only if 'Seasonalized' is checked; ", withMathJax("\\(ts\\)"),"=", withMathJax("\\(t_w\\)"), "- 0.5):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$data$par$ts), NA, round(elefan_ga$results$data$par$ts, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$data$par$phiL), "--", round(elefan_ga$results$data$par$phiL, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<br>")
      title
    } else {  "" }
  })
  output$downloadReport_ga <- renderUI({
    if ("results" %in% names(elefan_ga)) {
      downloadButton(session$ns('createElefanGAReport'), 'Download Report')
    }
  })
  output$ElefanGaVREUpload <- renderText(
    {
      text <- ""
      if ("results" %in% names(elefan_ga)) {
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
          if (isTRUE(elefanGaUploadVreResult$res)) {
            text <- paste0(text, VREUploadText)
          }
        }
      }
      text
    }
  )
  output$createElefanGAReport <- downloadHandler(
    filename = paste("ElefanGA_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
    content = function(file) {
      createElefanGaPDFReport(file, elefan_ga, input)
    }
  )
  output$tbl1_ga <- renderTable({
    if ('results' %in% names(elefan_ga)) {
      elefan_ga$results$plot3$df_Es
    }
  }, 
  include.rownames=TRUE)
  output$tbl2_ga <- renderTable({
    if ('results' %in% names(elefan_ga)) {
      CURR_GA<-elefan_ga$results$plot3$currents
      CURR_GA<-CURR_GA[,-7]
      names(CURR_GA)<-c("Length-at-1st-capture (Lc)", "Age-at-1st-capture (tc)", "Effort","Fishing mortality", "Catch", "Yield", "Biomass")
      CURR_GA
    }
  }, 
  include.rownames=TRUE)
  
  output$title_tbl1_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Biological reference levels:</p>"
      txt
    }
  })
  output$title_tbl2_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Current levels:</p>"
      txt
    }
  })
  output$titlePlot1_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Restructured LFQ data</p>"
      txt
    }
  })
  output$titlePlot3_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
      txt
    }
  })
  output$titlePlot4_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
      txt
    }
  })
  output$titleResultsOfTheComputation_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<h2>Results of the ELEFAN_GA computation</h2>"
      txt
    }
  })
  
  output$elefanGADataConsiderationsText <- renderText({
    text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getDataConsiderationTextForElefan())
    text
  })
  
  output$rnMax_ga <- renderText({
    if ("results" %in% names(elefan_ga)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan_ga$results$data$Rn_max, 3))
      title
    } else {  "" }
  })
  
  output$elefanGaTitle <- renderText({
    session$userData$page("elefan-ga")
    text <- "<span><h3><b>Elefan GA (Genetic Algorithm)</b></h3></span>"
    text
  })
}