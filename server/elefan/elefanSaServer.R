elefanSaModule <- function(input, output, session) {
  
  elefan_sa <- reactiveValues()
  elefanSaUploadVreResult <- reactiveValues()

  observeEvent(input$go_sa, {
    infile <- input$fileSa
    
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
    js$removeBox("box_elefan_ga_results")
    js$disableAllButtons()
    dataset <- read_elefan_csv(inputCsvFile)
    #ds1 <- lfqModify(lfqRestructure(dataset), bin_size = 4)
    
    #ds2 <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    result = tryCatch({ 
      res <- run_elefan_sa(dataset,binSize =  4, seasonalised = input$ELEFAN_SA_seasonalised, 
                           init_par = list(Linf = input$ELEFAN_SA_initPar_Linf, K = input$ELEFAN_SA_initPar_K, t_anchor = input$ELEFAN_SA_initPar_t_anchor),
                           low_par = list(Linf = as.numeric(input$ELEFAN_SA_lowPar_Linf), K = as.numeric(input$ELEFAN_SA_lowPar_K), t_anchor = as.numeric(input$ELEFAN_SA_lowPar_t_anchor), C = as.numeric(input$ELEFAN_SA_lowPar_C), ts = as.numeric(input$ELEFAN_SA_lowPar_ts)),
                           up_par = list(Linf = as.numeric(input$ELEFAN_SA_upPar_Linf), K = as.numeric(input$ELEFAN_SA_upPar_K), t_anchor = as.numeric(input$ELEFAN_SA_upPar_t_anchor), C = as.numeric(input$ELEFAN_SA_upPar_C), ts = as.numeric(input$ELEFAN_SA_upPar_ts)),
                           SA_time = input$ELEFAN_SA_SA_time, SA_temp = input$ELEFAN_SA_SA_temp, MA = input$ELEFAN_SA_MA, addl.sqrt = input$ELEFAN_SA_addl.sqrt,
                           agemax = input$ELEFAN_SA_agemax, plus_group = input$ELEFAN_SA_PLUS_GROUP)
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
        js$showBox("box_elefan_sa_results")
        elefan_sa$results <- res
        session$userData$fishingMortality$FcurrSA <- round(elefan_sa$results$plot3$currents[4]$curr.F, 2)
        
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
          flog.info("Uploading Elefan SA report to VRE")
          reportFileName <- paste("/tmp/","ElefanSA_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
          createElefanSaPDFReport(reportFileName,elefan_sa, input)
          elefanSaUploadVreResult$res <- FALSE
          tryCatch({
            if (fileFolderExistsInPath(session$userData$sessionUsername(),session$userData$sessionToken(),paste0("/Home/",session$userData$sessionUsername(),"/Workspace/"), uploadFolderName) == FALSE) {
              print("Creating folder")
              createFolderWs(
                session$userData$sessionUsername(), session$userData$sessionToken(),
                paste0("/Home/",sessionUsername(),"/Workspace/"),
                uploadFolderName, 
                uploadFolderDescription)
            }
            uploadToVREFolder(
              username = session$userData$sessionUsername(), 
              token = session$userData$sessionToken(), 
              relativePath = paste0("/Home/",session$userData$sessionUsername(),"/Workspace/", uploadFolderName, "/"), 
              file = reportFileName,
              overwrite = TRUE,
              archive = FALSE
            )
            elefanSaUploadVreResult$res <- TRUE
          }, error = function(err) {
            flog.error("Error uploading Elefan SA report to the Workspace: %s", err)
            elefanSaUploadVreResult$res <- FALSE
          }, finally = {})
        }
      }
    } , error = function(err) {
      flog.error("Error in Elefan SA: %s ",err)
      showModal(modalDialog(
        title = "Error",
        HTML(sprintf("General error running Elefan SA <hr/> <b>%s</b>", err)),
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
  
  observeEvent(input$reset_sa, {
    resetElefanSaInputValues()
  })
  
  output$tbl1_sa <- renderTable({
    if ('results' %in% names(elefan_sa)) {
      elefan_sa$results$plot3$df_Es
    }
  }, 
  include.rownames=TRUE)
  output$tbl2_sa <- renderTable({
    if ('results' %in% names(elefan_sa)) {
      CURR_SA<-elefan_sa$results$plot3$currents
      CURR_SA<-CURR_SA[,-7]
      names(CURR_SA)<-c("Length-at-1st-capture (Lc)", "Age-at-1st-capture (tc)", "Effort","Fishing mortality", "Catch", "Yield", "Biomass")
      CURR_SA
    }
  }, 
  include.rownames=TRUE)
  output$plot_sa_1 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot1, Fname = "catch", date.axis = "modern")
    }
  })
  output$plot_sa_2 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot2, Fname = "rcounts", date.axis = "modern")
    }
  })
  output$plot_sa_3 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot3, mark = TRUE)
      mtext("(a)", side = 3, at = -1, line = 0.6)
    }
  })
  output$plot_sa_4 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
      mtext("(b)", side = 3, at = -0.1, line = 0.6)
    }
  })
  output$plot_sa_5 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$data)
    }
  })
  output$par_sa <- renderText({
    if ("results" %in% names(elefan_sa)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity (", withMathJax("\\(L_\\infty\\)"), "in cm):</strong>&nbsp;", round(elefan_sa$results$data$par$Linf, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient (K):</strong>&nbsp;", round(elefan_sa$results$data$par$K, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor):</strong>&nbsp;", round(elefan_sa$results$data$par$t_anchor, 2))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation (NOTE: only if 'Seasonalized' is checked; C):</strong>&nbsp;", ifelse(is.na(elefan_sa$results$data$par$C), NA, round(elefan_sa$results$data$par$C, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Winter point of oscillation (</strong>&nbsp;", withMathJax("\\(t_w\\)") , "<strong>)</strong>&nbsp;")
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (NOTE: only if 'Seasonalized' is checked; ", withMathJax("\\(ts\\)"),"=", withMathJax("\\(t_w\\)"), "- 0.5):</strong>&nbsp;", ifelse(is.na(elefan_sa$results$data$par$ts), NA, round(elefan_sa$results$data$par$ts, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", ifelse(is.na(elefan_sa$results$data$par$phiL), NA, round(elefan_sa$results$data$par$phiL, 2)))
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<br>")
      title
    } else {  "" }
  })
  output$downloadReport_sa <- renderUI({
    if ("results" %in% names(elefan_sa)) {
      downloadButton(session$ns('createElefanSAReport'), 'Download Report')
    }
  })
  output$ElefanSaVREUpload <- renderText(
    {
      text <- ""
      if ("results" %in% names(elefan_sa)) {
        if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
          if (isTRUE(elefanSaUploadVreResult$res)) {
            text <- paste0(text, VREUploadText)
          }
        }
      }
      text
    }
  )
  output$title_tbl1_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Biological reference levels:</p>"
      txt
    }
  })
  output$title_tbl2_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Current levels:</p>"
      txt
    }
  })
  output$titlePlot1_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Restructured LFQ data</p>"
      txt
    }
  })
  output$titlePlot3_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
      txt
    }
  })
  output$titlePlot4_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
      txt
    }
  })
  output$titleResultsOfTheComputation_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<h2>Results of the ELEFAN_SA computation</h2>"
      txt
    }
  })
  output$elefanSADataConsiderationsText <- renderText({
    text <- gsub("%%ELEFAN%%", "ELEFAN_SA", getDataConsiderationTextForElefan())
    text
  })
  output$createElefanSAReport <- downloadHandler(
    filename = paste("ElefanSA_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
    content = function(file) {
      createElefanSaPDFReport(file, elefan_sa, input)
    }
  )
}