elefanGaModule <- function(input, output, session) {

    ns <- session$ns

    elefan_ga <- reactiveValues(
        dataExplo = NULL,
        results = NULL
    )

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
        if (is.null(contents$catch)) {
            shinyjs::disable("go_ga")
            showModal(modalDialog(
                title = "Error",
                if(!is.null(contents$checkDelim)){
                    if(contents$checkDelim=="not ok"){"Please ensure that your .csv file delimiter is a comma ','"  }
                }else if(!is.null(contents$checkDec)){
                    if(contents$checkDec=="not point"){"Please ensure your separate decimals using points ‘.’ or you don't have non numeric value"
                    }else if(contents$checkName=="colname error"){"Please ensure your first column name is : 'midLength'"
                    } else{"Input file seems invalid"}},
                easyClose = TRUE,
                footer = NULL
            ))
            return (NULL)
        } else {
            if(is.Date(contents$dates)&&is.unsorted(contents$dates)){
                shinyjs::disable("go_ga")
                showModal(modalDialog(
                    title = "Error",
                    "Please ensure that your dates are input in chronological order from left to right.  If dates are in the right order select the date format coresponding to your file.",
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

    elefanGaDataExplo1 <- reactive({
        req(inputElefanGaData$data)

        elefan_ga$dataExplo <- list()
        years <- input$ELEFAN_years_selected
        if(is.null(years)){
            shinyjs::disable("go_ga")
            showModal(modalDialog(
                title = "Error",
                "No year selected for the analysis. Please select at least one year of uploaded data set.",
                easyClose = TRUE,
                footer = NULL
            ))
            return(NULL)
        }
        agg <- input$ELEFAN_agg
        if(agg == "none") agg <- NA
        dat <- inputElefanGaData$data
        class(dat) <- "lfq"
        lfq <- lfqModify(dat,
                         bin_size = input$ELEFAN_GA_binSize,
                         years = years,
##                         plus_group = input$ELEFAN_GA_PLUS_GROUP,
                         aggregate = agg)
        return(lfq)
    })

    elefanGaDataExplo2 <- reactive({
        req(elefan_ga$dataExplo$lfq)

        lfqbin <- lfqRestructure(elefan_ga$dataExplo$lfq,
                                 MA = input$ELEFAN_GA_MA,
                                 addl.sqrt = input$ELEFAN_GA_addlsqrt)
        return(lfqbin)
    })

    resetElefanGaInputValues <- function() {
        shinyjs::reset("fileGa")
        shinyjs::reset("ELEFAN_GA_seasonalised")
        shinyjs::reset("ELEFAN_GA_binSize")
        shinyjs::reset("ELEFAN_GA_popSize")
        shinyjs::reset("ELEFAN_GA_maxiter")
        shinyjs::reset("ELEFAN_GA_run")
        shinyjs::reset("ELEFAN_GA_addl.sqrt")
        shinyjs::reset("ELEFAN_GA_pmutation")
        shinyjs::reset("ELEFAN_GA_pcrossover")
        shinyjs::reset("ELEFAN_GA_elitism")
        shinyjs::reset("ELEFAN_GA_MA")
        shinyjs::reset("ELEFAN_GA_PLUS_GROUP")
        shinyjs::reset("ELEFAN_GA_Linf")
        shinyjs::reset("ELEFAN_GA_K")
        shinyjs::reset("ELEFAN_GA_t_anchor")
        shinyjs::reset("ELEFAN_GA_C")
        shinyjs::reset("ELEFAN_GA_ts")

        shinyjs::disable("go_ga")
        ## clearResults("box_results")
        ## clearResults("box_exploPlots")
        shinyjs::reset("elefanGaDateFormat")

        elefan_ga$dataExplo <- NULL
        elefan_ga$results <- NULL
        inputElefanGaData$data <- NULL
        fileGaState$upload <- NULL
    }


    ## UIs
    ## ----------------------------

    output$ELEFAN_years_selected_out <- renderUI({
        allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
        if(inherits(allyears,"try-error")) allyears = NULL
        selectInput(ns("ELEFAN_years_selected"), "",
                    choices = allyears, selected = allyears,
                    multiple = TRUE,
                    width = "50%")
    })

    output$ELEFAN_binSize_out <- renderUI({
        binSize <- try(min(diff(inputElefanGaData$data$midLengths)),silent=TRUE)
        maxL <- try(round(max(inputElefanGaData$data$midLengths)/4),silent=TRUE)
        if(inherits(binSize,"try-error")){
            binSize <- 2
            maxL <- 10
        }
        numericInput(ns("ELEFAN_GA_binSize"), "",
                     binSize, min = binSize, max = maxL, step=0.5,
                     width ='50%')
    })

    output$ELEFAN_GA_Linf_out <- renderUI({
        maxL <- try(round(max(inputElefanGaData$data$midLengths)/0.95),silent=TRUE)
        if(inherits(maxL,"try-error")){
            maxL <- 100
        }
        min <- 0.5 * maxL
        max <- 1.5 * maxL
        sel <- c(0.8,1.2) * maxL
        sliderInput(ns("ELEFAN_GA_Linf"),"",
                    value=sel, min = min, max = max, step=1)
    })

    output$ELEFAN_years_selected_cc_out <- renderUI({
        allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
        if(inherits(allyears,"try-error")) allyears = NULL
        selectInput(ns("ELEFAN_years_selected_cc"), "",
                    choices = allyears, selected = allyears,
                    multiple = TRUE,
                    width = "30%")
    })



    observe({
        if(input$natM == "Then_growth"){
            js$removeBox2("box_natM_pauly")
            js$removeBox2("box_natM_then_tmax")
        }else if(input$natM == "Then_tmax"){
            js$showBox2("box_natM_then_tmax")
            js$removeBox2("box_natM_pauly")
        }else if(input$natM == "Pauly_Linf"){
            js$showBox2("box_natM_pauly")
            js$removeBox2("box_natM_then_tmax")
        }
    })


    observe({
        if(!input$ELEFAN_GA_seasonalised){
            js$removeBox2("box_elefan_ga_seasonPar")
        }else{
            js$showBox2("box_elefan_ga_seasonPar")
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
        js$disableAllButtons()

        result = tryCatch({

            ## needed because of renderUI in hidden tabs
            if(is.null(input$ELEFAN_GA_Linf)){
                linf <- c(0.8,1.2) * round(max(inputElefanGaData$data$midLengths)/0.95)
            }else{
                linf <- range(input$ELEFAN_GA_Linf)
            }

            if(is.null(input$yearsCC)){
                yearsCC <- unique(format(inputElefanGaData$data$dates,"%Y"))
            }else{
                yearsCC <- input$ELEFAN_years_selected_cc
            }


            flog.info("Starting Elegan GA computation")
            res <- run_elefan_ga(x = inputElefanGaData$data,
                                 binSize =  input$ELEFAN_GA_binSize,
                                 seasonalised = input$ELEFAN_GA_seasonalised,
                                 low_par = list(Linf = linf[1], K = min(input$ELEFAN_GA_K),
                                                t_anchor = min(input$ELEFAN_GA_t_anchor), C = min(input$ELEFAN_GA_C),
                                                ts = min(input$ELEFAN_GA_ts)),
                                 up_par = list(Linf = linf[2], K = max(input$ELEFAN_GA_K),
                                               t_anchor = max(input$ELEFAN_GA_t_anchor), C = max(input$ELEFAN_GA_C),
                                               ts = max(input$ELEFAN_GA_ts)),
                                 popSize = input$ELEFAN_GA_popSize, maxiter = input$ELEFAN_GA_maxiter,
                                 run = input$ELEFAN_GA_run, pmutation = input$ELEFAN_GA_pmutation,
                                 pcrossover = input$ELEFAN_GA_pcrossover,
                                 elitism = input$ELEFAN_GA_elitism,
                                 MA = input$ELEFAN_GA_MA,
                                 addl.sqrt = input$ELEFAN_GA_addlsqrt,
                                 plus_group = input$ELEFAN_GA_PLUS_GROUP,
                                 years = input$ELEFAN_years_selected,
                                 agg = input$ELEFAN_agg,
                                 method_natM = input$natM,
                                 schooling = input$schooling,
                                 tmax = input$tmax,
                                 temp = input$temp,
                                 binSizeCC = input$ELEFAN_GA_binSize2,
                                 yearsCC = yearsCC,
                                 LWa = input$LWa,
                                 LWb = input$LWb,
                                 stockSize = input$stockSize,
                                 fmchangeRange = input$fmchangeRange,
                                 fmLengthOut = input$fmLengthOut,
                                 lcLengthOut = input$lcLengthOut
                                 )

            js$hideComputing()
            js$enableAllButtons()


            if ('error' %in% names(res)) {
                showModal(modalDialog(
                    title = "Error",

                    if(!is.null(res$error)){
                        if (!is.null(grep("MA must be an odd integer",res$error))) {
                            HTML(sprintf("Incorrect moving average (MA) value! Please provide an odd integer (e.g. 3,5,7) and run again.<hr/> <b>%s</b>",
                                         res$error))
                        }else if (!is.null(grep("specified bin_size is smaller than",res$error))) {
                            HTML(sprintf(paste0("The specified bin size is smaller than the resolution in uploaded data! Please set bin size equal to ",min(diff(inputElefanGaData$data$midLengths))," or higher and run again.<hr/> <b>%s</b>"),
                                         res$error))
                        }else if(grep("POSIXlt",res$error)==1) {
                            HTML(sprintf("The date could not be recognized! Please check that the chosen date format matches the date format in your data file.<hr/> <b>%s</b>", res$error))
                        }else{
                            res$error
                        }},
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
                ##          js$showBox("box_elefan_ga_results")
                js$collapseBox("box_datupload")
                js$expandBox("box_results")
                elefan_ga$results <- res
                session$userData$fishingMortality$FcurrGA <-
                    round(elefan_ga$results$resYPR1$currents[4]$curr.F, 2)

                if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode()=="GCUBE") {
                    flog.info("Uploading Elefan GA report to i-Marine workspace")
                    reportFileName <- paste(tempdir(),"/","ElefanGA_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep="")
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


    ## Data exploration plots
    ## --------------------------
    output$plot_explo1 <- renderPlot({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        elefan_ga$dataExplo[['lfq']] <- elefanGaDataExplo1()
        par(mar = c(4,4,1,2))
        plot(elefan_ga$dataExplo$lfq, Fname = "catch", date.axis = "modern")
    })
    output$title_explo1 <- renderText({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        txt <- "<p class=\"pheader_elefan\">Fig 1: Length-frequency data.</p>"
        txt
    })
    output$plot_explo2 <- renderPlot({
        req(inputElefanGaData$data, elefan_ga$dataExplo$lfq, input$ELEFAN_years_selected)
        elefan_ga$dataExplo[['lfqbin']] <- elefanGaDataExplo2()
        par(mar = c(4,4,1,2))
        plot(elefan_ga$dataExplo$lfqbin, Fname = "rcounts", date.axis = "modern")
    })
    output$title_explo2 <- renderText({
        req(inputElefanGaData$data, elefan_ga$dataExplo$lfq, input$ELEFAN_years_selected)
        txt <- "<p class=\"pheader_elefan\">Fig 2: Restructured length-frequency data.</p>"
        txt
    })


    ## Growth plots
    ## --------------------------
    output$plot_ga_3 <- renderPlot({
        req(elefan_ga$results)
        plot(elefan_ga$results$resYPR1, mark = TRUE)
##            mtext("(a)", side = 3, at = -1, line = 0.6)
    })
    output$plot_ga_4 <- renderPlot({
        req(elefan_ga$results)
        plot(elefan_ga$results$resYPR2, type = "Isopleth",
             xaxis1 = "FM", mark = TRUE, contour = 6)
##            mtext("(b)", side = 3, at = -0.1, line = 0.6)

    })
    output$plot_ga_5 <- renderPlot({
        req(elefan_ga$results)
        plot(elefan_ga$results$resGA)
    })
    output$par_ga <- renderText({
        if ("results" %in% names(elefan_ga)) {
            title <- "<hr>"
            title <- paste0(title, "<strong>Length infinity (", withMathJax("\\(L_\\infty\\)"), "in cm):</strong>&nbsp;", round(elefan_ga$results$resGA$par$Linf, 2))
            title <- paste0(title, "<br/>")
            title <- paste0(title, "<strong>Curving coefficient (K):</strong>&nbsp;", round(elefan_ga$results$resGA$par$K, 2))
            title <- paste0(title, "<br/>")
            title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corresponds to peak spawning month (t_anchor):</strong>&nbsp;", round(elefan_ga$results$resGA$par$t_anchor, 2))
            title <- paste0(title, "<br/>")
            title <- paste0(title, "<strong>Amplitude of growth oscillation (NOTE: only if 'Seasonalized' is checked; C):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$resGA$par$C), NA, round(elefan_ga$results$resGA$par$C, 2)))
            title <- paste0(title, "<br/>")
            title <- paste0(title, "<strong>Winter point of oscillation (</strong>&nbsp;", withMathJax("\\(t_w\\)") , "<strong>)</strong>&nbsp;")
            title <- paste0(title, "<br/>")
            title <- paste0(title, "<strong>Summer point of oscillation (NOTE: only if 'Seasonalized' is checked; ", withMathJax("\\(ts\\)"),"=", withMathJax("\\(t_w\\)"), "- 0.5):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$resGA$par$ts), NA, round(elefan_ga$results$resGA$par$ts, 2)))
            title <- paste0(title, "<br/>")
            title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", ifelse(is.na(elefan_ga$results$resGA$par$phiL), "--", round(elefan_ga$results$resGA$par$phiL, 2)))
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
            elefan_ga$results$resYPR1$df_Es
        }
    },
    include.rownames=TRUE)
    output$tbl2_ga <- renderTable({
        if ('results' %in% names(elefan_ga)) {
            CURR_GA<-elefan_ga$results$resYPR1$currents
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

    output$methodConsiderationsText <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getMethodConsiderationTextForElefan())
        text
    })



    output$rnMax_ga <- renderText({
        if ("results" %in% names(elefan_ga)) {
            title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", round(elefan_ga$results$resGA$Rn_max, 3))
            title
        } else {  "" }
    })

    output$elefanGaTitle <- renderText({
        session$userData$page("elefan-ga")
        text <- "<span><h3><b>Length-based stock assessment with TropFishR</b></h3></span>"
        text
    })


}
