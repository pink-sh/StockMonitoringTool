elefanGaModule <- function(input, output, session) {

    ns <- session$ns

    ## Definition of reactive values
    ## ----------------------------

    elefan_ga <- reactiveValues(
        dataExplo = NULL,
        results = NULL
    )

    elefanGaUploadVreResult <- reactiveValues()

    inputElefanGaData <- reactiveValues()

    fileGaState <- reactiveValues(
        upload = NULL
    )

    ## Definition of functions
    ## ----------------------------

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
##        shinyjs::reset("ELEFAN_GA_PLUS_GROUP")
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


    ## Input-dependent UIs
    ## ----------------------------

    output$ELEFAN_years_selected_out <- renderUI({
        allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
        if(inherits(allyears,"try-error")) allyears = NULL
        selectInput(ns("ELEFAN_years_selected"), "",
                    choices = allyears, selected = allyears,
                    multiple = TRUE,
                    width = "100%")
    })

    output$ELEFAN_binSize_out <- renderUI({
        binSize <- try(min(diff(inputElefanGaData$data$midLengths)),silent=TRUE)
        maxL <- try(round(max(inputElefanGaData$data$midLengths)/4),silent=TRUE)
        if(inherits(binSize,"try-error")){
            binSize <- 2
            maxL <- 10
        }
        inputElefanGaData[['binSize2']] <- binSize
        numericInput(ns("ELEFAN_GA_binSize"), "",
                     binSize, min = binSize, max = maxL, step=0.5,
                     width ='100%')
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

    output$ELEFAN_binSize2_out <- renderUI({
        binSize <- try(min(diff(inputElefanGaData$data$midLengths)),silent=TRUE)
        maxL <- try(round(max(inputElefanGaData$data$midLengths)/4),silent=TRUE)
        binSize2 <- inputElefanGaData[['binSize2']]
        if(inherits(binSize,"try-error")){
            binSize2 <- binSize <- 2
            maxL <- 10
        }
        if(is.na(binSize2) || is.null(binSize2)) binSize2 <- binSize
        numericInput(ns("ELEFAN_GA_binSize2"), "",
                     binSize, min = binSize2, max = maxL, step=0.5,
                     width ='80%')
    })

    output$ELEFAN_years_selected_cc_out <- renderUI({
        allyears <- try(unique(format(inputElefanGaData$data$dates,"%Y")),silent=TRUE)
        if(inherits(allyears,"try-error")) allyears = NULL
        selectInput(ns("ELEFAN_years_selected_cc"), "",
                    choices = allyears, selected = allyears,
                    multiple = TRUE,
                    width = "80%")
    })


    ## Interactive UIs & Reactive values
    ## ----------------------------

    observe({
        if(!input$ELEFAN_GA_seasonalised){
            js$removeBox2("box_elefan_ga_seasonPar")
        }else{
            js$showBox2("box_elefan_ga_seasonPar")
        }
    })

    observe({
        if(input$natM == "Then's growth formula"){
            shinyjs::hide("ui_natM_pauly", asis = TRUE)
            shinyjs::hide("ui_natM_then_tmax", asis = TRUE)
        }else if(input$natM == "Then's max. age formula"){
            shinyjs::show("ui_natM_then_tmax", asis = TRUE)
            shinyjs::hide("ui_natM_pauly", asis = TRUE)
        }else if(input$natM == "Pauly's growth & temp. formula"){
            shinyjs::show("ui_natM_pauly", asis = TRUE)
            shinyjs::hide("ui_natM_then_tmax", asis = TRUE)
        }
    })

    observe({
        if(input$select == "Estimate"){
            shinyjs::hide("ui_l50", asis = TRUE)
            shinyjs::hide("ui_l75", asis = TRUE)
            shinyjs::hide("ui_wqs", asis = TRUE)
            shinyjs::hide("ui_lcMin", asis=TRUE)
            shinyjs::hide("ui_lcMax", asis=TRUE)
        }else if(input$select == "Define L50 & L75"){
            shinyjs::show("ui_l50", asis = TRUE)
            shinyjs::hide("ui_wqs", asis = TRUE)
            shinyjs::show("ui_l75", asis = TRUE)
            shinyjs::show("ui_lcMin", asis=TRUE)
            shinyjs::show("ui_lcMax", asis=TRUE)
        }else if(input$select == "Define L50 & (L75-L25)"){
            shinyjs::show("ui_l50", asis = TRUE)
            shinyjs::hide("ui_l75", asis = TRUE)
            shinyjs::show("ui_wqs", asis = TRUE)
            shinyjs::show("ui_lcMin", asis=TRUE)
            shinyjs::show("ui_lcMax", asis=TRUE)
        }
    })

    observeEvent(input$fileGa, {
        fileGaState$upload <- 'uploaded'
        inputElefanGaData$data <- elefanGaFileData()
    })

    observeEvent(input$elefanGaDateFormat, {
        inputElefanGaData$data <- elefanGaFileData()
    })

    observeEvent(input$ELEFAN_GA_binSize,{
        inputElefanGaData[['binSize2']] <- input$ELEFAN_GA_binSize
    })

    observeEvent(input$ELEFAN_GA_binSize2,{
        inputElefanGaData[['binSize2']] <- input$ELEFAN_GA_binSize2
    })




    ## Action buttons
    ## ----------------------------

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
##                                 plus_group = input$ELEFAN_GA_PLUS_GROUP,
                                 years = input$ELEFAN_years_selected,
                                 agg = input$ELEFAN_agg,
                                 binSizeCC = inputElefanGaData[['binSize2']],
                                 yearsCC = yearsCC,
                                 LWa = input$LWa,
                                 LWb = input$LWb,
                                 natM_method = input$natM,
                                 temp = input$temp,
                                 cor_schooling = input$schooling,
                                 tmax = input$tmax,
                                 select_method = input$select,
                                 l50_user = input$l50_user,
                                 l75_user = input$l75_user,
                                 wqs_user = input$wqs_user,
                                 fRangeSteps = input$fRangeSteps,
                                 fRangeMin = input$fRangeMin,
                                 fRangeMax = input$fRangeMax,
                                 lcRangeSteps = input$lcRangeSteps,
                                 lcRangeMin = input$lcRangeMin,
                                 lcRangeMax = input$lcRangeMax,
                                 Lm50 = input$Lm50,
                                 Lm75 = input$Lm75
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
                js$collapseBox("box_datupload")
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
        elefan_ga$dataExplo[['lfqbin']] <- elefanGaDataExplo2()
        par(mfrow = c(2,1), mar = c(1,4,0,1), oma = c(3,1,1,0))
        plot(elefan_ga$dataExplo$lfq, Fname = "catch", date.axis = "")
        legend("topleft",legend=as.expression(bquote(bold("A"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg = "white")
        plot(elefan_ga$dataExplo$lfqbin, Fname = "rcounts", date.axis = "modern")
        legend("topleft",legend=as.expression(bquote(bold("B"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg = "white")
    })
    output$title_explo1 <- renderText({
        req(inputElefanGaData$data, input$ELEFAN_years_selected)
        txt <- "<p class=\"pheader_elefan\">Fig 1: Uploaded raw (A) and restructured (B) length-frequency data.</p>"
        txt
    })


    ## Growth plot
    ## --------------------------
    output$plot_growthCurves <- renderPlot({
        req(elefan_ga$results)
        par(mfrow = c(2,1), mar = c(1,4,0,1), oma = c(3,1,1,0))
        plot(elefan_ga$dataExplo$lfqbin, Fname = "catch", date.axis = "")
        lt <- lfqFitCurves(lfq = elefan_ga$dataExplo$lfqbin,
                           par=as.list(elefan_ga$results$resGA$par),
                           draw = TRUE, lty = 1, col = "dodgerblue2", lwd=2.5)
        legend("topleft",legend=as.expression(bquote(bold("A"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg = "white")
        plot(elefan_ga$dataExplo$lfqbin, Fname = "rcounts")
        lt <- lfqFitCurves(lfq = elefan_ga$dataExplo$lfqbin,
                           par=as.list(elefan_ga$results$resGA$par),
                           draw = TRUE, lty = 1, col = "dodgerblue2", lwd=2.5)
        legend("topleft",legend=as.expression(bquote(bold("B"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")

    })
    output$title_growthCurves <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Fig 2: Uploaded raw (A) and restructured (B) length-frequency data with von Bertalanffy growth curves fitted by ELEFAN.</p>"
        txt
    })

    ## ELEFAN fit plot
    ## --------------------------
    output$plot_elefanFit <- renderPlot({
        req(elefan_ga$results)
        par(mar=c(5,5,2,1))
        GA::plot(elefan_ga$results$resGA$gafit)
    })
    output$title_elefanFit <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Fig 3: ELEFAN genetic algorithm search path.</p>"
        txt
    })

    ## Catch curve plot
    ## --------------------------
    output$plot_catchCurve <- renderPlot({
        req(elefan_ga$results)
        resCC <- elefan_ga$results$resCC
        ind <- resCC$reg_int[1]:resCC$reg_int[2]
        par(mar=c(5,5,2,1))
        plot(resCC$t_midL[-ind], resCC$lnC_dt[-ind],
             xlab = "Relative age [years]", ylab = "ln(C/dt)",
             cex=1.4)
        points(resCC$t_midL[ind], resCC$lnC_dt[ind],
               col = "dodgerblue2", pch = 16, cex=1.4)
        abline(resCC$linear_mod, lwd=2.5, col = "dodgerblue2")
        box()
    })
    output$title_catchCurve <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Fig 4: Catch curve.</p>"
        txt
    })

    ## Selectivity plot
    ## --------------------------
    output$plot_select <- renderPlot({
        req(elefan_ga$results)
        L50 <- elefan_ga$results$L50
        L75 <- elefan_ga$results$L75
        slist <- list(selecType = "trawl_ogive",
                      L50 = L50, L75 = L75)
        lt <- seq(0, 1.5 * max(elefan_ga$results$lfqbin$midLengths), 0.01)
        sest <- TropFishR::select_ogive(slist, Lt = lt)
        par(mar=c(5,5,2,1))
        plot(lt, sest, ty='n', lwd=2,
             xlab = "Length", ylab = "Probability of capture")
        tmp <- TropFishR::select_ogive(slist, Lt = L50)
        segments(L50, -1, L50, tmp, lty = 2, lwd=1.5, col="grey60")
        segments(-10, tmp, L50, tmp, lty = 2, lwd=1.5, col="grey60")
        tmp <- TropFishR::select_ogive(slist, Lt = L75)
        segments(L75, -1, L75, tmp, lty = 3, lwd=1.5, col="grey60")
        segments(-10, tmp, L75, tmp, lty = 3, lwd=1.5, col="grey60")
        lines(lt, sest, lwd=2.5, col="dodgerblue2")
        legend("bottomright", legend = c("Selection ogive","L50","L75"),
               lty = c(1,2,3), col=c("dodgerblue2","grey60","grey60"),
               lwd=c(2,1.5,1.5))
        box()
    })
    output$title_select <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Fig 5: Probability of capture at length.</p>"
        txt
    })

    ## YPR plot
    ## --------------------------
    output$plot_ypr <- renderPlot({
        req(elefan_ga$results)

        resYPR <- elefan_ga$results$resYPR1
        refs <- as.numeric(resYPR$df_Es)

        if(all(is.na(resYPR$SPR)) || is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){

            par(mfrow=c(2,1), mar=c(1,4,0,2), oma=c(4,1,1,0))
            ## YPR
            plot(resYPR$FM_change, resYPR$totY, ty='n',
                 ylim = c(0,1.25) * range(resYPR$totY),
                 xaxt = "n",
                 xlab = "", ylab = "")
            tmp <- resYPR$totY[which.min(abs(resYPR$FM_change-refs[1]))]
            segments(refs[1], -10, refs[1], tmp,
                     lty=2, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[1], tmp,
                     lty=2, lwd=1.5, col="grey60")
            tmp <- resYPR$totY[which.min(abs(resYPR$FM_change-refs[2]))]
            segments(refs[2], -10, refs[2], tmp,
                     lty=3, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[2], tmp,
                     lty=3, lwd=1.5, col="grey60")
            lines(resYPR$FM_change, resYPR$totY, lwd=2.5,
                  col="dodgerblue2")
            legend("topleft",legend=as.expression(bquote(bold("A"))),
                   x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
            legend("topright",legend=c("Fmax","F0.1"), lty = c(2,3), cex=1.1, bg= "white")
            mtext("Yield per recruit", 2, 3.5)
            ## BPR
            plot(resYPR$FM_change, resYPR$meanB, ty='n',
                 ylim = c(0,1.1) * range(resYPR$meanB),
                 xlab = "", ylab = "")
            tmp <- resYPR$meanB[which.min(abs(resYPR$FM_change-refs[3]))]
            segments(refs[3], -10, refs[3], tmp,
                     lty=3, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[3], tmp,
                     lty=3, lwd=1.5, col="grey60")
            lines(resYPR$FM_change, resYPR$meanB, lwd=2.5,
                  col="dodgerblue2")
            legend("topleft",legend=as.expression(bquote(bold("B"))),
                   x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
            legend("topright",legend=c("F0.5"), lty = c(2), cex=1.1, bg= "white")
            mtext("Biomass per recruit", 2, 3.5)
            mtext("Fishing mortality", 1, 3)
            box()


        }else{

            par(mfrow=c(3,1), mar=c(1,4,0,2), oma=c(4,1,1,0))
            ## YPR
            plot(resYPR$FM_change, resYPR$totY, ty='n',
                 ylim = c(0,1.25) * range(resYPR$totY),
                 xaxt = "n",
                 xlab = "", ylab = "")
            tmp <- resYPR$totY[which.min(abs(resYPR$FM_change-refs[1]))]
            segments(refs[1], -10, refs[1], tmp,
                     lty=2, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[1], tmp,
                     lty=2, lwd=1.5, col="grey60")
            tmp <- resYPR$totY[which.min(abs(resYPR$FM_change-refs[2]))]
            segments(refs[2], -10, refs[2], tmp,
                     lty=3, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[2], tmp,
                     lty=3, lwd=1.5, col="grey60")
            lines(resYPR$FM_change, resYPR$totY, lwd=2.5,
                  col="dodgerblue2")
            legend("topleft",legend=as.expression(bquote(bold("A"))),
                   x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
            legend("topright",legend=c("Fmax","F0.1"), lty = c(2,3), cex=1.1, bg= "white")
            mtext("Yield per recruit", 2, 3.5)
            ## BPR
            plot(resYPR$FM_change, resYPR$meanB, ty='n',
                 ylim = c(0,1.1) * range(resYPR$meanB),
                 xaxt = "n",
                 xlab = "", ylab = "")
            tmp <- resYPR$meanB[which.min(abs(resYPR$FM_change-refs[3]))]
            segments(refs[3], -10, refs[3], tmp,
                     lty=2, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[3], tmp,
                     lty=2, lwd=1.5, col="grey60")
            lines(resYPR$FM_change, resYPR$meanB, lwd=2.5,
                  col="dodgerblue2")
            legend("topleft",legend=as.expression(bquote(bold("B"))),
                   x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
            legend("topright",legend=c("F0.5"), lty = c(2), cex=1.1, bg= "white")
            mtext("Biomass per recruit", 2, 3.5)
            ## SPR
            plot(resYPR$FM_change, resYPR$SPR, ty='n',
                 ylim = c(0,1.1) * range(resYPR$SPR),
                 xlab = "", ylab = "")
            tmp <- resYPR$SPR[which.min(abs(resYPR$FM_change-refs[4]))]
            segments(refs[4], -10, refs[4], tmp,
                     lty=2, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[4], tmp,
                     lty=2, lwd=1.5, col="grey60")
            tmp <- resYPR$SPR[which.min(abs(resYPR$FM_change-refs[5]))]
            segments(refs[5], -10, refs[5], tmp,
                     lty=3, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[5], tmp,
                     lty=3, lwd=1.5, col="grey60")
            tmp <- resYPR$SPR[which.min(abs(resYPR$FM_change-refs[6]))]
            segments(refs[6], -10, refs[6], tmp,
                     lty=4, lwd=1.5, col="grey60")
            segments(-10, tmp, refs[6], tmp,
                     lty=4, lwd=1.5, col="grey60")
            lines(resYPR$FM_change, resYPR$SPR, lwd=2.5,
                  col="dodgerblue2")
            legend("topleft",legend=as.expression(bquote(bold("C"))),
                   x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg= "white")
            legend("topright",legend=c("F30","F35","F40"),
                   lty = c(2,3,4), cex=1.1, bg= "white")
            mtext("Spawning potential ratio", 2, 3.5)
            mtext("Fishing mortality", 1, 3)
            box()
        }
    })
    output$title_ypr <- renderText({
        req(elefan_ga$results)
        if(is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){
            txt <- "<p class=\"pheader_elefan\">Fig 6: Yield (A) and biomass (B) per recruit for a range of fishing mortality rates.</p>"
        }else{
            txt <- "<p class=\"pheader_elefan\">Fig 6: Yield (A) and biomass (B) per recruit as well as spawning potential ratio (C) for a range of fishing mortality rates.</p>"
        }
        txt
    })

    ## YPR-Isopleth plot
    ## --------------------------
    output$plot_ypr_iso <- renderPlot({
        req(elefan_ga$results)
        par(mfrow = c(2,1), mar = c(4,4,0,1), oma = c(2,0,1,0))
        plot(elefan_ga$results$resYPR2, type = "Isopleth",
             xaxis1 = "FM", mark = TRUE, contour = 6, xlab="")
        legend("topleft",legend=as.expression(bquote(bold("A"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3, bg = "white")
        plot(elefan_ga$results$resYPR2, type = "Isopleth",
             xaxis1 = "FM", yaxis1 = "B_R", mark = TRUE,
             contour = 6, xlab = "Fishing mortality")
        legend("topleft",legend=as.expression(bquote(bold("B"))),
               x.intersp = -0.3, y.intersp = 0.3, cex=1.3,
               bg = "white")

    })
    output$title_ypr_iso <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Fig 7: Yield (A) and biomass (B) per reruit for a range fishing mortality rates and length at 50% selectivity (L50) values.</p>"
        txt
    })


    ## Growth parameter table
    ## --------------------------
    output$table_growth <- renderTable({
        req(elefan_ga$results)
        tmp <- as.data.frame(c(elefan_ga$results$resGA$par,
                               list(Rn_max = elefan_ga$results$resGA$Rn_max)))
        names(tmp) <- replace(names(tmp), names(tmp)=="t_anchor", "ta")
        names(tmp) <- replace(names(tmp), names(tmp)=="Rn_max", "Rn")
        names(tmp) <- replace(names(tmp), names(tmp)=="phiL", "phi'")
        tmp
    })
    output$title_table_growth <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Table 1: Estimated growth parameters.</p>"
        txt
    })


    ## Mortality rates
    ## --------------------------
    output$table_mort <- renderTable({
        req(elefan_ga$results)
        Z <- elefan_ga$results$resCC$Z
        M <- elefan_ga$results$resM
        FM <- Z - M
        E <- FM / Z
        tmp <- as.data.frame(t(as.matrix(c(Z, M, FM, E,
                 elefan_ga$results$L50,
                 elefan_ga$results$L75))))
        names(tmp) <- c("Z","M","F","E","L50","L75")
        tmp
    })
    output$title_table_mort <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Table 2: Estimated mortality rates and (estimated/provided) selectivity parameters.</p>"
        txt
    })

    ## Reference points
    ## --------------------------
    output$table_refs <- renderTable({
        req(elefan_ga$results)
        if(is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){
            tmp <- elefan_ga$results$resYPR1$df_Es[1:3]
            names(tmp) <- c("Fmax","F0.1","F0.5")
        }else{
            tmp <- elefan_ga$results$resYPR1$df_Es
            names(tmp) <- c("Fmax","F0.1","F0.5","F30","F35","F40")
        }
        tmp
    })
    output$title_table_refs <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Table 3: Estimated reference points (SPR-based reference points only estimated if maturity parameters provided).</p>"
        txt
    })


    ## Stock status table
    ## --------------------------
    output$table_stockstatus <- renderTable({
        req(elefan_ga$results)
        Z <- elefan_ga$results$resCC$Z
        M <- elefan_ga$results$resM
        FM <- Z - M
        if(is.null(elefan_ga$results$Lm50) || is.null(elefan_ga$results$Lm75)){
            tmp <- cbind(FM/elefan_ga$results$resYPR1$df_Es[1:3])
            names(tmp) <- c("F/Fmax","F/F0.1","F/F0.5")
        }else{
            tmp <- cbind(FM/elefan_ga$results$resYPR1$df_Es,
                         elefan_ga$results$resYPR1$currents$curr.SPR)
            names(tmp) <- c("F/Fmax","F/F0.1","F/F0.5","F/F30","F/F35","F/F40","SPR")
        }
        tmp
    })
    output$title_table_stockstatus <- renderText({
        req(elefan_ga$results)
        txt <- "<p class=\"pheader_elefan\">Table 4: Estimated stock status (SPR-based stock status only estimated if maturity parameters provided).</p>"
        txt
    })



    output$downloadReport_ga <- renderUI({
        req(elefan_ga$results)
        downloadButton(session$ns('createElefanGAReport'), 'Download Report')
    })


    output$ElefanGaVREUpload <- renderText({
        text <- ""
        req(elefan_ga$results)
            if (!is.null(session$userData$sessionMode()) && session$userData$sessionMode() == "GCUBE") {
                if (isTRUE(elefanGaUploadVreResult$res)) {
                    text <- paste0(text, VREUploadText)
                }
            }
        text
    })

    output$createElefanGAReport <- downloadHandler(
        filename = paste("ElefanGA_report_",format(Sys.time(), "%Y%m%d_%H%M_%s"),".pdf",sep=""),
        content = function(file) {
            createElefanGaPDFReport(file, elefan_ga, input)
        }
    )

    output$tbl1_ga <- renderTable({
        req(elefan_ga$results)
        elefan_ga$results$resYPR1$df_Es
    },
    include.rownames=TRUE)

    output$tbl2_ga <- renderTable({
        req(elefan_ga$results)
            CURR_GA<-elefan_ga$results$resYPR1$currents
            CURR_GA<-CURR_GA[,-7]
            names(CURR_GA)<-c("Length-at-1st-capture (Lc)", "Age-at-1st-capture (tc)", "Effort","Fishing mortality", "Catch", "Yield", "Biomass")
            CURR_GA
    },
    include.rownames=TRUE)



    output$elefanGADataConsiderationsText <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getDataConsiderationTextForElefan())
        text
    })

    output$methodConsiderationsText <- renderText({
        text <- gsub("%%ELEFAN%%", "ELEFAN_GA", getMethodConsiderationTextForElefan())
        text
    })



    output$rnMax_ga <- renderText({
        if(req(elefan_ga$results)){
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
