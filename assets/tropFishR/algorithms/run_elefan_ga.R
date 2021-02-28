library(TropFishR)


shinyMonitor <- function(object, digits = getOption("digits")){
    shiny::incProgress(amount = 1/object@maxiter,
                       detail = paste0("Iteration: ", object@iter,"/",object@maxiter))
}

run_elefan_ga <- function(
                          x,
                          binSize = NULL,
                          seasonalised = FALSE,
                          low_par = NULL,
                          up_par = NULL,
                          popSize = 50,
                          maxiter = 100,
                          run = maxiter,
                          parallel = FALSE,
                          pmutation = 0.1,
                          pcrossover = 0.8,
                          elitism = base::max(1, round(popSize*0.05)),
                          MA = 5,
                          addl.sqrt = FALSE,
                          agemax = NULL,
                          flagging.out = TRUE,
                          seed = 1,
                          plot = FALSE,
                          plot.score = TRUE,
                          plus_group = FALSE,
                          years = NA,
                          agg = "month",
                          binSizeCC = NULL,
                          yearsCC = NA,
                          LWa = 0.001,
                          LWb = 3,
                          natM_method = "Then_growth",
                          temp = 20,
                          cor_schooling = FALSE,
                          tmax = 10,
                          select_method = "est",
                          l50_user = NA,
                          l75_user = NA,
                          wqs_user = NA,
                          fRangeSteps = 100,
                          fRangeMin = 0,
                          fRangeMax = 3,
                          lcRangeSteps = 100,
                          lcRangeMin = NA,
                          lcRangeMax = NA,
                          Lm50 = NA,
                          Lm75 = NA,
                          ...
                          ) {
    set.seed(1)
    pdf(NULL)

    returnResults <- list()
    out <- tryCatch( {

        print(paste0("TropFishR version:", packageVersion("TropFishR")))


        ##--------------------
        ##  Length data
        ##--------------------
        if(agg == "none") agg <- NA
        class(x) <- "lfq"
        lfq <- lfqModify(x,  bin_size = binSize, years = years, aggregate = agg)
        lfqbin <- lfqRestructure(lfq, MA = MA, addl.sqrt = addl.sqrt)
        returnResults[['lfqbin']] <- lfqbin

        ##--------------------
        ##  ELEFAN
        ##--------------------
        withProgress(message = "Running ELEFAN", value = 0, {
            resGA <- ELEFAN_GA_temp(lfq, MA = MA, seasonalised = seasonalised,
                                    maxiter = maxiter, addl.sqrt = addl.sqrt,
                                    low_par=low_par, up_par=up_par,
                                    pmutation = pmutation, pcrossover = pcrossover,
                                    elitism = elitism, popSize = popSize,
                                    run = run,
                                    monitor=shinyMonitor)
        })
        Linf <- resGA$par$Linf
        K <- resGA$par$K
        returnResults[['resGA']] <- resGA

        ##--------------------
        ##  Natural mortality
        ##--------------------
        if(natM_method == "Then's growth formula"){
            natM <- "Then_growth"
        }else if(natM_method == "Pauly's growth & temp. formula"){
            natM <- "Pauly_Linf"
        }else if(natM_method == "Then's max. age formula"){
            natM <- "Then_tmax"
        }
        M <- as.numeric(M_empirical(Linf = Linf, K_l = K,
                                    method = natM, schooling = cor_schooling,
                                    tmax = tmax, temp = temp))
        returnResults[['resM']] <- M


        ##--------------------
        ##  Catch curve
        ##--------------------
        lfq <- lfqModify(x,
                         bin_size = binSizeCC,
                         years = yearsCC)
        lfq <- c(lfq, resGA$par)
        class(lfq) <- "lfq"

        ## account for est. Linf < Lmax
        if(any(lfq$midLengths > Linf)){
            lmax <- Linf
        }else lmax <- NA
        if(length(yearsCC) > 1){
            catch_columns <- 1:length(yearsCC)
        }else{
            catch_columns <- NA
        }
        ## summarise catch matrix into vector
        lfq2 <- lfqModify(lfq,
                          vectorise_catch = TRUE,
                          Lmax = lmax)
        ## catch curve with auto-fitting
        resCC <- catchCurve(lfq2, reg_int = NULL,
                            calc_ogive = TRUE,
                            catch_columns = catch_columns,
                            plot=FALSE, auto = TRUE)
        Z <- resCC$Z
        FM <- as.numeric(Z - M)
        E <- FM/Z
        L50 <- resCC$L50
        L75 <- resCC$L75
        returnResults[['resCC']] <- resCC


        ##--------------------
        ##  YPR/SPR
        ##--------------------
        ## length-weight relationship
        lfq2$a <- LWa
        lfq2$b <- LWb

        ## other parameters
        lfq2$Z <- Z
        lfq2$M <- M

        ## Selectivity
        if(select_method == "Define L50 & L75"){
            L50 <- l50_user
            L75 <- l75_user
        }else if(select_method == "Define L50 & (L75-L25)"){
            L50 <- l50_user
            L75 <- l50_user + wqs_user/2
        }
        slist <- list(selecType = "trawl_ogive",
                      L50 = L50, L75 = L75)
        sest <- TropFishR::select_ogive(slist, Lt = lfq2$midLengths)
        lfq2$FM <- FM * sest
        returnResults[['L50']] <- lfq2$L50 <- L50
        returnResults[['L75']] <- lfq2$L75 <- L75


        if(fRangeMax < fRangeMin) error("Maximum value of F range must be larger than minimum value!")
        fchange <- seq(fRangeMin, fRangeMax, length.out = fRangeSteps)

        if(select_method == "Estimate"){
            lcchange <- seq(0.1*L50, max(2*L50, 0.99*Linf), length.out = lcRangeSteps)
        }else{
            if(lcRangeMax < lcRangeMin) error("Maximum value of L50 range must be larger than minimum value!")
            lcchange <- seq(lcRangeMin, lcRangeMax, length.out = lcRangeSteps)
        }

        ## for SPR
        if(Lm50 == 0) Lm50 <- NULL
        if(Lm75 == 0) Lm75 <- NULL
        returnResults[['Lm50']] <- lfq2$Lm50 <- Lm50
        returnResults[['Lm75']] <- lfq2$Lm75 <- Lm75

        ## Thompson and Bell model with changes in F
        resYPR1 <- predict_mod_temp(lfq2, type = "ThompBell",
                                    FM_change = fchange,
                                    stock_size_1 = 1,
                                    curr.E = E,
                                    plot = FALSE,
                                    hide.progressbar = TRUE)

        ## Thompson and Bell model with changes in F and Lc
        resYPR2 <- predict_mod_temp(lfq2, type = "ThompBell",
                                    FM_change = fchange,
                                    Lc_change = lcchange,
                                    stock_size_1 = 1,
                                    curr.E = E, curr.Lc = L50,
                                    s_list = slist,
                                    plot = FALSE, hide.progressbar = TRUE)

        returnResults[['resYPR1']] <- resYPR1
        returnResults[['resYPR2']] <- resYPR2
    },
    error=function(cond) {
        print("There was an error here")
        message(paste0("Error!!", cond))
        errorResult = list()
        errorResult[['error']] <- gettext(cond)
        return (errorResult)
        ## Choose a return value in case of error
    },
    finally={
        print("Done")
    })
    if ('error' %in% names(out)) {
        returnResults[['error']] <- out$error
    }
    return (returnResults)
}
