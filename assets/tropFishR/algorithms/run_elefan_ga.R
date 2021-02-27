library(TropFishR)

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
                          plus_group = 0,
                          years = NA,
                          agg = "month",
                          method_natM = "Then_growth",
                          schooling = FALSE,
                          tmax = NULL,
                          temp = NULL,
                          binSizeCC = NULL,
                          yearsCC = NA,
                          LWa = 0.001,
                          LWb = 3,
                          stockSize = 1,
                          fmchangeRange = c(0,5),
                          fmLengthOut = 100,
                          lcLengthOut = 100,
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
        resGA <- ELEFAN_GA(lfq, MA = MA, seasonalised = seasonalised,
                           maxiter = maxiter, addl.sqrt = addl.sqrt,
                           low_par=low_par, up_par=up_par,
                           pmutation = pmutation, pcrossover = pcrossover,
                           elitism = elitism, popSize = popSize,
                           run = run,
                           monitor=FALSE)
        Linf <- resGA$par$Linf
        K <- resGA$par$K
        returnResults[['resGA']] <- resGA

        ##--------------------
        ##  Natural mortality
        ##--------------------
        M <- as.numeric(M_empirical(Linf = Linf, K_l = K,
                                    method = method_natM, schooling = schooling,
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
        returnResults[['resCC']] <- resCC


        ##--------------------
        ##  YPR/SPR
        ##--------------------
        ## TODO: potentially allow new binsize but: reestimate Sest
        ## length-weight relationship
        lfq2$a <- LWa
        lfq2$b <- LWb

        ## Fishing mortality at length based on RESRESCC
        lfq2$FM <- FM * resCC$Sest

        ## other parameters
        lfq2$Z <- Z
        lfq2$M <- M

        ## acounting for renderUi in hidden tabs
        if(is.null(stockSize)) stockSize <- 1
        if(is.null(fmLengthOut)) fmLengthOut <- 100
        if(is.null(fmchangeRange)){
            fmchange <- seq(0, 2*FM, length.out = fmLengthOut)
        }else{
            fmchange <- seq(fmchangeRange[1], fmchangeRange[2], length.out = fmLengthOut)
        }
        if(is.null(lcLengthOut)) lcLengthOut <- 100
        if(is.numeric(L50)){
            lcchange <- seq(0.1*L50, max(2*L50, 0.9*Linf), length.out = lcLengthOut)
        }else{
            lcchange <- seq(1, 0.9*Linf, length.out = lcLengthOut)
        }

        ## Thompson and Bell model with changes in F
        resYPR1 <- predict_mod(lfq2, type = "ThompBell",
                               FM_change = fmchange,
                               stock_size_1 = stockSize,
                               curr.E = E,
                               plot = FALSE,
                               hide.progressbar = TRUE)

        ## Thompson and Bell model with changes in F and Lc
        resYPR2 <- predict_mod(lfq2, type = "ThompBell",
                               FM_change = fmchange,
                               Lc_change = lcchange,
                               stock_size_1 = stockSize,
                               curr.E = E, curr.Lc = L50,
                               s_list = list(selecType = "trawl_ogive",
                                             L50 = L50, L75 = resCC$L75),
                               plot = FALSE, hide.progressbar = TRUE)

        returnResults[['resYPR1']] <- resYPR1
        returnResults[['reYPR2']] <- resYPR2
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
