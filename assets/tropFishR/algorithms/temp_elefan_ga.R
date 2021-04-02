

## modify ELEFAN_GA function for plotting score function (raw GA fit needed)

ELEFAN_GA_temp <- function (lfq, seasonalised = FALSE, low_par = NULL, up_par = NULL,
    popSize = 50, maxiter = 100, run = maxiter, parallel = FALSE,
    pmutation = 0.1, pcrossover = 0.8, elitism = base::max(1,
        round(popSize * 0.05)), MA = 5, addl.sqrt = FALSE, agemax = NULL,
    flagging.out = TRUE, seed = NULL, monitor = FALSE, plot = FALSE,
    plot.score = TRUE, ...)
{
    classes <- lfq$midLengths
    n_classes <- length(classes)
    Linf_est <- classes[n_classes]
    low_par_ALL <- list(Linf = Linf_est * 0.5, K = 0.01, t_anchor = 0,
        C = 0, ts = 0)
    low_Linf <- ifelse("Linf" %in% names(low_par), get("Linf",
        low_par), get("Linf", low_par_ALL))
    low_K <- ifelse("K" %in% names(low_par), get("K", low_par),
        get("K", low_par_ALL))
    low_tanc <- ifelse("t_anchor" %in% names(low_par), get("t_anchor",
        low_par), get("t_anchor", low_par_ALL))
    low_C <- ifelse("C" %in% names(low_par), get("C", low_par),
        get("C", low_par_ALL))
    low_ts <- ifelse("ts" %in% names(low_par), get("ts", low_par),
        get("ts", low_par_ALL))
    up_par_ALL <- list(Linf = Linf_est * 1.5, K = 1, t_anchor = 1,
        C = 1, ts = 1)
    up_Linf <- ifelse("Linf" %in% names(up_par), get("Linf",
        up_par), get("Linf", up_par_ALL))
    up_K <- ifelse("K" %in% names(up_par), get("K", up_par),
        get("K", up_par_ALL))
    up_tanc <- ifelse("t_anchor" %in% names(up_par), get("t_anchor",
        up_par), get("t_anchor", up_par_ALL))
    up_C <- ifelse("C" %in% names(up_par), get("C", up_par),
        get("C", up_par_ALL))
    up_ts <- ifelse("ts" %in% names(up_par), get("ts", up_par),
        get("ts", up_par_ALL))
    lfq <- lfqRestructure(lfq, MA = MA, addl.sqrt = addl.sqrt)
    sofun <- function(lfq, par, agemax, flagging.out) {
        Lt <- lfqFitCurves(lfq, par = list(Linf = par[1], K = par[2],
            t_anchor = par[3], C = par[4], ts = par[5]), agemax = agemax,
            flagging.out = flagging.out)
        return(Lt$fESP)
    }
    fun <- function(lfq, par, agemax, flagging.out) {
        Lt <- lfqFitCurves(lfq, par = list(Linf = par[1], K = par[2],
            t_anchor = par[3], C = 0, ts = 0), agemax = agemax,
            flagging.out = flagging.out)
        return(Lt$fESP)
    }
    if (seasonalised) {
        min = c(low_Linf, low_K, low_tanc, low_C, low_ts)
        max = c(up_Linf, up_K, up_tanc, up_C, up_ts)
        writeLines("Genetic algorithm is running. This might take some time.")
        flush.console()
        fit <- GA::ga(type = "real-valued", fitness = sofun,
            lfq = lfq, lower = min, upper = max, agemax = agemax,
            flagging.out = flagging.out, popSize = popSize, maxiter = maxiter,
            run = run, parallel = parallel, pmutation = pmutation,
            pcrossover = pcrossover, elitism = elitism, seed = seed,
            monitor = monitor, ...)
        pars <- as.list(fit@solution[1, ])
        names(pars) <- c("Linf", "K", "t_anchor", "C", "ts")
    }
    else {
        min = c(low_Linf, low_K, low_tanc)
        max = c(up_Linf, up_K, up_tanc)
        writeLines("Genetic algorithm is running. This might take some time.")
        flush.console()
        fit <- GA::ga(type = "real-valued", fitness = fun, lfq = lfq,
            lower = min, upper = max, agemax = agemax, flagging.out = flagging.out,
            popSize = popSize, maxiter = maxiter, run = run,
            parallel = parallel, pmutation = pmutation, pcrossover = pcrossover,
            elitism = elitism, seed = seed, monitor = monitor,
            ...)
        pars <- as.list(fit@solution[1, ])
        names(pars) <- c("Linf", "K", "t_anchor")
    }
    if (plot.score) {
        GA::plot(fit)
    }
    final_res <- lfqFitCurves(lfq = lfq, par = pars, flagging.out = flagging.out,
        agemax = agemax)
    phiL <- log10(pars$K) + 2 * log10(pars$Linf)
    pars$phiL <- phiL
    lfq$ncohort <- final_res$ncohort
    lfq$agemax <- final_res$agemax
    lfq$par <- pars
    lfq$fESP <- fit@fitnessValue
    lfq$Rn_max <- fit@fitnessValue
    lfq$gafit <- fit
    if (plot) {
        plot(lfq, Fname = "rcounts")
        Lt <- lfqFitCurves(lfq, par = lfq$pars, draw = TRUE)
    }
    return(lfq)
}
