
stock_sim <- function (param, age_unit = "year", stock_size_1 = NA, plus_group = NA)
{
    res <- param
    meanWeight <- res$meanWeight
    meanValue <- res$meanValue
    FM <- res$FM
    if (!is.null(res$M)) {
        nM <- res$M
        Z <- FM + nM
    }
    else {
        Z <- res$Z
        nM <- mean(Z - FM, na.rm = T)
    }
    if ("midLengths" %in% names(res))
        classes <- as.character(res$midLengths)
    if ("age" %in% names(res))
        classes <- as.character(res$age)
    classes.num <- do.call(rbind, strsplit(classes, split = "\\+"))
    classes.num <- as.numeric(classes.num[, 1])
    if ("age" %in% names(res)) {
        dt <- c(diff(classes.num), NA)
        if (age_unit == "month") {
            dt <- dt * 1/12
        }
        N <- rep(NA, length(classes.num))
        N[1] <- ifelse(is.na(stock_size_1), 1000, stock_size_1)
        for (x1 in 2:length(N)) {
            N[x1] <- N[x1 - 1] * exp(-Z[x1 - 1] * dt[x1 - 1])
        }
        dead <- c(abs(diff(N)), NA)
        C <- dead * (FM/Z)
        Y <- C * meanWeight
        B <- Y/(FM * dt)
        V <- Y * meanValue
        totals <- data.frame(totC = sum(C, na.rm = TRUE), totY = sum(Y,
            na.rm = TRUE), totV = sum(V, na.rm = TRUE), meanB = sum((B *
            dt), na.rm = TRUE)/sum(dt, na.rm = TRUE))
        res2 <- list(dt = dt, N = N, dead = dead, C = C, Y = Y,
            B = B, V = V, totals = totals)
        if (!is.na(plus_group)) {
            df <- do.call(cbind, res2[1:7])
            df <- df[1:plus_group, ]
            classes <- classes[1:plus_group]
            classes[length(classes)] <- paste(classes[length(classes)],
                "+", sep = "")
            df[plus_group, "dead"] <- df[plus_group, "N"]
            new.C <- (FM[plus_group]/Z[plus_group]) * df[plus_group,
                "N"]
            catch.plus.dif <- new.C - df[plus_group, "C"]
            df[plus_group, "C"] <- new.C
            df[plus_group, "Y"] <- meanWeight[plus_group] * catch.plus.dif
            df[plus_group, "V"] <- df[plus_group, "Y"] * meanValue[plus_group]
            df[plus_group, "B"] <- df[plus_group, "Y"]/(FM[plus_group] *
                df[plus_group, "dt"])
            res2 <- as.list(as.data.frame(df))
            df2 <- do.call(cbind, res)
            df2 <- df2[1:plus_group, ]
            res <- as.list(as.data.frame(df2))
            res$age <- classes
            totals <- data.frame(totC = sum(res2$C, na.rm = TRUE),
                totY = sum(res2$Y, na.rm = TRUE), totV = sum(res2$V,
                  na.rm = TRUE), meanB = sum((res2$B * res2$dt),
                  na.rm = TRUE)/sum(dt, na.rm = TRUE))
            res2 <- c(res2, totals = list(totals))
        }
    }
    if ("midLengths" %in% names(res)) {
        Linf <- res$Linf
        K <- res$K
        a <- res$a
        b <- res$b
        int <- classes.num[2] - classes.num[1]
        lowL <- classes.num - (int/2)
        upL <- classes.num + (int/2)
        H <- ((Linf - lowL)/(Linf - upL))^(nM/(2 * K))
        N <- rep(NA, length(classes.num))
        N[1] <- ifelse(is.na(stock_size_1), 1000, stock_size_1)
        for (x1 in 2:length(classes.num)) {
            N[x1] <- N[x1 - 1] * ((1/H[x1 - 1]) - (FM[x1 - 1]/Z[x1 -
                1]))/(H[x1 - 1] - (FM[x1 - 1]/Z[x1 - 1]))
        }
        dead <- c(abs(diff(N)), NA)
        C <- dead * (FM/Z)
        W <- a * ((lowL + upL)/2)^b
        Y <- C * W
        V <- Y * meanValue
        B <- (dead/Z) * W
        mat <- select_ogive(list(selecType = "trawl_ogive", L50 = res$Lm50, L75 = res$Lm75), Lt=classes.num)
        SSB <- B * mat
        x2 <- length(classes.num)
        C[x2] <- N[x2] * FM[x2]/Z[x2]
        W[x2] <- a * ((lowL[x2] + Linf)/2)^b
        Y[x2] <- C[x2] * W[x2]
        B[x2] <- N[x2]/Z[x2] * W[x2]
        mat[x2] <- select_ogive(list(selecType = "trawl_ogive", L50 = res$Lm50, L75 = res$Lm75), Lt=(lowL[x2] + Linf)/2)
        SSB[x2] <- B[x2] * mat[x2]
        V[x2] <- Y[x2] * meanValue[x2]
        totals <- data.frame(totC = sum(C, na.rm = TRUE), totY = sum(Y,
            na.rm = TRUE), totV = sum(V, na.rm = TRUE), meanB = sum((B),
            na.rm = TRUE), meanSSB = sum(SSB,na.rm=TRUE))
        res2 <- list(N = N, dead = dead, C = C, Y = Y, B = B, SSB = SSB,
            V = V, totals = totals)
    }
    ret <- c(res, res2)
    return(ret)
}







predict_mod_temp <- function (param, type, FM_change = NA, E_change = NA, FM_relative = FALSE,
    Lc_change = NULL, tc_change = NULL, s_list = NA, stock_size_1 = NA,
    age_unit = "year", curr.E = NA, curr.Lc = NA, plus_group = NA,
    Lmin = NA, Lincr = NA, plot = FALSE, mark = TRUE, hide.progressbar = FALSE,monitor=NA){

    res <- param
    res$FM_relative = FM_relative

    if (type == "ypr") {
        if (FM_relative)
            stop(noquote("ypr does not work with relative changes in FM, please provide absolute values."))
        M <- res$M
        K <- res$K
        t0 <- ifelse(!is.null(res$t0), res$t0, 0)
        a <- res$a
        b <- res$b
        Winf <- res$Winf
        Linf <- res$Linf
        if ("Linf" %in% names(res) & "a" %in% names(res) & "b" %in%
            names(res)) {
            Winf <- a * (Linf^b)
        }
        if (length(FM_change) == 1 & is.na(FM_change[1]) & length(E_change) ==
            1 & is.na(E_change[1])) {
            FM_change <- seq(0, 10, 0.1)
            print(noquote("No fishing mortality (FM_change) or exploitation rate (E_change) was provided, a default range for fishing mortality of 0 to 10 is used."))
        }
        if (length(FM_change) == 1 & is.na(FM_change[1]) & length(E_change) !=
            1 & !is.na(E_change[1])) {
            E_change <- E_change[E_change <= 0.9]
            FM_change <- (E_change * M)/(1 - E_change)
        }
        tr <- res$tr
        Lr <- res$Lr
        if (is.null(tr) & is.null(Lr))
            stop("Either the age or the length at recruitment (tr or Lr) has to be provided in param!")
        if (!is.null(Linf)) {
            if (is.null(tr))
                tr <- VBGF(L = Lr, param = list(Linf = Linf,
                  K = K, t0 = t0))
            if (is.null(Lr))
                Lr <- VBGF(t = tr, param = list(Linf = Linf,
                  K = K, t0 = t0))
        }
        tc <- res$tc
        Lc <- res$Lc
        if (is.null(tc) & is.null(Lc)) {
            if ("L50" %in% s_list)
                Lc <- s_list$L50
            if ("Lc" %in% s_list)
                Lc <- s_list$Lc
        }
        if (!is.null(Linf)) {
            if (is.null(tc) & !is.null(Lc))
                tc <- VBGF(L = Lc, param = list(Linf = Linf,
                  K = K, t0 = t0))
            if (is.null(Lc) & !is.null(tc))
                Lc <- VBGF(t = tc, param = list(Linf = Linf,
                  K = K, t0 = t0))
            if (is.null(tc_change) & !is.null(Lc_change))
                tc_change <- VBGF(L = Lc_change, param = list(Linf = Linf,
                  K = K, t0 = t0))
            if (is.null(Lc_change) & !is.null(tc_change))
                Lc_change <- VBGF(t = tc_change, param = list(Linf = Linf,
                  K = K, t0 = t0))
        }
        tc <- c(tc, tc_change)
        Lc <- c(Lc, Lc_change)
        if (length(s_list) > 1) {
            selecType <- s_list$selecType
        }
        else {
            selecType <- "knife_edge"
        }
        list_Lc_runs <- vector("list", length(Lc))
        list_Es <- vector("list", length(Lc))
        if (!hide.progressbar) {
            nlk <- length(Lc)
            if (nlk > 1) {
                pb <- txtProgressBar(min = 1, max = nlk, style = 3)
                counter <- 1
            }
        }
        if (is.null(Lc))
            Lc_tc <- tc
        else Lc_tc <- Lc
        for (i in 1:length(Lc_tc)) {
            Lci <- Lc[i]
            tci <- tc[i]
            Z <- (M + FM_change)
            E <- FM_change/Z
            if (length(s_list) == 1) {
                input <- list(Linf = Linf, Winf = Winf, K = K,
                              M = M, t0 = t0, tr = tr, tc = tci)
                output <- ypr(input, FM_change)
                B_R <- output$br
                Y_R <- output$yr
                B_R.rel <- output$rbr
                Y_R.rel <- output$ryr
                deri <- output$derivative
            }
            if (length(s_list) > 1) {
                if ("midLengths" %in% names(res)) {
                  classes <- as.character(res$midLengths)
                  classes.num <- do.call(rbind, strsplit(classes,
                    split = "\\+"))
                  classes.num <- as.numeric(classes.num[, 1])
                  Lt <- classes.num
                }
                if (!"midLengths" %in% names(res)) {
                  if (is.na(Lmin) | is.na(Lincr))
                    writeLines("No midpoints of length classes are provided. This can be done using Lmin and Lincr or by a \n midLengths element in param. A standard range from 1cm to Linf * 0.98 by 2cm is assumed")
                  Lmin <- ifelse(is.na(Lmin), 1, Lmin)
                  Lincr <- ifelse(is.na(Lincr), 2, Lincr)
                  Lt <- seq(Lmin, (Linf * 0.98), Lincr)
                }
                P <- select_ogive(s_list, Lt = Lt, Lc = Lci)
                input <- list(Linf = ifelse(length(Linf) > 0,
                  Linf, NA), Winf = ifelse(length(Winf) > 0,
                  Winf, NA), K = K, M = M, t0 = t0, tr = tr,
                  tc = tci)
                output <- ypr_sel(input, FM_change, Lt, P)
                B_R.rel <- output$rbr
                Y_R.rel <- output$ryr
                deri <- output$derivative
                Y_R <- Y_R.rel * Winf * exp(M * (tr - t0))
                B_R <- B_R.rel * Winf * exp(M * (tr - t0))
            }
            if (0 %in% FM_change) {
                Bv_R <- B_R[FM_change == 0]
            }
            else {
                Bv_R <- B_R[FM_change == min(FM_change, na.rm = TRUE)]
                writeLines(paste0("Biomass was not estimated for a fishing mortality (FM) of 0, thus the virgin biomass corresponds to a FM of ",
                  min(FM_change, na.rm = TRUE)))
            }
            B_R.percent <- round((B_R/Bv_R) * 100, digits = 1)
            Ty <- (1/Z) + tci
            S <- exp(-K * (tci - t0))
            Ly <- Linf * (1 - ((Z * S)/(Z + K)))
            Wy <- (Z) * Winf * ((1/Z) - ((3 * S)/(Z + K)) + ((3 *
                (S^2))/(Z + (2 * K))) - ((S^3)/(Z + (3 * K))))
            results.PBH <- data.frame(FM = FM_change, E = E)
            if (length(Ty) > 0)
                results.PBH$Ty <- Ty
            if (length(Ly) > 0)
                results.PBH$Ly <- Ly
            if (length(Wy) > 0)
                results.PBH$Wy <- Wy
            results.PBH$Y_R.rel <- Y_R.rel
            results.PBH$B_R.rel <- B_R.rel
            if (length(Y_R) > 0)
                results.PBH$Y_R = Y_R
            if (length(B_R) > 0)
                results.PBH$B_R = B_R
            if (length(B_R.percent) > 0)
                results.PBH$B_R.percent = B_R.percent
            list_Lc_runs[[i]] <- results.PBH
            Nmsy <- which.max(Y_R.rel)
            deri_pot <- deri[1:Nmsy]
            N01 <- which.min(abs(deri_pot - (deri[1] * 0.1)))
            N05 <- which.min(abs(B_R.percent - 50))
            df_loop_Es <- data.frame(Lc = ifelse(!is.null(Lci),
                Lci, NA), tc = ifelse(!is.null(tci), tci, NA),
                F01 = FM_change[N01], Fmsy = FM_change[Nmsy])
            if (length(B_R.percent) > 0)
                df_loop_Es$F05 <- FM_change[N05]
            df_loop_Es$E01 <- E[N01]
            df_loop_Es$Emsy <- E[Nmsy]
            if (length(B_R.percent) > 0)
                df_loop_Es$E05 <- E[N05]
            list_Es[[i]] <- df_loop_Es
            if (!hide.progressbar) {
                if (nlk > 1) {
                  setTxtProgressBar(pb, counter)
                  counter <- counter + 1
                }
            }
        }
        df_Es <- do.call(rbind, list_Es)
        names(list_Lc_runs) <- paste0("Lc_", Lc_tc)
        ret <- c(res, list(FM_change = FM_change, Lc = Lc, tc = tc,
            list_Lc_runs = list_Lc_runs, df_Es = df_Es))
        if (!is.na(curr.E) & !is.na(curr.Lc)) {
            curr.tc <- VBGF(L = curr.Lc, param = list(Linf = Linf,
                K = K, t0 = t0))
            curr.F = (M * curr.E)/(1 - curr.E)
            tmpList <- list(Linf = Linf, Winf = Winf, K = K,
                M = M, t0 = t0, tr = tr, tc = curr.tc)
            if (length(s_list) == 1 | selecType == "knife_edge") {
                tmpRES <- ypr(param = tmpList, FM_change = curr.F)
            }
            if (length(s_list) > 1 & selecType != "knife_edge") {
                P <- select_ogive(s_list, Lt = Lt, Lc = curr.Lc)
                tmpRES <- ypr_sel(param = tmpList, FM_change = curr.F,
                  Lt, P)
                tmpRES$yr <- tmpRES$ryr * Winf * exp(M * (tr -
                  t0))
                tmpRES$br <- tmpRES$rbr * Winf * exp(M * (tr -
                  t0))
            }
            df_currents <- data.frame(curr.Lc = curr.Lc, curr.tc = curr.tc,
                curr.E = curr.E, curr.F = curr.F, curr.YPR = tmpRES$yr,
                curr.YPR.rel = tmpRES$ryr, curr.BPR = tmpRES$br,
                curr.BPR.rel = tmpRES$rbr)
            ret$currents <- df_currents
        }
    }
    if (type == "ThompBell") {
        meanWeight <- res$meanWeight
        meanValue <- res$meanValue
        FM <- res$FM
        if (is.null(res$M) & is.null(res$Z))
            stop(noquote("Either M or Z (in 'param') has to be provided!"))
        if (!is.null(res$M)) {
            nM <- res$M
            Z <- FM + nM
        }
        else {
            Z <- res$Z
            nM <- Z - FM
        }
        Linf <- res$Linf
        K <- res$K
        t0 <- ifelse("t0" %in% names(res), res$t0, 0)
        tc <- res$tc
        Lc <- res$Lc
        if (is.null(tc) & is.null(Lc)) {
            if ("L50" %in% s_list)
                Lc <- s_list$L50
            if ("Lc" %in% s_list)
                Lc <- s_list$Lc
        }
        if (!is.null(Linf)) {
            if (is.null(tc) & !is.null(Lc))
                tc <- VBGF(L = Lc, param = list(Linf = Linf,
                  K = K, t0 = t0))
            if (is.null(Lc) & !is.null(tc))
                Lc <- VBGF(t = tc, param = list(Linf = Linf,
                  K = K, t0 = t0))
            if (is.null(tc_change) & !is.null(Lc_change))
                tc_change <- VBGF(L = Lc_change, param = list(Linf = Linf,
                  K = K, t0 = t0))
            if (is.null(Lc_change) & !is.null(tc_change))
                Lc_change <- VBGF(t = tc_change, param = list(Linf = Linf,
                  K = K, t0 = t0))
        }
        tc <- c(tc, tc_change)
        Lc <- c(Lc, Lc_change)
        if ("age" %in% names(res))
            classes <- as.character(res$age)
        if ("midLengths" %in% names(res))
            classes <- as.character(res$midLengths)
        classes.num <- do.call(rbind, strsplit(classes, split = "\\+"))
        classes.num <- as.numeric(classes.num[, 1])
        if (length(FM_change) == 1 & is.na(FM_change[1]) & length(E_change) ==
            1 & is.na(E_change[1])) {
            FM_change <- seq(0, 10, 0.1)
            print(noquote("No fishing mortality (FM_change) or exploitation rate (E_change) \nwas provided, a default range for the absolute \nfishing mortality of 0 to 10 is used."))
        }
        if (length(FM_change) == 1 & is.na(FM_change[1]) & length(E_change) !=
            1 & !is.na(E_change[1])) {
            E_change <- E_change[E_change <= 0.9]
            FM_change <- (E_change * nM)/(1 - E_change)
        }
        if (length(E_change) == 1 & is.na(E_change[1])) {
            E_change <- FM_change/(FM_change + nM)
        }
        Lt <- classes.num
        if ((is.null(tc_change) & is.null(Lc_change))) {
            if (is.null(res$FM))
                stop(noquote("Please provide fishing mortality FM (in 'param')!"))
            if (length(res$FM) == 1) {
                if (length(s_list) > 1 | !is.null(Lc[1])) {
                  print(noquote("Fishing mortality per length class not povided, using selectivity information to derive fishing mortality per length class."))
                  if (length(s_list) == 1) {
                    s_list <- list(selecType = "knife_edge",
                      L50 = Lc[1])
                  }
                  sel <- select_ogive(s_list, Lt = Lt)
                  FM <- res$FM * sel
                }
                else {
                  stop(noquote("Please provide either fishing mortality FM (in 'param') per length class or a Lc value!"))
                }
            }
            if (!FM_relative) {
                pred_mat <- as.matrix(FM/max(FM, na.rm = TRUE)) %*%
                  FM_change
            }
            if (FM_relative) {
                pred_mat <- as.matrix(FM) %*% FM_change
            }
            pred_res_list <- list()
            for (x7 in 1:length(FM_change)) {
                param$Z <- pred_mat[, x7] + nM
                param$FM <- pred_mat[, x7]
                resL <- stock_sim(param, age_unit = age_unit,
                  stock_size_1 = stock_size_1, plus_group = plus_group)
                pred_res_list[[x7]] <- resL$totals
            }
            ## SSB0 (for SPR)
            param$Z <- rep(0,nrow(pred_mat)) + nM
            param$FM <- rep(0,nrow(pred_mat))
            resL <- stock_sim(param, age_unit = age_unit,
                              stock_size_1 = stock_size_1, plus_group = plus_group)
            meanSSB0 <- resL$totals$meanSSB
            pred_res_df <- do.call(rbind, pred_res_list)
            pred_res_df$FM_change <- FM_change
            pred_res_df$E_change <- E_change
            pred_res_df$SPR <- pred_res_df$meanSSB / meanSSB0
            res2 <- pred_res_df
            res3 <- c(res, res2)
            Bper <- rep(NA, length(pred_res_df$meanB))
            Bper[1] <- 100
            for (ix in 2:length(Bper)) {
                Bper[ix] <- pred_res_df$meanB[ix]/pred_res_df$meanB[1] *
                  100
            }
            ## NEW: added for F0.1
            midLengthsYPR <- seq(0.5, Linf, 0.5)
            pSel <- select_ogive(list(selecType="trawl_ogive", L50 = res$L50, L75 = res$L75),
                                 midLengthsYPR)
            Fvec <- seq(0,max(FM_change),0.01)
            yprs <- rep(NA,length(Fvec))
            bprs <- rep(NA,length(Fvec))
            ssbprs <- rep(NA,length(Fvec))
            for(i in 1:length(Fvec)){
                tmp <- list(midLengths = midLengthsYPR, M = nM,
                            FM = as.numeric(Fvec[i] * pSel),
                            Linf = Linf, K = K, a = res$a, b = res$b,
                            Lm50 = res$Lm50, Lm75 = res$Lm75)
                yprs[i] <- stock_sim(param = tmp)$totals$totY
                bprs[i] <- stock_sim(param = tmp)$totals$meanB
                ssbprs[i] <- stock_sim(param = tmp)$totals$meanSSB
            }
            nmax <- which.max(yprs)
            slopeOrg <- (yprs[2] - yprs[1])/(Fvec[2] - Fvec[1])
            slope01 <- round(0.1 * slopeOrg, 2)
            slopes <- rep(NA, length(Fvec))
            slopes[1] <- slopeOrg
            for (i in 3:length(Fvec)) {
                slopes[i - 1] <- round((yprs[i] - yprs[i - 1])/(Fvec[i] - Fvec[i - 1]), 2)
            }
            dif <- abs(slopes - slope01)
            dif[is.na(dif)] <- 1e+11
            difpot <- dif[1:nmax]
            n01 <- which.min(difpot)
            Bper <- rep(NA, length(bprs))
            Bper[1] <- 100
            for (ix in 2:length(Bper)) {
                Bper[ix] <- bprs[ix]/bprs[1] * 100
            }
            n05 <- which.min(abs(Bper - 50))
            SPRper <- rep(NA, length(bprs))
            SPRper[1] <- 100
            for (ix in 2:length(SPRper)) {
                SPRper[ix] <- ssbprs[ix]/ssbprs[1] * 100
            }
            n04 <- which.min(abs(SPRper - 40))
            n035 <- which.min(abs(SPRper - 35))
            n03 <- which.min(abs(SPRper - 30))
            ## N01 <- which.min()
            ## N05 <- which.min(abs(Bper - 50))
            ## Nmsy <- which.max(pred_res_df$totY)
            ## if (!is.null(Lc[1]) & !is.null(tc[1])) {
            ##     df_Es <- data.frame(Lc = Lc, tc = tc, Fmsy = FM_change[Nmsy],
            ##       F05 = FM_change[N05], Emsy = E_change[Nmsy],
            ##       E05 = E_change[N05])
            ## }
            ## else {
            df_Es <- data.frame(Fmax = Fvec[nmax],
                                F01 = Fvec[n01],
                                F05 = Fvec[n05],
                                F30 = Fvec[n03],
                                F35 = Fvec[n035],
                                F40 = Fvec[n04])
            ## }
            ret <- c(res3, list(df_Es = df_Es))
            if (!is.na(curr.E)) {
                if (!is.na(curr.Lc)) {
                  curr.tc <- VBGF(L = curr.Lc, param = list(Linf = Linf,
                    K = K, t0 = t0))
                }
                else curr.tc <- NA
                curr.F = (nM * curr.E)/(1 - curr.E)
                if (is.na(curr.Lc)) {
                  sel <- (FM/max(FM, na.rm = TRUE))
                }
                else if (!is.na(curr.Lc)) {
                  s_list <- list(selecType = "knife_edge", L50 = curr.Lc)
                  Lt <- res$midLengths
                  sel <- select_ogive(s_list, Lt = Lt, Lc = curr.Lc)
                }
                if (length(s_list) != 1) {
                  Lt <- res$midLengths
                  sel <- select_ogive(s_list, Lt = Lt)
                }
                mati <- sel * curr.F
                param.loop <- res
                param.loop$FM <- mati
                param.loop$Z <- mati + nM
                res2 <- stock_sim(param = param.loop, age_unit = age_unit,
                  stock_size_1 = stock_size_1, plus_group = plus_group)
                mati2 <- res2$totals
                param.loop$FM <- rep(0,length(mati))
                param.loop$Z <- mati + nM
                res3 <- stock_sim(param = param.loop, age_unit = age_unit,
                                  stock_size_1 = stock_size_1, plus_group = plus_group)
                df_currents <- data.frame(curr.Lc = curr.Lc,
                  curr.tc = curr.tc, curr.E = curr.E, curr.F = curr.F,
                  curr.C = mati2$totC, curr.Y = mati2$totY, curr.V = mati2$totV,
                  curr.B = mati2$meanB, curr.SPR = mati2$meanSSB/res3$totals$meanSSB*100)
                ret$currents <- df_currents
            }
        }
        if (!is.null(tc_change) | !is.null(Lc_change)) {
            if (length(s_list) == 1) {
                s_list <- list(selecType = "knife_edge", L50 = Lc[1])
            }
            sel <- select_ogive(s_list, Lt = Lt)
            sel.list <- list()
            for (x19 in 1:length(Lc)) {
                sel.list[[x19]] <- select_ogive(s_list, Lt = Lt,
                  Lc = Lc[x19])
            }
            Lc_mat <- do.call(cbind, sel.list)
            colnames(Lc_mat) <- Lc
            Lc_mat_FM <- Lc_mat
            FM_Lc_com_mat.list <- list()
            if (!FM_relative) {
                for (x20 in 1:length(colnames(Lc_mat_FM))) {
                  FM_Lc_com_mat.list[[x20]] <- as.matrix(Lc_mat_FM[,
                    x20]) %*% FM_change
                  colnames(FM_Lc_com_mat.list[[x20]]) <- FM_change
                }
            }
            if (FM_relative) {
                for (x20 in 1:length(colnames(Lc_mat_FM))) {
                  FM_Lc_com_mat.list[[x20]] <- as.matrix(Lc_mat_FM[,
                    x20] * FM) %*% FM_change
                  colnames(FM_Lc_com_mat.list[[x20]]) <- FM_change
                }
            }
            param.loop <- res
            pred.FM_Lc_com_res_loopC_list <- vector("list", length(FM_Lc_com_mat.list))
            pred.FM_Lc_com_res_loopY_list <- vector("list", length(FM_Lc_com_mat.list))
            pred.FM_Lc_com_res_loopB_list <- vector("list", length(FM_Lc_com_mat.list))
            pred.FM_Lc_com_res_loopV_list <- vector("list", length(FM_Lc_com_mat.list))
            if(is.function(monitor)){
                nlk <- length(FM_Lc_com_mat.list)
                counter <- 1
            }else if (!hide.progressbar) {
                nlk <- prod(length(FM_Lc_com_mat.list), dim(FM_Lc_com_mat.list[[1]])[2])
                pb <- txtProgressBar(min = 1, max = nlk, style = 3)
                counter <- 1
            }
            for (x21 in 1:length(FM_Lc_com_mat.list)) {
                mati <- FM_Lc_com_mat.list[[x21]]
                pred.FM_Lc_com_res_loop1_list <- list()
                for (x22 in 1:dim(mati)[2]) {
                  param.loop$FM <- mati[, x22]
                  param.loop$Z <- mati[, x22] + nM
                  res2 <- stock_sim(param = param.loop, age_unit = age_unit,
                    stock_size_1 = stock_size_1, plus_group = plus_group)
                  pred.FM_Lc_com_res_loop1_list[[x22]] <- res2$totals
                  if(is.function(monitor)){
                  }else if (!hide.progressbar) {
                    setTxtProgressBar(pb, counter)
                    counter <- counter + 1
                  }
                }
                prev_mat <- do.call(rbind, pred.FM_Lc_com_res_loop1_list)
                prev_matC <- prev_mat[, "totC"]
                prev_matY <- prev_mat[, "totY"]
                prev_matB <- prev_mat[, "meanB"]
                prev_matV <- prev_mat[, "totV"]
                pred.FM_Lc_com_res_loopC_list[[x21]] <- prev_matC
                pred.FM_Lc_com_res_loopY_list[[x21]] <- prev_matY
                pred.FM_Lc_com_res_loopB_list[[x21]] <- prev_matB
                pred.FM_Lc_com_res_loopV_list[[x21]] <- prev_matV
                if(is.function(monitor)){
                    monitor(counter, nlk)
                    counter <- counter + 1
                }
            }
            mat_FM_Lc_com.C <- do.call(rbind, pred.FM_Lc_com_res_loopC_list)
            rownames(mat_FM_Lc_com.C) <- Lc
            colnames(mat_FM_Lc_com.C) <- FM_change
            mat_FM_Lc_com.Y <- do.call(rbind, pred.FM_Lc_com_res_loopY_list)
            rownames(mat_FM_Lc_com.Y) <- Lc
            colnames(mat_FM_Lc_com.Y) <- FM_change
            mat_FM_Lc_com.B <- do.call(rbind, pred.FM_Lc_com_res_loopB_list)
            rownames(mat_FM_Lc_com.B) <- Lc
            colnames(mat_FM_Lc_com.B) <- FM_change
            mat_FM_Lc_com.V <- do.call(rbind, pred.FM_Lc_com_res_loopV_list)
            rownames(mat_FM_Lc_com.V) <- Lc
            colnames(mat_FM_Lc_com.V) <- FM_change
            mat_FM_Lc_com.C <- t(mat_FM_Lc_com.C)
            mat_FM_Lc_com.Y <- t(mat_FM_Lc_com.Y)
            mat_FM_Lc_com.B <- t(mat_FM_Lc_com.B)
            mat_FM_Lc_com.V <- t(mat_FM_Lc_com.V)
            mat_FM_Lc_com.Bper <- matrix(NA, ncol = dim(mat_FM_Lc_com.B)[2],
                nrow = dim(mat_FM_Lc_com.B)[1])
            mat_FM_Lc_com.Bper[1, ] <- 100
            for (ix in 2:dim(mat_FM_Lc_com.B)[1]) {
                mat_FM_Lc_com.Bper[ix, ] <- mat_FM_Lc_com.B[ix,
                  ]/mat_FM_Lc_com.B[1, ] * 100
            }
            N05 <- apply(mat_FM_Lc_com.Bper, MARGIN = 2, FUN = function(x) which.min(abs(x -
                50)))
            Nmsy <- apply(mat_FM_Lc_com.Y, MARGIN = 2, FUN = which.max)
            if ((!is.null(Lc[1]) & !is.null(tc[1])) | (!is.na(Lc[1]) &
                !is.na(tc[1]))) {
                df_Es <- data.frame(Lc = Lc, tc = tc, Fmsy = FM_change[Nmsy],
                  F05 = FM_change[N05], Emsy = E_change[Nmsy],
                  E05 = E_change[N05])
            }
            else {
                df_Es <- data.frame(Fmsy = FM_change[Nmsy], F05 = FM_change[N05],
                  Emsy = E_change[Nmsy], E05 = E_change[N05])
            }
            ret <- c(res, list(FM_change = FM_change, E_change = E_change,
                Lc_change = Lc_change, tc_change = tc_change,
                Lt = Lt, sel = sel, mat_FM_Lc_com.C = mat_FM_Lc_com.C,
                mat_FM_Lc_com.Y = mat_FM_Lc_com.Y, mat_FM_Lc_com.V = mat_FM_Lc_com.V,
                mat_FM_Lc_com.B = mat_FM_Lc_com.B, df_Es = df_Es))
            if (!is.na(curr.E)) {
                if (!is.na(curr.Lc)) {
                  curr.tc <- VBGF(L = curr.Lc, param = list(Linf = Linf,
                    K = K, t0 = t0))
                }
                else curr.tc <- NA
                curr.F = (nM * curr.E)/(1 - curr.E)
                if (is.na(curr.Lc)) {
                  sel <- FM/max(FM, na.rm = TRUE)
                }
                else if (!is.na(curr.Lc) | length(s_list) ==
                  1) {
                  s_list <- list(selecType = "knife_edge", L50 = curr.Lc)
                  sel <- select_ogive(s_list, Lt = Lt, Lc = curr.Lc)
                }
                else if (!is.na(curr.Lc) | length(s_list) !=
                  1) {
                  sel <- select_ogive(s_list, Lt = Lt, Lc = curr.Lc)
                }
                mati <- sel * curr.F
                param.loop <- res
                param.loop$FM <- mati
                param.loop$Z <- mati + nM
                res2 <- stock_sim(param.loop, age_unit, stock_size_1,
                  plus_group = plus_group)
                mati2 <- res2$totals
                df_currents <- data.frame(curr.Lc = curr.Lc,
                  curr.tc = curr.tc, curr.E = curr.E, curr.F = curr.F,
                  curr.C = mati2$totC, curr.Y = mati2$totY, curr.V = mati2$totV,
                  curr.B = mati2$meanB)
                ret$currents <- df_currents
            }
        }
    }
    class(ret) <- "predict_mod"
    if (plot)
        plot(ret, mark = mark)
    return(ret)
}




plot_predict_mod <- function (x, type = "ypr", xaxis1 = "FM", yaxis1 = "Y_R.rel",
    yaxis2 = "B_R.rel", yaxis_iso = "Lc", identify = FALSE, mark = FALSE,
    contour = TRUE, xlab = NA, ylab1 = NA, ylab2 = NA, ylab3 = NA,
    ...)
{
    pes <- x
    image.identifier <- function(xyz, markII = TRUE, digits = 2) {
        intiX <- (xyz$x[2] - xyz$x[1])/2
        intiY <- (xyz$y[2] - xyz$y[1])/2
        newX <- c(xyz$x - intiX, xyz$x[length(xyz$x)] + intiX)
        newY <- c(xyz$y - intiY, xyz$y[length(xyz$y)] + intiY)
        nx <- length(xyz$x)
        ny <- length(xyz$y)
        mesi <- data.frame()
        xy <- locator(1)
        while (!is.null(xy)) {
            xbin <- as.numeric(cut(xy$x, newX))
            ybin <- as.numeric(cut(xy$y, newY))
            cat("[", xbin, ",", ybin, "] = ", xyz$z[xbin, ybin],
                "\n", sep = "")
            lcc <- xy$y * pes$Linf
            mesi <- rbind(mesi, data.frame(i = xbin, j = ybin,
                x = xy$x, y = xy$y, z = xyz$z[xbin, ybin], Lc = lcc))
            rm(xy)
            xy <- locator(1)
        }
        if (markII) {
            points(mesi$x, mesi$y, pch = 19, cex = 0.5, col = "blue")
            text(mesi$x, mesi$y, format(mesi$z, digits = digits),
                adj = -0.2, col = "blue")
        }
        colnames(mesi) <- c("i", "j", p.FE, "Lc/Linf", p.yield,
            "Lc")
        mesi
    }
    if ("totY" %in% names(pes)) {
        df_Es <- pes$df_Es
        if (xaxis1 == "FM") {
            px <- pes$FM_change
            xlabel1 <- "Fishing mortality"
            if (pes$FM_relative) {
                xlabel1 <- "rel. Fishing mortality"
            }
            N05 <- df_Es$F05
            Nmax <- df_Es$Fmsy
            if (length(N05) == 1) {
                legend.lab <- c("F0.5", "Fmsy")
            }
            else legend.lab <- c("Fmsy")
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.F
        }
        else {
            px <- pes$E_change
            xlabel1 <- "Exploitation rate"
            if (pes$FM_relative) {
                xlabel1 <- "rel. Exploitation rate"
            }
            N05 <- df_Es$E05
            Nmax <- df_Es$Emsy
            if (length(N05) == 1) {
                legend.lab <- c("E0.5", "Emsy")
            }
            else legend.lab <- c("Emsy")
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.E
        }
        if (!is.na(xlab[1])) {
            xlabel1 <- xlab
        }
        if (is.na(ylab1[1])) {
            ylabel1 <- "Yield"
        }
        if (!is.na(ylab1[1])) {
            ylabel1 <- ylab1
        }
        if (is.na(ylab2[1])) {
            ylabel2 <- "Biomass"
        }
        if (!is.na(ylab2[1])) {
            ylabel2 <- ylab2
        }
        if (is.na(ylab3[1])) {
            ylabel3 <- "Value"
        }
        if (!is.na(ylab3[1])) {
            ylabel3 <- ylab3
        }
        max_val <- round(max(pes$totV, na.rm = TRUE), digits = 0)
        dim_val <- 10^(nchar(max_val) - 1)
        max_yiel <- round(max(pes$totY, na.rm = TRUE), digits = 0)
        dim_yiel <- 10^(nchar(max_yiel) - 1)
        max_bio <- round(max(pes$meanB, na.rm = TRUE), digits = 0)
        dim_bio <- 10^(nchar(max_bio) - 1)
        py <- pes$totY[1:length(px)]
        py2 <- pes$meanB[1:length(px)]
        plot(px, py, type = "l", ylab = ylabel1, xlab = xlabel1,
            col = "black", ylim = c(0, ceiling(max_yiel/dim_yiel) *
                dim_yiel), lwd = 1.6)
        segments(x0 = -1, x1 = Nmax, y0 = py[which(px == Nmax)],
            y1 = py[which(px == Nmax)], col = "goldenrod1", lty = 2,
            lwd = 1.6)
        segments(x0 = Nmax, x1 = Nmax, y0 = -1, y1 = py[which(px ==
            Nmax)], col = "goldenrod1", lty = 2, lwd = 1.6)
        if (!is.null(pes$currents) & mark) {
            currents <- pes$currents
            if (!is.na(currents$curr.E) & yaxis1 == "Y_R" | yaxis1 ==
                "Y_R.rel") {
                px1 <- ifelse(xaxis1 == "FM", currents$curr.F,
                  currents$curr.E)
                py1 <- currents$curr.Y
                points(px1, py1, pch = 16, col = "grey30")
                abline(v = px1, col = "grey30", lty = 2)
            }
        }
        par(new = TRUE)
        plot(px, py2, type = "l", ylab = "", xlab = "", col = "blue",
            lwd = 1.6, axes = FALSE)
        axis(4, at = pretty(c(0, max(pes$meanB))), col = "blue",
            col.axis = "blue")
        mtext(ylabel2, side = 4, line = 2.5, col = "blue", cex = 1)
        segments(x0 = -1, x1 = N05, y0 = py2[which(px == N05)],
            y1 = py2[which(px == N05)], col = "red", lty = 3,
            lwd = 1.5)
        segments(x0 = N05, x1 = N05, y0 = -1, y1 = py2[which(px ==
            N05)], col = "red", lty = 3, lwd = 1.5)
        if (!is.null(pes$currents) & mark) {
            currents <- pes$currents
            if (!is.na(currents$curr.E) & yaxis1 == "B_R" | yaxis1 ==
                "B_R.rel") {
                px1 <- ifelse(xaxis1 == "FM", currents$curr.F,
                  currents$curr.E)
                py1 <- currents$curr.B
                points(px1, py1, pch = 16, col = "grey30")
                abline(v = px1, col = "grey30", lty = 2)
            }
        }
        legend("top", legend = legend.lab, xpd = TRUE, horiz = TRUE,
            inset = c(0, 0), bty = "n", lty = c(1, 2), col = c("red",
                "goldenrod1"), seg.len = 1, pt.cex = 2, x.intersp = c(0.7,
                0.7), merge = TRUE, y.intersp = -2, box.lty = 0,
            cex = 0.8, lwd = 2)
        if (any(pes$totV != 0)) {
            py3 <- pes$totV[1:length(px)]
            par(new = TRUE)
            plot(px, py3, type = "l", axes = FALSE, ylab = "",
                xlab = "", col = "darkgreen", lwd = 1.6, ylim = c(0,
                  ceiling(max_val/dim_val) * dim_val))
            axis(4, at = pretty(c(0, pes$totV)), line = 3.6,
                col.axis = "darkgreen", col = "darkgreen")
            mtext(ylabel3, side = 4, line = 5.7, col = "darkgreen")
        }
    }
    if ("mat_FM_Lc_com.C" %in% names(pes)) {
        p.yield <- yaxis1
        p.FE <- xaxis1
        p.B <- yaxis2
        xlabel1 <- ifelse(xaxis1 == "FM", "Fishing mortality",
            "Exploitation rate")
        if (pes$FM_relative) {
            xlabel1 <- ifelse(xaxis1 == "FM", "rel. Fishing mortality",
                "rel. Exploitation rate")
        }
        ylabel_iso <- ifelse(yaxis_iso == "Lc", "Lc", "Lc / Linf")
        if (!is.na(xlab[1])) {
            xlabel1 <- xlab
        }
        if (!is.na(ylab1[1])) {
            ylabel_iso <- ylab1
        }
        Lc_change <- pes$Lc_change
        FM_change <- pes$FM_change
        if (p.FE == "FM") {
            px <- FM_change
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.F
        }
        else {
            px <- FM_change/(FM_change + pes$M)
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.E
        }
        if (p.yield == "Y_R.rel" | p.yield == "Y_R") {
            pz <- pes$mat_FM_Lc_com.Y
        }
        else if (p.yield == "B_R.rel" | p.yield == "B_R") {
            pz <- pes$mat_FM_Lc_com.B
        }
        else if (p.yield == "value") {
            pz <- pes$mat_FM_Lc_com.V
        }
        else if (p.yield == "catch") {
            pz <- pes$mat_FM_Lc_com.C
        }
        pal <- colorRampPalette(rev(c(rgb(1, 0.5, 0.5), rgb(1,
            1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5, 1))))
        if (yaxis1 == "B_R" | yaxis1 == "B_R.rel") {
            pal <- colorRampPalette(rev(c(rgb(1, 0.5, 0.5), rgb(1,
                1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5, 1))))
        }
        m <- list(x = px, y = Lc_change, z = pz)
        if ("currents" %in% names(pes))
            curr_markY <- pes$currents$curr.Lc
        if (yaxis_iso != "Lc" & "Linf" %in% names(pes)) {
            m$y <- Lc_change/pes$Linf
            if ("currents" %in% names(pes))
                curr_markY <- pes$currents$curr.Lc/pes$Linf
        }
        if (identify == TRUE) {
            dev.new(noRStudioGD = TRUE)
        }
        image(m, col = pal(100), xlab = xlabel1, ylab = ylabel_iso)
        if (is.numeric(contour)) {
            contour(m, add = TRUE, nlevels = contour)
        }
        else if (contour == TRUE) {
            contour(m, add = TRUE)
        }
        if ("currents" %in% names(pes) & mark) {
            points(x = curr_markX, y = curr_markY, pch = 16,
                col = "grey30")
            abline(v = curr_markX, col = "grey30", lty = 2)
            abline(h = curr_markY, col = "grey30", lty = 2)
        }
        if (identify == TRUE)
            image.identifier(m)
    }
    if ("list_Lc_runs" %in% names(pes) | "list_tc_runs" %in%
        names(pes)) {
        FM <- pes$FM
        if ("tc" %in% names(pes))
            if (!is.null(pes$tc))
                tc_Lc <- pes$tc
        if ("Lc" %in% names(pes))
            if (!is.null(pes$Lc))
                tc_Lc <- pes$Lc
        if (is.null(pes$tc) & is.null(pes$Lc))
            tc_Lc <- names(pes$list_Lc_runs)
        if ("list_tc_runs" %in% names(pes))
            list_tc_Lc_runs <- pes$list_tc_runs
        if ("list_Lc_runs" %in% names(pes))
            list_tc_Lc_runs <- pes$list_Lc_runs
        p.yield <- yaxis1
        p.FE <- xaxis1
        p.B <- yaxis2
        xlabel1 <- ifelse(xaxis1 == "FM", "Fishing mortality",
            "Exploitation rate")
        if (pes$FM_relative) {
            xlabel1 <- ifelse(xaxis1 == "FM", "rel. Fishing mortality",
                "rel. Exploitation rate")
        }
        ylabel1 <- ifelse(yaxis1 == "Y_R", "Y/R", "rel. Y/R")
        ylabel2 <- ifelse(yaxis2 == "B_R", "B/R", "B/R [%]")
        ylabel_iso <- ifelse(yaxis_iso == "Lc", "Lc", "Lc / Linf")
        if (!is.na(xlab[1])) {
            xlabel1 <- xlab
        }
        if (!is.na(ylab1[1])) {
            ylabel1 <- ylab1
        }
        if (!is.na(ylab2[1])) {
            ylabel2 <- ylab2
        }
        if (!is.na(ylab1[1])) {
            ylabel_iso <- ylab1
        }
        df_Es <- pes$df_Es
        if (xaxis1 == "FM") {
            N01 <- df_Es$F01
            N05 <- df_Es$F05
            Nmax <- df_Es$Fmsy
            if (length(N05) == 1) {
                legend.lab <- c("F0.1", "F0.5", "Fmsy")
            }
            else legend.lab <- c("F0.1", "Fmsy")
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.F
        }
        else {
            N01 <- df_Es$E01
            N05 <- df_Es$E05
            Nmax <- df_Es$Emsy
            if (length(N05) == 1) {
                legend.lab <- c("E0.1", "E0.5", "Emsy")
            }
            else legend.lab <- c("E0.1", "Emsy")
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.E
        }
        if (type == "ypr") {
            label <- ifelse("Lc" %in% names(pes), "Lc", "tc")
            tc_Lc_start <- which(tc_Lc == max(tc_Lc, na.rm = T))
            p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
            py <- p.dat[, which(names(p.dat) == p.yield)]
            px <- p.dat[, which(names(p.dat) == p.FE)]
            offset_text <- py[length(py)] * 0.05
            offset_x <- py[length(px)] * 0.1
            runs <- sapply(strsplit(names(list_tc_Lc_runs), split = "_"),
                "[[", 2)
            plot(px, py, type = "l", ylim = c(0, max(py, na.rm = TRUE) *
                1.2), ylab = ylabel1, xlab = xlabel1, lty = tc_Lc_start,
                ...)
            text(x = px[length(px)] - offset_x, y = py[length(py)] +
                offset_text, labels = bquote(.(label)[.(round(as.numeric(as.character(runs[tc_Lc_start])),
                2))]))
            seq_tc_Lc <- 1:length(list_tc_Lc_runs)
            seq_tc_Lc <- seq_tc_Lc[-tc_Lc_start]
            for (j in seq_tc_Lc) {
                p.dat <- list_tc_Lc_runs[[j]]
                py <- p.dat[, which(names(p.dat) == p.yield)]
                px <- p.dat[, which(names(p.dat) == p.FE)]
                lines(px, py, type = "l", ylab = ylabel1, xlab = xlabel1,
                  lty = j)
                text(x = px[length(px)], y = (py[length(py)] +
                  offset_text), labels = bquote(.(label)[.(round(as.numeric(as.character(runs[j])),
                  2))]))
            }
            if (length(N01) == 1) {
                p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
                py <- p.dat[, which(names(p.dat) == p.yield)]
                px <- p.dat[, which(names(p.dat) == p.FE)]
                if (N01 == Nmax) {
                  add_shift <- 1.005
                }
                else add_shift <- 1
                segments(x0 = -1, x1 = N01, y0 = py[which(px ==
                  N01)], y1 = py[which(px == N01)], col = "darkgreen",
                  lty = 1, lwd = 1.5)
                segments(x0 = N01, x1 = N01, y0 = -1, y1 = py[which(px ==
                  N01)], col = "darkgreen", lty = 1, lwd = 1.5)
                segments(x0 = -1, x1 = Nmax, y0 = py[which(px ==
                  Nmax)] * add_shift, y1 = py[which(px == Nmax)] *
                  add_shift, col = "goldenrod1", lty = 2, lwd = 1.5)
                segments(x0 = Nmax * add_shift, x1 = Nmax * add_shift,
                  y0 = -1, y1 = py[which(px == Nmax)], col = "goldenrod1",
                  lty = 2, lwd = 1.5)
                if (length(N05) == 1) {
                  legend("top", legend = legend.lab, xpd = TRUE,
                    horiz = TRUE, inset = c(0, 0), bty = "n",
                    lty = c(1, 3, 2), col = c("darkgreen", "red",
                      "goldenrod1"), seg.len = 1, pt.cex = 2,
                    x.intersp = c(0.7, 0.7, 0.7), merge = TRUE,
                    y.intersp = -2, box.lty = 0, cex = 0.8, lwd = 2)
                }
                else {
                  legend("top", legend = legend.lab, xpd = TRUE,
                    horiz = TRUE, inset = c(0, 0), bty = "n",
                    lty = c(1, 2), col = c("darkgreen", "goldenrod1"),
                    seg.len = 1, pt.cex = 2, x.intersp = c(0.7,
                      0.7, 0.7), merge = TRUE, y.intersp = -2,
                    box.lty = 0, cex = 0.8, lwd = 2)
                }
                if (!is.null(pes$currents)) {
                  currents <- pes$currents
                  if (!is.na(currents$curr.E)) {
                    px <- ifelse(p.FE == "FM", currents$curr.F,
                      currents$curr.E)
                    py <- ifelse(p.yield == "Y_R", currents$curr.YPR,
                      currents$curr.YPR.rel)
                    points(px, py, pch = 16)
                    abline(v = px, col = "grey30", lty = 2)
                  }
                }
            }
            par(new = T)
            px <- list_tc_Lc_runs[[1]][, which(names(list_tc_Lc_runs[[1]]) ==
                p.FE)]
            py <- list_tc_Lc_runs[[1]][, which(names(list_tc_Lc_runs[[1]]) ==
                p.B)]
            plot(px, py, type = "l", axes = F, ylab = "", xlab = "",
                lty = tc_Lc_start, col = "blue")
            axis(side = 4, at = pretty(range(py, na.rm = TRUE)),
                col = "blue", col.axis = "blue")
            mtext(side = 4, text = ylabel2, line = 3, col = "blue")
            for (j in seq_tc_Lc) {
                p.dat <- list_tc_Lc_runs[[j]]
                py <- p.dat[, which(names(p.dat) == p.B)]
                px <- p.dat[, which(names(p.dat) == p.FE)]
                lines(px, py, type = "l", ylab = ylabel1, xlab = xlabel1,
                  col = "blue", lty = j)
            }
            if (length(N05) == 1) {
                p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
                px <- p.dat[, which(names(p.dat) == p.FE)]
                py2 <- p.dat[, which(names(p.dat) == p.B)]
                segments(x0 = -1, x1 = N05, y0 = py2[which(px ==
                  N05)], y1 = py2[which(px == N05)], col = "red",
                  lty = 3, lwd = 1.5)
                segments(x0 = N05, x1 = N05, y0 = -1, y1 = py2[which(px ==
                  N05)], col = "red", lty = 3, lwd = 1.5)
            }
        }
        if (type == "Isopleth") {
            tc_Lc_start <- which(tc_Lc == max(tc_Lc, na.rm = T))
            p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
            px <- p.dat[, which(names(p.dat) == p.FE)]
            if (length(N01) > 1) {
                Lc_change <- pes$Lc
                list_Lc_runs <- pes$list_Lc_runs
                Y_R.vec <- vector("list", length(list_Lc_runs))
                for (fx in 1:length(list_Lc_runs)) {
                  yr <- list_Lc_runs[[fx]][, which(names(list_Lc_runs[[fx]]) ==
                    p.yield)]
                  Y_R.vec[[fx]] <- yr
                }
                mat_FM_Lc_com.Y <- as.matrix(do.call(cbind, Y_R.vec))
                rownames(mat_FM_Lc_com.Y) <- round(px, digits = 2)
                colnames(mat_FM_Lc_com.Y) <- round(Lc_change/pes$Linf,
                  digits = 2)
                m <- list(x = px, y = Lc_change, z = mat_FM_Lc_com.Y)
                if ("currents" %in% names(pes))
                  curr_markY <- pes$currents$curr.Lc
                if (yaxis_iso != "Lc" & "Linf" %in% names(pes)) {
                  m$y <- Lc_change/pes$Linf
                  if ("currents" %in% names(pes))
                    curr_markY <- pes$currents$curr.Lc/pes$Linf
                }
                pal <- colorRampPalette(rev(c(rgb(1, 0.5, 0.5),
                  rgb(1, 1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5,
                    1))))
                if (yaxis1 == "B_R" | yaxis1 == "B_R.rel") {
                  pal <- colorRampPalette(rev(c(rgb(1, 0.5, 0.5),
                    rgb(1, 1, 0.5), rgb(0.5, 1, 1), rgb(0.5,
                      0.5, 1))))
                }
                if (identify == TRUE) {
                  dev.new(noRStudioGD = TRUE)
                }
                image(m, col = pal(100), xlab = xlabel1, ylab = ylabel_iso)
                if (is.numeric(contour)) {
                  contour(m, add = TRUE, nlevels = contour)
                }
                else if (contour == TRUE) {
                  contour(m, add = TRUE)
                }
                if ("currents" %in% names(pes) & mark) {
                  points(x = curr_markX, y = curr_markY, pch = 16,
                    col = "grey30")
                  abline(v = curr_markX, col = "grey30", lty = 2)
                  abline(h = curr_markY, col = "grey30", lty = 2)
                }
                if (identify == TRUE)
                  image.identifier(m)
            }
        }
    }
}
