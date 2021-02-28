
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
    Lmin = NA, Lincr = NA, plot = FALSE, mark = TRUE, hide.progressbar = FALSE){

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
            if (!hide.progressbar) {
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
                  if (!hide.progressbar) {
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
