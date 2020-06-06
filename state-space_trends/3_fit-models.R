##----------------
## Fit time series models to relative B and relative U
##----------------


## load the TMB function
dyn.load(dynlib("dlm_ar1w"))

get.trend <- function(grp.val, variable) {
    dat <- get(paste0(variable, ".df"))
    dat <- na.omit(dat)
    dat <- subset(dat, grp == grp.val)
    dat <- dat[order(dat$stockid, dat$year), ]
    dat$fyear <- factor(dat$year)
    dat$lnvar <- log(dat[, variable] + 0.001)
    dat <- droplevels(dat)
    ## coverage
    coverage.df <- get(paste0(variable, ".coverage"))
    coverage.df <- subset(coverage.df, grp == grp.val)
    ##-----
    ## LM
    ##-----
    dat$stockid2 <- dat$stockid
    lm.fit <- lm(lnvar ~ stockid + factor(year), data = dat, weights = wvar)
    coef.lm <- coef(lm.fit)
    lm.effects <- c(coef.lm["(Intercept)"], coef.lm["(Intercept)"] +
                      coef.lm[grep("year", names(coef.lm))])
    ##-----
    ## TMB
    ##-----
    ## reshape the data for tmb
      dat.wide_y <- reshape(dat[, c("stockid", "year", "lnvar")],
                          idvar = "stockid", timevar = "year", direct = "wide")
      dat.wide_w <- reshape(dat[, c("stockid", "year", "wvar")],
                          idvar = "stockid", timevar = "year", direct = "wide")
      rownames(dat.wide_y) <- dat.wide_y[, "stockid"]
      rownames(dat.wide_w) <- dat.wide_w[, "stockid"]
      dat.wide_y <- dat.wide_y[, names(dat.wide_y) != "stockid"]
      dat.wide_w <- dat.wide_w[, names(dat.wide_w) != "stockid"]
      y <- as.matrix(dat.wide_y)
      w <- as.matrix(dat.wide_w)
      if (FALSE %in% (dim(y) == dim(w))) {
        stop("Matrices of values (y) and weights (w) must have same dimensions")
      }
      if (sum(is.na(y)) != sum(is.na(w))) {
        stop("Matrices of values (y) and weights (w) must have same number of NAs")
      }
      if (FALSE %in% unique(is.na(y) == is.na(w))) {
        stop("Matrices of values (y) and weights (w) must have same locations of NAs")
      }

    ## make sure ordered
    y <- y[, order(as.numeric(gsub("lnvar.", "", colnames(y))))]
    if (!all(order(colnames(y)) == 1:ncol(y))) {
        stop("y matrix not ordered correctly")
    }
    w <- w[, order(as.numeric(gsub("wvar.", "", colnames(w))))]
    if (!all(order(colnames(w)) == 1:ncol(w))) {
      stop("weights matrix not ordered correctly")
    }
    n <- ncol(y)
    m <- nrow(y)
    ypresent <- ifelse(is.na(y), 0, 1)
    first.obs <- apply(ypresent, 1, which.max) - 1 ## -1 for start at zero in TMB
    ## create the AD object
    data_list = list(y = y,
                     w = w,
                     ypresent = ypresent,
                     first_obs = first.obs)
    dll_use <- "dlm_ar1w"
    obj <- MakeADFun(
      data = data_list,
      parameters = list(
          lnsde = log(0.1),
          lnsdx = log(0.1),
          logitrho = -log(2/(1 + 0.5) - 1), ## for AR(1) = 0.5
          x = rep(0, n),
          Apar = rep(0, m - 1)
      ),
      random = c("x"),
      DLL = dll_use,
      silent = TRUE)
    ## fit the model
    opt <- nlminb(objective = obj$fn,
                  gradient = obj$gr,
                  start = obj$par,
                  lower = c(lnsde = log(0.05), lnsdx = log(0.05)),
                  control = list(iter.max = 1e9, eval.max = 1e9),
                  verbose = TRUE)
    # summary(opt)
    if (opt$convergence == 0 | opt$message == "relative convergence (4)") {
        ## report
        rep <- sdreport(obj)
        srep <- summary(rep)
        xhat <- srep[rownames(srep) == "x", ]
        rownames(xhat) <- NULL
        xhat <- as.data.frame(xhat)
        ## finite population correction
        xhat$grp <- grp.val
        xhat$year <- as.numeric(gsub("lnvar.", "", colnames(y)))
        xhat <- merge(xhat, coverage.df)
        xhat <- xhat[order(xhat$year), ]
        years <- xhat$year
        coverage <- xhat$Coverage
        N_year <- xhat$N
        N_total <- xhat$Ntotal
        ## finite-population corrected
        xhat$fpc.se <- xhat[, "Std. Error"] * with(xhat, sqrt((Ntotal - N)/(Ntotal - 1)))
        dlm.geomean <- exp(xhat[, "Estimate"])
        dlm.upper <- exp(xhat[, "Estimate"] + 1.96 * xhat[, "fpc.se"])
        dlm.lower <- exp(xhat[, "Estimate"] - 1.96 * xhat[, "fpc.se"])
    } else {
        na.vec <- rep(NA, n)
        years <- coverage <- dlm.geomean <- dlm.upper <- dlm.lower <- na.vec
    }
    ## predictions
    pred.df <- data.frame(grp = grp.val,
                          variable = variable,
                          year = years,
                          Coverage = coverage,
                          n_year = N_year,
                          n_total = N_total,
                          dlm.geomean = dlm.geomean,
                          dlm.lower = dlm.lower,
                          dlm.upper = dlm.upper,
                          fixed.effects = exp(as.numeric(lm.effects)),
                          stringsAsFactors = FALSE)
    pred.df <- pred.df[order(pred.df$year), ]
    ## re-scale to median of full coverage years
    box <- boxplot(as.formula(paste(variable, "~ grp + year")), data = dat, plot = FALSE)
    stats.df <- as.data.frame(t(box$stats))
    names(stats.df) <- c("lower.whisker", "q.25", "median", "q.75", "upper.whisker")
    stats.df$year <- as.numeric(gsub(paste0(grp.val, "."), "", box$name))
    if (wt != "eq") {
      wt_meds <- data.frame(year = min(dat$year):max(dat$year))
      wt_meds$median <- as.numeric(NA)
      for (yr in 1:length(wt_meds$year)) {
        var_yr <- dat[dat$year == wt_meds$year[yr], c(variable, "wvar")]
        wtmed_yr <- weightedMedian(var_yr[, variable], w = var_yr[, "wvar"])
        wt_meds[yr, "median"] <- wtmed_yr
      }
      stats.df$unwt.median <- stats.df$median
      if (all(stats.df$year == wt_meds$yr)) {
        stats.df$median <- wt_meds$median
      } else {
        stop("years do not align in stats.df and wt_meds")
      }
    }
    ## link to coverage to get scaling
    median.df <- stats.df[, c("year", "median")]
    median.df <- merge(median.df, coverage.df)
    median.df <- subset(median.df, grp == grp.val & Coverage > covthr)
    tmp <- merge(pred.df, median.df)
    ## scale
    scale.dlm <- with(tmp, sum(median) / sum(dlm.geomean))
    pred.df[, c("dlm.geomean", "dlm.lower", "dlm.upper")] <- scale.dlm *
      pred.df[, c("dlm.geomean", "dlm.lower", "dlm.upper")]
    ## scale fixed effects
    scale.fixed <- with(tmp, sum(median) / sum(fixed.effects))
    pred.df[, "fixed.effects"] <- scale.fixed * pred.df[, "fixed.effects"]
    pred.df <- merge(pred.df, stats.df)
    pred.df <- pred.df[order(pred.df$year), ]
    return(pred.df)
}

## container for estimates
est.df <- NULL
for (i in 1:length(plot_vars)) {
    for (j in 1:length(grps)) {
        if (isTRUE(show.prog))  print.noquote(paste(plot_vars[i], ",", grps[j]))
        tmp <- get.trend(grp.val = grps[j], variable = plot_vars[i])
        est.df <- rbind(est.df, tmp)
    }
}

## data for barplots
  Bv.df <- merge(Bv.df, Bv.coverage)
  Uv.df <- merge(Uv.df, Uv.coverage)

## to long format for legend
est.long.df <- melt(est.df[, c("year", "grp", "variable", "dlm.geomean",
                               "fixed.effects", "median")],
                    id.vars = c("year", "grp", "variable"),
                    variable_name = "method")
est.long.df$Method <- NA
est.long.df$Method[est.long.df$method == "dlm.geomean"] <- "State space model"
est.long.df$Method[est.long.df$method == "fixed.effects"] <- "Fixed effects"
est.long.df$Method[est.long.df$method == "median"] <- "Median"
names(est.long.df)[names(est.long.df) == "value"] <- "Estimate"

## subset for median and dlm
est.long.df <- subset(est.long.df, method %in% c("dlm.geomean", "median"))
est.long.df <- droplevels(est.long.df)

## output a csv
write.csv(est.df,
          file = paste0("./data-out/state-space-results_", file_info(), ".csv"),
          row.names = FALSE)


if (isTRUE(show.prog)) print.noquote("3_fit-models.R complete")
