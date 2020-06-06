##----------------------
## Prepare data and grouping structure
##----------------------


variables <- c("BdivBmsypref", "UdivUmsypref", "BdivBmgtpref", "UdivUmgtpref")

# build stock table
st <- data.frame(stockid = unique(tv$stockid), row.names = unique(tv$stockid),
                 stringsAsFactors = FALSE)
st <- merge(y = stock[, c("stockid", "stocklong", "scientificname", "region",
                          "primary_country", "primary_FAOarea", "state")],
            x = st, by = "stockid", all.x = TRUE, all.y = FALSE, sort = FALSE)
st <- merge(x = st, y = taxonomy[, c("scientificname", "family", "taxGroup")],
            by = "scientificname", all.x = TRUE, all.y = FALSE, sort = FALSE)
tv <- tv[tv$stockid %in% unique(st$stockid), ]
tid <- tid[tid$stockid %in% unique(st$stockid), ]
pv <- pv[pv$stockid %in% unique(st$stockid), ]
st$grp <- "all"
grps <- unique(st$grp)
rm(stock, taxonomy)

# set up weighting variable
if (wt == "eq")  st$wvar <- 1
if (wt != "eq") {
  if (wt == "avb") {
    avtb <- aggregate(x = tv[, "TBbest"], by = list(tv$stockid),
                             FUN = "mean", na.rm = TRUE)
    stocks_ssbmt <- tid[!is.na(tid$SSB) & tid$SSB == "SSB-MT", "stockid"]
    avssb <- aggregate(x = tv[, "SSB"], by = list(tv$stockid),
                             FUN = "mean", na.rm = TRUE)
    avssb[!(avssb$Group.1 %in% stocks_ssbmt), "x"] <- NA
    wts <- avtb
    wts[wts$x == "NaN", ] <- avssb[wts$x == "NaN", ]
    wts[is.na(wts$x), ] <- avssb[is.na(wts$x), ]
    names(wts) <- c("stockid", "wvar")
    rm(avtb, avssb, stocks_ssbmt)
}
  if (wt == "avc" | wt == "msy") {
    for (s in unique(tv$stockid)) {   # remove leading zeros in catch
      i = min(which(tv$stockid == s), na.rm = TRUE)
      if (!is.na(tv[i, "TCbest"]) & tv[i, "TCbest"] == 0) {
        while (!is.na(tv[i, "TCbest"]) & tv[i, "TCbest"] == 0) {
          tv[i, "TCbest"] <- NA
          i = i + 1
        }
      }
    }
    wts <- aggregate(x = tv[, "TCbest"], by = list(tv$stockid),
                     FUN = "mean", na.rm = TRUE)
    names(wts) <- c("stockid", "wvar")
    if (wt == "msy" | wt == "mslv") {
      wts <- merge(x = wts, y = pv[, c("stockid", "MSYbest")],
                   by = "stockid", all = TRUE, sort = FALSE)
      wts[!is.na(wts$MSYbest), "wvar"] <- wts[!is.na(wts$MSYbest), "MSYbest"]
      wts$MSYbest <- NULL
      names(wts) <- c("stockid", "wvar")
    }
    rm(s)
  }
  wts$wvar <- wts$wvar / median(wts$wvar, na.rm = TRUE)
  st <- merge(x = st, y = wts, by = "stockid",
              all.x = TRUE, all.y = FALSE, sort = FALSE)
  rm(wts)
}
rm(tid)

# create dataframe objects from the timeseries views table
grp.df <- st[, c("stockid", "grp", "wvar")]
for (i in 1:length(variables)) {
  if (isTRUE(show.prog)) print.noquote(variables[i])
  var <- variables[i]
  df <- tv[, c("stockid", "year", var)]
  df <- merge(x = df, y = grp.df, by = "stockid",
              all.x = TRUE, all.y = FALSE, sort = FALSE)
  df <- df[with(df, order(grp, stockid, year)), ]
  assign(paste0(var, ".df"), df)
  rm(list = c("var", "df"))
}
rm(grp.df, i)

## generalize msy-based and mgt-based ratios for remainder of code
if (vtype == "msy") {
  Bv.df <- BdivBmsypref.df
  Uv.df <- UdivUmsypref.df
}
if (vtype == "mgt") {
  Bv.df <- BdivBmgtpref.df
  Uv.df <- UdivUmgtpref.df
}
colnames(Bv.df)[3] <- "Bv"
colnames(Uv.df)[3] <- "Uv"
rm(BdivBmsypref.df, BdivBmgtpref.df, UdivUmsypref.df, UdivUmgtpref.df, variables)

## subset the data based on specified first year
Bv.df <- na.omit(subset(Bv.df, year >= fy))
Uv.df <- na.omit(subset(Uv.df, year >= fy))

## subset the data based on optionally-specified last year
if (exists("ly")) {
  if (ly <= fy) {
    stop("Last year (ly) must be after first year (fy).")
  } else {
    Bv.df <- na.omit(subset(Bv.df, year <= ly))
    Uv.df <- na.omit(subset(Uv.df, year <= ly))
    Bv.df <- Bv.df[with(Bv.df, order(grp, stockid, year)), ]
    Uv.df <- Uv.df[with(Uv.df, order(grp, stockid, year)), ]
  }
}

## scale catch to longterm average
Cbar.df <- data.frame(stockid = st$stockid, grp = st$grp, Cbar = as.numeric(NA),
                      stringsAsFactors = FALSE)
tv2 <- na.omit(tv[with(tv, order(stockid, year)), c("stockid", "year", "TCbest")])
tv2 <- tv2[tv2$year >= fy, ]
if (exists("ly"))  tv2 <- tv2[tv2$year <= ly, ]
tmp <- ddply(tv2, .(stockid), function(x) {
  cbar <- mean(x$TCbest, na.rm = TRUE)
  Cbar.df[Cbar.df$stockid == unique(x$stockid), "Cbar"] <<- cbar
})
rm(tmp, tv2)

## sum up mean catch by group
sumMCgrp <- aggregate(formula = Cbar ~ grp, data = Cbar.df, FUN = "sum")
names(sumMCgrp) <- c("grp", "sumCbar")

## get coverage (percentage of stocks present per year)
for (i in plot_vars) {
  # subset for given variable
  var.df <- get(paste0(i, ".df"))
  # count by grouping variable and year
  stock.count <- with(var.df, aggregate(as.formula(paste(i, "~ grp + year")),
                                        FUN = length))
  stock.count <- stock.count[with(stock.count, order(grp, year)), ]
  names(stock.count)[names(stock.count) == i] <- "N"
  # total number of stocks by grouping variable
  stock.total <- aggregate(stockid ~ grp, data = unique(var.df[, c("grp", "stockid")]),
                           FUN = length)
  names(stock.total)[names(stock.total) == "stockid"] <- "Ntotal"
  stock.count <- merge(stock.count, stock.total)
  stock.count$Coverage <- with(stock.count, N / Ntotal)
  assign(paste0(i, ".coverage"), stock.count)
}
rm(var.df, stock.count, stock.total)

## plot coverage
ggplot(Bv.coverage, aes(x = year, y = Coverage)) + geom_point() + facet_wrap(~ grp) +
    geom_point(data = Uv.coverage, colour = "red")

rm(pv, tso, i, tv)


if (isTRUE(show.prog)) print.noquote("2_make-data.R complete")
