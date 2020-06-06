##############################
### State-space model for aggregated time-series trends across stocks
### Individual stocks are treated as observations around mean trend
### Model code by Coilin Minto, modifications by Mike Melnychuk
###
### This file is part of an RStudio project, "state-space mean trend Nature"
### Analysis will reconstruct Figure 2 and Extended Data Figure 1 panels for
### the Matters Arising commentary, in review at Nature, titled:
### "What is the trend in abundance of fish stocks?"
###
### The following packages will be loaded, installing first if needed:
### "plyr", "ggplot2", "ggpubr", "TMB", "reshape", "devEMF", "matrixStats"
###
### INSTRUCTIONS:
###
### From the Zenodo repository at https://doi.org/10.5281/zenodo.3676087 ,
###   download and unzip file "RAMLDB v4.491.zip", then copy the file:
###   ..RAMLDB v4.491\DB Files With Assessment Data\R Data\"DBdata[asmt][v4.491].RData"
###   and paste file into the following sub-directory in this project's root directory:
###   ..\data-in\RAM4-491_asmt_2020-01-10\
###
### From the Zenodo repository at https://doi.org/10.5281/zenodo.3877544 ,
###   download and unzip file "RAMLDB v4.491 (extended).zip", then copy the file:
###   ..DB Files With Model Fit Data\R Data\"DBdata[mdl][v4.491].RData"
###   and paste file into the following sub-directory in this project's root directory:
###   ..\data-in\RAM4-491_mdl_2020-01-10\
###
### Select 8 options below and then source this file
###
##############################



# USER OPTIONS -----------------------------------------------------------------

## 1) select one weighting variable for mean trends and medians of individual stocks
  wt <- "eq"  # equal weighting
  # wt <- "avb"  # prefer mean TB, otherwise mean SSB
  # wt <- "avc"  # mean catch, excluding leading zeros
  # wt <- "msy"  # prefer MSY, otherwise mean catch

## 2) select assessment-only or model-fit type of RAM Legacy Database
  ramtype <- "asmt"
  # ramtype <- "mdl"

## 3) select whether to prioritise MSY-based or mgt-based target reference points
  vtype <- "msy"
  # vtype <- "mgt"

## 4) specify start year for analysis and plots
  fy <- 1970

## 5) optionally specify end year for analysis and plots
  ly <- 2018  # leave blank to avoid cutting off dataset for analysis

## 6) select whether to show or suppress ggplot warnings during plotting
  # show.ggwarn <- TRUE
  show.ggwarn <- FALSE

## 7) select whether to show progress counters on console
  show.prog <- TRUE
  # show.prog <- FALSE

## 8) select whether TMB file requires compiling before running analysis
        # (only need to run once to generate dynamic library files)
  # compileTMB <- TRUE  # if it has not yet been compiled on your machine
  compileTMB <- FALSE  # if it has already been compiled on your machine



# SET OTHER (FIXED) SPECIFICATIONS ---------------------------------------------
  grp <- "nog"  # no grouping variable" (i.e. "global" mean)
  plot_vars <- c("Bv", "Uv")
  ramvers <- "RAM4-491"
  covthr <- 0.9  # threshold for minimum coverage, for re-scaling state-space mean
                 # to median in years of high coverage)
  date <- format(Sys.Date(), "%Y-%m-%d")



# LOAD DATA AND PACKAGES -------------------------------------------------------

## load RAM data
  # directory of RAM data to use (folder name within "data-in" folder)
  if (ramtype == "mdl")  ramdir <- "RAM4-491_mdl_2020-01-10"
  if (ramtype == "asmt")  ramdir <- "RAM4-491_asmt_2020-01-10"
  # name of RAM data file to use (within ramdir folder)
  if (ramtype == "mdl")  ramdata <- "DBdata[mdl][v4.491].RData"
  if (ramtype == "asmt")  ramdata <- "DBdata[asmt][v4.491].RData"

  if (grepl(pattern = ramtype, x = ramdir) == TRUE &
      grepl(pattern = ramvers, x = ramdir) == TRUE) {
    load(paste0("./data-in/", ramdir, "/", ramdata))
    tv <- timeseries_values_views
    tid <- timeseries_ids_views
    tso <- timeseries_sources_views
    pv <- bioparams_values_views
    rm(area, assessment, assessmethod, assessor, biometrics, management, metadata,
       tsmetrics, bioparams, timeseries,
       bioparams_assessments_views, bioparams_ids_views, bioparams_units_views,
       bioparams_notes_views, bioparams_sources_views, bioparams_values_views,
       timeseries_assessments_views, timeseries_ids_views, timeseries_units_views,
       timeseries_notes_views, timeseries_sources_views, timeseries_years_views,
       timeseries_values_views,
       diver.data, diver.mgt.data, divf.data, divf.mgt.data,
       divssb.data, divssb.mgt.data, divtb.data, divtb.mgt.data, divtn.data, divtn.mgt.data,
       divupref.data, divupref.mgt.data, divbpref.data, divbpref.mgt.data,
       cdivmeanc.data, cdivmsy.data,
       tcbest.data, tc.data, tl.data, cadv.data, cpair.data, recc.data, tac.data,
       survb.data, effort.data, cpue.data, r.data,
       tbbest.data, ssb.data, tb.data, tn.data, erbest.data, er.data, f.data)
  } else {
    stop("Ensure that ramtype, ramvers, and ramdir all correspond in user options")
  }


## check if packages already installed, and if not, install
packages <- c("plyr", "ggplot2", "ggpubr", "TMB", "reshape", "devEMF", "matrixStats")
InstallPackages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE, logical.return = TRUE)
}
InstallPackages(packages)
rm(packages, InstallPackages, ramdata)

## set ggplot theme
  theme_set(theme_bw())

## helper function for filename descriptor tags
file_info <- function() {
  paste0("wt-", wt,
         "_", ramvers,
         "-", ramtype,
         "_BUv-", vtype,
         "_", date)
}

## compile TMB code, with AR(1) on the residuals
if (compileTMB == TRUE)  compile("dlm_ar1w.cpp")
rm(compileTMB)



# SOURCE OTHER FILES -----------------------------------------------------------
source("2_make-data.R")
source("3_fit-models.R")
source("4_plot-figs.R")
