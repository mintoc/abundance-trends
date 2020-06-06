##----------------------
## Plot figures from state-space model fits
##----------------------


### PLOTTING FUNCTIONS ---------------------------------------------------------

## single-panel figure for each variable
plot_1var <- function(i) {
  var.df <- NULL
  if (variables[i] == "Bv") var.df <- Bv.df
  if (variables[i] == "Uv") var.df <- Uv.df

  fig <- ggplot(var.df, aes(x = year, y = get(variables[i]))) +
    geom_boxplot(aes(group = cut_width(year, 1), fill = Coverage, colour = Coverage),
                 outlier.shape = NA, size = 0.2) +
    coord_cartesian(ylim = c(0, 4), xlim = c(fy, ly), clip = "off") +
    scale_fill_gradient(name = NULL, low = "#56B1F7", high = "#132B43",
                        limits = c(0, 1)) +
    scale_colour_gradient(name = NULL, low = "#56B1F7", high = "#132B43",
                          limits = c(0, 1)) +
    geom_ribbon(data = subset(est.df, variable == variables[i]),
                aes(ymin = dlm.lower, ymax = dlm.upper, y = dlm.geomean),
                fill = "darkorange", alpha = 0.4) +
    geom_line(data = subset(est.df, variable == variables[i]),
              aes(y = dlm.geomean, linetype = "State-space model prediction"),
              color = "darkorange", size = 0.5) +
    geom_point(data = subset(est.long.df, variable == variables[i] &
                               !method %in% c("dlm.geomean")),
               aes(y = Estimate), colour = "red", size = 0.5) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0.01, 0.01, 0, 0.01), "in"),
          legend.position = "none") +
    geom_hline(yintercept = 1, colour = "lightgrey", linetype = 1) +
    ylab(labs[i])
  return(fig)
}



### PLOTTING COMMANDS ----------------------------------------------------------

variables <- plot_vars
dflist <- list(Bv.df, Uv.df)
wid <- 2.95  # 3.75
ht <- 2.95  # 2.25

if (vtype == "msy") {
  labs <- c(expression("Relative biomass " * "(B/" * B[MSY] * ")"),
            expression("Relative fishing pressure " * "(U/" * U[MSY] * ")"))
}
if (vtype == "mgt") {
  labs <- c(expression("Relative biomass " * "(B/" * B[target] * ")"),
            expression("Relative fishing pressure " * "(U/" * U[target] * ")"))
}

## plot single-panel figure
for (i in 1:length(variables)) {
  if (isTRUE(show.ggwarn)) {
    fig.list <- plot_1var(i = i)
    ggexport(fig.list, height = ht, width = wid, filename = paste0(
      "./figs/trends_", variables[i], "_", file_info(), ".pdf"))
  }
  if (!isTRUE(show.ggwarn)) {
    fig.list <- suppressWarnings(plot_1var(i = i))
    suppressMessages(ggexport(fig.list, height = ht, width = wid,
                              filename = paste0("./figs/trends_", variables[i], "_",
                                                file_info(), ".pdf")))
  }
}

if (isTRUE(show.prog)) print.noquote("4_plot-figs.R complete")
