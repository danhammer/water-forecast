source("covariate.R")

data <- read.dta("../data/restat_data.dta")

## national time index and crisis dummies
T <- max(data[["time"]])
crisis.mat <- data[1:T, c("crisis1", "crisis2", "crisis3")]

## example income measure
inc <- data[1:T, c("income")]


time.opts <- c("linear.trend", "quad.trend", "year.fe")

income.opts <- c("spline.3", "spline.4", "spline.5", "spline.6", "spline.7",
                 "spline.8", "spline.9", "spline.10", "poly.1", "poly.2",
                 "poly.3", "poly.4", "poly.5")

popdens.opts <- c("poly.1", "poly.2")

for (t in time.opts) {
  for (i in income.opts) {
    for (p in popdens.opts) {
      print(cat(t, i, p))
    }
  }
}
