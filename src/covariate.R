library(foreign)
library(stats)

data <- read.dta("../data/restat_data.dta")

## national time index and crisis dummies
T <- max(data[["time"]])
crisis.mat <- data[1:T, c("crisis1", "crisis2", "crisis3")]

## example income measure
inc <- data[1:T, c("income")]

body <- function(string) {
  ## returns the body of the supplied string, before the period
  x <- strsplit(string, "\\.")
  return(x[[1]][1])
}

idx <- function(string) {
  ## returns the numeric index of the supplied string, after the
  ## period
  x <- strsplit(string, "\\.")
  return(as.numeric(x[[1]][2]))
}

time.covariate <- function(time.opt = "linear.trend") {
  ## accepts an option to specify the method to account for temporal
  ## heterogeneity, and returns the covariate matrix that will be used
  ## in the forecasting regression.

  ## create time index, which is common for each state
  t <- 1:T
  
  ## return the time indicator for a linear trend
  if (time.opt == "linear.trend") {
    return(cbind(t, crisis.mat))
  }

  ## return the log of the time index for a logarithmic trend
  else if (time.opt == "log.trend") {
    return(cbind(log(t), crisis.mat))
  }

  ## return a matrix of dummies, indicating each time index for year
  ## fixed effects, noting that there are no repeated years for each
  ## state (e.g., not monthly observation)
  else if (time.opt == "year.fe") {
    return(diag(length(t)))
  }

  ## return nothing if the option is invalid
  else {
    return()
  }
}

income.covariate <- function(income.var, opt = "spline.3") {
  ## accepts the income variable and returns the matrix of the income
  ## covariate

  ## returns the spline of the supplied income variable with the
  ## specified number of segments
  if (body(opt) == "spline") {
    ss <- smooth.spline(income.var, nknots = idx(opt) + 1)
    return(matrix(ss$y))
  }

  ## returns the polynomial matrix of specified degree
  else if (body(opt) == "poly") {
    n <- idx(opt)
    return(sapply(1:n, function(x) matrix(income.var^x)))
  }
}

popdens.covariate <- function(popdens.var, opt = "poly.2") {
  ## population density polynomial matrix
  n <- idx(opt)
  return(sapply(1:n, function(x) matrix(popdens.var^x)))
}
