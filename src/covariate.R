library(foreign)
library(stats)

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

time.covariate <- function(time.var, crisis.mat, opt = "linear.trend") {
  ## accepts an option to specify the method to account for temporal
  ## heterogeneity, and returns the covariate matrix that will be used
  ## in the forecasting regression.

  ## return the time indicator for a linear trend
  if (opt == "linear.trend") {
    return(cbind(time.var, crisis.mat))
  }

  ## return the log of the time index for a logarithmic trend
  else if (opt == "log.trend") {
    return(cbind(log(time.var), crisis.mat))
  }

  ## return a matrix of dummies, indicating each time index for year
  ## fixed effects, noting that there are no repeated years for each
  ## state (e.g., not monthly observation)
  else if (opt == "year.fe") {
    return(diag(length(time.var)))
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
