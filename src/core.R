source("covariate.R")

data <- read.dta("../data/restat_data.dta")

## national time index and crisis dummies
T <- max(data[["time"]])
crisis.mat <- data[1:T, c("crisis1", "crisis2", "crisis3")]

## example income measure
inc <- data[1:T, c("income")]


