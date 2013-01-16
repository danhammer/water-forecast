library(foreign)
data <- read.dta("../data/restat_data.dta")

dim(data)

sub.data <- data[ ,c("state", "year", "income", "popdens")]
d <- sub.data[sub.data$year < 1980, ]

crisis.mat <- data[ ,c("crisis1", "crisis2", "crisis3")]

time.covariate(time, crisis.mat)
income.covariate(income)[1:50]

ddply(d, .(state), summarize, mean = income.covariate(income))
