library(foreign)
library(plm)
library(combinat)
library(plyr)


## All permutations selecting one element from each of the provided
## lists.

c1 <- c(0,1,2,3)
c2 <- c(8,9)
full.list <- list(c1=c1, c2=c2, c3)

for (col in full.list) {
  print(col)
  for (i in col) {
    print(i)
  }
}









data <- read.dta("../data/restat_data.dta")

cofactor.names <- c("income", "popdens", "oil", "coal", "coastal", "hdd", "pop")

data[sample(1:2100,10), ]

cofactor.matrix <- function(col.names, full.data = data) {
  as.matrix(cbind(1, full.data[ , col.names]))
}

combo.list <- function(num.cofactors, cofactor.names) {
  name.mat <- apply(combn(cofactor.names, num.cofactors), 1, cbind)
  alply(name.mat, 1)
}

col.combo <- combo.list(2, cofactor.names)
lapply(col.combo, cofactor.matrix)

linear.reg <- function(X, y = data[,"co2p"]) {
  head(y)
}
