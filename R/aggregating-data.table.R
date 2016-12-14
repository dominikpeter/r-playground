
rm(list=ls())

set.seed(2323)

library(data.table)
col1 = rep(c("a","b", "c", "a"), 1000000)
col2 = rpois(1000000*4, 25)
col3 = rep(c("hans", "fritz", "meier", "rolf"), 1000000)
col4 = rnorm(1000000*4)

dt <- data.table(col1, col2, col3, col4)
nbr <- colnames(dt)[vapply(dt, is.numeric, logical(1))]
string <- colnames(dt)[vapply(dt, is.character, logical(1))]

dt[, lapply(.SD,sum), by = eval(string), .SDcols = nbr]

