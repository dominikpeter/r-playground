
# -------------------------------------------------------------------------------------------------------------------
# Crossvalidation
# -------------------------------------------------------------------------------------------------------------------
rm(list=ls())
library(gapminder)
library(magrittr)
library(broom)
library(ggplot2)
library(data.table)
library(snow)
library(microbenchmark)
library(foreign)
library(stringr)
library(caret)




raw_df <- read.dta("http://www.farys.org/daten/ebay.dta") %>% as.data.table()
raw_df[, rating := sepos/rowSums(.SD), .SDcols = c("sepos", "seneg")]
raw_df[, `:=` (makellos = factor(rating > .98, levels = c(TRUE, FALSE), labels = c("Ja", "Nein")),
               cat = str_replace(subcat, "\\ \\(\\d+\\)", ""))]


bootstrap <- function(x, ...) UseMethod()


bootstrap <- function(df,
                      n = 1000,
                      partition = .8,
                      nested = TRUE,
                      .idcol = "id",
                      ...) 
  {
  .exp <- substitute(expression(.idcol))
  nrow <- nrow(df)
  
  sample_df <- function(n) {
    df[sample(nrow*partition, replace = FALSE), ]
  }
  
  boot <- rbindlist(lapply(1:n, sample_df), idcol = .idcol, ...)
  
  if (nested)
    boot <- boot[, list(boot = list(.SD)), by = eval(.idcol)]
  boot
}

b <- bootstrap(raw_df, n = 100)
b

split_train_test <- function(df, partition = .75) {
  n <- nrow(df)
  train <- df[sample(n * partition, replace = FALSE), ]
  test <- df[!train, on = colnames(df)]
  list(train = train, test = test)
}


b <- bootstrap(df)

b[, c("train", "test") := lapply(.SD[[1]], split_train_test), .SDcols=c("boot")]





mtcars.dt <- data.table(mtcars, keep.rownames = TRUE)
mtcars.dt
mtcars.dt[, lapply(.SD, function(x) list(mean(x), median(x)))[[1]],
          by="cyl", .SDcols=c("mpg")]
#or
mtcars.dt[, lapply(.SD, function(x) list(mean(x), median(x)))$mpg,
          by="cyl", .SDcols=c("mpg")]


