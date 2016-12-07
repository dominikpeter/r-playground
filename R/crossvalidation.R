
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
set.seed(3456)


cl <- makeCluster(3)

raw_df <- read.dta("http://www.farys.org/daten/ebay.dta") %>% as.data.table()
raw_df[, rating := sepos/rowSums(.SD), .SDcols = c("sepos", "seneg")]
raw_df[, `:=` (makellos = factor(rating > .98, levels = c(TRUE, FALSE), labels = c("Ja", "Nein")),
               cat = str_replace(subcat, "\\ \\(\\d+\\)", ""))]
#leave-one-out
crossv <- rbindlist(lapply(1:nrow(raw_df), function(x) raw_df[-x]), idcol = 'crossval')

by_group <- crossv[, .(data = .(.SD)), by = crossval]

model_1 <- function(df) lm(price ~ cat + rating, data = df)
model_2 <- function(df) lm(price ~ cat + rating + listpic, data = df)

by_group[, `:=` (model1 = lapply(data, model_1),
                 model2 = lapply(data, model_2))]

by_group[, `:=` (AIC_model1 = lapply(model1, AIC),
                 AIC_model2 = lapply(model2, AIC))]

####does not make much sense!!!¨
by_group[, .(crossval, AIC_model1, AIC_model2)] %>%
  tidyr::unnest() %>% 
  melt(id.vars = "crossval") %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(notch = TRUE)


split