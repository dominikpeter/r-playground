
# -------------------------------------------------------------------------------------------------------------------
# Benchmarking many Models (data.table vs purrr)
# -------------------------------------------------------------------------------------------------------------------

rm(list=ls())
library(gapminder)
library(magrittr)
library(broom)
library(ggplot2)
library(data.table)
library(tidyr)
library(purrr)
library(dplyr)
library(snow)
library(microbenchmark)


# -------------------------------------------------------------------------------------------------------------------
# Regression on many models
# -------------------------------------------------------------------------------------------------------------------
df <- gapminder %>% as.data.table

big_df <- rbindlist(rep(list(df), 10000)) # get some bigger data to test


reg <- function(df) {
  lm(gdpPercap ~ year, df)
}

dt <- function(df){
  by_group <- df[, .(data = .(.SD)), by = .(continent, country)]
  
  
  # model by group
  by_group[, model := lapply(data, reg)]
  by_group[, `:=` (tidy = lapply(model, tidy),
                   glance = lapply(model, glance))]
  by_group[, r.squared := vapply(glance, `[[`, double(1), "r.squared")]
  
  by_group
  
}

dt_par <- function(df, cluster){
  by_group <- df[, .(data = .(.SD)), by = .(continent, country)]
  
  
  # model by group
  by_group[, model := parLapply(cl = cluster, data, reg)]
  by_group[, `:=` (tidy = parLapply(cl = cluster, model, tidy),
                   glance = parLapply(cl = cluster, model, glance))]
  by_group[, r.squared := parSapply(cl = cluster, glance,`[[`, "r.squared")]
  
  by_group
  
}




# purrr way
# ------------------------------------------------------------------------------------------------

pr <- function(df){
  df <- df %>% group_by(country, continent) %>% 
    nest()
  
  df <- df %>% mutate(model = data %>% map(reg),
                      tidy = model %>% map(broom::tidy),
                      glance = model %>% map(broom::glance),
                      r.squared = glance %>% map_dbl("r.squared")
  )
  df
  
}

cl <- makeCluster(3)

microbenchmark(purrr = pr(df),
               data.table = dt(df),
               dt_parallel = dt_par(df, cl))

# Unit: milliseconds
# expr              min       lq     mean   median       uq      max neval cld
# pr(df)          321.5931 352.5270 375.1240 367.9685 385.8287 593.8242   100   b
# dt(df)          302.5097 339.0640 357.2328 350.6424 369.3118 583.3654   100   b
# dt_par(df, cl)  103.7035 116.3931 136.3069 121.9473 131.8487 952.4037   100  a 


# "big" data
microbenchmark(pr(big_df), dt(big_df), dt_par(big_df, cl)))

# Unit: seconds
# expr                min       lq        mean   median       uq       max neval cld
# pr(big_df)          24.05595 25.11174 25.87698 25.52638 25.99323 39.53848   100  b
# dt(big_df)          20.64331 21.35062 22.00183 21.64625 22.19227 30.80878   100  a 
# dt_par(big_df, cl)  20.33935 21.21181 21.91533 21.70614 22.11915 37.73931   100  a 


setdi

nest_impl <- function(data, key_col, group_cols, nest_cols) {
  data <- dplyr::ungroup(data)
  

  nest_cols <- setdiff(nest_cols, group_cols)
  
  out <- dplyr::distinct_(dplyr::select_(data, .dots = group_cols))
  
  idx <- dplyr::group_indices_(data, .dots = group_cols)
  out[[key_col]] <- unname(split(data[nest_cols], idx))[unique(idx)]
  
  out
}

# nesting
microbenchmark(tidyr = nest(df, country, continent),
               data.table =  df[, .(data = .(.SD)), by = .(country, continent)])


