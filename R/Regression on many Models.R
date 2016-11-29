
# -------------------------------------------------------------------------------------------------------------------
# Regression on many models
# -------------------------------------------------------------------------------------------------------------------
rm(list=ls())
if(!require(gapminder)) install.packages(gapminder)
if(!require(magrittr)) install.packages(magrittr)
if(!require(broom)) install.packages(broom)
if(!require(ggplot2)) install.packages(ggplot2)


by_group <- gapminder %>% 
  as.data.table %>% 
  .[, .(data = .(.SD)), by = .(country, continent)]


reg <- function(df) {
  lm(gdpPercap ~ year, df)
}

get_slope <- function(df) {
  df$estimate[2]
}

by_group[, model := lapply(data, reg)]
by_group[, tidy := lapply(model, broom::tidy)]
by_group[, slope := vapply(tidy, get_slope, double(1))]

by_group[slope > quantile(slope,0.01)] %>%
  ggplot(aes(x=continent, y = slope)) +
  geom_boxplot()



