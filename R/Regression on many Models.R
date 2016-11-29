
# -------------------------------------------------------------------------------------------------------------------
# Regression on many models
# -------------------------------------------------------------------------------------------------------------------
rm(list=ls())
if(!require(gapminder)) install.packages(gapminder)
if(!require(magrittr)) install.packages(magrittr)
if(!require(broom)) install.packages(broom)
if(!require(ggplot2)) install.packages(ggplot2)


df <- gapminder %>% as.data.table 

by_group <- df[, .(data = .(.SD)), by = .(continent, country)]

reg <- function(df) {
  lm(gdpPercap ~ year, df)
}


by_group %>% 
  .[, model := lapply(data, reg)] %>%
  .[, tidy := lapply(model, broom::tidy)] %>% 
  .[, glance := lapply(model, broom::glance)] %>% 
  .[, slope := vapply(tidy, `[`, double(1), 2, 2)] %>% 
  .[, r.squared := vapply(glance, `[[`, double(1), "r.squared")]


by_group[slope > quantile(slope, 0.01) & continent != "Oceania"] %>% 
  ggplot(aes(y=slope, x=continent)) +
  geom_boxplot(fill="#16a085", alpha = 3/5)


by_group[r.squared > quantile(r.squared, 0.01) & continent != "Oceania"] %>% 
  ggplot(aes(y=r.squared, x=continent)) +
  geom_boxplot(fill="#16a085", alpha = 3/5)





