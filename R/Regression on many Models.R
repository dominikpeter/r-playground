rm(list=ls())
if(!require(gapminder)) install.packages(gapminder)
if(!require(magrittr)) install.packages(magrittr)
if(!require(broom)) install.packages(broom)
if(!require(ggplot2)) install.packages(ggplot2)
if(!require(data.table)) install.packages(data.table)

library(snow)
cl <- makeCluster(6)

# -------------------------------------------------------------------------------------------------------------------
# Regression on many models
# -------------------------------------------------------------------------------------------------------------------
df <- gapminder %>% as.data.table

df <- rbindlist(rep(list(df), 1000)) #get some bigger data to test


reg <- function(df) {
  lm(gdpPercap ~ year, df)
}


by_group <- df[, .(data = .(.SD)), by = .(continent, country)]


# model by group
by_group[, model := lapply(data, reg)]
by_group[, `:=` (tidy = lapply(model, tidy),
                 glance = lapply(model, glance))]
by_group[, `:=` (slope = vapply(tidy, `[`, double(1), 2, 2),
                 r.squared = vapply(glance, `[[`, double(1), "r.squared"))]

# in parallel
# ----------------------------------------------------------------------------
# by_group[, model := parLapply(cl, data, reg)]
# by_group[, `:=` (tidy = parLapply(cl, model, broom::tidy),
#                  glance = parLapply(cl, model, broom::glance))]
# by_group[, `:=` (slope = parSapply(cl, tidy, `[`, 2, 2),
#                  r.squared = parSapply(cl, glance, `[[`, "r.squared"))]



# plot slope by continent
by_group[slope > quantile(slope, 0.01) & continent != "Oceania"] %>% 
  ggplot(aes(y=slope, x=continent)) +
  geom_boxplot(fill="#16a085", alpha = 3/5)

# plot r.squared by contnent
by_group[r.squared > quantile(r.squared, 0.01) & continent != "Oceania"] %>% 
  ggplot(aes(y=r.squared, x=continent)) +
  geom_boxplot(fill="#16a085", alpha = 3/5)


df <- tidyr::unnest(by_group[, .(continent, country, data, slope, r.squared)])

by_group[, idx := 1:ncol(df)]
by_group %>% setkey(idx)

unnest <- by_group[rbindlist(by_group$data)]



