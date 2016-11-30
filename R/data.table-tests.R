
# -------------------------------------------------------------------------------------------------------------------
# data.table tests
# -------------------------------------------------------------------------------------------------------------------
rm(list=ls())
if(!require(gapminder)) install.packages(gapminder)
if(!require(magrittr)) install.packages(magrittr)
if(!require(ggplot2)) install.packages(ggplot2)
if(!require(data.table)) install.packages(data.table)

df <- gapminder %>% as.data.table 

df[, `:=`(Grösser50 = lifeExp > 50, Grösser65 = lifeExp > 65, Grösser10 = lifeExp > 10)]

df[, any := any(Grösser50, Grösser65, Grösser10),
   by = .(country, continent, year, lifeExp, pop, gdpPercap)]

