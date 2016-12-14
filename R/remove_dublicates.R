library(dplyr)
library(gapminder)

x <- as_data_frame(gapminder)

x <- bind_rows(list(x, x))

x <- x %>%
  group_by(country, continent, year) %>% 
  mutate(row = row_number()) %>% 
  filter(row == 1)


library(data.table)
library(gapminder)

x <- as.data.table(gapminder)
x <- rbindlist(list(x, x))

setkey(x, year, country, continent)
x[ , i := seq_len(.N), by = key(x)]
x[i==1]
