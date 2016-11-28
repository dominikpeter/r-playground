
# -------------------------------------------------------------------------------------------------------------------
# Kombinatorik
# -------------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(magrittr)
library(data.table)
library(ggplot2)

set.seed(2323)



# Würfel Simulieren / 2 Würfel
# -------------------------------------------------------------------------------------------------------------------


würfeln <- function(n, k, ...) {
  würfel <- 1:6
  lapply(1:n, function(x) sample(würfel, k, ...))
}

simulation <- würfeln(111111, 2, replace = TRUE)
summen <- sapply(simulation, sum) %>% data.table(x = .)
summen[, check := x == 9 | x == 10]

summen %>%
  ggplot(aes(x = x, y = ..count.., fill = check)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(min(summen$x),max(summen$x),1)) +
  scale_fill_manual(values = c("black", "red"))
  

# Würfel Simulieren / 4 Würfel
# -------------------------------------------------------------------------------------------------------------------

simulation <- würfeln(111111, 4, replace = TRUE)
summen <- sapply(simulation, sum) %>% data.table(x = .)
summen[, count := .N, by = x][, max := max(count)][, check := count == max]


summen %>%
  ggplot(aes(x = x, y = ..count.., fill = check)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(min(summen$x),max(summen$x),1)) +
  scale_fill_manual(values = c("black", "red"))


# Würfel Simulieren mit ohne Wiederholung
# -------------------------------------------------------------------------------------------------------------------

simulation <- würfeln(111111, 2, replace = FALSE)
summen <- sapply(simulation, sum) %>% data.table(x = .)
summen[, count := .N, by = x][, max := max(count)][, check := count == max]


summen %>%
  ggplot(aes(x = x, y = ..count.., fill = check)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(min(summen$x),max(summen$x),1)) +
  scale_fill_manual(values = c("black", "red"))





