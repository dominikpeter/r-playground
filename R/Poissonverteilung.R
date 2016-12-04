
# -------------------------------------------------------------------------------------------------------------------
# Poisson Distribution
# -------------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(magrittr)
library(data.table)
library(ggplot2)

set.seed(2323)



# Simulation von 10000000 Versuchen
# -------------------------------------------------------------------------------------------------------------------

# Kopf = 1
# Zahl = 0

n <- 10000000

df <- data.table(value = rpois(n, 12))

df[, `:=` (mittelwert = mean(value)),
           standardabweichung = mean(value))]


nbr <- nclass.Sturges(df$value)

# Histogramm der Verteilung
df %>% ggplot(aes(x=value)) +
  geom_histogram(fill = '#2574A9', color = "white", alpha = 2/3, bins = nbr) +
  ggtitle("Poissonverteilung")



# The probability of having sixteen or less cars crossing the bridge in a particular minute is given by the function ppois.
df[, check := value <= 16]
mean(df$check)

# Lösungsansatz während des Unterrichts
ppois(16, lambda=12)


# The probability of having sixteen or less cars crossing the bridge in a particular minute is given by the function ppois.
df[, check := value <= 16]
mean(df$check)

# Lösungsansatz während des Unterrichts
ppois(16, lambda=12)



# Wahrscheinlichkeit von genau 16
df[, check := value == 20]
mean(df$check)


dpois(20, lambda = 12)



# Übung
# -------------------------------------------------------------------------------------------------------------------


pois <- dpois(1:30, lambda = 12)

dt <- pois %>%
  data.table(x=1:length(pois), y=.)


dt %>% 
  ggplot(aes(x=x, y=y)) +
  stat_sum(geom="bar", fill = color, alpha = 4/5) +
  scale_x_continuous(breaks = seq(1,30, by = 1))

dpois(90, lambda = 90)
dpois(89, lambda = 90)


# wie viel Zeit vergeht



