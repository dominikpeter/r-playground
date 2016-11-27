
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

df <- rpois(n, 12) %>% data.table(value = .)


# Histogramm der Verteilung
df %>% ggplot(aes(x=value)) +
  geom_histogram(fill = '#2574A9', color = "white", alpha = 2/3, bins = 35) +
  ggtitle("Binominalverteilung")



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








