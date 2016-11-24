library(magrittr)
library(data.table)

set.seed(2323)


# Simulation von 100000 Versuchen
# -------------------------------------------------------------------------------------------------------------------

# Kopf = 1
# Zahl = 0

n <- 1:100000
# es werden 10 Mal eine Münze mit den zwei Möglichkeiten Kopf oder Zahl geworfen
df <- data.table(n) %>% .[, sample := list(lapply(1:100000, function(x) rbinom(10, 1, 0.5)))] 

# für jeden Versuch wird die relative Häufigkeit von Kopf berechnet
df[, `:=` (relative_häufigkeit = unlist(lapply(.SD$sample, function(x) mean(x))),
           häufigkeit = unlist(lapply(.SD$sample, function(x) sum(x))))]

# Histogramm der Verteilung
df$relative_häufigkeit %>% hist(main ="Histogramm der relativen Häufigkeiten")

# Anzahl relative und absolute Häufigkeit
df[, .N, by = .(häufigkeit, relative_häufigkeit)][order(-N)]

# 101 wird kein Kopf geworfen und 96 wird nur Kopf geworfen, wobei 24519mal 5 Mal Kopf und 5 Mal Zahl geworfen wird


