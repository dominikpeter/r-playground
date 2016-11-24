library(magrittr)
library(data.table)

set.seed(2323)



# Simulation von 100000 Versuchen
# -------------------------------------------------------------------------------------------------------------------
n <- 1:100000
#es werden 10 Mal eine Münze mit Kopf oder Zahl geworfen
df <- data.table(n) %>% .[, sample := list(lapply(1:100000, function(x) rbinom(10, 1, 0.5)))] 

# für jeden Versuch wird die relative Häufigkeit von Kopf berechnet
df[, `:=` (relative_häufigkeit = unlist(lapply(.SD$sample, function(x) mean(x))),
           häufigkeit = unlist(lapply(.SD$sample, function(x) sum(x))))]
# Histogramm
df$relative_häufigkeit %>% hist

# Anzahl 
df[, .N, by = .(häufigkeit, relative_häufigkeit)][order(-N)]


