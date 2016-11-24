rm(list=ls())

library(magrittr)
library(data.table)
library(ggplot2)

set.seed(2323)


# Simulation von 100000 Versuchen
# -------------------------------------------------------------------------------------------------------------------

# Kopf = 1
# Zahl = 0

n <- 1:100000
# es werden 10 Mal eine Münze mit den zwei Möglichkeiten Kopf oder Zahl geworfen
df <- data.table(n) %>% .[, sample := list(lapply(n, function(x) rbinom(10, 1, 0.5)))] 

# für jeden Versuch wird die relative Häufigkeit von Kopf berechnet
df[, `:=` (relative_häufigkeit = sapply(.SD$sample, function(x) mean(x)),
           häufigkeit = sapply(.SD$sample, function(x) sum(x)))]

# Histogramm der Verteilung
df %>% ggplot(aes(x=relative_häufigkeit)) +
  geom_histogram(binwidth = 0.1, color = "white", fill = "#2980b9", alpha = 4/5) +
  ggtitle("Histogramm der relativen Häufigkeiten")


# Anzahl relative und absolute Häufigkeit
df[, .N, by = .(häufigkeit, relative_häufigkeit)][order(-N)]

# 101 wird kein Kopf geworfen und 96 wird nur Kopf geworfen, wobei 24519mal 5 Mal Kopf und 5 Mal Zahl geworfen wird



# Beispiel Prüfung
# -------------------------------------------------------------------------------------------------------------------
# Suppose there are twelve multiple choice questions in an English class quiz.
# Each question has five possible answers, and only one of them is correct.
# Find the probability of having four or less correct answers if a student attempts to answer every question at random.

df <- NULL
n <- 1:1000000
# Richtige Antwort = 1
# Falsche Antwort = 0

df <- data.table(n) %>% .[, sample := list(lapply(n, function(x) rbinom(12, 1, 1/5)))]  #12 Fragen 
# für jeden Versuch wird die relative Häufigkeit von Kopf berechnet
df[, `:=` (relative_häufigkeit = sapply(.SD$sample, function(x) mean(x)),
           häufigkeit = sapply(.SD$sample, function(x) sum(x)))]

# Histogramm der Verteilung
# df %>% ggplot(aes(x=relative_häufigkeit)) +
#   geom_histogram(binwidth = 0.1, color = "white", fill = "#2980b9", alpha = 4/5) +
#   ggtitle("Histogramm der relativen Häufigkeiten")


# Anzahl relative und absolute Häufigkeit
verteilung <- df[, .N, by = .(häufigkeit, relative_häufigkeit)][order(-N)]

# Wahrscheinlichkeit genau 4 richtig zu haben:

verteilung[häufigkeit == 4, N] / sum(verteilung[, N])

# Lösungsansatz während des Unterrichts / gleiche Lösung bzw. Ähnliche Lösung bei vielen Versuchen!
dbinom(4, size=12, prob=0.2) 

