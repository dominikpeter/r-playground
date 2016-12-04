# Problem: Beim Schweizer Zahlenlotto sind 6 Zahlen aus 42 zu ziehen.
# Wir bezeichnen mit x die Anzahl der richtig angekreutzten Zahlen.
# Bestimmen Sie die Wahrscheinlichkeitsverteilung und stellen Sie diese
# grafisch dar.

library(ggplot2)
library(magrittr)

color <- "#2980b9"

yprob <- dhyper(0:6, m=6, n=36, k=6)

yprob %>%
  data.frame(x=1:length(yprob), y=.) %>%
  ggplot(aes(x=x, y=y)) +
  stat_sum(geom="bar", fill = color, alpha = 4/5)


barplot(yprob, col = "#e74c3c")


yprob1 <- dhyper(0:3, m=4, n=6, k=3)

yprob1 %>%
  data.frame(x=1:length(yprob1), y=.) %>%
  ggplot(aes(x=x, y=y)) +
  stat_sum(geom="bar", fill = color, alpha = 4/5)
