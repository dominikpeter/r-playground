
# Stetige Verteilung
# -------------------------------------------------------------------------------------------------------------------
# 
# Wahrscheinlichkeit sind Flächen
# 
# Die Funktionsdauer x ist normalverteilt mit Erwartungswert u = 120h und Varianz 100
# Wie Wahrscheinlich ist es, dass die Funktionsdauer

# 135 hält

# 135 hält
pnorm(135, mean = 120, sd=sqrt(100))


# mehr als 135 hält
pnorm(135, mean = 120, sd = sqrt(100), lower.tail = FALSE)

# Gleichverteilung
xv <- seq(0, 5, length = 100)
plot(xv, dunif(xv, min = 1, max = 3), type = "l")
grid()

punif(2.5, 1, 3)

runif(n = 100, min = 1, max = 3)


# Exponentialverteilung / pendant zu Poissonverteilung
xv <- seq(0, 5, length = 100)

plot(xv, dexp(xv, rate = 1), type = "l")
grid()

pexp(2, rate=1/3)

# wie wahrscheinlich 50%
qexp(0.5, rate = 1/3)



# normalverteilung
x <- seq(-5, 5, length = 200)
plot(x, dnorm(x), type= "l")
grid()

# stetiges model, daher ab 84
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE)

# 3 Standardabweichungen
pnorm(5)-pnorm(-5)



#chi quadrat
x <- seq(0, 50, length=200)
degf <- c(2,3,7,30)

color <- RColorBrewer::brewer.pal(5, "Set1")

plot(x, dchisq(x, df = degf[1]), col = color, type = "l")
for (i in seq_along(degf[-1])){
  lines(x, dchisq(x, df = degf[i]), lwd =2, col = color[i])
}
grid()


qchisq(0.95, 7)


# t-verteilung

xv <- seq(-4, 4, length = 100)
hx <- dnorm(xv)
color <- RColorBrewer::brewer.pal(5, "Set2")

degf <- c(2,3,7,30)
labels <- c("df2", "df3", "df7", "df30", "normal")



plot(xv, hx, col = color, type = "l")
for (i in seq_along(degf)){
  lines(xv, dt(xv, df = degf[i]), lwd = 2, col = color[i])
}
legend("topright", inset = 0.05, labels , lty = c(1,1,1,1,2), col = color)
grid()

qt(c(0.025, 0.975), df=5)














