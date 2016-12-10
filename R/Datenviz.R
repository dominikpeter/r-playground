
# -------------------------------------------------------------------------------------------------------------------
# DATENVISUALISIERUNG
# -------------------------------------------------------------------------------------------------------------------

################################################################
## Skript:      1 WarmUpmitR
## Studiengang: CAS Datenanalyse 16/17
## Modul:       Graphische Datenexploration und Datenvisualisierung  
## Lernziele:   (1) Verstehen, wie bereits installierte Datensätze geladen werden
##              (2) Erste (nichtvisuelle) Datenexploration 
##
####################################
library(data.table)
library(magrittr)

#############################
## Übungsdatensätze in R
##### 

# Lassen Sie sich die Datensätze anzeigen, die in R für Übungszwecke implementiert sind
d <- data()
ncol(mtcars)
names(mtcars)
nrow(mtcars)
rownames(mtcars)
str(mtcars)
rbindlist(head(mtcars, 6), tail(mtcars, 6))
?mtcars

###########################
# Was ist der Wertebereich für die Anzahl Zylinder (cyl)?
# Wie hoch ist die durchschnittliche Pferdestärke (hp)? Wie hoch ist der Median?




#######################
# Installieren Sie mit untenstehender ipak-Funktion die Pakete, die später benötigt werden
# Quelle: https://gist.github.com/stevenworthington/3178163
# "ggplot2", "gcookbook", "vcd", "corrplot","ggthemes","ReporteRs","dplyr","GGally","scales","reshape2","tibble"

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "gcookbook", "vcd", "corrplot","ggthemes","ReporteRs","dplyr","GGally","scales","reshape2","tibble")
ipak(packages)



################################################################
## Skript:      2 Plot-Techniken-eine-Variable
## Studiengang: CAS Datenanalyse 16/17
## Modul:       Graphische Datenexploration und Datenvisualisierung  
## Lernziele:   (1) Univarite Techniken der Datenexploration mit R
##              (2) Unterschiede konventionelle Plot-Methode und ggplot erkennen 
##
####################################



## Libraries
library(ggplot2)


###
# Daten - Motor Trend Car Road Tests - mtcars
# Führen Sie eine erste Datenbegutachtung durch, damit Sie eine grundlegende Vorstellung
# der verwendeten Daten haben
help(mtcars)



###############
# Stabdiagramme
# Frage: Welche Zylinderzahl kommt am häufigsten vor?
plot(table(mtcars$cyl))



###############
# Balken-Diagramme

# Erstellen Sie ein Balken-Diagramm für die Zylinderzahl mit der konventionellen Plot Funktion

barplot(table(mtcars$cyl))


# Erstellen Sie ein Balken-Diagramm mit ggplot 
# Übergeben Sie die Varialbe cyl einmal als kategoriales Merkmal (factor) 
# und einmal als metrisches Merkmal (numeric)

mtcars %>% ggplot(aes(x=factor(cyl))) + stat_sum(aes(y=cyl), geom = "bar")
mtcars %>% ggplot(aes(x=factor(cyl))) + geom_bar()


###############
# Kuchendiagramme

# Zeichnen Sie ein Kuchendiagram mit der konventionellen Plot Funktion

pie(table(mtcars$cyl))
title("Kuchendiagramm")

## In ggplot gibt es keine direkte Umsetzung eines Kuchendiagrammes, weil Hadley Wickham, wie viele andere Statistiker
## glaubt, dass Kuchendiagramme ungenau sind
## Mit ein bisschen Arbeit lässt sich jedoch ein Kuchendiagramm über einen Barchart und der Funktion coord_polar()
## erstellen
## vgl. http://www.r-chart.com/2010/07/pie-charts-in-ggplot2.html

mtcars %>%
  ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y")



###############
# Histogramme

# Untersuchen Sie die Verteilung der Variable mpg (miles per gallon)
# Die Variable gibt Auskunft zum Benzinverbrauch
# (eine Meile ~1.6 KM, eine Gallone ~ 3.8 liter)

# Erstellen Sie ein Histogramm mit der konventionellen Plot Funktion
# In welche Kategorie fallen am meisten Fahrzeuge?

hist(mtcars$mpg, breaks = pretty(mtcars$mpg, 7))

# Verändern Sie die Zahl der Klassen über die Option breaks 
# Wie sieht das Histogram aus mit 5,7,10 Unterteilungen aus?
# Ändert sich etwas in Bezug auf die Aussage, welch Kategorie von Benzinverbrauch am häufigsten vorkommt?


# Erstellen Sie ein Histogramm mit ggplot
# Wie geht ggplot bei der Bestimmung der Breite der Intervalle vor?

# Mit ggplot lässt sich die Breite der Klassen über "binwidth" steuern 
# Justieren Sie die Intervallbreite so, dass sie ungefähr der Einteilung von breaks=10 mit der konvetionellen Plot-Funktion entspricht.

mtcars %>% ggplot(aes(x = mpg)) + geom_histogram(binwidth = 5, color = "white")




#####
# EXTRA
# Alternativ lässt sich für metrische Variablen eine WahrschenlichkeitsDichte-Funktion schätzen.
# Sie zeigt an, in welchen Bereiche viele und und in welchen
# Bereichen wenige Wahrscheinlichkeits"Masse" vorliegt
ggplot(mtcars, aes(x=mpg))+
  geom_density()

# Die Linien am Rand sind etwas unschön, vergrössern wir doch einfach die x-Achse
ggplot(mtcars, aes(x=mpg))+
  geom_density() +
  xlim(1,45)

# Mit adjust, lässt sich die Glättung kontrollieren
# Standard ist adjust=1, grössere Werte=stärkere Glättung und umgekehrt
# Testen Sie verschiedene Parameter für adjust und beobachten Sie, wie sich die 
# Dichtefunktion verändert
ggplot(mtcars, aes(x=mpg))+
  geom_density(adjust = 0.1) +
  xlim(1,45)
# EXTRA
#####




###############
# Boxplot


# Erstellen Sie ein Boxplot für die Pferdestärke (hp=horsepower) mit der konventionellen Plotfunktion
boxplot(mtcars$hp ~ mtcars$cyl)



# Erstellen Sie ein Boxplot für die Pferdestärke (hp=horsepower) mit ggplot

mtcars %>% ggplot(aes(x = factor(cyl), y = hp, fill = factor(am))) +
  geom_boxplot(position = position_dodge(0.8))


# Gibt es ein Auto, dass Aufgrund der Daten als Ausreiser bezeichnet werden kann? Um welches Auto handelt es sich?






################################################################
## Skript:      3 Plot-Techniken-zwei-Variablen
## Studiengang: CAS Datenanalyse 16/17
## Modul:       Graphische Datenexploration und Datenvisualisierung  
## Lernziel:    Bivariate Techniken der Datenexploration mit R
##
####################################




## Libraries
library(ggplot2)
library(dplyr)



#################
# Liniendiagramme
####
# Geeignet für ein ordinales Merkmal (viele Ausprägungen 5+) auf der x-Achse  
# und ein metrisches Merkmal auf der y-Achse (Bsp. eine Zeitreihe)

# Daten- BOD > Biochemical Oxygen Demand
# Schauen Sie sich den Datensatz an. Was wurde mit dem Datensatz untersucht? Was wurde gemessen?
help(BOD)

# Wir untersuchen, wie sich die Variable "demand" über die Zeit entwickelt
# Wie entwickelt sich die biochemische Sauerstoff-Nachfrage von Wasser über die Zeit?
# Erstellen Sie ein Liniendiagram mit der Zeit (Time) auf der x-Achse und der Sauerstoff-Nachfrage (demand) auf der y-Achse

BOD %>% ggplot(aes(x = Time, y = demand)) + geom_line() +ylim(0,max(BOD$demand))

# Standardmässig verwendet ggplot eine Wertebereich für die Y-Achsen, 
# die gerade ausreichend ist um alle Punkte anzuzeigen. 
# Wie sieht das Bild aus, wenn die Y-Achse bei Null startet? +ylim(0,max(BOD$demand))
BOD %>% ggplot(aes(x = Time, y = demand)) +
  geom_line() + 
  ylim(0,max(BOD$demand)) +
  geom_point(size = 2)

# Fügen Sie der Grafik ebenfalls die Messpunkte hinzu (mit +geom_point)
# Nur so wird ersichtlich, für welche Zeitpunkte tatsächlich Messungen vorliegen.
# Sind für alle Zeitpunkte Messdaten vorhanden?


############
# Balkendiagramme 
############

# Geeignet für ein ordinales Merkmal (wenige Ausprägungen) oder ein nominales Merkmal auf der x-Achse
# und ein metrisches Merkmal auf der y-Achse (Bpsw. Gruppenvergleiche für wenig Beobachtungen)

# Wie sieht der BOD-Plot von oben (x=Time, y=demand) mit Balken aus?

BOD %>% ggplot(aes(x = Time, y = demand)) +
  stat_sum(geom = "bar")

BOD %>% ggplot(aes(x = Time, y = demand)) +
  geom_bar(stat = "identity")

# Achtung: Die Standardeinstellung von geom_bar() ist stat="count", d.h. die Höhe der Balken wird entsprechend der Anzahl Ausprägungen je Kategorie gezeichnet. 
help(geom_bar)
# Das passt prima für eine Häufigkeitsauszählung, nicht jedoch für die Anwendung hier.
# Im vorliegenden Fall soll die Länge der Balken entsprechend der beobachtet Werte gezeichnet werden. 
# Überschreiben Sie den Standardparameter mit dem Zusatz stat="identity"


############
# EXTRA zu Balkendiagramme 
# Eine elegante Alternative zum Bar Charts ist der Cleveland Dot Plot
# Er ist übersichtichler, weil weniger "Tinte" verwendet wird (Edward Tufte, Daten-Design-Guru, empfiehlt möglichst auf Grafik-Junk zu verzichten, d.h. überflüssige "Tinte" zu entfernen)
# Der Cleveland-Dot-Plot eignet sich daher für den Vergleich vieler Gruppen/Objekte 
# (nominale Variablen mit vielen Ausprägungen) weil er übersichtlicher ist

# Benötigte Library (für die Daten)
library(gcookbook)

# Daten: tophitters2001: Batting averages of the top hitters in Major League Baseball in 2001
# Baseball-Statistik der besten 144 Hitter 

# Inspizieren Sie die Daten
str(tophitters2001)
help("tophitters2001")

# Wir wollen die mittlere Anzahl getroffener Schläge je Spieler untersuchen (avg)

## Als Bar-Plot (geom_bar)
ggplot(tophitters2001, aes(x=name,y=avg)) +
  geom_bar(stat="identity")

# Als Punkt-Plot (geom_point)
ggplot(tophitters2001, aes(x=avg,y=name)) +
  geom_point()

# Es sind zu viele! Wir wollen wirklich nur die Besten
# Grenzen Sie die Daten auf die jene Hitters ein, die eine Trefferquote >0.31 haben
tophit<-tophitters2001[tophitters2001$avg>0.30,]

# Erstellen Sie nochmals einen Punkte-Plot
ggplot(tophit, aes(x=avg,y=name)) +
  geom_point()

# Jetzt ordnen wir die Namen (mit reorder())
ggplot(tophit, aes(x=avg,y=reorder(name,avg))) +
  geom_point()


# Und noch ein bisschen Zusatzästhetik.
# Voilà ein Cleveland-Dot-Plot 
ggplot(tophit, aes(x=avg,y=reorder(name,avg))) +
  geom_segment(aes(yend=name),xend=0, colour="grey50") +
  geom_point(size=2.5) +
  xlab("Mittlere Trefferquote je Versuch") +
  ylab("") +
  theme(panel.grid.major.y=element_blank()) +
  theme_minimal()

# EXTRA 
############

######
# Boxplots 

# Sind besonders für Gruppenvergleiche von metrischen Variablen mit vielen Beobachtungen geeignet
# Nutzen Sie erneut den Auto-Datensatz (mtcars)
# Übergeben Sie die Zahl der Zylinder(cyl) als x-Wert und die PS(hp) als Y-Wert

rm(list=ls())
# Beeinflusst die Zahl der Zylinder die PS?
mtcars <- as.data.table(mtcars, keep.rownames = TRUE)
outlier <- mtcars[, outlier := hp > quantile(hp, 0.75) + (IQR(hp)*1.5), by = cyl]
outlier <- mtcars[, outlier_wert :=  quantile(hp, 0.75) + (IQR(hp)*1.5) , by = cyl]




mtcars %>% ggplot(aes(x = factor(cyl), y = hp)) +
  geom_boxplot() +
  geom_text(data = mtcars[outlier == T], aes(label = rn, group = outlier), position = position_stack(1.1))
  




###################
# Streudiagram / Scatterplot
#####

## 
# Geeignet zur Darstellung beobachteter Wertepaare zweier metrischer Variablen

# Benötigte Libraries (für die Daten)
library(gcookbook)

# Daten (heightweight): Height and weight of schoolchildren
# Machen Sie sich mit den Daten vertraut. Welche Informationen beinhaltet der Datensatz?


# Erstellen Sie einen Scatterplot mit dem Alter der Schulkinder(ageYear) auf der X-Achse 
# und dem Grösse (heightIn) auf der Y-Achse

# Frage: Gibt es einen Zusammenhang zwischen dem Alter und der Grösse der Schulkinder?

# Der Zusammenhang kann  mit einer Regressionslinie veranschauchlicht werden
# Die Regressionslinie zeigt den linearen mittlere Veränderung der Grösse in Abhängigkeit des Alters
# +stat_smooth(method=lm, se=FALSE)








##### 
# EXTRA zu Scatterplot
# Was geschieht, wenn viele Datenpunkte vorliegen (grosse Datensätze).
# Die Datenpunkte überlagern (Overplotting) und 
# es wird schwierig die Verteilung der Daten in diesem Bereich zu erkennen

# Daten: diamonds- diamonds Data - Prices of 50,000 round cut diamonds

# Erste Dateninspektion
diamonds
help(diamonds)
str(diamonds)

# Nun zeichnen wir einen Scatterplot mit über 54'000 Datenpunkten
# Plotten Sie das Gewicht der Diamanten (carat) auf der x-Achse und 
# den Preis in US-Dollars (price) auf der y-Achse
ggplot(diamonds, aes(x=carat,y=price))+
  geom_point()

# Einige Muster werden kenntlich, Grenzen bei 1, 1.5 und 2 carat
# Insbesondere im Bereich von 0 bis 2 carat bleibt die Sache  obskur
# Es besteht die Möglichkeit, die Dichte zusätzlich mit einer Farbe zu visualisieren

# Dichte ist je Bins visualisiert
ggplot(diamonds, aes(x=carat,y=price))+
  stat_bin2d(bins=50)+
  scale_fill_gradient(low="lightblue",high="red",limits=c(0,6000))

# Werden Diamanten immer wertvoller je schwerer sie sind?
ggplot(diamonds, aes(x=carat,y=price))+
  stat_bin2d(bins=50)+
  scale_fill_gradient(low="lightblue",high="red",limits=c(0,6000))+
  stat_smooth(method=lm, se=FALSE,colour="black")

# Lineare Regression ist nicht die einzige Methode zur graphischen Beschreibung des Zusammenhanges
# Default für stat_smooth ist auch nicht lm, sondern loess
# loess sind locally weighted polynomiale Kurven, d.h. es wird nicht ein linearer Zusammenhang über alle Daten abzubilden versucht.
# Vielmehr werden die Daten in kleine Abschnitte zerlegt und eine Linie mit lokaler Anpassung an die Daten erzeugt
# Das ist ein guter Weg, um die Linearität eines Zusammenhanges zu überprüfen
ggplot(diamonds, aes(x=carat,y=price))+
  stat_bin2d(bins=50)+
  scale_fill_gradient(low="lightblue",high="red",limits=c(0,6000))+
  stat_smooth(method=loess, se=FALSE,colour="black")



















