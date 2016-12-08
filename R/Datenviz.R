
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




