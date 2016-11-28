devtools::install_github("ropensci/RSelenium")
library(RSelenium)
checkForServer() # nur 1x aufrufen. Installiert den selenium server
# Selenium Server muss das manuell starten. Bei mir liegt er hier: C:\Users\rudi\Documents\R\win-library\3.2\RSelenium\bin

remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "chrome" # aktuelle Version von FF macht Probleme, ggf. alte Version nutzen
)


# chrome geht auch nicht per default sondern braucht den chrome webdriver
# runterladen von: http://chromedriver.storage.googleapis.com/index.html
# und dann definieren wo das entpackte file liegt. am besten in PATH der umgebungsvariablen

remDr$open() # �ffnet ein chrome Fenster
remDr$navigate("https://www.sauspiel.de/") # login page �ffnen

wxbox <- remDr$findElement(using = 'css selector', "#ontop_login") # das erste textfeld finden (via css selector, d.h. den namen in "id=")
wxbox$sendKeysToElement(list("hackstutz")) # ein paar keystrokes dort hinschicken
wxbox <- remDr$findElement(using = 'name', "password") # das erste textfeld finden
mypassword <- ""
wxbox$sendKeysToElement(list(mypassword)) # ein paar keystrokes dort hinschicken
wxbutton <- remDr$findElement(using = 'name', "commit") # submitbutton per css selector suchen
wxbutton$clickElement() # das element clicken

games <- c(676310800,676310440,676309631,676309249,676308828) # meine f�nf letzten Spiele

for(game in games) {
  remDr$navigate(paste0("",game))
  roh <- remDr$getPageSource()[[1]]
  file.create(paste0("",game,".html"))
  fileConn <- file(paste0("",game,".html"))
  writeLines(roh, fileConn)
  close(fileConn)
}


# ab hier kann man die HTML Files ausschlachten

# xpath sachen
library(rvest)
library(stringr)
library(data.table)
library(dplyr)

getGameDetails <- function(id) {
  
  # das HTML wieder einlesen  
  roh <- read_html(paste0("",id,".html"),encoding="latin1")
  
  # das Regel-Set des Spiels steht im title Attribut des Elements mit der Klasse rules__rule
  regeln <- roh %>%
    html_nodes(".rules__rule") %>%
    html_attr("title") %>% sort() %>% paste(collapse=" ")
  
  # die Tabelle rechtts enth�lt mehrere relevante Informationen: Tarif, Laufende, Klopfer (verdoppeln nach dem Geben des ersten 4 Karten), Kontra/Retour (verdoppeln nach Spielansage), Spielausgang (Punkte)
  infotab <- roh %>%
    html_table()
  tarif <- iconv(infotab[[1]][1,2],from="UTF-8",to="latin1")
  laufende <- as.numeric(infotab[[1]][3,2])
  klopfer <- strsplit(infotab[[1]][6,2],", ")[[1]]
  kontra <- strsplit(infotab[[1]][7,2], ", ")[[1]]
  augen <- as.numeric(str_extract(infotab[[1]][4,2],"[0-9]+"))
  
  # Im Titel steht, welcher Spieltyp gespielt wurde: Solo, Rufspiel, Wenz, ..
  # Den Titel und den Spieler finden wir als Text innerhalb der Node, die class=game-name-title hat
  spiel <- roh %>% 
    html_nodes(".game-name-title") %>%
    html_text() 
  spiel <- strsplit(spiel," von ")
  spieler <- spiel[[1]][2]
  spieltyp <- spiel[[1]][1]
  
  # um die Karten jedes Spielers zu lesen m�ssen wir alle Nodes mit class card-image suchen
  # die Karten haben mehrere classes, also lesen wir das class Attribut (html_attr()) und zerlegen den String
  cards <- roh %>% 
    html_nodes(".show-hand .card-image") %>%
    html_attr("class") 
  cards <- sub(x=cards,pattern = "card-image by g3 ", replacement = "")
  karten <- word(cards,1)
  erstehand <- word(cards,-1)=="highlighted" # karten die gehighlighted sind geh�ren zu den ersten 4 von 8 Karten
  position <- sort(rep(1:4,8)) # sitzposition kann man an dieser Stelle nebenbei abgreifen
  df_karten <- data.frame(position, erstehand, karten)
  sorteddeck <- df_karten %>%
    arrange(position,-erstehand)
  # ins long Format anordnen (4 Spieler pro Spiel-ID)
  karten <- data.frame(t(matrix(sorteddeck$karten,ncol=4,nrow=8)))
  names(karten) <- paste0("k",1:8)
  
  spielername <- roh %>%
    html_nodes(".userpic ") %>%
    html_attr("data-username") %>% head(4)
  position <- 1:4
  
  #kontra/legen je spieler
  klopfer <- spielername %in% klopfer
  kontra <- spielername %in% kontra
  
  
  rolle_payout <- roh %>%
    html_nodes(".players div p") %>%
    html_text()
  # hat man Geld/Punkte gekriegt/verloren als Spieler oder als Gegenspieler?
  rolle <- str_extract(rolle_payout,"Gegenspieler|Spieler|Mitspieler")
  payout <- str_extract(rolle_payout,"[0-9,\\-]+")
  payout <- as.numeric(sub(x=payout, pattern = ",", replacement = "."))
  
  # zusammenpacken
  data.table(id, regeln, tarif, spielername, position, rolle, klopfer, kontra, payout, laufende, spieler, spieltyp, karten)
}

dt <- rbindlist(lapply(games, getGameDetails))

