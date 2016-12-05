
library(magrittr)
library(openxlsx)
library(zoo)
library(data.table)
library(stringr)
library(rvest)
library(ggplot2)
library(httr)

current <- as.yearqtr(Sys.Date())

set_config( config( ssl_verifypeer = 0L ))
url <- "https://www.kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-baublatt-indicator.html"


url_excel <- httr::GET(url) %>%
  read_html() %>% 
  html_node(xpath = '//*[@id="contentContainer"]/aside/div/div[1]/div/div[4]/div/div/p/a') %>% 
  html_attr("href")

path <- getwd()
excel_path <- paste0(path, "/data/excel/index.xlsx")
download.file(url_excel, excel_path, mode = "wb")

df <- read.xlsx(excel_path) %>%
  as.data.table()

df[, `:=` (Year = str_extract(Quartal, "[0-9]+"),
           Month = str_extract(Quartal, "[A-z]+"))]

df <- list(i = 1:12,
          Quarter = rep(1:4, each = 3),
          Month = month.abb) %>% 
  as.data.table() %>% 
  merge(x = df, y =  ., by = "Month")

df[, `:=` (Quartal = as.yearqtr(paste(Year, Quarter, sep = '-')),
           Wohnbauindikator = as.numeric(Wohnbauindikator_CHF_nominal_saisonbereinigt),
           Gesamtbauindikator = as.numeric(Gesamtbauindikator_CHF_nominal_saisonbereinigt))]

df <- df[, .(Jahr = Year,
             Quartal,
             Wohnbauindikator,
             Gesamtbauindikator)]


df %>% setorder(Index)

output_path <- paste0(path, "/data/output/", Sys.Date(), "_bauindikator.csv")
write.csv(df, output_path)

mx <- max(df$Wohnbauindikator, df$Gesamtbauindikator)
sd <- sd(df$Wohnbauindikator, df$Gesamtbauindikator)
breaks <- seq(0, mx + (sd/2), by = floor(sd/200)*200)

tidy_df <- df %>% 
  melt(id.vars = 1:2)

plot_df <- tidy_df[Jahr > year(Sys.Date())-6]

plot_df %>% 
  ggplot(aes(x = Quartal, y = value, color = variable)) +
  geom_point(size = 2) +
  geom_line(size= 1) +
  ylab("") +
  scale_color_manual(values = c("#2980B9", "#27AE60"), name = "") +
  scale_x_yearqtr(breaks = seq(min(plot_df$Quartal), max(plot_df$Quartal),by = .25),
                  format = "%YQ%q") +
  ggtitle("KOF Baublatt Indicator") +
  theme(panel.background = element_rect(fill = "#F0F1F5"),
        panel.grid.major = element_line(color = "white", size = 4/5),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1))



