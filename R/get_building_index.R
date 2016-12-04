
library(magrittr)
library(openxlsx)
library(zoo)
library(data.table)
library(stringr)
library(rvest)
library(ggplot2)


# get url
url <- "https://www.kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-baublatt-indicator.html"
url_excel <- read_html(url) %>%
  html_node(xpath = '//*[@id="contentContainer"]/aside/div/div[1]/div/div[4]/div/div/p/a') %>% 
  html_attr("href")

path <- getwd()
excel_path <- paste0(path, "/data/excel/index.xlsx")

download.file(url_excel, excel_path)

df <- read.xlsx(excel_path) %>% as.data.table()

df[, `:=` (Year = str_extract(Quartal, "[0-9]+"),
           Month = str_extract(Quartal, "[A-z]+"))]

df <- list(i = 1:12,
          Quarter = rep(1:4, each = 3),
          Month = month.abb) %>% 
  as.data.table() %>% 
  merge(x = df, y =  ., by = "Month")

df[, `:=` (Quartal = as.yearqtr(paste(Year, Quarter, sep = "-")),
           Index = as.character((as.numeric(Year)*10) + Quarter),
           Wohnbauindikator = as.numeric(Wohnbauindikator_CHF_nominal_saisonbereinigt),
           Gesamtbauindikator = as.numeric(Gesamtbauindikator_CHF_nominal_saisonbereinigt))]

df <- df[, .(Index,
             Jahr = Year,
             Quartal,
             Index,
             Wohnbauindikator,
             Gesamtbauindikator)]

df %>% setorder(Index)

output_path <- paste0(path, "/data/output/", Sys.Date(), "_bauindikator.csv")
write.csv(df, output_path)

mx <- max(df$Wohnbauindikator, df$Gesamtbauindikator)
sd <- sd(df$Wohnbauindikator, df$Gesamtbauindikator)
breaks <- seq(0, mx + (sd/2), by = floor(sd/200)*200)

tidy_df <- df %>% 
  melt(id.vars = 1:4)

tidy_df[Jahr > year(Sys.Date())-19] %>% 
  ggplot(aes(x = Quartal, y = value, color = variable)) +
  geom_point(alpha = .7) +
  geom_smooth() +
  scale_x_yearqtr(n = 20) +
  ylab("") +
  scale_colour_manual(values = c("#2980B9", "#27AE60"), name = "") +
  scale_y_continuous(limits = c(0, mx + (sd/2)), breaks = breaks) +
  ggtitle("KOF Baublatt Indicator")
  theme(panel.background = element_rect(fill = "#F0F1F5"),
        panel.grid.major = element_line(color = "white", size = 4/5),
        panel.grid.minor = element_blank()) 


