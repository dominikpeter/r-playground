
library(magrittr)
library(openxlsx)
library(zoo)
library(data.table)
library(stringr)
library(rvest)


# get url
url <- "https://www.kof.ethz.ch/en/forecasts-and-indicators/indicators/kof-baublatt-indicator.html"
url_excel <- read_html(url) %>%
  html_node(xpath = '//*[@id="contentContainer"]/aside/div/div[1]/div/div[4]/div/div/p/a') %>% 
  html_attr("href")

path <- getwd()
excel_path <- paste0(path, "/data/excel/index.xlsx")

download.file(url_excel, excel_path)

df <- read.xlsx(excel_path) %>% as.data.table()

df[, Year := str_extract(Quartal, "[0-9]+")]
df[, Month := str_extract(Quartal, "[A-z]+")]

df
