rm(list=ls())
#setwd("~/Marketing-Jotform/Algoritma/Sales")

# libraries
library(readxl)
library(dplyr)
library(tidytext)
library(janitor)
library(tidyr)

# ambil data
data = 
  read_excel("2020 tes.xlsx") %>% 
  janitor::clean_names()

data = 
  data %>% 
  separate(location_coordinates,
           into = c("longitude","latitude","csv"),
           sep = "\r\n") %>%
  mutate(longitude = gsub("Longitude: ","",longitude),
         latitude = gsub("Latitude: ","",latitude),
         longitude = trimws(longitude),
         latitude = trimws(latitude),
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         submission_date = as.Date(submission_date,"%Y-%m-%d"),
         submission_date = lubridate::date(submission_date),
         tanggal_transaksi = as.Date(tanggal_transaksi,"%Y-%m-%d"),
         tanggal_transaksi = lubridate::date(tanggal_transaksi),
         csv = gsub("CSV: ","",csv))

tes = colnames(data)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data) = proper(tes)
openxlsx::write.xlsx(data,"hasil v2.xlsx")