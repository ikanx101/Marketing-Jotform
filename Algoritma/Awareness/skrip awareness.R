setwd("~/Marketing-Jotform/Algoritma/Awareness")

library(readxl)
library(dplyr)
library(tidyr)

rm(list=ls())

data = read_excel("Contoh Data.xlsx") %>% janitor::clean_names()

data_final = 
  data %>% 
  mutate(submission_date = lubridate::date(submission_date)) %>% 
  separate(departemen_area_nama,
           into = c("departemen","area","nama"),
           sep = ";") %>% 
  separate(brand_projek_materi,
           into = c("brand","project","materi"),
           sep = ";") %>% 
  mutate(tanggal_aktivasi = gsub("\\/","-",tanggal_aktivasi),
         tanggal_aktivasi = as.Date(tanggal_aktivasi,"%m-%d-%Y"),
         tanggal_aktivasi = lubridate::date(tanggal_aktivasi)) %>% 
  separate(jenis_channel_sub_channel_klasifikasi_channel,
           into = c("jenis_channel","sub_channel","klasifikasi_channel"),
           sep = ";") %>% 
  separate(canal_platform_lokasi_room,
           into = c("canal","platform","lokasi_room"),
           sep = ";") %>% 
  mutate(departemen = trimws(departemen),
         area = trimws(area),
         nama = trimws(nama),
         brand = trimws(brand),
         project = trimws(project),
         materi = trimws(materi),
         jenis_channel = trimws(jenis_channel),
         sub_channel = trimws(sub_channel),
         klasifikasi_channel = trimws(klasifikasi_channel),
         canal = trimws(canal),
         platform = trimws(platform),
         lokasi_room = trimws(lokasi_room))

tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)