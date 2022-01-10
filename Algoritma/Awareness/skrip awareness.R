setwd("~/Documents/Marketing-Jotform/Algoritma/Awareness")

library(readxl)
library(dplyr)
library(tidyr)

rm(list=ls())

data = read_excel("Contoh Data.xlsx") %>% janitor::clean_names()

data_final = 
  data %>% 
  mutate(submission_date = lubridate::date(submission_date)) %>% 
  separate(department_area_nama,
           into = c("departemen","area","nama"),
           sep = ";") %>% 
  separate(provinsi_kota_kab,
           into = c("provinsi","kota_kab"),
           sep = ";") %>% 
  separate(brand_projek_materi,
           into = c("brand","project","materi"),
           sep = ";") %>% 
  mutate(tanggal_kegiatan = gsub("\\/","-",tanggal_kegiatan),
         tanggal_kegiatan = as.Date(tanggal_kegiatan,"%m-%d-%Y"),
         tanggal_kegiatan = lubridate::date(tanggal_kegiatan)) %>% 
  separate(jenis_channel_sub_channel_klasifikasi,
           into = c("jenis_channel","sub_channel","klasifikasi_channel"),
           sep = ";") %>% 
  separate(kanal_platform_lokasi_room,
           into = c("kanal","platform","lokasi_room"),
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
         kanal = trimws(kanal),
         platform = trimws(platform),
         lokasi_room = trimws(lokasi_room),
         provinsi = trimws(provinsi),
         kota_kab = trimws(kota_kab))

tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)