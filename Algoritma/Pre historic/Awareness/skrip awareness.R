setwd("~/Documents/Marketing-Jotform/Algoritma/Awareness")

library(readxl)
library(dplyr)
library(tidyr)

rm(list=ls())

data = read_excel("AWARENESS_PROJECT_FORM_CPD_20222022-01-21_22_22_29.xlsx") %>% janitor::clean_names()


# copy paste dari sini

data_final = 
  data %>% 
  mutate(submission_date = as.Date(submission_date,"%Y-%m-%d"),
         tanggal_kegiatan = as.Date(tanggal_kegiatan,"%d/%m/%Y")) %>% 
  separate(dept_provinsi_kota_kab_kecamatan,
           into = c("department","provinsi","kota_kab","kecamatan"),
           sep = ";") %>% 
  separate(brand_projek_materi,
           into = c("brand","project","materi"),
           sep = ";") %>% 
  separate(jenis_channel_sub_channel_klasifikasi,
           into = c("jenis_channel","sub_channel","klasifikasi_channel"),
           sep = ";") %>% 
  separate(kanal_platform_lokasi_room,
           into = c("kanal","platform","lokasi_room"),
           sep = ";") %>% 
  rename(nama = nama_spg_mr) %>% 
  mutate(department = trimws(department),
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
         kota_kab = trimws(kota_kab),
         kecamatan = trimws(kecamatan))

tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)