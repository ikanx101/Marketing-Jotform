setwd("/cloud/project/Algoritma/2023/Awareness")

library(readxl)
library(dplyr)
library(tidyr)

rm(list=ls())

# nama file
file_name = "dummy.xlsx"

# kita ambil semua sheet yang mungkin
sht = excel_sheets(file_name)

# kita akan ambil sheet pertama dulu
data = read_excel(file_name,sheet = sht[1]) %>% janitor::clean_names()

# copy paste dari sini

data_final = 
  data %>% 
  mutate(submission_date = as.Date(submission_date,format = "%d/%m/%Y"),
         tanggal_kegiatan = as.Date(tanggal_kegiatan,"%B %d, %Y")) %>% 
  separate(dept_provinsi_kota_kab_kecamatan,
           into = c("dept","provinsi","kota_kab","kecamatan"),
           sep = ";") %>% 
  separate(projek_jenis_channel_sub_channel,
           into = c("projek","jenis_channel","sub_channel"),
           sep = ";") %>%  
  separate(dimana_event_aktivasi_atau_meeting_dilaksanakan,
           into = c("jenis_event","platform","jenis_platform"),
           sep = ";") %>%
  mutate(
         dept = trimws(dept),
         projek = trimws(projek),
         jenis_channel = trimws(jenis_channel),
         sub_channel = trimws(sub_channel),
         jenis_event = trimws(jenis_event),
         platform = trimws(platform),
         jenis_platform = trimws(jenis_platform),
         provinsi = trimws(provinsi),
         kota_kab = trimws(kota_kab),
         kecamatan = trimws(kecamatan)) %>% 
  mutate(bulan = lubridate::month(tanggal_kegiatan,label = T)) %>% 
  relocate(bulan,.after = tanggal_kegiatan)
  
tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)
write.xlsx(data_final,file = "output awareness.xlsx")