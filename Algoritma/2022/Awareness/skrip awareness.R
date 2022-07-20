setwd("/cloud/project/Algoritma/2022/Awareness")

library(readxl)
library(dplyr)
library(tidyr)

rm(list=ls())

data = read_excel("uji coba.xlsx") %>% janitor::clean_names()

# copy paste dari sini

data_final = 
  data %>% 
  mutate(submission_date = as.Date(submission_date,format = "%B %d, %Y"),
         tanggal_kegiatan = as.Date(tanggal_kegiatan,"%B %d, %Y"),) %>% 
  separate(dept_provinsi_kota_kab_kecamatan,
           into = c("dept","provinsi","kota_kab","kecamatan"),
           sep = ";") %>%
  separate(projek_jenis_channel_sub_channel,
           into = c("projek","jenis_channel","sub_channel"),
           sep = ";") %>%  
  separate(dimana_event_aktivasi_atau_meeting_dilaksanakan,
           into = c("jenis_event","platform","jenis_platform"),
           sep = ";") %>% 
  mutate(materi_hslp = ifelse(grepl("hslp",kegiatan_di_sekolah,ignore.case = T),
                              "Ya",
                              "Tidak"),
         senam = ifelse(grepl("senam",kegiatan_di_sekolah,ignore.case = T),
                        "Ya",
                        "Tidak"),
         ranking_1 = ifelse(grepl("ranking",kegiatan_di_sekolah,ignore.case = T),
                            "Ya",
                            "Tidak"),
         cerita_challenge = ifelse(grepl("cerita",kegiatan_di_sekolah,ignore.case = T),
                                   "Ya",
                                   "Tidak"),
         aktivasi_kantin = ifelse(grepl("kantin",kegiatan_di_sekolah,ignore.case = T),
                              "Ya",
                              "Tidak"),
         kegiatan_di_sekolah = gsub("Materi HSLP|Senam|Ranking 1|Cerita Challenge|Aktivasi Kantin","",kegiatan_di_sekolah,ignore.case = T),
         kegiatan_di_sekolah = stringr::str_trim(kegiatan_di_sekolah),
         lainnya = kegiatan_di_sekolah,
         )  %>% 
    select(-kegiatan_di_sekolah) %>% 
    mutate(kantin = ifelse(grepl("kantin",apakah_titik_sekolah_memiliki,ignore.case = T),
                           "Ya",
                           "Tidak"),
           chiller = ifelse(grepl("chiller",apakah_titik_sekolah_memiliki,ignore.case = T),
                           "Ya",
                           "Tidak")
           ) %>% 
    select(-apakah_titik_sekolah_memiliki) %>% 
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
  relocate(bulan,.after = tanggal_kegiatan) %>% 
  relocate(apakah_kantin_menjual_minuman_serbuk_premium_nutrisari_hi_lo_mi_lo_dsb,
           .after = chiller) %>% 
  relocate(participant,
           .after = apakah_kantin_menjual_minuman_serbuk_premium_nutrisari_hi_lo_mi_lo_dsb) %>% 
  relocate(apakah_ada_penjualan,.after = participant) %>% 
  relocate(lainnya,.after = aktivasi_kantin)

tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)
write.xlsx(data_final,file = "output.xlsx")