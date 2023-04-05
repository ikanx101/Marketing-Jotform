rm(list=ls())

setwd("/cloud/project/Algoritma/2023/SPG Event")

library(dplyr)
library(tidyr)

# kita ambil datanya terlebih dahulu
file = "Struktur Data SPG Event TS.xlsx"
df   = read_excel(file) %>% janitor::clean_names()

# kita akan buat function tanggal terlebih dahulu
konversi = function(tgl){
  as.Date(tgl,"%B %d, %Y")
}

df = 
  df %>% 
  mutate(id = 1:nrow(df)) %>% 
  relocate(id,.before = submission_date) %>% 
  rowwise() %>% 
  mutate(submission_date   = konversi(submission_date),
         tanggal_transaksi = konversi(tanggal_transaksi)) %>% 
  mutate(submission_date   = format(submission_date,"%B %d, %Y"),
         tanggal_transaksi = format(tanggal_transaksi,"%B %d, %Y")) %>% 
  ungroup() %>% 
  separate(dept_provinsi_kota_kab_nama_toko_nama_spg_event,
           into = c("dept","provinsi","kota_kab","nama_toko","nama_spg_event"),
           sep  = ";") %>% 
  mutate(dept           = trimws(dept),
         provinsi       = trimws(provinsi),
         kota_kab       = trimws(kota_kab),
         nama_toko      = trimws(nama_toko),
         nama_spg_event = trimws(nama_spg_event)) %>% 
  separate_rows(penjualan,
                sep = "\r\n") %>% 
  filter(!grepl("total",penjualan,ignore.case = T)) %>% 
  separate(penjualan,
           into = c("item","info","qty"),
           sep  = ":") %>% 
  mutate(item = gsub("\\(Amount","",item),
         item = trimws(item)) %>% 
  separate(info,
           into = c("price","info_2"),
           sep  = "IDR,") %>% 
  mutate(price = gsub("\\.00","",price),
         price = gsub("\\,","",price),
         price = as.numeric(price)) %>% 
  mutate(quantity = gsub("\\)","",qty),
         quantity = as.numeric(quantity)) %>% 
  select(-info_2,-qty) %>% 
  mutate(total_value = price * quantity)
  

openxlsx::write.xlsx(df,file = "output.xlsx")
