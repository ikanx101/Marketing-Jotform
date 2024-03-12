setwd("~/Marketing-Jotform/Algoritma/2024/Jessyanti")

# bebersih global environment
rm(list=ls())

# memanggil libraries
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)

# nama file jotform
nama_file = "Jotform_Availability_MPO_Area_M2024-03-07_20_38_06.xlsx"

# function untuk split tanggal submisi
tanggal_submisi_func = function(tgl){
  # proses split tanggal
  tgl = as.Date(tgl,"%B %d, %Y")
  tgl = format(tgl,"%m/%d/%Y")
  # output tanggal
  return(tgl)
}

# fungsi untuk bikin judul proper
proper_new = function(x){
  tess = stringi::stri_trans_general(x,id = "Title")
  gsub("\\_"," ",tess)
}

# memanggil dataset baru
data = 
  read_excel(nama_file) %>% 
  janitor::clean_names() %>% 
  rowwise() %>% 
  mutate(submission_date = tanggal_submisi_func(submission_date)) %>% 
  ungroup() %>% 
  separate(area_nama_spg_klasifikasi_toko_nama_toko,
           into = c("area","nama_spg","klasifikasi_toko","nama_toko"),
           sep = "\\,") %>% 
  mutate(`area`             = trimws(area),
         `nama_spg`         = trimws(`nama_spg`),
         `klasifikasi_toko` = trimws(`klasifikasi_toko`),
         `nama_toko`        = trimws(`nama_toko`)) 

# kita tambahkan variabel id
data$id = 1:nrow(data)
  
# kita akan pisah terlebih dahulu
data_1 = data %>% select(id,submission_date,area,nama_spg,klasifikasi_toko,nama_toko)

# kita akan pilih hanya data product list
data_2 = data %>% select(id,produk_list)

# data kedua adalah bentuk tabular dari penjualan
data_3 = 
  data_2 %>% 
  rename(penjualan = produk_list) %>%
  mutate(penjualan = gsub("\\\r","",penjualan)) %>% 
  separate_rows(penjualan,
                sep = "\n") %>%  # ini perubahan terbaru ya
  filter(!grepl("total",penjualan,ignore.case = T)) %>% 
  separate(penjualan,
           into = c("produk_list","dummy"),
           sep = "\\(Amount:") %>% 
  mutate(produk_list = trimws(produk_list)) %>% 
  separate(dummy,
           into = c("amount","quantity"),
           sep = "IDR, Quantity:") %>% 
  mutate(amount = as.numeric(amount)) %>% 
  separate(quantity,
           into = c("quantity","status_produk"),
           sep = "\\,") %>% 
  mutate(quantity = trimws(quantity),
         quantity = as.numeric(quantity),
         status_produk = gsub("[^[:alnum:]]", " ", status_produk),
         status_produk = trimws(status_produk)) %>% 
  rowwise() %>% 
  mutate(brand = case_when(
    grepl("ts",produk_list,ignore.case = T) ~ "TS",
    grepl("lmen|l men|l-men",produk_list,ignore.case = T) ~ "L-Men",
    grepl("ns|nutri",produk_list,ignore.case = T) ~ "NS",
    grepl("lokal",produk_list,ignore.case = T) ~ "Lokalate",
    grepl("Diabetamil",produk_list,ignore.case = T) ~ "Diabetamil",
    grepl("hilo",produk_list,ignore.case = T) ~ "HILO"
  )
         ) %>% 
  ungroup()

# kita kumpulin dulu data_1, data_2, data_3
data_kumpul = merge(data_1,data_3) %>% select(-id)

colnames(data_kumpul) = proper_new(colnames(data_kumpul))
openxlsx::write.xlsx(data_kumpul,file = "output.xlsx")