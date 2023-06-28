setwd("~/Marketing-Jotform/Algoritma/2023/Semester 2")

# bebersih global environment
rm(list=ls())

# memanggil libraries
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)

# nama file jotform
nama_file = list.files(pattern = "*.xlsx")

# function untuk split tanggal submisi
tanggal_submisi_func = function(tgl){
  # proses split tanggal
  tgl = lubridate::date(tgl)
  tgl = format(tgl,"%d/%m/%y")
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
  read_excel(nama_file[2]) %>% 
  janitor::clean_names() %>% 
  rename(penjualan_products = penjualan)
  #rowwise() %>% 
  #mutate(submission_date   = tanggal_submisi_func(submission_date),
  #       tanggal_kegiatan  = ifelse(is.na(tanggal_kegiatan),
  #                                  NA,
  #                                  tanggal_submisi_func(tanggal_kegiatan)),
  #       tanggal_transaksi = tanggal_submisi_func(tanggal_transaksi)) %>% 
  #ungroup() %>% 
  #select(-ip,-submission_id)

# bikin unique id dulu
data$id = 1:nrow(data)

# ini kita pindahin ke depan si id-nya
data = data %>% relocate(id,.before = submission_date)

colnames(data)

# kita pisah terlebih dahulu
# data pertama
data_1 = 
  data %>% 
  separate(dept_provinsi_kota_kab_kecamatan,
           into = c("dept","provinsi","kota_kab","kecamatan"),
           sep = "\\;") %>% 
  mutate(dept = trimws(dept),
         provinsi = trimws(provinsi),
         kota_kab = trimws(kota_kab),
         kecamatan = trimws(kecamatan)) %>% 
  separate(projek_jenis_channel_sub_channel,
           into = c("project","jenis_channel","sub_channel"),
           sep = "\\;") %>% 
  mutate(project = trimws(project),
         jenis_channel = trimws(jenis_channel),
         sub_channel = trimws(sub_channel)) %>% 
  separate(brand_tidak_deal,
           into = c("brand_tidak_deal_1","brand_tidak_deal_2",
                    "brand_tidak_deal_3"),
           sep = "\\-") %>% 
  mutate(brand_tidak_deal_1 = trimws(brand_tidak_deal_1),
         brand_tidak_deal_2 = trimws(brand_tidak_deal_2),
         brand_tidak_deal_3 = trimws(brand_tidak_deal_3)) %>% 
  select(-penjualan_products)

# data kedua adalah bentuk tabular dari penjualan
data_2 = 
  data %>% 
  select(id,penjualan_products) %>%
  rename(penjualan = penjualan_products) %>%
  mutate(penjualan = gsub("\\\r","",penjualan)) %>% 
  separate_rows(penjualan,
                sep = "\n") %>%  # ini perubahan terbaru ya
  filter(!grepl("total",penjualan,ignore.case = T)) %>% 
  separate(penjualan,
           into = c("item","dummy"),
           sep = "\\(Amount:") %>% 
  separate(dummy,
           into = c("price","quantity"),
           sep = "IDR, Quantity:") %>% 
  mutate(quantity = gsub("\\)","",quantity),
         item = trimws(item),
         price = trimws(price),
         price = gsub("\\,","",price),
         price = as.numeric(price),
         quantity = trimws(quantity),
         quantity = gsub("\\,","",quantity),
         quantity = as.numeric(quantity),
         total_value = price * quantity) %>% 
  rowwise() %>% 
  mutate(brand = case_when(
    grepl("ts",item,ignore.case = T) ~ "TS",
    grepl("lmen|l men|l-men",item,ignore.case = T) ~ "L-Men",
    grepl("ns|nutri",item,ignore.case = T) ~ "NS",
    grepl("lokal",item,ignore.case = T) ~ "Lokalate",
    grepl("Diabetamil",item,ignore.case = T) ~ "Diabetamil",
    grepl("hilo",item,ignore.case = T) ~ "HILO"
  )
         ) %>% 
  ungroup()

# kita kumpulin dulu data_1, data_2, data_3
data_kumpul = merge(data_1,data_2)

# nah tugas kita belum sepenuhnya rebes karena ada baris duplikat yang
# harus dihapus
# kita pisah per id
data_temp = data_kumpul %>% group_split(id)

# kita buat function penghapus
rapihinkan_angka = function(tes,nrow_tes){
  tes$participant[2:nrow_tes]                   = NA
  tes$jumlah_voucher_dibagikan[2:nrow_tes]      = NA
  tes$jumlah_voucher_yang_di_redeem[2:nrow_tes] = NA
  return(tes)
}

# kita mulai
n_iter    = length(data_temp)
for(i in 1:n_iter){
  # dibuat temporary dulu
  tes       = data_temp[[i]]
  nbaris    = nrow(tes)
  if(nbaris > 1){
    output         = rapihinkan_angka(tes,nbaris)
    # kita kembalikan lagi ke data awal
    data_temp[[i]] = output
  }
  print(i)
}

data_final = do.call(rbind,data_temp)

colnames(data_final) = proper_new(colnames(data_final))
openxlsx::write.xlsx(data_final,file = "output.xlsx")