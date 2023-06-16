setwd("~/Marketing-Jotform/Algoritma/2023/Semester 2")

# bebersih global environment
rm(list=ls())

# memanggil libraries
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)

# nama file jotform
nama_file = "Format New Jotform S2.xlsx"
sht       = excel_sheets(nama_file)

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
  read_excel(nama_file,sheet = sht[1]) %>% 
  janitor::clean_names() %>% 
  rowwise() %>% 
  mutate(submission_date   = tanggal_submisi_func(submission_date),
         tanggal_kegiatan  = tanggal_submisi_func(tanggal_kegiatan),
         tanggal_transaksi = tanggal_submisi_func(tanggal_transaksi)) %>% 
  ungroup() %>% 
  select(-ip,-submission_id)

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

# data keempat
data_3 = 
  data %>% 
  select(id,merchant_collaboration) %>% 
  mutate(merchant_collaboration = ifelse(is.na(merchant_collaboration),
                                         "Tidak ada",
                                         merchant_collaboration)) %>% 
  mutate(merchant_collaboration = gsub("\r","",merchant_collaboration)) %>% 
  separate_rows(merchant_collaboration,
                sep = "\n") %>% 
  dcast(id ~ merchant_collaboration,
        length,
        value.var = "merchant_collaboration")

# jika tiada "Tidak ada"
if(is.null(data_3$`Branding Offline`)){data_3$`Branding Offline` = 0}
if(is.null(data_3$`Branding Online`)){data_3$`Branding Online` = 0}
if(is.null(data_3$`Product Bundling`)){data_3$`Product Bundling` = 0}
if(is.null(data_3$`Product listing`)){data_3$`Product listing` = 0}
if(is.null(data_3$`Product Collaboration`)){data_3$`Product Collaboration` = 0}
if(is.null(data_3$`Tidak ada`)){data_3$`Tidak ada` = 0}

# kita paksakan urutan kolom
data_3 = data_3 %>% select(id,`Branding Offline`,`Branding Online`,
                           `Product Bundling`,`Product listing`,
                           `Product Collaboration`,`Tidak ada`)

data_3[data_3 == 1] = "Yes"
data_3[data_3 == 0] = "No"
data_3$id[1] = 1

# kita kumpulin dulu data_1, data_2, data_3
data_kumpul = 
  merge(data_1,data_2) %>% 
  merge(data_3) %>% 
  merge(data_4) %>% 
  rowwise() %>% 
  mutate(nomor_telepon = gsub("+62","0",nomor_telepon,fixed = T),
         nomor_telepon = substr(nomor_telepon,
                                2,
                                stringr::str_length(nomor_telepon)),
         nomor_telepon = paste0("62",nomor_telepon)
         ) %>% 
  ungroup()

colnames(data_kumpul) = proper_new(colnames(data_kumpul))
# openxlsx::write.xlsx(data_kumpul,file = "output sales rev.xlsx")