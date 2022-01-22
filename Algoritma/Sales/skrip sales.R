rm(list=ls())

# libraries
library(readxl)
library(dplyr)
library(tidytext)
library(janitor)
library(tidyr)

# bikin function dulu
# extract tanggal
extract_tanggal = function(tes){
  tes = unlist(strsplit(tes,split = " "))
  tes = tes[1]
  tes = as.Date(tes,"%Y-%m-%d")
  return(tes)
}

# extract longitude
extract_long = function(tes){
  if(is.na(tes)){
    tes = NA
  }
  else if(!is.na(tes)){
    tes = unlist(strsplit(tes,split = "\\n"))
    # long
    n = 1
    tes = unlist(strsplit(tes[n],split = " "))
    tes = tes[2]
    tes = as.numeric(tes)
  }
  return(tes)
}

# extract latitude
extract_lat = function(tes){
  if(is.na(tes)){
    tes = NA
  }
  else if(!is.na(tes)){
    tes = unlist(strsplit(tes,split = "\\n"))
    # long
    n = 2
    tes = unlist(strsplit(tes[n],split = " "))
    tes = tes[2]
    tes = as.numeric(tes)
  }
  return(tes)
}

# ambil data
data = 
  read_excel("SALES_PROJECT_FORM_CPC_20222022-01-21_22_17_52.xlsx") %>% 
  janitor::clean_names()


# ============================================================================
# copy paste dari sini
# tambahin id
data$id = 1:nrow(data)
data$longitude = 1
data$latitude = 1
for(i in 1:nrow(data)){
  data$longitude[i] = extract_long(data$location_coordinate[i])
  data$latitude[i] = extract_lat(data$location_coordinate[i])
}

data = 
  data %>% 
  rowwise() %>% 
  mutate(tanggal_transaksi = as.Date(tanggal_transaksi,"%d/%m/%Y"),
         submission_date = extract_tanggal(submission_date)) %>%
  ungroup() %>% 
  separate(projek_sub_projek,
           into = c("projek","sub_projek"),
           sep = ";") %>% 
  separate(sumber_barang_intermediaries_name,
           into = c("sumber_barang","intermediaries_name"),
           sep = ";") %>% 
  separate(dept_provinsi_kota_kab_kecamatan,
           into = c("department","provinsi","kota_kab","kecamatan"),
           sep = ";") %>% 
  separate(jenis_channel_sub_channel_klasifikasi,
           into = c("jenis_channel","sub_channel","klasifikasi"),
           sep = ";") %>% 
  mutate(department = trimws(department),
         jenis_channel = trimws(jenis_channel),
         sub_channel = trimws(sub_channel),
         provinsi = trimws(provinsi),
         kota_kab = trimws(kota_kab),
         kecamatan = trimws(kecamatan),
         location_coordinate = NULL
         ) %>% 
  mutate(klasifikasi = stringr::str_trim(klasifikasi)) %>% 
  rename(nama = nama_spg_mr)

# penjualan products
judul = colnames(data)
judul = ifelse(grepl("penjualan",judul),"penjualan",judul)
colnames(data) = judul

# pecah data
data_1 = data %>% select(id,penjualan)
data_2 = data %>% select(-penjualan)

# data_1
# pecah produk penjualan
data_all = 
  data_1 %>% 
  unnest_tokens(out,penjualan,token = "regex",pattern = "\n") %>% 
  filter(!grepl("subtotal|tax|total",out,ignore.case = T)) %>% 
  separate(out,into = c('produk','amount','quantity'),sep = "\\:") %>% 
  filter(!is.na(amount)) %>% 
  mutate(amount = gsub(" idr, quantity","",amount),
         amount = gsub(".00","",amount,fixed = T),
         amount = gsub(" idr)","",amount),
         amount = gsub("\\ ","",amount),
         amount = gsub("\\,","",amount),
         quantity = gsub("\\)","",quantity),
         quantity = ifelse(is.na(quantity),0,quantity),
         amount = as.numeric(amount),
         quantity = as.numeric(quantity),
         produk = gsub(" (amount","",produk,fixed = T),
         produk = toupper(produk)) %>% 
  mutate(brand = case_when(grepl("lokalate",produk,ignore.case = T) ~ "Lokalate",
                           grepl("tropicana|ts|slim",produk,ignore.case = T) ~ "Tropicana Slim",
                           grepl("nutrisari|ns|sari",produk,ignore.case = T) ~ "NutriSari",
                           grepl("diabetamil",produk,ignore.case = T) ~ "Diabetamil",
                           grepl("l-men",produk,ignore.case = T) ~ "L-Men",
                           grepl("hilo",produk,ignore.case = T) ~ "HiLo")
        ) %>% 
  rename(price = amount) %>% 
  mutate(total_value = price*quantity)

# data_2
data_final = 
  merge(data_2,data_all,all = T) %>% 
  arrange(id,brand) %>% 
  distinct() %>% 
  select(-id) %>% 
  relocate(latitude,.before = ada_platform_online) %>% 
  relocate(longitude,.before = latitude)
  

tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)
colnames(data_final)[colnames(data_final) == "Produk"] = "SKU"
openxlsx::write.xlsx(data_final,"hasil v9.xlsx",overwrite = T)
