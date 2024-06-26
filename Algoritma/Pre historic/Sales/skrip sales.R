rm(list=ls())

# libraries
library(readxl)
library(dplyr)
library(tidytext)
library(janitor)
library(tidyr)
library(reshape2)

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

# nama file
nama_file = "cpa.xlsx"

# ambil data
data = 
  read_excel(nama_file) %>% 
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

# mulai asiknya di sini
data = 
  data %>% 
  rowwise() %>% 
  mutate(tanggal_transaksi = as.Date(tanggal_transaksi,"%d/%m/%Y"),
         submission_date = extract_tanggal(submission_date)) %>%
  ungroup() %>% 
  separate(dept_provinsi_kota_kab_kecamatan,
           into = c("department","provinsi","kota_kab","kecamatan"),
           sep = ";") %>% 
  separate(projek_sub_projek,
           into = c("projek","sub_projek"),
           sep = ";") %>% 
  separate(jenis_channel_sub_channel_klasifikasi,
           into = c("jenis_channel","sub_channel","klasifikasi"),
           sep = ";") %>% 
  separate_rows(sumber_barang_intermediaries_name,
           sep = ";") %>% 
  mutate(location_coordinate = NULL) %>% 
  mutate(klasifikasi = stringr::str_trim(klasifikasi)) %>% 
  rename(nama = nama_spg_mr) %>% 
  mutate_if(is.character,trimws)

# penjualan products
judul = colnames(data)
judul = ifelse(grepl("penjualan",judul),"penjualan",judul)
colnames(data) = judul

# pecah data
data_1 = data %>% select(id,penjualan)
data_2 = data %>% select(-penjualan) %>% select(-sumber_barang_intermediaries_name) %>% distinct()
data_3 = data %>% select(id,sumber_barang_intermediaries_name) %>% distinct()

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
  distinct() 

# data 3
data_3$sumber_barang = 1
for(i in 2:nrow(data_3)){
  data_3$sumber_barang[i] = ifelse(data_3$id[i] == data_3$id[i-1],
                                   data_3$sumber_barang[i-1] + 1,
                                   1)
}

data_3 = 
  data_3 %>% 
  mutate(sumber_barang = case_when(
    sumber_barang == 1 ~ "asal_barang",
    sumber_barang == 2 ~ "nama_sumber",
    sumber_barang == 3 ~ "jenis_marketplace"
  )
         ) %>% 
  spread(key = sumber_barang,value = sumber_barang_intermediaries_name)

# ini kita harus cek dulu apakah "jenis_marketplace" itu ada atau gak?
cek_nama = colnames(data_3)
cek_final = cek_nama[cek_nama == "jenis_marketplace"] %>% rlang::is_empty()

# seandainya gak ada "jenis_marketplace"
if(cek_final){
  data_3 = 
    data_3 %>% 
    mutate(jenis_marketplace = NA) %>% 
    relocate(nama_sumber,.before = jenis_marketplace)
}

# kita gabung finalnya
data_final = merge(data_final,data_3) %>% select(-id)

# tentang nomor invoice
data_final = 
  data_final %>% 
  relocate(nomor_order_invoice,.before = asal_barang)


tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)
colnames(data_final)[colnames(data_final) == "Produk"] = "SKU"

# copaste berakhir sampe sini

# ===========================================================
openxlsx::write.xlsx(data_final,"hasil cpa.xlsx",overwrite = T)