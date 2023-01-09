setwd("~/Documents/Marketing-Jotform/Algoritma/2023/Sales")

# bebersih global environment
rm(list=ls())

# memanggil libraries
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)

# nama file jotform
nama_file = "dummy sales.xlsx"
sht = excel_sheets(nama_file)

# function untuk split tanggal submisi
tanggal_submisi_func = function(tgl){
  # proses split tanggal
  tgl = tgl %>% as.Date(format = "%d/%m/%Y") # perubahan tanggal transaksi terbaru d sini
  # output tanggal
  return(tgl)
}

# function untuk tanggal transaksi
tanggal_trans_func = function(tgl){
  tgl = tgl %>% as.Date(format = "%B %d, %Y") # perubahan tanggal transaksi terbaru d sini
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
  mutate(submission_date = tanggal_submisi_func(submission_date),
         tanggal_transaksi = tanggal_trans_func(tanggal_transaksi)) %>% 
  ungroup()

# bikin unique id dulu
data$id = 1:nrow(data)

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
  separate(project_jenis_channel_sub_channel,
           into = c("project","jenis_channel","sub_channel"),
           sep = "\\;") %>% 
  mutate(project = trimws(project),
         jenis_channel = trimws(jenis_channel),
         sub_channel = trimws(sub_channel)) %>% 
  # ini adalah perbedaan di tahun 2023
  separate(brand_tidak_deal,
           into = c("brand_tidak_deal_1","brand_tidak_deal_2","brand_tidak_deal_3"),
           sep = " ") %>% 
  select(-platform_online_merchant, #ini akan kita pecah sesuai dengan kategori
         -penjualan #ini kita pecah jadi tabular
         )


# data kedua, hanya platform online merchant yang akan kita pecah sendiri
data_2 = 
  data %>% 
  select(id,platform_online_merchant) %>% 
  mutate(platform_online_merchant = ifelse(is.na(platform_online_merchant),
                                           "Tidak ada",
                                           platform_online_merchant)) %>% 
  separate_rows(platform_online_merchant,
                sep = "\r\n") %>% 
  dcast(id ~ platform_online_merchant,
        length,
        value.var = "platform_online_merchant") 

# jika tiada "Tidak ada"
if(is.null(data_2$`Tidak ada`)){data_2$`Tidak ada` = NA}

# kita hapus dulu
data_2 =
  data_2 %>% 
  select(-id,-`Tidak ada`)

# jika tiada isiannya
if(is.null(data_2$GoFood)){data_2$GoFood = 0}
if(is.null(data_2$GrabFood)){data_2$GrabFood = 0}
if(is.null(data_2$ShopeeFood)){data_2$ShopeeFood = 0}
if(is.null(data_2$`Delivery Mandiri`)){data_2$`Delivery Mandiri` = 0}
if(is.null(data_2$`Online Food Delivery Lainnya`)){data_2$`Online Food Delivery Lainnya` = 0}
# kita ubah jadi ya dan no
data_2[data_2 == 0] = "No"
data_2[data_2 == 1] = "Yes"
data_2$id = 1:nrow(data_2)

# data ketiga adalah bentuk tabular dari penjualan
data_3 = 
  data %>% 
  select(id,penjualan) %>% 
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
data_kumpul = 
  merge(data_1,data_2) %>% 
  merge(data_3) %>% 
  rowwise() %>% 
  mutate(nomor_telepon = gsub("+62","0",nomor_telepon,fixed = T),
         nomor_telepon = substr(nomor_telepon,
                                2,
                                stringr::str_length(nomor_telepon)),
         nomor_telepon = paste0("62",nomor_telepon)
         ) %>% 
  ungroup()

colnames(data_kumpul) = proper_new(colnames(data_kumpul))

openxlsx::write.xlsx(data_kumpul,file = "output sales.xlsx")