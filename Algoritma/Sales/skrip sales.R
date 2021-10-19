rm(list=ls())

# libraries
library(readxl)
library(dplyr)
library(tidytext)
library(janitor)
library(tidyr)

# ambil data
data = 
  read_excel("tes.xlsx") %>% 
  janitor::clean_names()

# tambahin id
# lalu pecah-pecah
data = 
  data %>% 
  mutate(id = c(1:length(submission_date)),
         tanggal_transaksi = gsub("\\/","-",tanggal_transaksi),
         tanggal_transaksi = as.Date(tanggal_transaksi,"%m-%d-%Y"),
         tanggal_transaksi = lubridate::date(tanggal_transaksi),
         submission_date = lubridate::date(submission_date)) %>% 
  separate(departemen_area_nama,
           into = c("departemen","area","nama"),
           sep = ";") %>% 
  separate(jenis_channel_sub_channel_klasifikasi,
           into = c("jenis_channel","sub_channel","klasifikasi"),
           sep = ";") %>% 
  separate(provinsi_kota_kab_kecamatan_kelurahan,
           into = c("provinsi","kota_kab","kecamatan","kelurahan"),
           sep = ";") %>% 
  separate(location_coordinate,
           into = c("longitude","latitude","csv"),
           sep = "\r\n") %>% 
  mutate(departemen = trimws(departemen),
         area = trimws(area),
         nama = trimws(nama),
         jenis_channel = trimws(jenis_channel),
         sub_channel = trimws(sub_channel),
         provinsi = trimws(provinsi),
         kota_kab = trimws(kota_kab),
         kecamatan = trimws(kecamatan),
         kelurahan = trimws(kelurahan),
         longitude = gsub("Longitude: ","",longitude),
         latitude = gsub("Latitude: ","",latitude),
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         csv = gsub("CSV: ","",csv)
         ) %>% 
  mutate(klasifikasi = stringr::str_trim(klasifikasi))

# penjualan products
judul = colnames(data)
judul = ifelse(grepl("penjualan",judul),"penjualan",judul)
colnames(data) = judul

# pecah data
data_1 = data %>% select(id,penjualan)
data_2 = data %>% select(id,contains("gimmick"))
data_3 = data %>% select(-penjualan,-contains("gimmick"))

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
# oprek gimmick
data_2 = 
  data_2 %>% 
  reshape2::melt(id.vars = "id") %>% 
  rename(gimmick = variable) %>% 
  mutate(value = as.numeric(value),
         value = ifelse(is.na(value),0,value)) %>% 
  mutate(brand = case_when(grepl("hi_lo",gimmick) ~ "HiLo",
                           grepl("lokalate",gimmick) ~ "Lokalate",
                           grepl("nutrisari|ns",gimmick) ~ "NutriSari",
                           grepl("tropicana|ts",gimmick) ~ "Tropicana Slim")
         ) %>% 
  group_by(id,brand) %>% 
  summarise(tot_gim = sum(value)) %>% 
  ungroup() %>% 
  filter(tot_gim > 0)

brand_gimmick = sort(unique(data_2$brand))

for(xx in brand_gimmick){
  temp = data_2 %>% filter(brand == xx & !is.na(tot_gim))
  colnames(temp)[3] = paste("gimmick",xx,sep = "_")
  data_all = merge(data_all,temp,all = T)
}

# data_3
data_all = merge(data_3,data_all,all = T) %>% arrange(id,brand)

data_all_1 = data_all %>% select(-contains("gimmick"))

data_all_2 = 
  data_all %>% 
  group_by(id,brand) %>% 
  mutate(penanda = c(1:length(brand))) %>% 
  ungroup() %>% 
  select(contains("gimmick"),penanda) 

data_all_2[data_all_2$penanda>1,] = NA

data_final = 
  data.frame(data_all_1,data_all_2) %>% 
  mutate(penanda = NULL,
         id = NULL)

tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)
openxlsx::write.xlsx(data_final,"hasil.xlsx")

library(ggplot2)
library(leaflet)
library(ggalluvial)

new_data = 
  data_final %>% 
  filter(!is.na(Longitude)) %>% 
  distinct() %>% 
  mutate(label = paste0(stringi::stri_trans_general(`Nama Tempat Customer`,id = "Title"),
                        "<br/>Telp 0",`Nomor Telepon`),
         `Total Value` = ifelse(is.na(`Total Value`),0,`Total Value`)) %>% 
  group_by(label,Longitude,Latitude,`Nomor Telepon`) %>% 
  summarise(omset = sum(`Total Value`)) %>% 
  ungroup() %>% 
  mutate(label = paste0(label,"<br/>Total Value: Rp",omset))

leaflet() %>% addTiles() %>% addCircles(new_data$Longitude,
                                        new_data$Latitude,
                                        popup = new_data$label,
                                        radius = 10)

data_final %>% 
  mutate(`Nama Tempat Customer` = tolower(`Nama Tempat Customer`)) %>% 
  group_by(Nama,`Tanggal Transaksi`) %>% 
  summarise(Freq = length(`Nama Tempat Customer`)) %>% 
  ungroup() %>% 
  filter(!is.na(`Tanggal Transaksi`)) %>% 
  ggplot() +
  geom_tile(aes(x = `Tanggal Transaksi`,
                y = Nama,
                fill = Freq)) +
  scale_fill_gradient(low = "darkred",high = "steelblue") +
  theme_minimal() +
  labs(title = "Kalender Kunjungan",
       fill = "Banyak Toko Dikunjungi",
       subtitle = "Semua Data yang Diupload",
       caption = "Visualized using R\nikanx101.com") +
  theme(legend.position = "bottom")

data_final %>% 
  select(Nama,`Tanggal Transaksi`,`Nama Tempat Customer`,`Jenis Channel`,`Provinsi`,`Kota Kab`) %>% 
  distinct() %>% 
  group_by(`Provinsi`,`Kota Kab`,`Jenis Channel`) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  ggplot(aes(axis1 = `Provinsi`,
             axis2 = `Kota Kab`,
             axis3 = `Jenis Channel`,
             y = freq)) +
  scale_x_discrete(limits = c("Provinsi", "Kota Kab", "Jenis Channel"), expand = c(.2, .05)) +
  geom_alluvium(color = "Black",
                aes(fill = `Provinsi`)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)),
            size = 3) +
  theme_minimal() +
  labs(title = "Provinsi - Kota Kabupaten - Jenis Channel",
       subtitle = "Semua Data yang Diupload",
       caption = "Visualized using R\nikanx101.com",
       y = "Banyak Customer") +
  theme(legend.position = "none")
