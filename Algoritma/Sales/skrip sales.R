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
  read_excel("SALES_PROJECT_FORM_CPE_20222022-01-17_21_42_02.xlsx") %>% 
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
  mutate(tanggal_transaksi = as.Date(tanggal_transaksi,"%m/%d/%Y"),
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
openxlsx::write.xlsx(data_final,"hasil v8.xlsx",overwrite = T)


# =========================================================================
# =========================================================================
# =========================================================================

library(ggplot2)
library(leaflet)
library(ggalluvial)

new_data = 
  data_final %>% 
  filter(!is.na(Longitude)) %>% 
  distinct() %>% 
  mutate(label = paste0(stringi::stri_trans_general(`Nama Tempat Customer`,id = "Title"),
                        "<br/>Telp ",`Nomor Telepon`),
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
