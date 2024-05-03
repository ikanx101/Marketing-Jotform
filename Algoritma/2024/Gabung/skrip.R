setwd("~/Marketing-Jotform/Algoritma/2024/Gabung")

library(dplyr)
library(tidyr)
library(readxl)

files = list.files()

df_am = read_excel(files[1]) %>% janitor::clean_names()
df_dd = read_excel(files[2]) %>% janitor::clean_names()

df_target = df_am
marker    = ncol(df_target)

if(marker == 3){
  # kita pecahin
  tarjeta =
    df_target %>% 
    mutate(submission_date = as.Date(submission_date,"%Y-%m-%d")) %>% 
    separate(pic_area_klasifikasi_customer_customer_code,
             sep  = "\\,",
             into = c("pic","area","klasifikasi_customer","customer_name","customer_code")) %>% 
    mutate(pic                  = stringr::str_trim(pic),
           area                 = stringr::str_trim(area),
           klasifikasi_customer = stringr::str_trim(klasifikasi_customer),
           customer_code        = stringr::str_trim(customer_code)
    ) %>% 
    separate_rows(product_list,sep = "\r\n") %>% 
    separate(product_list,
             sep  = "\\(Amount:",
             into = c("products","pecah")) %>% 
    separate(pecah,
             sep  = "\\:",
             into = c("hapus","status_produk")) %>% 
    rename(amount = hapus) %>% 
    mutate(products      = stringr::str_trim(products),
           status_produk = stringr::str_trim(status_produk),
           status_produk = gsub("\\)","",status_produk),
           amount        = 0) %>% 
    rowwise() %>% 
    mutate(brand = case_when(
      grepl("ts",products,ignore.case = T)               ~ "TS",
      grepl("lmen|l men|l-men",products,ignore.case = T) ~ "L-Men",
      grepl("ns|nutri",products,ignore.case = T)         ~ "NS",
      grepl("lokal",products,ignore.case = T)            ~ "Lokalate",
      grepl("Diabetamil",products,ignore.case = T)       ~ "Diabetamil",
      grepl("hilo|hi lo",products,ignore.case = T)       ~ "HILO")) %>% 
    ungroup() %>% 
    filter(!grepl("total",products,ignore.case = T))
}
  

if(marker == 4){
  # kita pecahin
  tarjeta =
    df_target %>% 
    mutate(submission_date = as.Date(submission_date,"%Y-%m-%d")) %>% 
    separate(area_klasifikasi_customer_customer_code,
             sep  = "\\,",
             into = c("area","klasifikasi_customer","customer_name","customer_code")) %>% 
    mutate(pic                  = stringr::str_trim(pic),
           area                 = stringr::str_trim(area),
           klasifikasi_customer = stringr::str_trim(klasifikasi_customer),
           customer_code        = stringr::str_trim(customer_code)
    ) %>% 
    separate_rows(product_list,sep = "\r\n") %>% 
    separate(product_list,
             sep  = "\\(Amount:",
             into = c("products","pecah")) %>% 
    separate(pecah,
             sep  = "\\:",
             into = c("hapus","status_produk")) %>% 
    rename(amount = hapus) %>% 
    mutate(products      = stringr::str_trim(products),
           status_produk = stringr::str_trim(status_produk),
           status_produk = gsub("\\)","",status_produk),
           amount        = 0) %>% 
    rowwise() %>% 
    mutate(brand = case_when(
      grepl("ts",products,ignore.case = T)               ~ "TS",
      grepl("lmen|l men|l-men",products,ignore.case = T) ~ "L-Men",
      grepl("ns|nutri",products,ignore.case = T)         ~ "NS",
      grepl("lokal",products,ignore.case = T)            ~ "Lokalate",
      grepl("Diabetamil",products,ignore.case = T)       ~ "Diabetamil",
      grepl("hilo|hi lo",products,ignore.case = T)       ~ "HILO")) %>% 
    ungroup() %>% 
    filter(!grepl("total",products,ignore.case = T))
}

tarjeta
