setwd("~/Marketing-Jotform/Algoritma/2024/AV sales v2")

# bebersih global environment
rm(list = ls())

# memanggil libraries
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)

# nama file jotform
nama_file = "Jotform_Availability_MPO_Area_M2024-05-21_05_52_03.xlsx"

# function untuk split tanggal submisi
tanggal_submisi_func = function(tgl) {
  # proses split tanggal
  # tgl = as.Date(tgl,"%Y-%m-%d %hh:%mm:ss")
  tgl = lubridate::date(tgl)
  tgl = format(tgl, "%m/%d/%Y")
  # output tanggal
  return(tgl)
}

# fungsi untuk bikin judul proper
proper_new = function(x) {
  tess = stringi::stri_trans_general(x, id = "Title")
  gsub("\\_", " ", tess)
}

# memanggil dataset baru
data =
  read_excel(nama_file) %>%
  janitor::clean_names() %>%
  rowwise() %>%
  mutate(submission_date = tanggal_submisi_func(submission_date)) %>%
  ungroup() %>%
  separate(
    spg_area_klasifikasi_customer_customer_code,
    into = c(
      "nama_spg",
      "area",
      "klasifikasi_customer",
      "customer_code"
    ),
    sep = "\\,"
  ) %>%
  mutate(
    `nama_spg`             = trimws(`nama_spg`),
    `area`                 = trimws(area),
    `klasifikasi_customer` = trimws(`klasifikasi_customer`),
    `customer_code`        = trimws(`customer_code`)
  )

# kita tambahkan variabel id
data$id = 1:nrow(data)

# kita akan pisah terlebih dahulu
data_1 = data %>% select(id,
                         submission_date,
                         area,
                         nama_spg,
                         klasifikasi_customer,
                         customer_code)

# kita akan pilih hanya data product list
data_2 = data %>% select(id, my_products)

# data kedua adalah bentuk tabular dari penjualan
data_3 =
  data_2 %>%
  rename(penjualan = my_products) %>%
  separate_rows(penjualan, sep = "\n") %>%  
  mutate(penjualan = trimws(penjualan)) |> 
  filter(!grepl("total", penjualan, ignore.case = T)) |> 
  separate(penjualan,
           into = c("produk_list", "dummy"),
           sep = "\\(Amount:") |> 
  mutate(produk_list = trimws(produk_list)) %>%
  separate(dummy, into = c("amount", "quantity"), sep = "IDR, :") |> 
  mutate(amount = trimws(amount),
         amount = as.numeric(amount)) |> 
  mutate(quantity = gsub("\\)","",quantity),
         quantity = trimws(quantity)) |> 
  rename(status_produk = quantity) |> 
  rowwise() %>%
  mutate(
    brand = case_when(
      grepl("ts", produk_list, ignore.case = T) ~ "TS",
      grepl("lmen|l men|l-men", produk_list, ignore.case = T) ~ "L-Men",
      grepl("ns|nutri", produk_list, ignore.case = T) ~ "NS",
      grepl("lokal|lklt", produk_list, ignore.case = T) ~ "Lokalate",
      grepl("Diabetamil", produk_list, ignore.case = T) ~ "Diabetamil",
      grepl("hilo|hi lo", produk_list, ignore.case = T) ~ "HILO"
    )
  ) %>%
  ungroup()

# kita kumpulin dulu data_1, data_2, data_3
data_kumpul = merge(data_1, data_3) %>% select(-id)

colnames(data_kumpul) = proper_new(colnames(data_kumpul))
openxlsx::write.xlsx(data_kumpul, file = "output.xlsx")