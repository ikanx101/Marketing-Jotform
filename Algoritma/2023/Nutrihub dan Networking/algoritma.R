# ==============================================================================
# jotform converter
#
# khusus nutrihub dan networking
#
# created by ikanx101.com
# 9 januari 2024
# ==============================================================================


# ==============================================================================
rm(list=ls())

# libraries
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)

# ambil files
nama_file = "Format Jotform Nutrihub & Networking 2024.xlsx"
sht       = excel_sheets(nama_file)
df_input  = read_excel(nama_file,sheet = sht[1]) %>% janitor::clean_names()

# ==============================================================================


# ==============================================================================
# sekarang kita akan kerjakan proses konversinya
data_final = 
  df_input %>% 
  separate(departemen_provinsi_kota_kab,
           into = c("departemen","provinsi","kota_kab"),
           sep  = "\\;") %>% 
  mutate(departemen = trimws(departemen),
         provinsi   = trimws(provinsi),
         kota_kab   = trimws(kota_kab)) %>% 
  separate(pihak_yang_kerjasama_bentuk_kerjasama,
           into = c("pihak_yang_kerjasama","bentuk_kerjasama"),
           sep  = "\\;") %>% 
  mutate(pihak_yang_kerjasama = trimws(pihak_yang_kerjasama),
         bentuk_kerjasama     = trimws(bentuk_kerjasama)) %>% 
  separate(entitas_sub_entitas,
           into = c("entitas","sub_entitas"),
           sep  = "\\;") %>% 
  mutate(entitas     = trimws(entitas),
         sub_entitas = trimws(sub_entitas)) %>% 
  mutate(bulan       = as.Date(tanggal_kerjasama,"%B %d, %Y"),
         bulan       = months(bulan)) %>% 
  relocate(bulan,.after = "tanggal_kerjasama") 


tes = colnames(data_final)
tes = gsub("\\_"," ",tes)

proper <- function(x){
  stringi::stri_trans_general(x,id = "Title")
}

colnames(data_final) = proper(tes)
write.xlsx(data_final,file = "output.xlsx")
# ==============================================================================