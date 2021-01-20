#
#   ikanx101.com proudly present
#   jotform converter
#

# ---------------------------------
# panggil library 
library(reshape2)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(shinydashboard)
library(tidytext)
library(shiny)
library(shinymanager)


# ---------------------------------
rm(list=ls())

# buat credential
credentials = data.frame(
    user = c("ikanx", "nutrifood"), # mandatory
    password = c("suntea", "rawabali"), # mandatory
    admin = c(TRUE, TRUE),
    stringsAsFactors = FALSE
)

# ---------------------------------
# USER INTERFACE

# header
header = dashboardHeader(title = "Jotform Converter",
                         titleWidth = 250)

#sidebar menu
sidebar = dashboardSidebar(width = 250,
                           sidebarMenu(
                               menuItem(tabName = 'filterpane',
                                        text = 'Read Me',icon = icon('check')),
                               menuItem(tabName = 'converter',
                                        text = 'Converter',icon = icon('magic'))
                                    )
                           )

# tab Read Me
filterpane = tabItem(tabName = 'filterpane',
                     fluidRow(
                         column(width = 12,
                                h1('Read Me'),
                                br(),
                                h4("Web apps converter ini digunakan untuk mengubah struktur data hasil export dari JotForm menjadi struktur yang mudah dipivot di Ms. Excel."),
                                h4("Pastikan bahwa file yang hendak di-convert memiliki ukuran < 5 Mb dan memiliki penamaan variabel standar (belum pernah diubah atau diedit sebelumnya)."),
                                br(),
                                h5("Jika terjadi kendala atau pertanyaan, feel free to discuss ya: fadhli.mohammad@nutrifood.co.id"),
                                br(),
                                br(),
                                h3("update 6 Januari 2021 10:26 WIB"),
                                h4("Apa yang berubah?"),
                                h4("Selamat tahun baru semuanya. Kali ini sudah menggunakan format di tahun baru. Selamat dan semangat bekerja semuanya."),
                                h5("copyright 2020"),
                                h5("Dibuat menggunakan R")
                         )
                     )
            )

# tab Converter
converter = tabItem(tabName = 'converter',
                     fluidRow(
                         column(width = 12,
                                h1('Converter'),
                                h4("Silakan upload file Anda:"),
                                fileInput('target_upload', 'Pilih file',
                                          accept = c('xlsx')
                                          ),
                                br(),
                                downloadButton("downloadData", "Download")
                                )
                     )
)


# body
body = dashboardBody(tabItems(filterpane,converter))

# ui all
ui = secure_app(dashboardPage(skin = "green",header,sidebar,body))

server <- function(input, output,session) {
    
    # call the server part
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    data_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        
        # baca data
        data <- read_excel(inFile$datapath) %>% 
            janitor::clean_names() 
        
        # =================================
        # mulai dari sini
        
        data = 
            data %>% 
            mutate(id = c(1:length(submission_date)),
                   tanggal_transaksi = gsub("\\/","-",tanggal_transaksi),
                   tanggal_transaksi = as.Date(tanggal_transaksi,"%m-%d-%Y"),
                   tanggal_transaksi = lubridate::date(tanggal_transaksi),
                   tanggal_transaksi = format(tanggal_transaksi,"%m/%d/%Y")) %>% 
            separate(departemen_area_nama,
                     into = c("departemen","area","nama"),
                     sep = ";") %>% 
            separate(jenis_channel_sub_channel,
                     into = c("jenis_channel","sub_channel"),
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
            )
        
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
        
        data_final = 
            data_final %>% 
            mutate(tanggal_new = tanggal_transaksi) %>% 
            separate(tanggal_new, 
                     into = c("bulan_transaksi_new",
                              "tanggal_transaksi_new",
                              "tahun_transaksi_new"),
                     sep = "/") %>% 
            relocate(tanggal_transaksi_new,bulan_transaksi_new,tahun_transaksi_new,.after = tanggal_transaksi)
        
        tes = colnames(data_final)
        tes = gsub("\\_"," ",tes)
        
        proper <- function(x){
            stringi::stri_trans_general(x,id = "Title")
        }
        
        colnames(data_final) = proper(tes)
        
        # =================================
        # akhir di sini
        return(data_final)
    })
    
    data = data_upload
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Converted Jotform ", Sys.time(), ".xlsx", sep="")
        },
        content = function(file) {
            openxlsx::write.xlsx(data(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)