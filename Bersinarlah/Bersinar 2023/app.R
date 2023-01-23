#
#   ikanx101.com proudly present
#   jotform converter 2023
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
    user = c("nutrifood", "ikanx_server"), # mandatory
    password = c("nutrisari", "ahnaf"), # mandatory
    admin = c(TRUE, TRUE),
    stringsAsFactors = FALSE
)

waktu_update = Sys.time() %>% as.character()

# ---------------------------------
# USER INTERFACE

# header
header = dashboardHeader(title = "Jotform Converter AM v2.023",
                         titleWidth = 300)

#sidebar menu
sidebar = dashboardSidebar(width = 300,
                           sidebarMenu(
                               menuItem(tabName = 'filterpane',
                                        text = 'Read Me',icon = icon('check')),
                               menuItem(tabName = 'converter',
                                        text = 'Converter Sales Survey',icon = icon('dollar')),
                               menuItem(tabName = 'awareness',
                                        text = 'Converter Awareness Survey',icon = icon('spinner'))
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
                                h4(paste0("update ",waktu_update)),
                                h4("Apa yang berubah?"),
                                h5("Perubahan menjadi format survey tahun 2023"),
                                h5("Copyright 2023"),
                                h5("Dibuat menggunakan R")
                         )
                     )
            )

# tab Converter Sales pre historic
converter = tabItem(tabName = 'converter',
                     fluidRow(
                         column(width = 12,
                                h1('Converter Sales Survey'),
                                h4("Silakan upload file Anda:"),
                                fileInput('target_upload', 'Pilih file',
                                          accept = c('xlsx')
                                          ),
                                br(),
                                downloadButton("downloadData", "Download")
                                )
                          )
                    )


# tab Converter Awareness pre historic
awareness = tabItem(tabName = 'awareness',
                    fluidRow(
                        column(width = 12,
                               h1('Converter Awareness Survey'),
                               h4("Silakan upload file Anda:"),
                               fileInput('target_upload_2', 'Pilih file',
                                         accept = c('xlsx')
                               ),
                               br(),
                               downloadButton("downloadData_2", "Download")
                        )
                    )
              )

# body
body = dashboardBody(tabItems(filterpane,converter,awareness))

# ui all
ui = secure_app(dashboardPage(skin = "green",header,sidebar,body))

# server part
server <- function(input, output,session) {
    
    # call the server part
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    # =============================================================================
    # ini skrip untuk konverter data sales
    data_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        
        # baca data
        data <- read_excel(inFile$datapath) %>% 
            janitor::clean_names() 
        
        # =================================
        # mulai dari sini - paste
        
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
          data %>% 
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
                 kecamatan = trimws(kecamatan),
                 nama_project_others = as.character(nama_project_others)) %>% 
          separate(project_jenis_channel_sub_channel,
                   into = c("project","jenis_channel","sub_channel"),
                   sep = "\\;") %>% 
          mutate(project = trimws(project),
                 jenis_channel = trimws(jenis_channel),
                 sub_channel = trimws(sub_channel)) %>% 
          # ini adalah perbedaan di tahun 2023
          mutate(brand_tidak_deal = gsub("\r","",brand_tidak_deal)) %>%
          separate(brand_tidak_deal,
                   into = c("brand_tidak_deal_1","brand_tidak_deal_2","brand_tidak_deal_3"),
                   sep = "\n") %>% 
          select(-platform_online_merchant, #ini akan kita pecah sesuai dengan kategori
                 -penjualan, #ini kita pecah jadi tabular,
                 -merchant_collaboration
          )
        
        
        # data kedua, hanya platform online merchant yang akan kita pecah sendiri
        data_2 = 
          data %>% 
          select(id,platform_online_merchant) %>% 
          mutate(platform_online_merchant = ifelse(is.na(platform_online_merchant),
                                                   "Tidak ada",
                                                   platform_online_merchant)) %>% 
          mutate(platform_online_merchant = gsub("\r","",platform_online_merchant)) %>%
          separate_rows(platform_online_merchant,
                        sep = "\n") %>% 
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
        data_4 = 
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
        if(is.null(data_3$`Tidak ada`)){data_3$`Tidak ada` = 0}
        
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
        
        # kalau mau hapus, baris atas ini
        # =================================
        # akhir di sini
        return(data_kumpul)
    })
    
    data = data_upload
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Sales Survey Jotform ", Sys.time(), ".xlsx", sep="")
        },
        content = function(file) {
            openxlsx::write.xlsx(data(), file)
        }
    )
    
    
    # =============================================================================
    # converter awareness lama
    data_upload_2 <- reactive({
        inFile <- input$target_upload_2
        if (is.null(inFile))
            return(NULL)
        
        # baca data
        data <- read_excel(inFile$datapath) %>% 
            janitor::clean_names() 
        
        # =================================
        # mulai dari sini
        
        data_final = 
          data %>% 
          mutate(submission_date = as.Date(submission_date,format = "%d/%m/%Y"),
                 tanggal_kegiatan = as.Date(tanggal_kegiatan,"%B %d, %Y")) %>% 
          separate(dept_provinsi_kota_kab_kecamatan,
                   into = c("dept","provinsi","kota_kab","kecamatan"),
                   sep = ";") %>% 
          separate(projek_jenis_channel_sub_channel,
                   into = c("projek","jenis_channel","sub_channel"),
                   sep = ";") %>%  
          separate(dimana_event_aktivasi_atau_meeting_dilaksanakan,
                   into = c("jenis_event","platform","jenis_platform"),
                   sep = ";") %>%
          mutate(
            dept = trimws(dept),
            projek = trimws(projek),
            jenis_channel = trimws(jenis_channel),
            sub_channel = trimws(sub_channel),
            jenis_event = trimws(jenis_event),
            platform = trimws(platform),
            jenis_platform = trimws(jenis_platform),
            provinsi = trimws(provinsi),
            kota_kab = trimws(kota_kab),
            kecamatan = trimws(kecamatan)) %>% 
          mutate(bulan = lubridate::month(tanggal_kegiatan,label = T)) %>% 
          relocate(bulan,.after = tanggal_kegiatan) %>% 
          mutate(jumlah_kantin = gsub(" ","",jumlah_kantin),
                 jumlah_kantin = as.numeric(jumlah_kantin),
                 jumlah_voucher_teredeem = gsub(" ","",jumlah_voucher_teredeem),
                 jumlah_voucher_teredeem = as.numeric(jumlah_voucher_teredeem),
                 jumlah_sku_nutrifood_di_kantin_sekolah = gsub(" ","",jumlah_sku_nutrifood_di_kantin_sekolah),
                 jumlah_sku_nutrifood_di_kantin_sekolah = as.numeric(jumlah_sku_nutrifood_di_kantin_sekolah))
        
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
    
    data_2 = data_upload_2
    
    output$downloadData_2 <- downloadHandler(
        filename = function() {
            paste("Awareness Survey Jotform ", Sys.time(), ".xlsx", sep="")
        },
        content = function(file) {
            openxlsx::write.xlsx(data_2(), file)
        }
    )
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)

# alhamdulillah selesai
