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
library(ggplot2)
library(ggpubr)
library(ggalluvial)
library(leaflet)


# ---------------------------------
rm(list=ls())

# buat credential
credentials = data.frame(
    user = c("aa", "xx"), # mandatory
    password = c("aa", "xx"), # mandatory
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
                                        text = 'Converter Sales Survey',icon = icon('dollar')),
                               menuItem(tabName = 'awareness',
                                        text = 'Converter Awareness Survey',icon = icon('comment'))
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
                                h2("update 2 Maret 2022 10:36 WIB"),
                                h4("Apa yang berubah?"),
                                h5("Urutan kolom."),
                                h5("copyright 2022"),
                                h5("Dibuat menggunakan R")
                         )
                     )
            )

# tab Converter Sales
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
                     ),
                    br(),
                    fluidRow(
                        column(width = 5,
                               h2("Sebaran toko yang disurvey"),
                               leafletOutput('peta_plot',height = 350),
                               br(),
                               h2("Kalender Kunjungan"),
                               plotOutput("kalen_plot",height = 850)),
                        column(width = 7,
                               h2("Banyaknya Customer"),
                               plotOutput("alluvial_plot",height = 1200))
                    )
)

# tab Converter Awareness
converter_2 = tabItem(tabName = 'awareness',
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
body = dashboardBody(tabItems(filterpane,converter,converter_2))

# ui all
ui = secure_app(dashboardPage(skin = "black",header,sidebar,body))

# server part
server <- function(input, output,session) {
    
    # call the server part
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
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
    
    # bikin judul properly
    proper <- function(x){
        stringi::stri_trans_general(x,id = "Title")
    }
    
    data_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        
        # baca data
        data <- read_excel(inFile$datapath) %>% 
            janitor::clean_names() 
        
        # =================================
        # mulai dari sini - paste
        
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
        
        # kalau mau hapus, baris atas ini
        # =================================
        # akhir di sini
        return(data_final)
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
    
    # peta survey
    output$peta_plot = renderLeaflet({
        
        new_data = 
            data_upload() %>%
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
        })
    
    
    output$kalen_plot = renderPlot({
        data_upload() %>% 
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
    })
    
    output$alluvial_plot = renderPlot({
        data_upload() %>% 
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
    })
    

    
    # converter awareness ya
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
            mutate(submission_date = as.Date(submission_date,"%Y-%m-%d"),
                   tanggal_kegiatan = as.Date(tanggal_kegiatan,"%d/%m/%Y")) %>% 
            separate(dept_provinsi_kota_kab_kecamatan,
                     into = c("department","provinsi","kota_kab","kecamatan"),
                     sep = ";") %>% 
            separate(brand_projek_materi,
                     into = c("brand","project","materi"),
                     sep = ";") %>% 
            separate(jenis_channel_sub_channel_klasifikasi,
                     into = c("jenis_channel","sub_channel","klasifikasi_channel"),
                     sep = ";") %>% 
            separate(kanal_platform_lokasi_room,
                     into = c("kanal","platform","lokasi_room"),
                     sep = ";") %>% 
            rename(nama = nama_spg_mr) %>% 
            mutate(department = trimws(department),
                   nama = trimws(nama),
                   brand = trimws(brand),
                   project = trimws(project),
                   materi = trimws(materi),
                   jenis_channel = trimws(jenis_channel),
                   sub_channel = trimws(sub_channel),
                   klasifikasi_channel = trimws(klasifikasi_channel),
                   kanal = trimws(kanal),
                   platform = trimws(platform),
                   lokasi_room = trimws(lokasi_room),
                   provinsi = trimws(provinsi),
                   kota_kab = trimws(kota_kab),
                   kecamatan = trimws(kecamatan))
        
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