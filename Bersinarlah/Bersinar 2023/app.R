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
credentials  = data.frame(
    user     = c("nutrifood", "ikanx101","a"), # mandatory
    password = c("nutrisari", "suntea101","a"), # mandatory
    admin    = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
)

waktu_update = Sys.time() %>% format("%B %d, %Y - %H:%M")

# ---------------------------------
# USER INTERFACE

# header
header = dashboardHeader(title = "Jotform Converter AM 2024 v1.0",
                         titleWidth = 400)

#sidebar menu
sidebar = dashboardSidebar(width = 400,
                           sidebarMenu(
                               menuItem(tabName = 'filterpane',
                                        text = 'Read Me',icon = icon('check')),
                               menuItem(tabName = 'converter',
                                        text = 'Converter Sales Survey',icon = icon('dollar-sign')),
                               menuItem(tabName = 'awareness',
                                        text = 'Converter Awareness Survey',icon = icon('spinner')),
                               menuItem(tabName = 'spg',
                                        text = 'Converter SPG Event',icon = icon('child-dress')),
                               menuItem(tabName = 'form2023',
                                        text = 'Converter Semester II 2023',icon = icon('calendar')),
                               menuItem(tabName = 'form2024',
                                        text = 'Converter Nutrihub dan Networking',icon = icon('calendar')),
                               menuItem(tabName = 'jessyanti',
                                        text = 'AV Sales & AM',icon = icon('globe'),
                                        badgeLabel = "N E W !", badgeColor = "yellow")
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
                                h5("Jotform AV Sales dan AM"),
                                h5("Copyright 2024"),
                                h5("Dibuat menggunakan R")
                         )
                     )
            )

# tab Converter Sales BARU
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


# tab Converter Awareness BARU
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

# tab Converter SPG Event
spg_event = tabItem(tabName = 'spg',
                    fluidRow(
                      column(width = 12,
                             h1('Converter SPG Event'),
                             h4("Silakan upload file Anda:"),
                             fileInput('target_upload_3', 'Pilih file',
                                       accept = c('xlsx')
                             ),
                             br(),
                             downloadButton("downloadData_3", "Download")
                      )
                    )
            )

# tab Form 2023 Semester II
form_2023 = tabItem(tabName = 'form2023',
                    fluidRow(
                      column(width = 12,
                             h1('Converter Form Baru Semester II 2023'),
                             h3("Perhatikan FORMAT TANGGAL pada file yang hendak dikonversi!"),
                             br(),
                             h4("Silakan upload file Anda:"),
                             fileInput('target_upload_4', 'Pilih file',
                                       accept = c('xlsx')
                             ),
                             br(),
                             downloadButton("downloadData_4", "Download")
                      )
                    )
            )

# tab Form 2024 Semester II
form_2024 = tabItem(tabName = 'form2024',
                    fluidRow(
                      column(width = 12,
                             h1('Converter Form Baru Nutrihub dan Networking'),
                             h3("Perhatikan FORMAT TANGGAL pada file yang hendak dikonversi!"),
                             br(),
                             h4("Silakan upload file Anda:"),
                             fileInput('target_upload_5', 'Pilih file',
                                       accept = c('xlsx')
                             ),
                             br(),
                             downloadButton("downloadData_5", "Download")
                      )
                    )
          )

# tab khusus jessyanti
jessyanti = tabItem(tabName = 'jessyanti',
                    fluidRow(
                      column(width = 12,
                             h1('Converter Form Baru AV Sales dan AM'),
                             h3("Perhatikan FORMAT TANGGAL pada file yang hendak dikonversi!"),
                             br(),
                             h4("Silakan upload file Anda:"),
                             fileInput('target_upload_6', 'Pilih file',
                                       accept = c('xlsx')
                             ),
                             br(),
                             downloadButton("downloadData_6", "Download")
                      )
                    )
          )






# body
body = dashboardBody(tabItems(filterpane,converter,awareness,spg_event,form_2023,form_2024,
                              jessyanti))

# ui all
ui = secure_app(dashboardPage(skin = "blue",header,sidebar,body))

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
                 kecamatan = trimws(kecamatan)) %>% 
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
        data_2 = data_2 %>% select(id,GoFood,GrabFood,ShopeeFood,
                                   `Delivery Mandiri`,
                                   `Online Food Delivery Lainnya`)
        
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
        if(is.null(data_3$`Branding Offline`)){data_3$`Branding Offline` = 0}
        if(is.null(data_3$`Branding Online`)){data_3$`Branding Online` = 0}
        if(is.null(data_3$`Product Bundling`)){data_3$`Product Bundling` = 0}
        if(is.null(data_3$`Product listing`)){data_3$`Product listing` = 0}
        if(is.null(data_3$`Product Collaboration`)){data_3$`Product Collaboration` = 0}
        if(is.null(data_3$`Tidak ada`)){data_3$`Tidak ada` = 0}
        
        # kita paksakan urutan kolom
        data_3 = data_3 %>% select(id,`Branding Offline`,`Branding Online`,
                                   `Product Bundling`,`Product listing`,
                                   `Product Collaboration`,`Tidak ada`)
        
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
    # converter awareness baru
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
    
    
    # =============================================================================
    # converter SPG EVENT Terbaru
    data_upload_3 <- reactive({
      inFile <- input$target_upload_3
      if (is.null(inFile))
        return(NULL)
      
      # baca data
      df <- read_excel(inFile$datapath) %>% 
        janitor::clean_names() 
      
      # =================================
      # mulai dari sini
      
      # kita akan buat function tanggal terlebih dahulu
      konversi = function(tgl){
        as.Date(tgl,"%B %d, %Y")
      }
      
      data_final = 
        df %>% 
        mutate(id = 1:nrow(df)) %>% 
        relocate(id,.before = submission_date) %>% 
        rowwise() %>% 
        mutate(submission_date   = konversi(submission_date),
               tanggal_transaksi = konversi(tanggal_transaksi)) %>% 
        mutate(submission_date   = format(submission_date,"%B %d, %Y"),
               tanggal_transaksi = format(tanggal_transaksi,"%B %d, %Y")) %>% 
        ungroup() %>% 
        separate(dept_provinsi_kota_kab_nama_toko_nama_spg_event,
                 into = c("dept","provinsi","kota_kab","nama_toko","nama_spg_event"),
                 sep  = ";") %>% 
        mutate(dept           = trimws(dept),
               provinsi       = trimws(provinsi),
               kota_kab       = trimws(kota_kab),
               nama_toko      = trimws(nama_toko),
               nama_spg_event = trimws(nama_spg_event)) %>% 
        separate_rows(penjualan,
                      sep = "\n") %>% 
        filter(!grepl("total",penjualan,ignore.case = T)) %>% 
        separate(penjualan,
                 into = c("item","info","qty"),
                 sep  = ":") %>% 
        mutate(item = gsub("\\(Amount","",item),
               item = trimws(item)) %>% 
        separate(info,
                 into = c("price","info_2"),
                 sep  = "IDR,") %>% 
        mutate(price = gsub("\\.00","",price),
               price = gsub("\\,","",price),
               price = as.numeric(price)) %>% 
        mutate(quantity = gsub("\\)","",qty),
               quantity = as.numeric(quantity)) %>% 
        select(-info_2,-qty) %>% 
        mutate(total_value = price * quantity)
      
      
      # proses untuk membersihkan nama kolom
      proper <- function(x){
        stringi::stri_trans_general(x,id = "Title")
      }
      # kita simpan dulu nama kolomnya
      tes = colnames(data_final)
      tes = gsub("\\_"," ",tes)
      
      colnames(data_final) = proper(tes)
      
      # =================================
      # akhir di sini
      return(data_final)
    })
    
    data_3 = data_upload_3
    
    output$downloadData_3 <- downloadHandler(
      filename = function() {
        paste("SPG Event Jotform ", Sys.time(), ".xlsx", sep="")
      },
      content = function(file) {
        openxlsx::write.xlsx(data_3(), file)
      }
    )
    
    
    
    
    
    
    
    # =============================================================================
    # converter FORM 2023 TERBARU!!!
    data_upload_4 <- reactive({
      inFile <- input$target_upload_4
      if (is.null(inFile))
        return(NULL)
      
      # baca data
      df <- read_excel(inFile$datapath) %>% 
        janitor::clean_names() 
      
      # =================================
      # mulai dari sini
      
      # function untuk split tanggal submisi
      tanggal_submisi_func = function(tgl){
        # proses split tanggal
        tgl = as.Date(tgl,"%d/%m/%Y")
        tgl = format(tgl,"%d/%m/%Y")
        # output tanggal
        return(tgl)
      }
      
      
      # fungsi untuk bikin judul proper
      proper_new = function(x){
        tess = stringi::stri_trans_general(x,id = "Title")
        gsub("\\_"," ",tess)
      }
      
      # memanggil dataset baru
      data = 
        df %>% 
        rename(penjualan_products = penjualan) %>% 
        rowwise() %>% 
        mutate(submission_date   = tanggal_submisi_func(submission_date),
               #tanggal_kegiatan  = ifelse(is.na(tanggal_kegiatan),
               #                          NA,
               #                          tanggal_submisi_func(tanggal_kegiatan)),
               tanggal_transaksi = tanggal_submisi_func(tanggal_transaksi)) %>% 
        ungroup() 
      
      # bikin unique id dulu
      data$id = 1:nrow(data)
      
      # ini kita pindahin ke depan si id-nya
      data = data %>% relocate(id,.before = submission_date)
      
      colnames(data)
      
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
        separate(projek_jenis_channel_sub_channel,
                 into = c("project","jenis_channel","sub_channel"),
                 sep = "\\;") %>% 
        mutate(project = trimws(project),
               jenis_channel = trimws(jenis_channel),
               sub_channel = trimws(sub_channel)) %>% 
        separate(brand_tidak_deal,
                 into = c("brand_tidak_deal_1","brand_tidak_deal_2",
                          "brand_tidak_deal_3"),
                 sep = "\\-") %>% 
        mutate(brand_tidak_deal_1 = trimws(brand_tidak_deal_1),
               brand_tidak_deal_2 = trimws(brand_tidak_deal_2),
               brand_tidak_deal_3 = trimws(brand_tidak_deal_3)) %>% 
        select(-penjualan_products)
      
      # data kedua adalah bentuk tabular dari penjualan
      data_2 = 
        data %>% 
        select(id,penjualan_products) %>%
        rename(penjualan = penjualan_products) %>%
        mutate(penjualan = gsub("\\\r","",penjualan)) %>% 
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
      data_kumpul = merge(data_1,data_2)
      
      # nah tugas kita belum sepenuhnya rebes karena ada baris duplikat yang
      # harus dihapus
      # kita pisah per id
      data_temp = data_kumpul %>% group_split(id)
      
      # kita buat function penghapus
      rapihinkan_angka = function(tes,nrow_tes){
        tes$participant[2:nrow_tes]                   = NA
        tes$jumlah_voucher_dibagikan[2:nrow_tes]      = NA
        tes$jumlah_voucher_yang_di_redeem[2:nrow_tes] = NA
        return(tes)
      }
      
      # kita mulai
      n_iter    = length(data_temp)
      for(i in 1:n_iter){
        # dibuat temporary dulu
        tes       = data_temp[[i]]
        nbaris    = nrow(tes)
        if(nbaris > 1){
          output         = rapihinkan_angka(tes,nbaris)
          # kita kembalikan lagi ke data awal
          data_temp[[i]] = output
        }
        print(i)
      }
      
      # kita gabung data final
      data_final = do.call(rbind,data_temp) 
      # hitung bulan
      bulan_hit  = lubridate::month(data_final$submission_date,label = T) %>% as.character()
      
      # gabungan data dan bebersih
      data_final = 
        data_final %>% 
        mutate(bulan = ifelse(is.na(tanggal_kegiatan),
                              bulan_hit,
                              gsub("[0-9]|\\,| ","",tanggal_kegiatan)
        )) %>% 
        relocate(bulan,.after = "tanggal_kegiatan")
      
      
      colnames(data_final) = proper_new(colnames(data_final))
      
      
      # =================================
      # akhir di sini
      return(data_final)
    })
    
    data_4 = data_upload_4
    
    output$downloadData_4 <- downloadHandler(
      filename = function() {
        paste("Jotform Semester II ", Sys.time(), ".xlsx", sep="")
      },
      content = function(file) {
        openxlsx::write.xlsx(data_4(), file)
      }
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # =============================================================================
    # converter FORM nutrihub dan networking 
    data_upload_5 <- reactive({
      inFile <- input$target_upload_5
      if (is.null(inFile))
        return(NULL)
      
      # baca data
      df_input <- 
        read_excel(inFile$datapath) %>% 
        janitor::clean_names() 
      
      # =================================
      # mulai dari sini
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
        mutate(bulan       = as.Date(tanggal_kerjasama,"%d/%m/%Y"),
               bulan       = months(bulan)) %>% 
        relocate(bulan,.after = "tanggal_kerjasama") 
      
      # kita rapihin colnames
      tes = colnames(data_final)
      tes = gsub("\\_"," ",tes)
      
      # fungsi untuk bikin judul proper
      proper_new = function(x){
        tess = stringi::stri_trans_general(x,id = "Title")
        gsub("\\_"," ",tess)
      }
      
      colnames(data_final) = proper_new(colnames(data_final))
      
      # =================================
      # akhir di sini
      return(data_final)
    })
    
    data_5 = data_upload_5
    
    output$downloadData_5 <- downloadHandler(
      filename = function() {
        paste("Jotform Nutrihub dan Networking ", Sys.time(), ".xlsx", sep="")
      },
      content = function(file) {
        openxlsx::write.xlsx(data_5(), file)
      }
    )
    
    
    
    
    
    
    
    
    # =============================================================================
    # converter AV Sales & AM TERBARU!!!
    data_upload_6 <- reactive({
      inFile <- input$target_upload_6
      if (is.null(inFile))
        return(NULL)
      
      # baca data
      df_input <- 
        read_excel(inFile$datapath) %>% 
        janitor::clean_names() 
      
      # =================================
      # mulai dari sini
      # sekarang kita akan kerjakan proses konversinya
      
      # function untuk split tanggal submisi
      tanggal_submisi_func = function(tgl){
        # proses split tanggal
        tgl = as.Date(tgl,"%B %d, %Y")
        tgl = format(tgl,"%m/%d/%Y")
        # output tanggal
        return(tgl)
      }
      
      # fungsi untuk bikin judul proper
      proper_new = function(x){
        tess = stringi::stri_trans_general(x,id = "Title")
        gsub("\\_"," ",tess)
      }
      
      # memanggil dataset baru
      data = 
        df_input %>% 
        rowwise() %>% 
        mutate(submission_date = tanggal_submisi_func(submission_date)) %>% 
        ungroup() %>% 
        separate(area_nama_spg_klasifikasi_toko_nama_toko,
                 into = c("area","nama_spg","klasifikasi_toko","nama_toko"),
                 sep = "\\,") %>% 
        mutate(`area`             = trimws(area),
               `nama_spg`         = trimws(`nama_spg`),
               `klasifikasi_toko` = trimws(`klasifikasi_toko`),
               `nama_toko`        = trimws(`nama_toko`)) 
      
      # kita tambahkan variabel id
      data$id = 1:nrow(data)
      
      # kita akan pisah terlebih dahulu
      data_1 = data %>% select(id,submission_date,area,nama_spg,klasifikasi_toko,nama_toko)
      
      # kita akan pilih hanya data product list
      data_2 = data %>% select(id,produk_list)
      
      # data kedua adalah bentuk tabular dari penjualan
      data_3 = 
        data_2 %>% 
        rename(penjualan = produk_list) %>%
        mutate(penjualan = gsub("\\\r","",penjualan)) %>% 
        separate_rows(penjualan,
                      sep = "\n") %>%  # ini perubahan terbaru ya
        filter(!grepl("total",penjualan,ignore.case = T)) %>% 
        separate(penjualan,
                 into = c("produk_list","dummy"),
                 sep = "\\(Amount:") %>% 
        mutate(produk_list = trimws(produk_list)) %>% 
        separate(dummy,
                 into = c("amount","quantity"),
                 sep = "IDR, Quantity:") %>% 
        mutate(amount = as.numeric(amount)) %>% 
        separate(quantity,
                 into = c("quantity","status_produk"),
                 sep = "\\,") %>% 
        mutate(quantity = trimws(quantity),
               quantity = as.numeric(quantity),
               status_produk = gsub("[^[:alnum:]]", " ", status_produk),
               status_produk = trimws(status_produk)) %>% 
        rowwise() %>% 
        mutate(brand = case_when(
          grepl("ts",produk_list,ignore.case = T) ~ "TS",
          grepl("lmen|l men|l-men",produk_list,ignore.case = T) ~ "L-Men",
          grepl("ns|nutri",produk_list,ignore.case = T) ~ "NS",
          grepl("lokal",produk_list,ignore.case = T) ~ "Lokalate",
          grepl("Diabetamil",produk_list,ignore.case = T) ~ "Diabetamil",
          grepl("hilo",produk_list,ignore.case = T) ~ "HILO"
        )
        ) %>% 
        ungroup()
      
      # kita kumpulin dulu data_1, data_2, data_3
      data_final = merge(data_1,data_3) %>% select(-id)
      
      # kita rapihin colnames
      colnames(data_final) = proper_new(colnames(data_final))
      
      # =================================
      # akhir di sini
      return(data_final)
    })
    
    data_6 = data_upload_6
    
    output$downloadData_6 <- downloadHandler(
      filename = function() {
        paste("Jotform AV Sales & AM ", Sys.time(), ".xlsx", sep="")
      },
      content = function(file) {
        openxlsx::write.xlsx(data_6(), file)
      }
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)

# alhamdulillah selesai
