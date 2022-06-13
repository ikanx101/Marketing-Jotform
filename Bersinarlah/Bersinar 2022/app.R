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
    user = c("nutrifood", "xx"), # mandatory
    password = c("nutrisari", "xx"), # mandatory
    admin = c(TRUE, TRUE),
    stringsAsFactors = FALSE
)

# ---------------------------------
# USER INTERFACE

# header
header = dashboardHeader(title = "Jotform Converter AM 2022",
                         titleWidth = 300)

#sidebar menu
sidebar = dashboardSidebar(width = 300,
                           sidebarMenu(
                               menuItem(tabName = 'filterpane',
                                        text = 'Read Me',icon = icon('check')),
                               menuItem(tabName = 'converter_1',
                                        text = 'Converter Sales Survey',icon = icon('dollar')),
                               menuItem(tabName = 'converter_2',
                                        text = 'Converter Sales Survey 2022',icon = icon('thumbs-o-up'),
                                        badgeLabel = "New!", badgeColor = "blue"),
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
                                h2("update 31 May 2022 10:58 WIB"),
                                h4("Apa yang berubah?"),
                                h5("Struktur baru form survey sales 2022."),
                                h5("copyright 2022"),
                                h5("Dibuat menggunakan R")
                         )
                     )
            )

# tab Converter Sales pre historic
converter = tabItem(tabName = 'converter_1',
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

# tab Converter Sales 2022 TERBARU!!!
converter_2 = tabItem(tabName = 'converter_2',
                    fluidRow(
                      column(width = 12,
                             h1('Converter Sales Survey'),
                             h3("Untuk form isian terbaru Juni 2022"),
                             h4("Silakan upload file Anda:"),
                             fileInput('target_upload_3', 'Pilih file',
                                       accept = c('xlsx')
                             ),
                             br(),
                             downloadButton("downloadData_3", "Download")
                            )
                          )
                      )

# body
body = dashboardBody(tabItems(filterpane,converter,converter_2,awareness))

# ui all
ui = secure_app(dashboardPage(skin = "green",header,sidebar,body))

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
    
    
    # =============================================================================
    # ini skrip untuk konverter yang lama
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
        
        proper = function(x){
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
    
    
    # =============================================================================
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
    
    
    # =============================================================================
    # ini skrip untuk konverter yang baru
    # 2022 style
    data_upload_3 = reactive({
      inFile <- input$target_upload_3
      if (is.null(inFile))
        return(NULL)
      
      # baca data
      data = read_excel(inFile$datapath) %>% 
        janitor::clean_names() 
      
      # =================================
      # mulai dari sini - paste
      
      # function untuk split tanggal submisi
      tanggal_submisi_func = function(tgl){
        # proses split tanggal
        tgl = 
          tgl %>% 
          strsplit(split = " ") %>% 
          unlist() %>% 
          .[[1]] %>% 
          as.Date(format = "%Y-%m-%d")
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
        separate(darimana_asal_barang_yang_kamu_jual,
                 into = c("sumber_barang","jenis_nutrimart"),
                 sep = "\\;") %>% 
        mutate(sumber_barang = trimws(sumber_barang),
               jenis_nutrimart = trimws(jenis_nutrimart)) %>% 
        select(-platform_online_merchant,-merchant_collaboration,-penjualan)
      
      # data kedua
      data_2 = 
        data %>% 
        select(id,platform_online_merchant) %>% 
        separate_rows(platform_online_merchant,
                      sep = "\r\n") %>% 
        dcast(id ~ platform_online_merchant,
              length,
              value.var = "platform_online_merchant") %>% 
        select(-`NA`,-id)
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
      
      # data ketiga
      data_3 = 
        data %>% 
        select(id,merchant_collaboration) %>% 
        separate_rows(merchant_collaboration,
                      sep = "\r\n") %>% 
        dcast(id ~ merchant_collaboration,
              length,
              value.var = "merchant_collaboration") %>% 
        select(-id,-`NA`)
      # jika tiada isiannya
      if(is.null(data_3$`Product listing`)){data_3$`Product listing` = 0}
      if(is.null(data_3$`Product Bundling`)){data_3$`Product Bundling` = 0}
      if(is.null(data_3$`Product Collaboration`)){data_3$`Product Collaboration` = 0}
      if(is.null(data_3$`Branding Offline`)){data_3$`Branding Offline` = 0}
      if(is.null(data_3$`Branding Online`)){data_3$`Branding Online` = 0}
      # kita ubah jadi ya dan no
      data_3[data_3 == 0] = "No"
      data_3[data_3 == 1] = "Yes"
      data_3$id = 1:nrow(data_3)
      
      # data keempat
      data_4 = 
        data %>% 
        select(id,penjualan) %>% 
        separate_rows(penjualan,
                      sep = "\n") %>% 
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
    
    data_3 = data_upload_3
    
    output$downloadData_3 <- downloadHandler(
      filename = function() {
        paste("Sales Survey Jotform NEW VERSION", Sys.time(), ".xlsx", sep="")
      },
      content = function(file) {
        openxlsx::write.xlsx(data_3(), file)
      }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# alhamdulillah selesai


