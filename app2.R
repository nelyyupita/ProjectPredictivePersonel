library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(DT)
library(echarts4r) 

footer <- fluidRow(
  div(
    style = "
      width: 100vw;
      padding: 10px;
      position: fixed;
      bottom: 0;
      left: 0;
      background-color: #222;
      margin-top: 20px;
      z-index: 10000;
    ",
    p(
      style="
        text-align: center; 
        color: white; margin: 0;",
      "Copyright TNI AL 2023 Version 0.31")
  )
)

data <- read_csv2("data_pers_kri_rev1.csv")
data1 <- read_csv2("data_pers_kri_rev1.csv")
regulation <- read_csv2(file = "REGULATION.csv")

regulation %>% 
  mutate(DIK_BANG = trimws(DIK_BANG)) -> regulation

jabatan_pred_function <- function(data, nrp) {
  # regulation
  regulation %>% 
    separate(KEN_KAT, into = c("pkt_now", "pkt_next"), sep = "Ke") %>%
    mutate(
      pkt_now = trimws(pkt_now),
      pkt_next = trimws(pkt_next)
    ) %>% 
    rename(MDP_syarat = MDP, MDDP_syarat = MDDP) -> regulation_final
  
  data %>% 
    filter(NRP == nrp) -> one_person_data
  
  regulation_final %>% 
    filter(
      pkt_now == trimws(one_person_data$PKT_RIIL)
    ) -> filtered_jabatan
  
  if (!is.na(one_person_data$DIKBANG)) {
    if (!any(is.na(filtered_jabatan$DIK_BANG))) {
      filtered_jabatan %>% 
        filter(DIK_BANG == trimws(one_person_data$DIKBANG)) %>% 
        select(pkt_now, pkt_next,DIK_BANG, MDP_syarat, JABATAN) -> filtered_jabatan
      one_person_data %>% 
        bind_cols(filtered_jabatan) %>% 
        mutate(bisa_naik_pangkat = ifelse(MDP >= MDP_syarat, "Ya", "Tidak")) -> result
    } else {
      
      filtered_jabatan %>% 
        select(pkt_now, pkt_next,DIK_BANG, MDDP_syarat, JABATAN) -> filtered_jabatan
      
      one_person_data %>% 
        bind_cols(filtered_jabatan) %>% 
        mutate(bisa_naik_pangkat = ifelse(MDDP >= MDDP_syarat, "Ya", "Tidak")) -> result
    }
  } else {
    filtered_jabatan %>% 
      select(pkt_now, pkt_next,DIK_BANG, MDP_syarat, JABATAN) -> filtered_jabatan
    one_person_data %>% 
      bind_cols(filtered_jabatan) -> result
  }
  return(result)
}


ui <- dashboardPage(
  dashboardHeader(
    title = "Predictive Personel"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Main Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Procesing Data", tabName = "profile", icon = icon("users")),
      menuItem("Predictive Pers", tabName = "prediksi", icon = icon("user"))
    )
  ),
  dashboardBody(
    footer,
    tabItems(
      tabItem(tabName = "dashboard",
              h1("Main Dashboard - Predictive Personel"),
              fluidRow(
                valueBox(date(), "Date", icon = icon("calendar-days"), color = "light-blue",  width = 6),
                valueBox(nrow(data), "Jumlah Personel", icon = icon("users"), color = "orange",  width = 6)
              ),
              fluidRow(
                box(title = "Strata Personel", echarts4rOutput("stratadsp"), width = 12)
              ),
              fluidRow(
                box(title = "Korps Personel", echarts4rOutput("korps"), width = 6),
                box(title = "Posisi Jabatan", width = 6, echarts4rOutput("posjab")),
                box(title = "Pendidikan Umum", width = 6, echarts4rOutput("dikum")),
                box(title = "Pendidikan Militer", width = 6, echarts4rOutput("dikbang")),
                box(title = "Prestasi", width = 6, echarts4rOutput("prestasi")),
                box(title = "Hukuman", width = 6, echarts4rOutput("hukuman"))
              )
      ),
      tabItem(
        
        tabName = "profile",
        h1("Profile Personel"),
        
        selectInput(inputId = "profile", label = "NRP", choices = data1$NRP),
        actionButton(inputId = "Btn", label = "search", style= "margin-bottom:14px", icon = icon("magnifying-glass")),
        
        uiOutput("coba"),
        fluidRow( #img(src = "images/pngwing.com.png"),
          
          valueBoxOutput("personalInformation", width = 6),
          valueBoxOutput("pangkat", width = 6),
          
        ),
        
        box(width = 6,
            h3("Personal Information"),
            textOutput(outputId = "tgl"),
            textOutput(outputId = "usia"),
            textOutput(outputId = "tmt"),
            textOutput(outputId = "edufild")),
        
        box(width = 6,
            h3("DATA DUKUNG"),
            textOutput(outputId = "dapen"),
            textOutput(outputId = "urikes"),
            textOutput(outputId = "samapta"),
            textOutput(outputId = "bing")),
        
        
        
      ),
      tabItem(
        
        tabName =  "prediksi",
        h1("Prediksi", align = "center"),
        fluidPage(
          box(width = 6,
              selectInput(inputId = "predict", label = "Masukan Nrp", choices = data1$NRP),
              box(
                textOutput(outputId = "nama"),
                textOutput(outputId = "kat"),
                textOutput(outputId = "korp"),
                textOutput(outputId = "mdp"),
                textOutput(outputId = "mddp"),
                textOutput(outputId = "prediksi_usia")
                
              ),
              
              actionButton(inputId = "btn2", label = "Prediksi", icon = icon("magnifying-glass"))),
          box(width = 6, height = 260, 
              box(width = 4, h3("Prediksi Jabatan"),
                  textOutput(outputId = "predJab")),
              
              box(width = 4, h3("Prediksi Kenkat"),
                  textOutput(outputId = "predKenkat")),
              
              box(width = 4, h3("Prediksi Sekolah"),
                  textOutput(outputId = "predSekolah"))
              
          )
        )
        
        
      )
    )
  )
)

server <- function(input, output, session) {
  output$stratadsp <- renderEcharts4r({
    data %>% 
      filter(!is.na(STRATA_RIIL)) %>% 
      group_by(STRATA_RIIL) %>%
      summarize(count = n()) %>%
      arrange(desc(count))%>%
      e_charts(STRATA_RIIL) %>%
      e_bar(count, name = "STRATA") %>%
      e_color(color = "green", background = "white")%>% 
      e_tooltip(trigger = "axis", formatter = "{Perwira} <br/>{Bintara} : {Tamtama}") 
    #e_tooltip(trigger = "item", formatter = "{a} <br/>{b} : {c} ({d}%)")
    
    
  })
  
  output$korps <- renderEcharts4r({
    data %>%
      filter(!is.na(KORPS_RIIL)) %>% 
      group_by(KORPS_RIIL) %>%
      summarize(count = n()) %>%
      arrange(desc(count))%>%
      e_charts(KORPS_RIIL) %>%
      e_bar(count, name = "KORPS") %>%
      e_color(color = "teal", background = "white")%>% 
      e_tooltip(trigger = "axis", formatter = "{a} <br/>{b} : {c}") 
  })
  
  output$posjab <- renderEcharts4r({
    data %>%
      count(KET_RIIL) %>%
      e_charts(KET_RIIL) %>%
      e_pie(n, name = "POSISI JABATAN") %>% 
      e_color(
        c("green", "red", "blue", "yellow")) %>% 
      e_tooltip() %>%
      e_legend("terisi")
  })
  
  #DIKUM  
  output$dikum <- renderEcharts4r({
    data %>%
      filter(!is.na(DIKUM)) %>%
      group_by(DIKUM) %>%
      summarize(count = n()) %>%
      e_charts(DIKUM) %>%
      e_bar(count, name = "DIKUM") %>%
      e_color(color = "fuchsia", background = "white")%>% 
      e_flip_coords() %>%
      e_tooltip(trigger = "axis", formatter = "{a} <br/>{b} : {c}") 
  })
  
  output$dikbang <- renderEcharts4r({
    data %>%
      filter(!is.na(DIKBANG)) %>%
      group_by(DIKBANG) %>%
      summarize(count = n()) %>%
      e_charts(DIKBANG) %>%
      e_bar(count, name = "DIKBANG") %>%
      e_color(color = "orange", background = "white")%>% 
      e_tooltip(trigger = "axis", formatter = "{a} <br/>{b} : {c}") 
  })
  
  output$prestasi <- renderEcharts4r({
    data %>%
      filter(!is.na(PRESTASI)) %>%
      group_by(PRESTASI) %>%
      summarize(count = n()) %>%
      e_charts(PRESTASI) %>%
      e_bar(count, name = "PRESTASI") %>%
      e_color(color = "lightblue", background = "white")%>% 
      e_tooltip(trigger = "axis", formatter = "{a} <br/>{b} : {c}") 
  })
  
  output$hukuman <- renderEcharts4r({
    data %>%
      filter(!is.na(HUKUMAN)) %>% 
      group_by(HUKUMAN) %>%
      summarize(count = n()) %>%
      e_charts(HUKUMAN) %>%
      e_bar(count, name = "HUKUMAN") %>%
      e_tooltip(trigger = "axis", formatter = "{a} <br/>{b} : {c}") 
  })
  
  
  output$coba <- renderUI({
    req(input$Btn)
    paste0("ID Employee : ", input$profile)
  })
  
  filterData <- eventReactive(input$Btn,{
    data1 %>% filter(NRP == input$profile)
  })
  
  output$personalInformation <- renderValueBox({
    req(filterData())
    data_personalInformation<- filterData()%>%
      select(NAMA)
    
    valueBox(value= data_personalInformation$NAMA , subtitle = "NAMA", icon = icon("user-times"), color = "red")
  })
  
  ########################################################################
  
  output$pangkat <- renderValueBox({
    req(filterData())
    data_pangkat <-  filterData()%>%
      select(PKT_RIIL)
    
    
    valueBox(value= data_pangkat$PKT_RIIL,  subtitle = "PANGKAT", icon = icon("id-badge"))
  })
  ##################################################
  
  
  output$tgl <- renderText({
    paste0("1. Tanggal Lahir : ", filterData()$TGL_LAHIR)
  })
  
  output$usia <- renderText({
    paste0("2. Usia : ", filterData()$USIA_RIIL)
  })
  output$tmt <- renderText({
    paste0("3. TMT TNI : ", filterData()$TMT_TNI)
  })
  
  
  ####################################### 
  output$dapen <- renderText({
    paste0("1. Dapen : ", filterData()$DAPEN)
  })
  
  output$urikes <- renderText({
    paste0("2. Urikes : ", filterData()$URIKES)
  })
  
  output$samapta <- renderText({
    paste0("3. Samapta :  ", filterData()$SAMAPTA)
  })
  
  output$bing <- renderText({
    paste0("4. B. Inggris : ", filterData()$BHS.INGGRIS)
  })
  
  
  ###########################################################################################
  filterData1 <- eventReactive(input$btn2,{
    data1 %>% jabatan_pred_function(nrp = input$predict) %>% distinct_all()
  })
  
  
  output$nama <- renderText({
    req(filterData1())
    print(filterData1())
    paste0("Nama : ", filterData1()$NAMA)
  })
  
  output$kat <- renderText({
    req(filterData1())
    paste0("Pangkat : ", filterData1()$PKT_RIIL)
  })
  
  
  output$korp <- renderText({
    req(filterData1())
    paste0("Korps : ", filterData1()$KORPS_RIIL)
  })
  
  output$mdp <- renderText({
    req(filterData1())
    paste0("MDP : ", filterData1()$MDP)
  })
  
  output$mddp <- renderText({
    req(filterData1())
    paste0("MDDP : ", filterData1()$MDDP)
  })
  
  output$prediksi_usia <- renderText({
    req(filterData1())
    paste0("Usia : ", filterData1()$USIA_RIIL)
  })
  
  
  output$predJab <- renderText({
    req(filterData1())
    paste0("Jabatan : " , filterData1()$JABATAN )
  })
  
  output$predKenkat <- renderText({
    req(filterData1())
    paste0("Kenkat : ", filterData1()$pkt_next, "(", filterData1()$bisa_naik_pangkat,")" )
    
  })
  
  output$predSekolah <- renderText({
    req(filterData1())
    pkt_now <- trimws(filterData1()$pkt_now)
    MDDP <- filterData1()$MDDP
    MDP <- filterData1()$MDP
    USIA_RIIL <- filterData1()$USIA_RIIL
    DIKBANG <- filterData1()$DIKBANG
    
    if (pkt_now == "SERMA" & MDDP >=0 & USIA_RIIL >= 34) {
      sekolah <- "DIKTUKPA"
    } else if (pkt_now == "PELTU" & MDDP >= 0 & USIA_RIIL >= 45) {
      sekolah <- "DIKTUPAKAT"
    } else if (pkt_now == "KOPDA" & MDDP >= 2 & USIA_RIIL >= 31) {
      sekolah <- "DIKTUKBA"
    } else if (pkt_now == "KOPTU" & MDDP >= 2 & USIA_RIIL >= 31) {
      sekolah <- "DIKTUKBA"
    } else if(pkt_now == "KOPKA" & USIA_RIIL >= 41) {
      sekolah <- "DIKTUKBAKAT"
    } else if(pkt_now == "KAPTEN" & MDDP >= 2 & USIA_RIIL <= 41 & DIKBANG == "DIKSPESPA") {
      sekolah <- "DIKLAPA"
    } else if (pkt_now == "MAYOR" & MDDP >= 0 & USIA_RIIL <= 41 & DIKBANG == "DIKSPESPA") {
      sekolah <- "DIKLAPA"
    } else if (pkt_now == "KAPTEN" & MDDP >= 2 & USIA_RIIL <= 41 & DIKBANG == "DIKSPESPA") {
      sekolah <- "DIKMATRA 2/DIKAPLIKASI 2"
    } else if (pkt_now == "MAYOR" & MDDP >= 0 & USIA_RIIL <= 41 & DIKBANG == "DIKSPESPA") {
      sekolah <- "DIKMATRA 2/DIKAPLIKASI 2"
    } else if (pkt_now == "MAYOR" & MDDP >= 2 & USIA_RIIL <= 48 & DIKBANG == "DIKLAPA"){
      sekolah <- "SESKO ANGKATAN"
    } else if (pkt_now == "LETKOL" & MDDP >= 0 & USIA_RIIL <= 48 & DIKBANG == "DIKLAPA") {
      sekolah <- "SESKO ANGKATAN"
    } else if (pkt_now == "MAYOR" & MDDP >= 2 & USIA_RIIL <= 45 & DIKBANG == "DIKLAPA") {
      sekolah <- "DIKAPLIKASI 3"
    } else if (pkt_now == "LETKOL" & MDDP >= 0 & USIA_RIIL <= 45 & DIKBANG == "DIKLAPA") {
      sekolah <- "DIKAPLIKASI 3"
    } else if (pkt_now == "KOLONEL" & MDDP >= 2 & USIA_RIIL <= 49 & DIKBANG == "SESKO ANGKATAN") {
      sekolah <- "SESKO TNI"
    } else if (pkt_now == "KOLONEL" & MDP >= 24 & USIA_RIIL <= 53 & DIKBANG == "SESKO TNI") {
      sekolah <- "LEMHANAS PPRA"
    } else if (pkt_now == "LAKSAMANA PERTAMA" & MDDP >= 0 & USIA_RIIL <= 53 & DIKBANG == "SESKO TNI") {
      sekolah <- "LEMHANAS PPRA"
    } else {
      sekolah <- "Tidak Boleh Sekolah"
    }
    
    
    sekolah
    paste0("Sekolah : ", sekolah)
  })
  
}

shinyApp(ui, server)