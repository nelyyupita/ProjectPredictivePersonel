library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(DT)
library(echarts4r) 

data <- read_csv("data_pers_kri_rev1.csv")
view(data)

ui <- dashboardPage(
  dashboardHeader(
    title = "Predictive Personel"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Main Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Procesing Data", tabName = "emp", icon = icon("users")),
      menuItem("Predictive Pers", tabName = "empp", icon = icon("user"))
    )
  ),
  dashboardBody(
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
        tabName = "emp",
        h1("Attrition Employee Dashboard"),
      ),
      tabItem(
        tabName = "empp",
        h1("Employ Profile"),
        
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
      e_bar(count, name = "KORPS") %>%
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
      e_bar(count) %>%
      e_tooltip(trigger = "axis", formatter = "{a} <br/>{b} : {c}") 
  })
  
  
  output$coba <- renderUI({
    req(input$Btn)
    paste0("ID Employee: ", input$profile)
  })
}

shinyApp(ui, server)