library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)   # Binlik ve ondalık ayraçları için
library(writexl)  # Excel kaydetme için
library(ggplot2)


# Veriyi veri çerçevesine okuma
veri <- read_excel("TRB2veriler.xlsx")

# UI bölümü
ui <- fluidPage(
  titlePanel("TRB2 Bölgesi İstatistikleri"),
  
  sidebarLayout(
    sidebarPanel(
      # İl için dropdown menüsü
      selectInput("il", "İl Seçiniz:", 
                  choices = unique(veri$il)),
      
      # Kategori için dropdown menüsü
      selectInput("kategori", "Kategori Seçiniz:", 
                  choices = unique(veri$kategori)),
      
      # Veri için dropdown menüsü (başlangıçta boş olacak, kategori seçimine göre güncellenecek)
      selectInput("veri", "Veri Seçiniz:", choices = NULL),
      
      # Grafiği PNG olarak indirme butonu
      downloadButton("downloadPlot", "Grafiği İndir (PNG)"),
      
      # Tabloyu Excel olarak indirme butonu
      downloadButton("downloadData", "Veriyi İndir (Excel)")
    ),
    
    mainPanel(
      plotOutput("cizgiGrafik"),   # Çizgi grafik alanı
      dataTableOutput("veriTablosu") # Tablo gösterimi
    )
  )
)

# Server bölümü
server <- function(input, output, session) {
  
  # Kategori seçimine göre veri dropdown'ını güncelle
  observeEvent(input$kategori, {
    # Seçilen kategoriye göre verileri filtrele
    veri_filtreli <- veri %>% filter(kategori == input$kategori)
    
    # Veri dropdown'ını seçilen kategoriye uygun hale getir
    updateSelectInput(session, "veri", choices = unique(veri_filtreli$veri))
  })
  
  # Seçilen il, kategori ve veri değerlerine göre veriyi filtrele
  secilen_veri <- reactive({
    req(input$il, input$kategori, input$veri)
    veri %>%
      filter(il == input$il, kategori == input$kategori, veri == input$veri) %>%
      filter(yil >= 2010 & yil <= 2023)
  })
  
  # Çizgi grafiği oluşturma
  output$cizgiGrafik <- renderPlot({
    ggplot(secilen_veri(), aes(x = yil, y = deger)) +  
      geom_line(color = "blue", size = 1) +           
      geom_point(color = "red", size = 2) +           
      scale_x_continuous(breaks = 2010:2023) +        
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) + # Binlik nokta, ondalık virgül
      labs(title = paste(input$il, "-", input$kategori, "-", input$veri, "2010-2023 Yılları Arası Değişim"),
           x = "Yıl", y = input$veri) +               
      theme_minimal()                                 
  })
  
  # Grafiği PNG olarak indirme
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$il, input$kategori, input$veri, "grafik.PNG", sep = "_")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", dpi=300)
    }
  )
  
  # Filtrelenmiş veriyi tablo olarak göster
  output$veriTablosu <- renderDataTable({
    datatable(
      secilen_veri() %>%
        mutate(deger = format(deger, big.mark = ".", decimal.mark = ",")) # Binlik nokta, ondalık virgül
    )
  })
  
  # Veriyi Excel olarak indirme
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$il, input$kategori, input$veri, "veri.xlsx", sep = "_")
    },
    content = function(file) {
      write_xlsx(secilen_veri(), path = file)
    }
  )
}

# Shiny uygulamasını başlat
shinyApp(ui = ui, server = server)
