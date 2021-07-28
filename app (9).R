library(shiny)
library(leaflet)
library("leaflet.extras")
library(leaflet.minicharts)
library (RCurl)
library(rgdal)
library(shinydashboard)
library(shinydashboardPlus)
# Define UI for data download app ----
options(shiny.maxRequestSize = 120 * 1024 ^ 2)
httr::set_config(httr::config(ssl_verifypeer = 0L))
myCsv <-
  getURL(
    "https://docs.google.com/spreadsheets/d/1HeiJGOvmc57Zz5vmlG859Yygq66ERXlRKjp4KVKhNyI/gviz/tq?tqx=out:csv",
    ssl.verifypeer = FALSE
  )
RU2019 <-
  read.csv(textConnection(myCsv),
           stringsAsFactors = FALSE,
           dec = ",")
var = colnames(RU2019[, 10:16])
RU2019$RI.kg.ab. <- as.numeric(gsub("\\.", "", RU2019$RI.kg.ab.))
RU2019$RU.kg.ab. <- as.numeric(gsub("\\.", "", RU2019$RU.kg.ab.))
RU2019$RD.kg. <- as.numeric(gsub("\\.", "", RU2019$RD.kg.))
RU2019$RI.kg. <- as.numeric(gsub("\\.", "", RU2019$RI.kg.))
RU2019$tot <- RU2019$RI.kg. + RU2019$RD.kg.
tipo <- "Torte"
myCsv <-
  getURL(
    "https://docs.google.com/spreadsheets/d/17afd2zzGwzbmFbz75j_z0ADibzPRnBO7R2QdPET1xAE/gviz/tq?tqx=out:csv",
    ssl.verifypeer = FALSE
  )
stazioni <-
  read.csv(textConnection(myCsv),
           stringsAsFactors = FALSE,
           dec = ",")
myCsv <-
  getURL(
    "https://docs.google.com/spreadsheets/d/1il-yAux-BfvvMAbSE1DV7tEh04iKlyQIYuEgH7xypMM/gviz/tq?tqx=out:csv",
    ssl.verifypeer = FALSE
  )
rischio <-
  read.csv(textConnection(myCsv),
           stringsAsFactors = FALSE,
           dec = ",")
myCsv <-
  getURL(
    "https://docs.google.com/spreadsheets/d/1pN7lZW2kEExvL7y0ImfEGIg9_j_ACM3hpBkcaPi1XSU/gviz/tq?tqx=out:csv",
    ssl.verifypeer = FALSE
  )
acque_sup <-
  read.csv(textConnection(myCsv),
           stringsAsFactors = FALSE,
           dec = ".")
myCsv <-
  getURL(
    "https://docs.google.com/spreadsheets/d/1-IkpQnyC8bOQ7jczMLmKoHB0rfJDT_1bf7Gb-3yl4_E/gviz/tq?tqx=out:csv",
    ssl.verifypeer = FALSE
  )
acque_sup_19 <-
  read.csv(textConnection(myCsv),
           stringsAsFactors = FALSE,
           dec = ".")
myCsv <-
  getURL(
    "https://docs.google.com/spreadsheets/d/1mO00RcJggTTJWD9qscZ3-zczkI8K6ETri4b0ev6W22c/gviz/tq?tqx=out:csv",
    ssl.verifypeer = FALSE
  )
acque_sot <-
  read.csv(textConnection(myCsv),
           stringsAsFactors = FALSE,
           dec = ",")


ui <- dashboardPage(
  dashboardHeader(title = "DaDa-Dati Dashboard"),
  dashboardSidebar(selectInput(
    "dataset",
    "Scegli il dataset",
    choices = c(
      "SRB",
      "Rifiuti",
      "Rischio",
      "Acque superficiali 2020",
      "Acque superficiali <2020",
      "Acque sotterranee"
    )
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      infoBoxOutput("main", width = 3),
      infoBoxOutput("submain", width = 3),
      infoBoxOutput("linka", width = 2),
      box(
        width = 12,
        height = 700,
        leafletOutput("gvis", height = 700)
        
        
      )
    )
  )
)











# Define server logic to display and download selected file ----
mappa <- function() {
  p <-  leaflet()  %>%
    addSearchOSM() %>%
    #  leafletOptions(maxZoom=20)%>%
 #   flyToBounds(
 #     "gvis",
 #     lng1 = 10,
 #     lat1 = 43.8,
 #     lng2 = 13,
 #     lat2 = 45
 #   ) %>%
    
    
    
    
    addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    ) %>%
    addTiles(options = tileOptions(opacity = 0.3))
  return(p)
}

server <- function(input, output, session) {
  output$gvis <- renderLeaflet({
    p <- mappa()
    
    if (input$dataset == "Rifiuti") {
      if (tipo == "Barre") {
        p <-
          p %>% addMinicharts(
            lng = as.numeric(RU2019$LONGITUDE),
            lat = as.numeric(RU2019$LATITUDE),
            colorPalette = c(
              'red',
              'white',
              'blue',
              'green',
              'yellow',
              'grey',
              'orange',
              'black',
              'violet',
              'pink',
              'brown'
            ),
            type = "bar",
            chartdata = RU2019[, c("RD.kg.", "RI.kg.")],
            showLabels = TRUE,
            legend = TRUE
          )
      }
      if (tipo == "Torte") {
        p <-
          p %>% addMinicharts(
            lng = as.numeric(RU2019$LONGITUDE),
            lat = as.numeric(RU2019$LATITUDE),
            colorPalette = c(
              'white',
              'red',
              'blue',
              'green',
              'yellow',
              'grey',
              'orange',
              'black',
              'violet',
              'pink',
              'brown'
            ),
            type = "pie",
            chartdata = RU2019[, c("RD.kg.", "RI.kg.")],
            showLabels = TRUE,
            legend = TRUE,
            width = 8 + 3 * (RU2019$tot / 10000000)
          )
      }
    }
    
    if (input$dataset == "SRB")  {
      p <- mappa()
      p <-
        p %>% addCircleMarkers(
          lng = as.numeric(stazioni$LON),
          lat = as.numeric(stazioni$LAT),
          popup = paste(
            'Attivo:',
            stazioni$STATO_IMPIANTO,
            '<BR>',
            'Denominazione:',
            stazioni$DENOMINAZIONE,
            '<BR>',
            'Indirizzo:',
            stazioni$INDIRIZZO,
            '<BR>',
            'Operatore:',
            stazioni$OPERATORE,
            '<BR>',
            'Tecnologie:',
            stazioni$TECNOLOGIE_UTILIZZATE
          ),
          clusterOptions = markerClusterOptions()
        )
      
    }
    
    if (input$dataset == "Rischio")   {
      p <- mappa()
      p <-
        p %>% addCircleMarkers(
          lng = as.numeric(rischio$LONG_WGS84),
          lat = as.numeric(rischio$LAT._WGS84),
          popup = paste(
            'Denominazione:',
            rischio$RAGIONE.SOCIALE.,
            '<BR>',
            'Indirizzo:',
            paste0(rischio$INDIRIZZO, " ", rischio$N.),
            '<BR>',
            'Tipologia:',
            rischio$TIPOLOGIA,
            '<BR>',
            'Comune:',
            rischio$COMUNE
          ),
          clusterOptions = markerClusterOptions()
        )
    }
    
    
    if (input$dataset == "Acque superficiali 2020")   {
      p <- mappa()
      p <-
        p %>% addCircleMarkers(
          lng = as.numeric(acque_sup$LON),
          lat = as.numeric(acque_sup$LAT),
          popup = paste(
            'Denominazione:',
            acque_sup$DENOM,
            '<BR>',
            'Bacino:',
            acque_sup$BACINO,
            '<BR>',
            'Asta:',
            acque_sup$ASTA,
            '<BR>',
            'Tipo:',
            acque_sup$TIPO_CI
          ),
          clusterOptions = markerClusterOptions()
        )
    }
    if (input$dataset == "Acque superficiali <2020")   {
      p <- mappa()
      p <-
        p %>% addCircleMarkers(
          lng = as.numeric(acque_sup_19$LON),
          lat = as.numeric(acque_sup_19$LAT),
          popup = paste(
            'Denominazione:',
            acque_sup_19$Stazione,
            '<BR>',
            'Asta:',
            acque_sup_19$Asta,
            '<BR>',
            'Tipo:',
            acque_sup_19$Tipo
          ),
          clusterOptions = markerClusterOptions()
        )
    }
    
    if (input$dataset == "Acque sotterranee")   {
      p <- mappa()
      p <-
        p %>% addCircleMarkers(
          lng = as.numeric(acque_sot$LON),
          lat = as.numeric(acque_sot$LAT),
          popup = paste(
            'Denominazione:',
            acque_sot$GWB_Nome ,
            '<BR>',
            'Comune:',
            acque_sot$Comune,
            '<BR>',
            'Quota:',
            acque_sot$Quota.PC..m. ,
            '<BR>',
            'Profondità:',
            acque_sot$Profondità.pozzo..m.,
            '<BR>',
            'Tipologia:',
            acque_sot$GWB_Nome
          ),
          clusterOptions = markerClusterOptions()
        )
    }
    
    p
    # addBingTiles(apikey="An2d2Z5sjzawOU_NgrTtVJpUSj5Q6cXzsDRlI_Uu47KoWOL-UigtoZKeGxy2mBUX",
    #  imagerySet="AerialWithLabels",group="Bing")%>%
    
    
    
  })
  output$main <- renderInfoBox({
    if (input$dataset == "Rischio") {
      testo = "Mappa degli stabilimenti a rischio incidente rilevante"
    }
    if (input$dataset == "SRB") {
      testo = "Mappa delle stazioni radio base"
    }
    if (input$dataset == "Rifiuti") {
      testo = "Mappa dei rifiuti. La torta rappresenta la quantità di rifiuti differenziati e quella dei rifiuti non differenziati"
    }
    if (input$dataset == "Acque superficiali 2020") {
      testo = "Mappa della rete di monitoraggio delle acque superficiali - 2020"
    }
    if (input$dataset == "Acque superficiali <2020") {
      testo = "Mappa della rete di monitoraggio delle acque superficiali - Anni 2010-2020"
    }
    if (input$dataset == "Acque sotterranee") {
      testo = "Mappa della rete di monitoraggio delle acque sotterranee"
    }
    infoBox(title = "", subtitle = testo)
  })
  output$submain <- renderInfoBox({
    if (input$dataset == "Rischio") {
      testo = paste0("Numero di aziende mappate-->", nrow(rischio))
    }
    if (input$dataset == "SRB") {
      testo = paste0("Numero di SRB mappate-->", nrow(stazioni))
    }
    if (input$dataset == "Rifiuti") {
      totrd = sum(RU2019$RD.kg.)
      totind = sum(RU2019$RI.kg.)
      testo = (
        paste0(
          "Totale rifiuti differenziati->",
          totrd,
          " kg",
          "        Totale rifiuti indifferenziati-->",
          totind,
          " kg"
        )
      )
    }
    if (input$dataset == "Acque superficiali 2020") {
      testo = paste0("Numero di punti di monitoraggio-->", nrow(acque_sup))
    }
    if (input$dataset == "Acque superficiali <2020") {
      testo = paste0("Numero di punti di monitoraggio-->", nrow(acque_sup))
    }
    if (input$dataset == "Acque sotterranee") {
      testo = paste0("Numero di punti di monitoraggio-->", nrow(acque_sot))
    }
    infoBox(title = "", subtitle = testo)
  })
  output$linka <- renderInfoBox({
    if (input$dataset == "Rischio") {
      testo = "https://docs.google.com/spreadsheets/d/1il-yAux-BfvvMAbSE1DV7tEh04iKlyQIYuEgH7xypMM"
    }
    if (input$dataset == "SRB") {
      testo = "https://docs.google.com/spreadsheets/d/17afd2zzGwzbmFbz75j_z0ADibzPRnBO7R2QdPET1xAE"
    }
    if (input$dataset == "Rifiuti") {
      testo = "https://docs.google.com/spreadsheets/d/1HeiJGOvmc57Zz5vmlG859Yygq66ERXlRKjp4KVKhNyI"
    }
    if (input$dataset == "Acque superficiali 2020") {
      testo = "https://docs.google.com/spreadsheets/d/1pN7lZW2kEExvL7y0ImfEGIg9_j_ACM3hpBkcaPi1XSU"
    }
    if (input$dataset == "Acque superficiali <2020") {
      testo = "https://docs.google.com/spreadsheets/d/1-IkpQnyC8bOQ7jczMLmKoHB0rfJDT_1bf7Gb-3yl4_E"
    }
    if (input$dataset == "Acque sotterranee") {
      testo = "https://docs.google.com/spreadsheets/d/1mO00RcJggTTJWD9qscZ3-zczkI8K6ETri4b0ev6W22c"
    }
    infoBox(
      title = "",
      subtitle = "Clicca qui per la risorsa completa",
      href = testo,
      icon = icon("table"),
      color = "blue",
      fill = TRUE
    )
  })
  
}
shinyApp(ui, server)