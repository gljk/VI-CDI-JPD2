library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)


ui <- fluidPage(tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    sidebarLayout(
        sidebarPanel(
          shinyWidgets::sliderTextInput(inputId = "popis",
                        "Godina popisa:",animate = T,
                        choices = c(1981,1991,2002,2011)),
          selectInput("Zemlja", label = "Zemlja prijema", multiple = F,choices = c("Austrija", "Italija", "Francuska", "Holandija", "Nemačka", 
                                                                           "Švajcarska", "Švedska", "Ujedinjeno Kraljevstvo", "Kanada", 
                                                                           "SAD", "Australija", "Mađarska", "Rusija",  "Ostale"),selectize = T
          )
        ),
        mainPanel(
          leafletOutput("penevmap"), width=12,title = "Srbija", height = "100px")
    )
)


server <- function(input, output) {
  load("sr")
  inofull<- read.csv("jpd_penev_ino_full.csv")
  inofull<- inofull %>% gather(key="Zemlja", value="Broj", 5:18)
  inofull$Code[inofull$Code %in%   c(70858, 71285, 71307, 71315, 71323, 71331)] <- 70858
  
  #Surčin
  inofull$Code[inofull$Code %in%   c(71293, 70157)] <- 70157
  #Lapovo
  inofull$Code[inofull$Code %in%   c(71277, 70076)] <- 70076
  #kostolac
  inofull$Code[inofull$Code %in%   c(71340,70947)] <- 70947
  
  inofull<- inofull %>% group_by(Code, Godina,Zemlja) %>% summarise(Nepoznata.zemlja=sum(Nepoznata.zemlja), Ukupno=sum(Ukupno), Broj=sum(Broj))
  
  inofull<- inofull %>% mutate(Udeo=Broj/Ukupno*100)
  inofull$Zemlja[inofull$Zemlja=="Ujedinjeno.Kraljevstvo"] <- "Ujedinjeno Kraljevstvo"
  sr@data$Code[sr@data$Code==71293]=70157
  #Lapovo to Batočina fix
  sr@data$QNAME[sr@data$Code==71277]="Batočina"
  sr@data$Code[sr@data$Code==71277]=70076

  kod_data <- sr@data %>% select(Code, QNAME)
  sr_sf<- raster::aggregate(sr, by="Code") %>% st_as_sf(sr) 
  sr_sf<- left_join(sr_sf, kod_data)
  
  sr_sf <- left_join(sr_sf, inofull)
  sr_sf$Broj[sr_sf$Code %in% c(70963, 70351)&sr_sf$Godina!=1981] <- NA
  sr_sf$Udeo[sr_sf$Code %in% c(70963, 70351)&sr_sf$Godina!=1981] <- NA
  sr_sf$Ukupno[sr_sf$Udeo %in% c(70963, 70351)&sr_sf$Godina!=1981] <- NA
  load("koMunicipalities")
  kosovo <- raster::aggregate(koMunicipalities)
st_combine(sr_sf)
  dmap<- reactive({
    sr_sf %>% filter(Zemlja== input$Zemlja, Godina== input$popis)
  })
  paleta<- reactive({
    colorNumeric(switch(input$Zemlja,"Austrija"="Blues","Nemačka"="Oranges","Švedska"="YlGnBu","Italija"="RdPu","Holandija"="RdPu","Rusija"="RdPu","Mađarska"="RdPu","Švajcarska"="YlGnBu","Francuska"="Greens","Ujedinjeno Kraljevstvo"="Purples", "Kanada"="PuBuGn","SAD"="Greys", "Australija"="YlGn", "Rusija"="PuRd"),na.color = "#bdbdbd", unique(sr_sf$Udeo[sr_sf$Zemlja==input$Zemlja]))
    })
  values<- reactive({
    unique(sr_sf$Udeo[sr_sf$Zemlja==input$Zemlja])
  })
  
    output$penevmap <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 8),height = "900px") %>%
       # setMaxBounds(lng1 = 18.92303,lng2 =  22.87352,lat1 = 42.21051 ,lat2 = 44) %>% 
        addPolygons(data=dmap(),fillColor = paleta()(dmap()$Udeo)
                    ,weight = 1, fillOpacity = 1, opacity = 1,
                    label=lapply(paste("<p><b>",dmap()$QNAME,"-", format(dmap()$Udeo,decimal.mark=",", digits=1,nsmall=1),"%","</b><p>",paste0("Ukupan broj\ngrađana na radu/boravku u inostranstvu: "),"<b>",dmap()$Ukupno, "</b>", "<p>Zemlja prijema je ", input$Zemlja," za njih <b>", dmap()$Broj,"<b></p>"),FUN = HTML),
                    color = "#bdbdbd",
                    highlightOptions = highlightOptions(stroke = 2, color = "#252525", weight = 2,
                                                        opacity = NULL, fill = NULL, fillColor = NULL, fillOpacity = NULL,
                                                        dashArray = NULL),
                    labelOptions = labelOptions(noHide = F,
                                                style = list(
                                                  "color" = "#000000",
                                                  "background-color"="#FFFFFFAA",
                                                  "font-family" = "Open Sans",
                                                  "box-shadow" = "1px 1px rgba(0,0,0,0.25)",
                                                  "font-size" = "12px",
                                                  "border-color" = "#fafafa",
                                                  "border-width" = "1px",
                                                  "line-height" = "5px",
                                                  "padding-top"= "10px"
                                                ))) %>% 
      addPolygons(data=kosovo,weight = 1, fillOpacity = 1, opacity = 1,color = "#bdbdbd")%>% addLegend(title = paste0("Udeo<br>(",input$Zemlja, ")"),pal = paleta(),position = "topright",values = values(),opacity = 1,na.label = "Nema\npodataka",    labFormat = labelFormat(suffix = "%"))
    })

}

shinyApp(ui = ui, server = server)
