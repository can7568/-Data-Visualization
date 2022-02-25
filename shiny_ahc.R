library(shiny)
#install.pac/, ㅡs('tidyverse')
library(tidyverse)
#install.packages('leaflet')
library(leaflet)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

ui <- pageWithSidebar(
  #1. 헤더 패널
  headerPanel(
    h1("자전거 사고 다발 지역")
  ),
  
  sidebarPanel(
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        "자전거 사고 다발 지역",
        leafletOutput("table1"),
      )
    ))
)

ahc <- read.csv("accident_hazard_country.csv") %>% as_tibble

server <- function(input,output) {
  output$table1 <- renderLeaflet({
    leaflet(ahc) %>%
      #setView(lng=127.7669, lat=35.90776, zoom=6) %>%
      setView(lng=126.9784, lat=37.566, zoom=11) %>%
      addTiles() %>% # add default OpenStreetMap map tiles
      #addProviderTiles() %>%
      addCircles(lng=~경도, lat=~위도, color="red")
  })
    
}

shinyApp(ui, server)

# 해당 발생 지점을 click하면 발생 장소, 발생 건수를 확인할 수 있는 부분을 구현
# sidebarPanel에 '행정구역'마다 선택하여 원하는 범위의 구역에 속한 곳만 확인할 수 있게 구현