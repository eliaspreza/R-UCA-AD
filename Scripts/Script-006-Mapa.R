
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-06----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#/////////////////////////////////////////////////////////////////////////////////////////////////
#---Por Elias Preza

#=====================================================================
#===========================LIbrerias
library(haven)
library(tidyverse)
library(readxl)
library(leaflet)
library(sp)
library(cartography)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(RColorBrewer)
library(classInt)
library(sqldf)
library(tmap)
library(codebook)
library(rmapshaper)
library(maps)
library(shinyjs)
library(leaflet)

#----------------Abriendo el shape file con los municipios
#-------Paleta tmaptools::palette_explorer()



mtq2<-st_read("Shapes/Ahuachapan1.shp")
head(mtq2,5)

plot(mtq2)

#----Mapa ahuchapan
tmap_mode("view")
tm_shape(mtq2) + tm_fill(col = "personas",alpha=0.8, style = "pretty", id="CANTON",
                         title = "personas",palette="Oranges")+
  tm_shape(mtq2)+tm_borders(col = NA, lwd = 1, lty = "solid")


#===============================
#---------Forma ggplot
#===============================

#---Cargando el Shape file
mtq2<-st_read("Shapes/Ahuachapan1.shp")

head(mtq2,5)

#---Mapa ggplot
Map2<-ggplot(data=mtq2)+geom_sf()
Map2

Map3<-ggplot(mtq2) +
  geom_sf(aes(fill = personas ))+
  labs(title = "Mapa de segmentos: Ahuachapán",
       caption = "Fuente: DIGESTYC (2007)
                      Elaboración propia",
       x="Longitud",
       y="Latitud")+
  scale_fill_continuous(guide_legend(title = "Personas"))
Map3

#===================================================================================================
#=========================Mapa Cenagro
#=====================================================================================================

#--Llamando a la base
DB.Censo.Prod<-read_sav("Bases/SubProductores.sav")

#--Filtrando solo productores de cafe
MapaC<-DB.Censo.Prod %>% 
  dplyr::filter(CAFDSC=="SI") %>% 
  dplyr::select(DEPDSC,MUNDSC,S09C225,S09C226,S09C227,ACTDSC,CX,CY) 

#CartoDB.DarkMatter
#Esri.WorldStreetMap

leaflet() %>%
  addProviderTiles('OpenStreetMap.France')%>%
  #addTiles() %>% 
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  #addMarkers(lng=MapaC$CY, lat=MapaC$CX, popup=paste(MapaC$S01P02PN,MapaC$S01P02SN))
  addCircleMarkers(lng=MapaC$CY, lat=MapaC$CX, color = "#FF5733",radius = 2,
                   popup=paste(MapaC$S01P02PN,MapaC$S01P02SN,MapaC$S01P02PA,
                               "Bajío",MapaC$S09C225,"Media",MapaC$S09C226,
                               "Estricta Altura",MapaC$S09C227))


