
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-05----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#/////////////////////////////////////////////////////////////////////////////////////////////////
#---Por Elias Preza

#=======================================================================StartChunk

#--------------------------Librerias
library(tidyverse)#---Wrangling data
library(sjmisc)#---Frecuencias
library(haven)#---Abrir base en formatos: SPSS,STATA y SAS
library(codebook)#--Uso de frecuencias
library(githubinstall)#--Uso de Github
library(knitr)#--Uso de Kable
library(treemapify)#--Grafico de mapa
library(scales)#--Uso de grafico
library(pyramid)#--Uso Piramide
library(apyramid)#--Uso Piramide
library(dygraphs)#--Uso para dygraphs
library(REAT)#--Uso para indicadores eco
library(ineq)#--Uso para indicadores eco
library(RColorBrewer)#--Uso para indicadores eco
library(knitr)#--Uso para indicadores eco

#=======================================================================EndChunk


#=======================================================================StartChunk
#--------------------------Abriendo y explorando la EHPM

DF<-read_sav("Bases/EHPM 2022.sav")#---Se lee EHPM de datos
Dic <- read_csv("Bases/Diccionario.csv")#---Se lee diccionario
View(Dic)#---Ver Diccionario
View(EHPM)#---Ver EHPM de datos
#================================================Definiendo hogar
#---------------Filtro para definir hogar
DF_h <- DF %>% dplyr::filter(r103==1)
sum(DF_h$fac00)
#=======================================================================EndChunk


#================================================================================================================
#-----------------------------------------------------------------------------Construcción de quintiles y deciles

dplyr::glimpse(DF)


DF2<- DF %>% select(fac00,area,r104,ingfa)

DF2_10<-DF2 %>% 
  mutate(decile=ntile(ingfa,10)) %>% 
  arrange(desc(decile))

DF2_5<-DF2 %>% 
  mutate(decile=ntile(ingfa,5)) %>% 
  arrange(desc(decile))

DF3<-DF2_10 %>%
  group_by(decile) %>%
  summarize(
    ingreso_promedio = mean(ingfa),
    ingreso_mediana = median(ingfa),
    ingreso_desviacion_estandar = sd(ingfa)
  )

DF4<-DF2_5 %>%
  group_by(decile) %>%
  summarize(
    ingreso_promedio = mean(ingfa),
    ingreso_mediana = median(ingfa),
    ingreso_desviacion_estandar = sd(ingfa)
  )


frq(DF2_10$decile, weights = DF2_10$fac00)
frq(DF2_5$decile, weights = DF2_5$fac00)

#--------------------------------------Forma 2: Código para calcular deciles y quintiles de ingreso


#===deciles

#df<-read_sav("C:\\Users\\elias.preza\\Downloads\\EHPM 2022 (1).sav")

# Calcular los deciles
deciles <- quantile(DF2$ingfa, probs = seq(0, 1, by = 0.1))

# Etiquetar los registros con los deciles correspondientes
DF2$decil_etiqueta <- cut(DF2$ingfa, breaks = deciles, labels = FALSE, include.lowest = TRUE)


DF2_10<-DF2 %>% 
  arrange(desc(decil_etiqueta))


#===quintiles

#df<-read_sav("C:\\Users\\elias.preza\\Downloads\\EHPM 2022 (1).sav")

# Calcular los deciles
quintiles <- quantile(DF2$ingfa, probs = seq(0, 1, by = 0.2))

# Etiquetar los registros con los deciles correspondientes
DF2$decil_etiqueta <- cut(DF2$ingfa, breaks = quintiles, labels = FALSE, include.lowest = TRUE)


DF2_5<-DF2%>% 
  arrange(desc(decil_etiqueta))


#================================================================================================================
#-----------------------------------------------------------------------------Ingreso

dplyr::glimpse(DF$ingfa) #-------ingreso promedio mensual familiar

summary(DF$ingfa)#-------Resumen

dplyr::glimpse(DF$ingpe) #-------ingreso promedio percapita

summary(DF$ingpe)#-------Resumen

#================================================Definiendo hogar
#---------------Filtro para definir hogar
DF_h <- DF %>% dplyr::filter(r103==1)
sum(DF_h$fac00)


#=================================================Ingreso familiar promedio mensual
#-----------Ingreso familiar Promedio

ingresoFm<-DF_h %>%
  dplyr::select(area,ingfa,fac00)%>% 
  dplyr::summarise(Hogares=sum(fac00),IngresosT=sum(fac00*ingfa))%>%
  dplyr::mutate(IngresoFamiliarMedio=IngresosT/Hogares)

#-----------Ingreso familiar según área
ingresoFa<-DF_h %>%
  dplyr::select(area,ingfa,fac00)%>% 
  dplyr::group_by(area)%>% 
  dplyr::summarise(Hogares=sum(fac00),IngresosT=sum(fac00*ingfa))%>%
  dplyr::mutate(IngresoFamiliarMedio=IngresosT/Hogares)
ingresoFa$area<- as.factor(ingresoFa$area)
levels(ingresoFa$area )<- c("Rural","Urbano")

#-----graficando
g_ingresoFa <- ggplot(ingresoFa, aes(x=' ',IngresoFamiliarMedio, fill=area), group=area) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual familiar",
       subtitle ="Según área geográfica (En US$), año 2022",
       x = "Área Geográfica", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoFa, aes(label = round(IngresoFamiliarMedio,digits=2)),position = position_dodge(0.9),vjust = 5, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_ingresoFa

g2_ingresoFa <- ggplot(ingresoFa, aes(x=' ',IngresoFamiliarMedio, fill=area), group=area) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual familiar",
       subtitle ="Según área geográfica (En US$), año 2022",
       x = "Área Geográfica", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoFa, aes(label = round(IngresoFamiliarMedio,digits=2)),position = position_dodge(0.9),vjust = 0.7, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_ingresoFa 


#-----------Ingreso familiar según región
ingresoFr<-DF_h %>%
  dplyr::select(area,region,ingfa,fac00)%>% 
  dplyr::group_by(region)%>% 
  dplyr::summarise(Hogares=sum(fac00),IngresosT=sum(fac00*ingfa))%>%
  dplyr::mutate(IngresoFamiliarMedio=IngresosT/Hogares) %>% 
  dplyr::arrange(desc(IngresoFamiliarMedio))
ingresoFr$region<- as.factor(ingresoFr$region)
levels(ingresoFr$region)<- c("Occidental", "Central I", "Central II", "Oriental", "Ámss")

#-----graficando

p1 <- ggplot(ingresoFr, aes(x=' ',IngresoFamiliarMedio, fill=region), group=region) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual familiar",
       subtitle ="Según región geográfica (En US$), año 2022",
       x = "Área Geográfica", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoFr, aes(label = round(IngresoFamiliarMedio,digits=2)),position = position_dodge(0.9),vjust = 5, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
p1

p1 <- ggplot(ingresoFr, aes(x=' ',IngresoFamiliarMedio, fill=region), group=region) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual familiar",
       subtitle ="Según región geográfica (En US$), año 2022",
       x = "Área Geográfica", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoFr, aes(label = round(IngresoFamiliarMedio,digits=2)),position = position_dodge(0.9),vjust = 0.7, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
p1


#========================================================================================================================
#==============================================================Ingreso percapitado promedio mensual
#-----------Ingreso percapitado Promedio
ingresoPm<-DF %>%
  dplyr::select(area,ingpe,fac00)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*ingpe))%>%
  dplyr::mutate(IngresoFamiliarMedio=IngresosT/Poblacion)

#-----------Ingreso percapitado Promedio según área
ingresoPa<-DF %>%
  dplyr::select(area,ingpe,fac00)%>% 
  dplyr::group_by(area)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*ingpe))%>%
  dplyr::mutate(IngresoFamiliarMedio=IngresosT/Poblacion)
ingresoPa$area<- as.factor(ingresoPa$area)
levels(ingresoPa$area )<- c("Rural","Urbano")

g_ingresoPa <- ggplot(ingresoPa, aes(x=' ',IngresoFamiliarMedio, fill=area), group=area) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual percapita",
       subtitle ="Según área geográfica (En US$), año 2022",
       x = "Area", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoPa, aes(label = round(IngresoFamiliarMedio,digits=2)),position = position_dodge(0.9),vjust = 0.7, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_ingresoPa

g2_ingresoPa <- ggplot(ingresoPa, aes(x=' ',IngresoFamiliarMedio, fill=area), group=area) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual percapita",
       subtitle ="Según área geográfica (En US$), año 2022",
       x = "Area", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoPa, aes(label = round(IngresoFamiliarMedio,digits=2)),position = position_dodge(0.9),vjust = 0.7, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_ingresoPa


#-----------Ingreso percapita por sexo
ingresoPs<-DF %>%
  dplyr::select(r104,ingpe,fac00)%>% 
  dplyr::group_by(r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*ingpe))%>%
  dplyr::mutate(IngresoMedio=IngresosT/Poblacion)
ingresoPs$r104<- as.factor(ingresoPs$r104)
levels(ingresoPs$r104 )<- c("Hombre","Mujer")


g_ingresoPs <- ggplot(ingresoPs, aes(x=' ',IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual percapita",
       subtitle ="Según sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoPs, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 5, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_ingresoPs

g2_ingresoPs <- ggplot(ingresoPs, aes(x=' ',IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual percapita",
       subtitle ="Según sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoPs, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 0.7, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_ingresoPs


#-----------Ingreso percapita según región y sexo
ingresoFrs<-DF %>%
  dplyr::select(area,region,r104,ingpe,fac00)%>% 
  dplyr::group_by(region,r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*ingpe))%>%
  dplyr::mutate(IngresoPercapitaMedio=IngresosT/Poblacion) %>% 
  dplyr::arrange(desc(IngresoPercapitaMedio))
ingresoFrs$r104<- as.factor(ingresoFrs$r104)
levels(ingresoFrs$r104 )<- c("Hombre","Mujer")
ingresoFrs$region<- as.factor(ingresoFrs$region)
levels(ingresoFrs$region)<- c("Occidental", "Central I", "Central II", "Oriental", "Ámss")


g_ingresoFrs <- ggplot(ingresoFrs, aes(x=r104,IngresoPercapitaMedio, fill=region), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual percapita",
       subtitle ="Según región y sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoFrs, aes(label = round(IngresoPercapitaMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_ingresoFrs

g2_ingresoFrs <- ggplot(ingresoFrs, aes(x=r104,IngresoPercapitaMedio, fill=region), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual percapita",
       subtitle ="Según región y sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoFrs, aes(label = round(IngresoPercapitaMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_ingresoFrs



#-----------Ingreso percapita según región y sexo
ingresoFas<-DF%>%
  dplyr::select(area,region,r104,ingpe,fac00)%>% 
  dplyr::group_by(area,r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*ingpe))%>%
  dplyr::mutate(IngresoPercapitaMedio=IngresosT/Poblacion) %>% 
  dplyr::arrange(desc(IngresoPercapitaMedio))
ingresoFas$r104<- as.factor(ingresoFas$r104)
levels(ingresoFas$r104 )<- c("Hombre","Mujer")
ingresoFas$area<- as.factor(ingresoFas$area)
levels(ingresoFas$area)<- c("Rural","Urbano")



g_ingresoFas<- ggplot(ingresoFas, aes(x=r104,IngresoPercapitaMedio, fill=area), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual percapita",
       subtitle ="Según área geográfica y sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoFas, aes(label = round(IngresoPercapitaMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_ingresoFas


g2_ingresoFas<- ggplot(ingresoFas, aes(x=r104,IngresoPercapitaMedio, fill=area), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual percapita",
       subtitle ="Según área geográfica y sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoFas, aes(label = round(IngresoPercapitaMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_ingresoFas


#=======================================================================================================================
#===============================================================================Ingreso remesas familiar mensual
#-----------Ingreso remesas familiar Promedio

ingresoRm<-DF_h %>%
  dplyr::select(area,ingreso_remesas,fac00)%>% 
  dplyr::summarise(Hogares=sum(fac00),IngresosT=sum(fac00*na.omit(ingreso_remesas)))%>%
  dplyr::mutate(IngresoRemesasFamiliarMedio=IngresosT/Hogares)

#-----------Ingreso remesas familiar Promedio según area

ingresoRa<-DF_h %>%
  dplyr::select(area,ingreso_remesas,fac00)%>% 
  dplyr::group_by(area)%>% 
  dplyr::summarise(Hogares=sum(fac00),IngresosT=sum(fac00*na.omit(ingreso_remesas)))%>%
  dplyr::mutate(IngresoRemesasFamiliarMedio=IngresosT/Hogares)
ingresoRa$area<- as.factor(ingresoRa$area)
levels(ingresoRa$area )<- c("Rural","Urbano")


g_ingresoRa<- ggplot(ingresoRa, aes(x=area,IngresoRemesasFamiliarMedio, fill=area), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual remesas",
       subtitle ="Según área geográfica (En US$), año 2022",
       x = "Área", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoRa, aes(label = round(IngresoRemesasFamiliarMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_ingresoRa

g2_ingresoRa<- ggplot(ingresoRa, aes(x=area,IngresoRemesasFamiliarMedio, fill=area), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual remesas",
       subtitle ="Según área geográfica (En US$), año 2022",
       x = "Área", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ingresoRa, aes(label = round(IngresoRemesasFamiliarMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_ingresoRa


#=======================================================================================================================
#===============================================================================Ingreso promedio mensul Trabajo dependiente

#---imeds ingreso por trabajo dependiente

imedsTd<-DF %>%
  dplyr::select(r104,imeds,fac00)%>% 
  dplyr::group_by(r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*imeds))%>%
  dplyr::mutate(IngresoMedio=IngresosT/Poblacion)
imedsTd$r104<- as.factor(imedsTd$r104)
levels(imedsTd$r104 )<- c("Hombre","Mujer")

g_imedsTd<- ggplot(imedsTd, aes(x=r104,IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual trabajo dependiente",
       subtitle ="Según sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = imedsTd, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_imedsTd

g2_imedsTd<- ggplot(imedsTd, aes(x=r104,IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual trabajo dependiente",
       subtitle ="Según sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = imedsTd, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_imedsTd



#=======================================================================================================================
#===============================================================================Ingreso promedio mensul Trabajo independiente

#---imeds ingreso por trabajo independiente

imeiTi<-DF %>%
  dplyr::select(r104,imei,fac00)%>% 
  dplyr::group_by(r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00),IngresosT=sum(fac00*imei))%>%
  dplyr::mutate(IngresoMedio=IngresosT/Poblacion)
imeiTi$r104<- as.factor(imeiTi$r104)
levels(imeiTi$r104 )<- c("Hombre","Mujer")

g_imeiTi<- ggplot(imeiTi, aes(x=r104,IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual trabajo independiente",
       subtitle ="Según sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = imeiTi, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_imeiTi


g2_imeiTi<- ggplot(imeiTi, aes(x=r104,IngresoMedio, fill=r104), group=r104) +
  geom_col(position = "dodge")+
  labs(title ="Ingreso promedio mensual trabajo independiente",
       subtitle ="Según sexo (En US$), año 2022",
       x = "Sexo", y = "US$",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = imeiTi, aes(label = round(IngresoMedio,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_imeiTi

#=======================================================================================================================
#===============================================================================Coeficiente GINI


#----Coeficiente GINI ingreso familiar
GiniInfa<-ineq(DF_h$ingfa,type = "Gini")
kable(GiniInfa)

plot(Lc(DF_h$ingfa))


#----Coeficiente GINI ingreso percapita
GiniInpe<-ineq(DF$ingpe,type = "Gini")
kable(GiniInpe)

plot(Lc(DF$ingpe))


#----Coeficiente GINI ingreso remesas
GiniRemesas<-ineq(DF_h$ingreso_remesas,type = "Gini")
kable(GiniRemesas)

plot(Lc(DF_h$ingreso_remesas))


#----Coeficiente GINI ingreso trabajo dependiente
GiniTd<-ineq(DF$imeds,type = "Gini")
kable(GiniTd)

plot(Lc(DF$imeds))


#----Coeficiente GINI ingreso trabajo independiente
GiniTi<-ineq(DF$imei,type = "Gini")
kable(GiniTi)

plot(Lc(DF$imei))

Personas<-sum(DF$fac00)
CvP <-ineq(DF$fac00,type="square.var")
CvP <-var.coeff(DF$fac00,square = FALSE, na.rm = TRUE)
var.coeff(DF$fac00)


#=======================================================================================================================
#--------------------------------------------------------------------PET y PEA
#=======================================================================================================================

#--Explorando las variables

str(DF$r106)
str(DF$actpr)
str(DF$actpr2012)
str(DF$actse)

frq(DF$actpr,weights= DF$fac00 )
frq(DF$actpr2012,weights =DF$fac00)
frq(DF$actse,weights =DF$fac00 )

#========================================================================================================
#-----------------------------------------Personas de 16 años y más con condicion de trabajo
#---dplyr

#==================================================
#---Población en edad de trabajar PET


Pet <-DF %>% 
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16) %>% 
  dplyr::summarise(PET=sum(fac00))

Pea <-DF %>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30)%>% 
  dplyr::summarise(PEA=sum(fac00)) 


#====================================================
#----Tasa Neta o global de Participacion PEA/PET


TNP<-((Pea/Pet)*100)
TNP

#=====================================================
#----Tasa Bruta de Participacion PEA/Poblacion
poblacion<-sum(DF$fac00)

TBP<-((Pea/poblacion)*100)
TBP

#======================================================
#--------Condición de ocupación y tasa de desocupación

CondicionOcupacion<-DF%>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30) %>% 
  dplyr::group_by(actpr2012)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(Pob_16=sum(Poblacion))%>%
  dplyr::mutate(TasaOcupacionDesocupacion=scales::percent(Poblacion/Pob_16,accuracy =0.1))
CondicionOcupacion$actpr2012<- as.factor(CondicionOcupacion$actpr2012)
levels(CondicionOcupacion$actpr2012 )<- c("Ocupados","Desocupados")

Inacti<-DF%>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16) %>% 
  frq(actpr,weights = fac00)


#--------Tasa de desocupación abierta

Desocupacion<-DF%>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30) %>% 
  dplyr::group_by(actpr)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(Pob_16=sum(Poblacion))%>%
  dplyr::mutate(CondicionDesocupado=scales::percent(Poblacion/Pob_16,accuracy =0.1))

#--------segmento

Segmento<-DF%>%
  dplyr::select(r106,segm,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16) %>% 
  frq(segm,weights = fac00)


#========================================================================
#-----pobreza


#---Condicion de pobreza en hogares
ph<-DF_h %>%
  dplyr::select(r104,pobreza,fac00)%>% 
  dplyr::group_by(pobreza)%>% 
  dplyr::summarise(Hogares=sum(fac00)) %>% 
  dplyr::mutate(HogaresTotal=sum(Hogares)) %>% 
  dplyr::mutate(Porcentaje=(Hogares/HogaresTotal)*100)
ph$pobreza<-as.factor(ph$pobreza) #---Transformo a factor
levels(ph$pobreza)<-c("Pobreza Extrema","Pobreza Relativa","No Pobres") #--se asigna los niveles

frq(DF_h$pobreza, weights = DF_h$fac00)

g_ph<- ggplot(ph, aes(x=pobreza,Porcentaje, fill=pobreza), group=pobreza) +
  geom_col(position = "dodge")+
  labs(title ="Hogares en Condición de Pobreza",
       subtitle ="En %, año 2022",
       x = "Condición de Pobreza", y = "(%)",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ph, aes(label = round(Porcentaje,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_ph

g2_ph<- ggplot(ph, aes(x=pobreza,Porcentaje, fill=pobreza), group=pobreza) +
  geom_col(position = "dodge")+
  labs(title ="Hogares en Condición de Pobreza",
       subtitle ="En %, año 2022",
       x = "Condición de Pobreza", y = "(%)",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = ph, aes(label = round(Porcentaje,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_ph

#---Condicion de pobreza en personas
pp<-DF %>%
  dplyr::select(r104,pobreza,fac00)%>% 
  dplyr::group_by(pobreza)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)
pp$pobreza<-as.factor(pp$pobreza) #---Transformo a factor
levels(pp$pobreza)<-c("Pobreza Extrema","Pobreza Relativa","No Pobres") #--se asigna los niveles

frq(DF$pobreza, weights = DF$fac00)

g_pp<- ggplot(ph, aes(x=pobreza,Porcentaje, fill=pobreza), group=pobreza) +
  geom_col(position = "dodge")+
  labs(title ="Personas en Condición de Pobreza",
       subtitle ="En %, año 2022",
       x = "Condición de Pobreza", y = "(%)",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = pp, aes(label = round(Porcentaje,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")
g_pp

g2_pp<- ggplot(ph, aes(x=pobreza,Porcentaje, fill=pobreza), group=pobreza) +
  geom_col(position = "dodge")+
  labs(title ="Personas en Condición de Pobreza",
       subtitle ="En %, año 2022",
       x = "Condición de Pobreza", y = "(%)",
       caption = "Elaboración propia con información de la Base de Datos EHPM 2022 ONEC/BCR")+ 
  geom_text(data = pp, aes(label = round(Porcentaje,digits=2)),position = position_dodge(0.9),vjust = 0.1, size=7) +
  scale_radius(range = c(3,16))+
  scale_fill_brewer(palette = "YlOrRd")+
  coord_flip()
g2_pp

#------------------------Distribución por pobreza de personas según area
PobArea03<-DF%>% 
  dplyr::select(area,pobreza,fac00) %>% 
  dplyr::group_by(area,pobreza)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)
PobArea03$area<-as.factor(PobArea03$area) #---Transformo a factor
levels(PobArea03$area)<-c("Rural","Urbano") #--se asigna los niveles
PobArea03$pobreza<-as.factor(PobArea03$pobreza) #---Transformo a factor
levels(PobArea03$pobreza)<-c("Pobreza Extrema","Pobreza Relativa","No Pobres") #--se asigna los niveles
#------------------------Distribución por pobreza de hogares según area
PobArea04<-DF_h%>% 
  dplyr::select(area,pobreza,fac00) %>% 
  dplyr::group_by(area,pobreza)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)
PobArea04$area<-as.factor(PobArea03$area) #---Transformo a factor
levels(PobArea04$area)<-c("Rural","Urbano") #--se asigna los niveles
PobArea04$pobreza<-as.factor(PobArea04$pobreza) #---Transformo a factor
levels(PobArea04$pobreza)<-c("Pobreza Extrema","Pobreza Relativa","No Pobres") #--se asigna los niveles


