
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-02----------------------------------------------------------
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

#=======================================================================EndChunk


#=======================================================================StartChunk
#--------------------------Abriendo y explorando la EHPM

DF<-read_sav("Bases/EHPM 2022.sav")#---Se lee EHPM de datos
Dic <- read_csv("Bases/Diccionario.csv")#---Se lee diccionario
View(Dic)#---Ver Diccionario
View(EHPM)#---Ver EHPM de datos

#=======================================================================EndChunk

#=======================================================================StartChunk


#====================================================================================================
#------------------------------------------------Distribución por edad y sexo de la población: Ind001
#====================================================================================================

Ind001<-DF %>%
  dplyr::select(r104,r106,fac00) %>% 
  dplyr::rename(Sexo=r104,Edad=r106) %>% 
  dplyr::filter(Edad<=4) %>% 
  dplyr::group_by(Sexo) %>% 
  dplyr::summarise(Poblacion=round(sum(fac00),digits = 0)) %>% 
  dplyr::mutate(PopT=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=round((Poblacion/PopT)*100,digits = 1))

sum(Ind001$Poblacion)
kable(head(Ind001))

#Variante agrupada por edades: Ind001
Ind001.01<-DF %>%
  dplyr::select(r104,r106,fac00) %>% 
  dplyr::rename(Sexo=r104,Edad=r106) %>% 
  dplyr::filter(Edad<=4) %>% 
  dplyr::group_by(Edad) %>% 
  dplyr::summarise(Poblacion=round(sum(fac00),digits = 0)) %>% 
  dplyr::mutate(PopT=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=round((Poblacion/PopT)*100,digits = 1))

kable(head(Ind001.01))

#====================================================================================================
#-----------------------------------------------------------------------Razón de masculinidad: Ind002
#====================================================================================================

H<-DF %>%
  dplyr::select(r104,fac00) %>% 
  dplyr::filter(r104==1) %>%
  dplyr::group_by(r104) %>%
  dplyr::summarise(PoblacionH=sum(fac00)) 

H

#-Mujeres
M<-DF %>%
  dplyr::select(r104,fac00) %>% 
  dplyr::filter(r104==2) %>%
  dplyr::group_by(r104) %>%
  dplyr::summarise(PoblacionM=sum(fac00)) 

M

RM<-(H/M)*100 

kable(RM)

#====================================================================================================
#------------------------------------------------------------------------Razón de dependencia: Ind003
#====================================================================================================

#-Menores de 16 años

M16<-DF %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106<16) %>%
  dplyr::summarise(PoblacionMenor16=sum(fac00)) 

M16

#-Mayores de 64 años

M64<-DF %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>64) %>%
  dplyr::summarise(PoblacionMayor64=sum(fac00)) 

M64

#Numerador
SumM16M64<-sum(M16,M64)
SumM16M64

#-Denominador
PobEt<-sum(DF$fac00)-SumM16M64
PobEt

RD1<-(SumM16M64/PobEt)*100

kable(RD1)

#====================================================================================================
#------------------------------------------------------------Forma EHPM Razón de dependencia: Ind003
#====================================================================================================
#-------------------------------------Estimando población ocupada
PobOcup2<-DF %>% 
  dplyr::select(r106,actpr2012,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012==10) %>% 
  dplyr::summarise(PobOcupada=sum(fac00))

PobOcup2

#--------------------------------------Estimando población total
PobTotal<-DF%>% summarise(Poblacion=sum(fac00))

PobTotal

#---------------------------------Estimando población dependiente
PobDep<-PobTotal-PobOcup2

PobDep

#-----------------------------------Tasa de dependencia económica
RD2<-(PobDep/PobOcup2)

kable(RD2)

#====================================================================================================
#---------------------------------------------------------------------------Razón de vejez: Ind004
#====================================================================================================

#---Poblacion mayor o igual a 65 años
M65<-DF %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=65) %>%
  dplyr::summarise(PoblacionMayor65=sum(fac00)) 

kable(M65)

#----Población económicamente activa
Pea<-DF %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=15&r106<65) %>%
  dplyr::summarise(PoblacionActiva=sum(fac00)) 

kable(Pea)


RV<-(M65/Pea)*100

kable(RV)

#====================================================================================================
#----------------------------------------------Estimación de población por área geográfica: Ind005
#====================================================================================================

PobArea<-DF%>% 
  dplyr::select(area,fac00) %>% 
  dplyr::group_by(area)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)

PobArea$area<-as.factor(PobArea$area) #---Transformo a factor
levels(PobArea$area)<-c("Rural","Urbano") #--se asigna los niveles

kable(PobArea)

#------------------------Distribución por pobreza
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

kable(PobArea03)


#------------------------Distribución por sexo
PobArea02<-DF%>% 
  dplyr::select(area,r104,fac00) %>% 
  dplyr::group_by(area,r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)

PobArea02$area<-as.factor(PobArea02$area) #---Transformo a factor
levels(PobArea02$area)<-c("Rural","Urbano") #--se asigna los niveles

PobArea02$r104<-as.factor(PobArea02$r104) #---Transformo a factor
levels(PobArea02$r104)<-c("Hombre","Mujer") #--se asigna los niveles

kable(PobArea02)


#------------------------------------Gráfico de la población por área geográfica

ggplot(PobArea, aes(x=(Poblacion/1000000), y=area, fill=area)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según área geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "area",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobArea, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()+
  coord_flip()


ggplot(PobArea, aes(x=(Poblacion/1000000), y=area, fill=area)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según área geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "area",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobArea, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()


ggplot(PobArea, aes(x=(Poblacion/1000000), y=area, fill=area)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según área geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "area",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobArea, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()+
  facet_wrap(~area)

#----------Grafico con variante de sexo
ggplot(PobArea02, aes(x=(Poblacion/1000000), y=area, fill=area)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según área geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "area",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobArea02, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()+
  facet_wrap(~r104)

#----------Grafico con variante de pobreza
ggplot(PobArea03, aes(x=(Poblacion/1000000), y=area, fill=area)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según área geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "area",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobArea03, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()+
  facet_wrap(~pobreza)

#====================================================================================================
#----------------------------------------------Estimación de población por región geográfica: Ind006
#====================================================================================================

PobReg<-DF %>% 
  dplyr::select(region,fac00) %>% 
  dplyr::group_by(region)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)

PobReg$region<-as.factor(PobReg$region) #---Transformo a factor
levels(PobReg$region)<-c("Occidental","Central I","Central II","Oriental","AMSS") #--se asigna los niveles

Frecuencia<-frq(DF$region, weights = DF$fac00)

kable(Frecuencia)

kable(PobReg)

#-----Región por sexo

PobReg02<-DF %>% 
  dplyr::select(region,r104,fac00) %>% 
  dplyr::group_by(region,r104)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)

PobReg02$region<-as.factor(PobReg02$region) #---Transformo a factor
levels(PobReg02$region)<-c("Occidental","Central I","Central II","Oriental","AMSS") #--se asigna los niveles


PobReg02$r104<-as.factor(PobReg02$r104) #---Transformo a factor
levels(PobReg02$r104)<-c("Hombre","Mujer") #--se asigna los niveles


Frecuencia<-frq(DF$region, weights = DF$fac00)

kable(Frecuencia)

kable(PobReg02)



#-----Región por pobreza

PobReg03<-DF %>% 
  dplyr::select(region,pobreza,fac00) %>% 
  dplyr::group_by(region,pobreza)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)


PobReg03$region<-as.factor(PobReg03$region) #---Transformo a factor
levels(PobReg03$region)<-c("Occidental","Central I","Central II","Oriental","AMSS") #--se asigna los niveles

PobReg03$pobreza<-as.factor(PobReg03$pobreza) #---Transformo a factor
levels(PobReg03$pobreza)<-c("Pobreza Extrema","Pobreza Relativa","No Pobres") #--se asigna los niveles


kable(PobReg03)

Frecuencia<-frq(DF$pobreza, weights = DF$fac00)
#-------------De forma gráfica la ubicación de la población en regiones geográficas es la siguiente:

#Forma 1
ggplot(PobReg, aes(x=(Poblacion/1000000), y=region, fill=region)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según región geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "región",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobReg, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  #guides(fill=guide_legend(reverse=T))+
  scale_fill_brewer(palette = "YlOrRd") +
  theme_grey()+
  coord_flip()

#Forma 2
ggplot(PobReg, aes(x=(Poblacion/1000000), y=region, fill=region)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según región geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "región",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobReg, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  #guides(fill=guide_legend(reverse=T))+
  scale_fill_brewer(palette = "YlOrRd") +
  theme_grey()

#Forma 3
ggplot(PobReg, aes(x=(Poblacion/1000000), y=region, fill=region)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según región geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "región",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobReg, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  #guides(fill=guide_legend(reverse=T))+
  scale_fill_brewer(palette = "YlOrRd") +
  theme_grey()+
  facet_wrap(~region)

#Forma 4
ggplot(PobReg02, aes(x=(Poblacion/1000000), y=region, fill=region)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según región geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "región",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobReg02, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  #guides(fill=guide_legend(reverse=T))+
  scale_fill_brewer(palette = "YlOrRd") +
  theme_grey()+
  facet_wrap(~r104)

#Forma 5
ggplot(PobReg03, aes(x=(Poblacion/1000000), y=region, fill=region)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Población según región geográfica año 2022",
       subtitle ="(En millones de personas)",
       x = "Poblacion (millones)", y = "región",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = PobReg03, aes(label = round(Poblacion/1000000,digits=2)),size = 6) +
  #guides(fill=guide_legend(reverse=T))+
  scale_fill_brewer(palette = "YlOrRd") +
  theme_grey()+
  facet_wrap(~pobreza)


#====================================================================================================
#------------------------------------------------------------------Población por departamento: Ind007
#====================================================================================================

## Población por departamento

depto<-DF %>% 
  dplyr::select(region,r004,fac00) %>% 
  dplyr::group_by(r004)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)


depto$r004<-as.factor(depto$r004) #---Transformo a factor
levels(depto$r004)<-c("Ahuachapán","Santa Ana","Sonsonate","Chalatenango","La Libertad",
                      "San Salvador","Cuscatlán","La Paz","Cabañas","San Vicente","Usulután",
                      "San Miguel","Morazán","La Unión") #--se asigna los niveles

kable(depto)

## Grafica Población por departamento

ggplot(depto,
       aes(fill = r004,
           area = Porcentaje,
           label = r004)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre") +
  labs(title = "Peso de la población por departamentos (Fuente: ONEC/BCR EHPM 2022)") +
  theme(legend.position = "none")

#====================================================================================================
#---------------------------------------------------------------Distribución general por sexo: Ind008
#====================================================================================================

Sexo<-DF%>%
  dplyr::select(r104,fac00)%>% 
  dplyr::group_by(r104)%>% 
  dplyr::summarise(poblacion=sum(fac00))%>%
  dplyr::mutate(total=sum(DF$fac00)) %>% 
  dplyr::mutate(porcentaje=(poblacion/total*100))

Sexo$r104<-as.factor(Sexo$r104) #---Transformo a factor
levels(Sexo$r104)<-c("Hombre","Mujer") #--se asigna los niveles

kable(Sexo)


#--Forma de pie
ggplot(Sexo,aes(x="",y=porcentaje, fill=r104))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(porcentaje/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("steelblue","tan2"))+
  theme_void()+
  labs(title ="El Salvador: Población según género 2022",
       subtitle ="(En porcentajes)",
       caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")


#---Forma de dona

ggplot(Sexo,aes(x=2,y=porcentaje, fill=r104))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(porcentaje/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("steelblue","tan2"))+
  theme_void()+
  labs(title ="El Salvador: Población según género 2022",
       subtitle ="(En porcentajes)",
       caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  xlim(0.5,2.5)

#=====================================================================================================
#---------------------------------------------------------------------Edad promedio según sexo: Ind009
#=====================================================================================================

#----Edad promedio

EdadMedia<-DF%>%
  dplyr::select(r104,fac00,r106)%>% 
  dplyr::rename(Sexo=r104)%>% 
  dplyr::group_by(Sexo)%>% 
  dplyr::summarise(poblacion=sum(fac00),sumaedad=sum(fac00*r106)) %>% 
  dplyr::mutate(edadMedia=(sumaedad/poblacion))

EdadMedia$Sexo<-as.factor(EdadMedia$Sexo) #---Transformo a factor
levels(EdadMedia$Sexo)<-c("Hombre","Mujer") #--se asigna los niveles

kable(EdadMedia)

#--Forma gráfica de la edad media
ggplot(EdadMedia, aes(x=Sexo, y=poblacion/1000000, fill=Sexo)) +
  geom_bar(stat="identity") +
  labs(title ="El Salvador: Edad media por sexo según población año 2022",
       subtitle ="(En millones de personas y años)",
       x = "Sexo", y = "Población (Millones)",caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(data = EdadMedia, aes(label = round(edadMedia,digits=2)),size = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme_grey()

#=====================================================================================================
#---------------------------------------------------------------------Condición de Alfabetismo: Ind010
#=====================================================================================================


DF$r202a <- as.factor(DF$r202a)
levels(DF$r202a)<- c("Si sabe leer","No sabe leer","Solo leer","Ninguno")


DF$r104 <- as.factor(DF$r104)
levels(DF$r104)<- c("Hombre","Mujer")


p <- ggplot(DF,aes(r202a, group=r104,fill=r104))

p + geom_bar(aes(weight = fac00/1000000),position="fill")+
  labs(title ="Población según condición de alfabetismo por género",
       subtitle ="(En porcentajes)",
       x = "Condición de Alfabetismo", y = "En porcentaje",
       caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))


p + geom_bar(aes(weight = fac00/1000000))+
  labs(title ="Población según condición de alfabetismo por género",
       subtitle ="(En porcentajes de personas)",
       x = "Condición de Alfabetismo", y = "Porcentaje",
       caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")

p + geom_bar(position = "dodge",aes(weight = fac00/1000000))+
  labs(title ="Población según condición de alfabetismo por género",
       subtitle ="(En millones de personas)",
       x = "Condición de Alfabetismo", y = "Millones personas",
       caption = "Elaboración propia con información de la Base EHPM 2022 ONEC/BCR")
