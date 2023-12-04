


#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-Parcial 2----------------------------------------------------------
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
library(jsonlite)
library(httr)
library(openssl)
library(curl)
#=======================================================================EndChunk


#=======================================================================StartChunk
#--------------------------Abriendo y explorando la EHPM

DF<-read_sav("Bases/EHPM 2022.sav")#---Se lee EHPM de datos
Dic <- read_csv("Bases/Diccionario.csv")#---Se lee diccionario
View(Dic)#---Ver Diccionario
View(EHPM)#---Ver EHPM de datos
#================================================Filtrando el departamento

#-------Frecuencia de departamento 

frq(DF$r004, weights = DF$fac00)

#---------------Filtro para definir departamento
DF_SS <- DF %>% dplyr::filter(r004==6)#--Filtro para San Salvador
DF_LL <- DF %>% dplyr::filter(r004==5)#--Filtro para La Libertad
#=======================================================================EndChunk

#/////////////////////////////////////////////////////////Construyendo la piramide de población/////////////////////////////

#-----------------------------------------------------Piramide San Salvador


#---Codificar la variable edad
rangoE <- vector()
rangoE[DF_SS$r106 >= 0 & DF_SS$r106 <= 4] <- 1
rangoE[DF_SS$r106 >= 5 &  DF_SS$r106 <= 9] <- 2
rangoE[DF_SS$r106 >= 10 & DF_SS$r106 <= 14] <- 3
rangoE[DF_SS$r106 >= 15 & DF_SS$r106 <= 19] <- 4
rangoE[DF_SS$r106 >= 20 & DF_SS$r106 <= 24] <- 5
rangoE[DF_SS$r106 >= 25 & DF_SS$r106 <= 29] <- 6
rangoE[DF_SS$r106 >= 30 & DF_SS$r106 <= 34] <- 7
rangoE[DF_SS$r106 >= 35 & DF_SS$r106 <= 39] <- 8
rangoE[DF_SS$r106 >= 40 & DF_SS$r106 <= 44] <- 9
rangoE[DF_SS$r106 >= 45 & DF_SS$r106 <= 49] <- 10
rangoE[DF_SS$r106 >= 50 & DF_SS$r106 <= 54] <- 11
rangoE[DF_SS$r106 >= 55 & DF_SS$r106 <= 59] <- 12
rangoE[DF_SS$r106 >= 60 & DF_SS$r106 <= 64] <- 13
rangoE[DF_SS$r106 >= 65 & DF_SS$r106 <= 69] <- 14
rangoE[DF_SS$r106 >= 70 & DF_SS$r106 <= 74] <- 15
rangoE[DF_SS$r106 >= 75 & DF_SS$r106 <= 79] <- 16
rangoE[DF_SS$r106 >= 80 & DF_SS$r106 <= 84] <- 17
rangoE[DF_SS$r106 >= 85] <- 18
DF_SS$rangoE <- as.factor(rangoE)
levels(DF_SS$rangoE) <- c("De 0 a 4","De 5 a 9","De 10 a 14","De 15 a 19","De 20 a 24","De 25 a 29"," De 30 a 34","De 35 a 39",
                       "De 40 a 44","De 45 a 49", "De 50 a 54", "De 55 a 59", "De 60 a 64","De 65 a 69", "De 70 a 74",
                       "De 75 a 79","De 80 a 84","85 y más")

#--Se construye la variable sexo con factor
DF_SS$sexo<-as.factor(DF_SS$r104)
levels(DF_SS$sexo)<- c("Hombre","Mujer")


#--Se construye la variable cantidad de Hombres
Hombre<-DF_SS%>%
  dplyr::select(sexo,rangoE,fac00) %>% 
  dplyr::filter(sexo=="Hombre") %>% 
  dplyr::group_by(rangoE) %>% 
  dplyr::summarise(Hombres=round(sum(fac00),digits = 0)/1000)

#--Se construye la variable cantidad de Mujeres
Mujer<-DF_SS%>%
  dplyr::select(sexo,rangoE,fac00) %>% 
  dplyr::filter(sexo=="Mujer") %>% 
  dplyr::group_by(rangoE) %>% 
  dplyr::summarise(Mujeres=round(sum(fac00),digits = 0)/1000)

#--Construyendo la pirámide
DataPiramide<-cbind(Hombre,Mujer)

DataPiramide<-DataPiramide[,c(2,4,1)]

#--graf piramide
datos<-data.frame(DataPiramide)
pyramid(datos,Llab="Hombres",Rlab="Mujeres",Clab="",
        main="Población del Departamento San Salvador, EHPM 2022\n (en miles de personas)",Lcol="cyan",
        Rcol="pink", Cgap=0.2,Cadj=0)

datos<-data.frame(DataPiramide)
pyramid(datos,Llab="Hombres",Rlab="Mujeres",Clab="",
        main="Población del Departamento San Salvador, EHPM 2022\n (en miles de personas)",Lcol="cyan",
        Rcol="pink", Cgap=0)

#====================================================================================================
#------------------------------------------------------------Creando la pirámide con library(ggplot2)
#====================================================================================================

DPy<-DF_SS %>% 
  dplyr::select(rangoE,sexo,fac00) %>% 
  dplyr::group_by(rangoE,sexo) %>% 
  dplyr::summarise(Pob=round(sum(fac00),digits = 0)/1000) %>%
  dplyr::arrange(sexo)


p <- ggplot(DPy,aes(x=rangoE, fill=sexo,
                    y=ifelse(sexo=='Hombre',-Pob,Pob)))

p+geom_bar(stat = "identity")+
  scale_y_continuous(limits = max(DPy$Pob)*c(-1,1),labels=abs)+
  geom_text(aes(x=rangoE,y=ifelse(sexo=='Hombre',-Pob,Pob),label=abs(round(Pob,digits=2))),size = 3,hjust = "inward")+
  scale_fill_brewer(palette ="Blues")+
  ggtitle("Pirámide Poblacional", subtitle = "Departamento San Salvador 2022")+
  labs(y="Población en miles",x="Rango Edad",
       caption = ("Fuente: Base EHPM 2021 ONEC/BCR"))+
  coord_flip()+
  theme_classic()


p+geom_bar(stat = "identity")+
  scale_y_continuous(limits = max(DPy$Pob)*c(-1,1),labels=abs)+
  #geom_text(aes(x=rangoE,y=ifelse(sexo=='Hombre',-Pob,Pob),label=abs(round(Pob,digits=2))),size = 3,hjust = "inward")+
  scale_fill_brewer(palette ="Blues")+
  ggtitle("Pirámide Poblacional", subtitle = "Departamento de San Salvador 2022")+
  labs(y="Población en miles",x="Rango Edad",
       caption = ("Fuente: Base EHPM 2021 ONEC/BCR"))+
  coord_flip()+
  theme_classic()

#-----------------------------------------------------Piramide La Libertad


#---Codificar la variable edad
rangoE <- vector()
rangoE[DF_LL$r106 >= 0 & DF_LL$r106 <= 4] <- 1
rangoE[DF_LL$r106 >= 5 &  DF_LL$r106 <= 9] <- 2
rangoE[DF_LL$r106 >= 10 & DF_LL$r106 <= 14] <- 3
rangoE[DF_LL$r106 >= 15 & DF_LL$r106 <= 19] <- 4
rangoE[DF_LL$r106 >= 20 & DF_LL$r106 <= 24] <- 5
rangoE[DF_LL$r106 >= 25 & DF_LL$r106 <= 29] <- 6
rangoE[DF_LL$r106 >= 30 & DF_LL$r106 <= 34] <- 7
rangoE[DF_LL$r106 >= 35 & DF_LL$r106 <= 39] <- 8
rangoE[DF_LL$r106 >= 40 & DF_LL$r106 <= 44] <- 9
rangoE[DF_LL$r106 >= 45 & DF_LL$r106 <= 49] <- 10
rangoE[DF_LL$r106 >= 50 & DF_LL$r106 <= 54] <- 11
rangoE[DF_LL$r106 >= 55 & DF_LL$r106 <= 59] <- 12
rangoE[DF_LL$r106 >= 60 & DF_LL$r106 <= 64] <- 13
rangoE[DF_LL$r106 >= 65 & DF_LL$r106 <= 69] <- 14
rangoE[DF_LL$r106 >= 70 & DF_LL$r106 <= 74] <- 15
rangoE[DF_LL$r106 >= 75 & DF_LL$r106 <= 79] <- 16
rangoE[DF_LL$r106 >= 80 & DF_LL$r106 <= 84] <- 17
rangoE[DF_LL$r106 >= 85] <- 18
DF_LL$rangoE <- as.factor(rangoE)
levels(DF_LL$rangoE) <- c("De 0 a 4","De 5 a 9","De 10 a 14","De 15 a 19","De 20 a 24","De 25 a 29"," De 30 a 34","De 35 a 39",
                          "De 40 a 44","De 45 a 49", "De 50 a 54", "De 55 a 59", "De 60 a 64","De 65 a 69", "De 70 a 74",
                          "De 75 a 79","De 80 a 84","85 y más")

#--Se construye la variable sexo con factor
DF_LL$sexo<-as.factor(DF_LL$r104)
levels(DF_LL$sexo)<- c("Hombre","Mujer")


#--Se construye la variable cantidad de Hombres
Hombre<-DF_LL%>%
  dplyr::select(sexo,rangoE,fac00) %>% 
  dplyr::filter(sexo=="Hombre") %>% 
  dplyr::group_by(rangoE) %>% 
  dplyr::summarise(Hombres=round(sum(fac00),digits = 0)/1000)

#--Se construye la variable cantidad de Mujeres
Mujer<-DF_LL%>%
  dplyr::select(sexo,rangoE,fac00) %>% 
  dplyr::filter(sexo=="Mujer") %>% 
  dplyr::group_by(rangoE) %>% 
  dplyr::summarise(Mujeres=round(sum(fac00),digits = 0)/1000)

#--Construyendo la pirámide
DataPiramide<-cbind(Hombre,Mujer)

DataPiramide<-DataPiramide[,c(2,4,1)]

#--graf piramide
datos<-data.frame(DataPiramide)
pyramid(datos,Llab="Hombres",Rlab="Mujeres",Clab="",
        main="Población del Departamento La Libertad, EHPM 2022\n (en miles de personas)",Lcol="cyan",
        Rcol="pink", Cgap=0.2,Cadj=0)

datos<-data.frame(DataPiramide)
pyramid(datos,Llab="Hombres",Rlab="Mujeres",Clab="",
        main="Población del Departamento La Libertad, EHPM 2022\n (en miles de personas)",Lcol="cyan",
        Rcol="pink", Cgap=0)

#====================================================================================================
#------------------------------------------------------------Creando la pirámide con library(ggplot2)
#====================================================================================================

DPy<-DF_LL %>% 
  dplyr::select(rangoE,sexo,fac00) %>% 
  dplyr::group_by(rangoE,sexo) %>% 
  dplyr::summarise(Pob=round(sum(fac00),digits = 0)/1000) %>%
  dplyr::arrange(sexo)


p <- ggplot(DPy,aes(x=rangoE, fill=sexo,
                    y=ifelse(sexo=='Hombre',-Pob,Pob)))

p+geom_bar(stat = "identity")+
  scale_y_continuous(limits = max(DPy$Pob)*c(-1,1),labels=abs)+
  geom_text(aes(x=rangoE,y=ifelse(sexo=='Hombre',-Pob,Pob),label=abs(round(Pob,digits=2))),size = 3,hjust = "inward")+
  scale_fill_brewer(palette ="Blues")+
  ggtitle("Pirámide Poblacional", subtitle = "Departamento La Libertad 2022")+
  labs(y="Población en miles",x="Rango Edad",
       caption = ("Fuente: Base EHPM 2021 ONEC/BCR"))+
  coord_flip()+
  theme_classic()


p+geom_bar(stat = "identity")+
  scale_y_continuous(limits = max(DPy$Pob)*c(-1,1),labels=abs)+
  #geom_text(aes(x=rangoE,y=ifelse(sexo=='Hombre',-Pob,Pob),label=abs(round(Pob,digits=2))),size = 3,hjust = "inward")+
  scale_fill_brewer(palette ="Blues")+
  ggtitle("Pirámide Poblacional", subtitle = "Departamento de La Libertad 2022")+
  labs(y="Población en miles",x="Rango Edad",
       caption = ("Fuente: Base EHPM 2021 ONEC/BCR"))+
  coord_flip()+
  theme_classic()


#////////////////////////////////////////////////////////Calculando la condicion de ocupación//////////////////////////////

#======================================================San Salvador
#--------Condición de ocupación y tasa de desocupación
#---95.3(832,594) y 4.7(41,143)

CondicionOcupacion_SS<-DF_SS%>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30) %>% 
  dplyr::group_by(actpr2012)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(Pob_16=sum(Poblacion))%>%
  dplyr::mutate(TasaOcupacionDesocupacion=scales::percent(Poblacion/Pob_16,accuracy =0.1))
CondicionOcupacion_SS$actpr2012<- as.factor(CondicionOcupacion_SS$actpr2012)
levels(CondicionOcupacion_SS$actpr2012 )<- c("Ocupados","Desocupados")

#======================================================La Libertad
#--------Condición de ocupación y tasa de desocupación
#---95.6(379,629) y 4.4(17,347)

CondicionOcupacion_LL<-DF_LL%>%
  dplyr::select(r106,actpr,actpr2012,actse,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012<30) %>% 
  dplyr::group_by(actpr2012)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(Pob_16=sum(Poblacion))%>%
  dplyr::mutate(TasaOcupacionDesocupacion=scales::percent(Poblacion/Pob_16,accuracy =0.1))
CondicionOcupacion_LL$actpr2012<- as.factor(CondicionOcupacion_LL$actpr2012)
levels(CondicionOcupacion_LL$actpr2012 )<- c("Ocupados","Desocupados")


#/////////////////////////////////////////////////////////////////API de naciones unidas/////////////////////////////////



# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"

# Creates the target URL, indicators, in this instance
target <- paste0(base_url, "/indicators/")

# Get the response, which includes data as well as information on pagination and number of records
response <- fromJSON(target)


# Get the first page of data
df_Ind <- response$data

# Loop until there are new pages with data
while (!is.null(response$nextPage)){
  
  #call the API for the next page
  response <- fromJSON(response$nextPage)
  
  #add the data of the new page to the data.frame with the data of the precious pages
  df <- rbind(df, response$data)
  
}

status_code(GET(target))

#-----------------Example 2: Returning a list of geographical areas

# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"

# Update relative path to retrieve records on locations
target <- paste0(base_url, "/locations/")

# Call the API
response <- fromJSON(target)

# Get the first page of data
df_Loca<- response$data

# Get the other pages with data
while (!is.null(response$nextPage)){
  
  response <- fromJSON(response$nextPage)
  df_Loca <- rbind(df_Loca, response$data)
  
}

#//////////////////////////////Nicaragua
# Update the relative path to search for data on a specific indicator, location, and for specific years

target <- paste0(base_url, "/data/indicators/1/locations/558/start/2005/end/2011")

# Call the API
response <- fromJSON(target)

# Get the first page of data
df_Nica<- response$data


#----Otra forma de filtrar

base_url <- "https://population.un.org/dataportalapi/api/v1"
Indicador<-1
Pais<-558
AñoI<-2005
AñoF<-2011

target<-paste0(base_url,"/data/indicators/",Indicador,"/locations/",Pais,"/start/",AñoI,"/end/",AñoF)

# Call the API
response <- fromJSON(target)

# Get the first page of data
df_Nica<- response$data



#--------------------
df2_Nica<- df_Nica[(df_Nica$variant=="Median") & df_Nica$category=="All women", names(df_Nica) %in% c("timeLabel","timeMid", "value", "category", "variant")]
df2_Nica <- df_Nica[(df_Nica$variant=="Median") , names(df_Nica) %in% c("timeLabel","timeMid", "value", "category", "variant")]
df2_Nica$category<- as.factor(df2_Nica$category)
levels(df2_Nica$category)<- c("Total","Mujeres casadas o unión libre","Mujeres divorciadas")

p<-ggplot(df2_Nica,aes(x=timeLabel, y=value,group=category, fill=category))+
  geom_area()+
  #ggtitle("Prevalencia anticonceptiva: Cualquier método (Porcentaje)")
  labs(title ="Nicaragua prevalencia de anticonceptivos: cualquier método",
       subtitle ="(En porcentajes de personas 2005-2011)",
       x = "Años", y = "Porcentaje",
       caption = "Elaboración propia con información de ONU/Department of Economic and Social Affairs
Population Division")
p
