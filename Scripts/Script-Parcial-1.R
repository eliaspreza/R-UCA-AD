
#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-Parcial 1----------------------------------------------------------
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

#================================================Definiendo base hogar
#---------------Filtro para definir hogar
DF_h <- DF %>% dplyr::filter(r103==1) #--filtrando jefatura hogar=1

#=================================================================================Clave A
#---------Filtrando el departamento de San Salvador

#·--frecuencia para obtener el codigo de depto
frq(DF$r004)

#====================================
#---Filtro de base de personas

ss_p<-DF %>% 
    dplyr::filter(r004==6)#--San Salvador

sa_p<-DF %>% 
  dplyr::filter(r004==2)#--Santa Ana

la_p<-DF %>% 
  dplyr::filter(r004==5)#--La Libertad

sm_p<-DF %>% 
  dplyr::filter(r004==12)#--San Miguel

#=================================
#---Filtro de base de hogares
ss_h<-DF_h %>% 
  dplyr::filter(r004==6)#--San Salvador

sa_h<-DF_h %>% 
  dplyr::filter(r004==2)#--Santa Ana

la_h<-DF_h %>% 
  dplyr::filter(r004==5)#--La Libertad

sm_h<-DF_h %>% 
  dplyr::filter(r004==12)#--San Miguel
#=========================================================================================================================
#==============================================================Indicador 01=Calculando Poblacion
#--Forma 1
poblacion<-sum(ss_p$fac00)
poblacion

#--Forma 2
poblacion2<-DF %>% 
           frq(r004, weights = fac00)
poblacion2

#--Forma 3
frq(DF$r004, weights = DF$fac00)

#====================================================================Indicador 02 y 03=Calculando Poblacion Urbana y Rural
#--Forma 1
PobArea<-ss_p%>% #---Se utiliza el df filtrado por personas y por departamento
  dplyr::select(area,fac00) %>% 
  dplyr::group_by(area)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)
PobArea$area<-as.factor(PobArea$area) #---Transformo a factor
levels(PobArea$area)<-c("Rural","Urbano") #--se asigna los niveles

PobArea

#--Forma 2
PobArea2<-DF%>% 
  dplyr::select(r004,area,fac00) %>% 
  dplyr::group_by(r004,area)%>% 
  dplyr::summarise(Poblacion=sum(fac00)) %>% 
  dplyr::mutate(PoblacionTotal=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=(Poblacion/PoblacionTotal)*100)
PobArea2$area<-as.factor(PobArea2$area) #---Transformo a factor
levels(PobArea2$area)<-c("Rural","Urbano") #--se asigna los niveles
PobArea2$r004<-as.factor(PobArea2$r004) #---Transformo a factor
levels(PobArea2$r004)<-c("Ahuachapán","Santa Ana","Sonsonate","Chalatenango","La Libertad","San Salvador",
                         "Cuscatlán","La Paz","Cabañas","San Vicente","Usulután","San Miguel","Morazán","La Unión") #--se asigna los niveles
PobArea2

#=======================================================================================Indicador 04=Calculando Hogares

#--Forma 1
hogares<-sum(ss_h$fac00)
hogares

#--Forma 2
hogares2<-DF_h %>% 
  frq(r004, weights = fac00)
hogares2

#--Forma 3
frq(DF_h$r004, weights = DF_h$fac00)


#=========================================================Indicador 05 y 06=Calculando poblacion < 4 años hombres y mujeres

#--Forma 1
Pob4menos<-ss_p %>%
  dplyr::select(r104,r106,fac00) %>% 
  dplyr::rename(Sexo=r104,Edad=r106) %>% 
  dplyr::filter(Edad<=4) %>% 
  dplyr::group_by(Sexo) %>% 
  dplyr::summarise(Poblacion=round(sum(fac00),digits = 0)) %>% 
  dplyr::mutate(PopT=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=round((Poblacion/PopT)*100,digits = 1))

Pob4menos

#--Forma 2
Pob4menos2<-DF %>%
  dplyr::select(r004,r104,r106,fac00) %>% 
  dplyr::rename(Sexo=r104,Edad=r106) %>% 
  dplyr::filter(Edad<=4) %>% 
  dplyr::group_by(r004,Sexo) %>% 
  dplyr::summarise(Poblacion=round(sum(fac00),digits = 0)) %>% 
  dplyr::mutate(PopT=sum(Poblacion)) %>% 
  dplyr::mutate(Porcentaje=round((Poblacion/PopT)*100,digits = 1))
Pob4menos2$Sexo<-as.factor(Pob4menos2$Sexo) #---Transformo a factor
levels(Pob4menos2$Sexo)<-c("Hombre","Mujer") #--se asigna los niveles
Pob4menos2$r004<-as.factor(Pob4menos2$r004) #---Transformo a factor
levels(Pob4menos2$r004)<-c("Ahuachapán","Santa Ana","Sonsonate","Chalatenango","La Libertad","San Salvador",
                         "Cuscatlán","La Paz","Cabañas","San Vicente","Usulután","San Miguel","Morazán","La Unión") #--se asigna los niveles
Pob4menos2

sum(Pob4menos2$Poblacion)


#===================================================================================Indicador 07 razon de masculinidad

#--Forma 1
H<-ss_p %>%
  dplyr::select(r104,fac00) %>% 
  dplyr::filter(r104==1) %>%
  dplyr::group_by(r104) %>%
  dplyr::summarise(PoblacionH=sum(fac00)) 

H

#-Mujeres
M<-ss_p %>%
  dplyr::select(r104,fac00) %>% 
  dplyr::filter(r104==2) %>%
  dplyr::group_by(r104) %>%
  dplyr::summarise(PoblacionM=sum(fac00)) 

M

RM<-(H/M)*100 

RM

#--Forma 2
H2<-DF %>%
  dplyr::select(r004,r104,fac00) %>% 
  dplyr::filter(r104==1) %>%
  dplyr::group_by(r004,r104) %>%
  dplyr::summarise(PoblacionH=sum(fac00)) 

M2<-DF %>%
  dplyr::select(r004,r104,fac00) %>% 
  dplyr::filter(r104==2) %>%
  dplyr::group_by(r004,r104) %>%
  dplyr::summarise(PoblacionM=sum(fac00)) 

h2m2<-cbind(H2,M2)

rm_2<-h2m2 %>% 
      dplyr::mutate(RM=(PoblacionH/PoblacionM)*100)
      rm_2$r004...1<-as.factor(rm_2$r004...1) #---Transformo a factor
      levels(rm_2$r004...1)<-c("Ahuachapán","Santa Ana","Sonsonate","Chalatenango","La Libertad","San Salvador",
                           "Cuscatlán","La Paz","Cabañas","San Vicente","Usulután","San Miguel","Morazán","La Unión") #--se asigna los niveles
rm_2

#==========================================================================================Indicador 08 razón de dependencia

#================================================================San Salvador
#-------------------------------------Estimando población ocupada
PobOcup2_ss<-ss_p %>% 
  dplyr::select(r106,actpr2012,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012==10) %>% 
  dplyr::summarise(PobOcupada=sum(fac00))

PobOcup2_ss

#--------------------------------------Estimando población total
PobTotal_ss<-ss_p%>% summarise(Poblacion=sum(fac00))

PobTotal_ss

#---------------------------------Estimando población dependiente
PobDep_ss<-PobTotal_ss-PobOcup2_ss

PobDep_ss

#-----------------------------------Tasa de dependencia económica
RD2_ss<-(PobDep_ss/PobOcup2_ss)

kable(RD2_ss)


#--Forma 2

#-Menores de 16 años

M16_ss2<-ss_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106<16) %>%
  dplyr::summarise(PoblacionMenor16=sum(fac00)) 

M16_ss2

#-Mayores de 64 años

M64_ss2<-ss_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>64) %>%
  dplyr::summarise(PoblacionMayor64=sum(fac00)) 

M64_ss2

#Numerador
SumM16M64_ss2<-sum(M16_ss2,M64_ss2)
SumM16M64_ss2

#-Denominador
PobEt_ss2<-sum(ss_p$fac00)-SumM16M64_ss2
PobEt_ss2

RD1_ss2<-(SumM16M64_ss2/PobEt_ss2)*100

kable(RD1_ss2)



#================================================================Santa Ana
#-------------------------------------Estimando población ocupada
PobOcup2_sa<-sa_p %>% 
  dplyr::select(r106,actpr2012,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012==10) %>% 
  dplyr::summarise(PobOcupada=sum(fac00))

PobOcup2_sa

#--------------------------------------Estimando población total
PobTotal_sa<-sa_p%>% summarise(Poblacion=sum(fac00))

PobTotal_sa

#---------------------------------Estimando población dependiente
PobDep_sa<-PobTotal_sa-PobOcup2_sa

PobDep_sa

#-----------------------------------Tasa de dependencia económica
RD2_sa<-(PobDep_sa/PobOcup2_sa)

kable(RD2_sa)


#--Forma 2

#-Menores de 16 años

M16_sa2<-sa_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106<16) %>%
  dplyr::summarise(PoblacionMenor16=sum(fac00)) 

M16_sa2

#-Mayores de 64 años

M64_sa2<-sa_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>64) %>%
  dplyr::summarise(PoblacionMayor64=sum(fac00)) 

M64_sa2

#Numerador
SumM16M64_sa2<-sum(M16_sa2,M64_sa2)
SumM16M64_sa2

#-Denominador
PobEt_sa2<-sum(sa_p$fac00)-SumM16M64_sa2
PobEt_sa2

RD1_sa2<-(SumM16M64_sa2/PobEt_sa2)*100

kable(RD1_sa2)

#================================================================La Libertad
#-------------------------------------Estimando población ocupada
PobOcup2_la<-la_p %>% 
  dplyr::select(r106,actpr2012,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012==10) %>% 
  dplyr::summarise(PobOcupada=sum(fac00))

PobOcup2_la

#--------------------------------------Estimando población total
PobTotal_la<-la_p%>% summarise(Poblacion=sum(fac00))

PobTotal_la

#---------------------------------Estimando población dependiente
PobDep_la<-PobTotal_la-PobOcup2_la

PobDep_la

#-----------------------------------Tasa de dependencia económica
RD2_la<-(PobDep_la/PobOcup2_la)

kable(RD2_la)



#--Forma 2

#-Menores de 16 años

M16_la2<-la_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106<16) %>%
  dplyr::summarise(PoblacionMenor16=sum(fac00)) 

M16_la2

#-Mayores de 64 años

M64_la2<-la_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>64) %>%
  dplyr::summarise(PoblacionMayor64=sum(fac00)) 

M64_la2

#Numerador
SumM16M64_la2<-sum(M16_la2,M64_la2)
SumM16M64_la2

#-Denominador
PobEt_la2<-sum(la_p$fac00)-SumM16M64_la2
PobEt_la2

RD1_la2<-(SumM16M64_la2/PobEt_la2)*100

kable(RD1_la2)

#================================================================San Miguel
#-------------------------------------Estimando población ocupada
PobOcup2_sm<-sm_p %>% 
  dplyr::select(r106,actpr2012,fac00) %>% 
  dplyr::filter(r106>=16 & actpr2012==10) %>% 
  dplyr::summarise(PobOcupada=sum(fac00))

PobOcup2_sm

#--------------------------------------Estimando población total
PobTotal_sm<-sm_p%>% summarise(Poblacion=sum(fac00))

PobTotal_sm

#---------------------------------Estimando población dependiente
PobDep_sm<-PobTotal_sm-PobOcup2_sm

PobDep_sm

#-----------------------------------Tasa de dependencia económica
RD2_sm<-(PobDep_sm/PobOcup2_sm)

kable(RD2_sm)



#--Forma 2

#-Menores de 16 años

M16_sm2<-sm_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106<16) %>%
  dplyr::summarise(PoblacionMenor16=sum(fac00)) 

M16_sm2

#-Mayores de 64 años

M64_sm2<-sm_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>64) %>%
  dplyr::summarise(PoblacionMayor64=sum(fac00)) 

M64_sm2

#Numerador
SumM16M64_sm2<-sum(M16_sm2,M64_sm2)
SumM16M64_sm2

#-Denominador
PobEt_sm2<-sum(sm_p$fac00)-SumM16M64_sm2
PobEt_sm2

RD1_sm2<-(SumM16M64_sm2/PobEt_sm2)*100

kable(RD1_sm2)




#==========================================================================================Indicador 09 razón de vejez


#================================================================San Salvador

#---Poblacion mayor o igual a 65 años
M65_ss<-ss_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=65) %>%
  dplyr::summarise(PoblacionMayor65=sum(fac00)) 

kable(M65_ss)

#----Población económicamente activa
Pea_ss<-ss_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=15&r106<65) %>%
  dplyr::summarise(PoblacionActiva=sum(fac00)) 

kable(Pea_ss)

RV_ss<-(M65_ss/Pea_ss)*100
kable(RV_ss)

#================================================================Santa Ana

#---Poblacion mayor o igual a 65 años
M65_sa<-sa_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=65) %>%
  dplyr::summarise(PoblacionMayor65=sum(fac00)) 

kable(M65_sa)

#----Población económicamente activa
Pea_sa<-sa_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=15&r106<65) %>%
  dplyr::summarise(PoblacionActiva=sum(fac00)) 

kable(Pea_sa)

RV_sa<-(M65_sa/Pea_sa)*100
kable(RV_sa)


#================================================================La Libertad

#---Poblacion mayor o igual a 65 años
M65_la<-la_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=65) %>%
  dplyr::summarise(PoblacionMayor65=sum(fac00)) 

kable(M65_la)

#----Población económicamente activa
Pea_la<-la_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=15&r106<65) %>%
  dplyr::summarise(PoblacionActiva=sum(fac00)) 

kable(Pea_la)

RV_la<-(M65_la/Pea_la)*100
kable(RV_la)



#================================================================San Miguel

#---Poblacion mayor o igual a 65 años
M65_sm<-sm_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=65) %>%
  dplyr::summarise(PoblacionMayor65=sum(fac00)) 

kable(M65_sm)

#----Población económicamente activa
Pea_sm<-sm_p %>%
  dplyr::select(r106,fac00) %>% 
  dplyr::filter(r106>=15&r106<65) %>%
  dplyr::summarise(PoblacionActiva=sum(fac00)) 

kable(Pea_sm)

RV_sm<-(M65_sm/Pea_sm)*100
kable(RV_sm)

#==========================================================================================Indicador 10,11 y 12 Pobreza

#================================================================San Salvador
Pobreza_ss <-ss_h %>% 
             frq(pobreza, weights = fac00)           
Pobreza_ss 
#================================================================Santa Ana
Pobreza_sa <-sa_h %>% 
  frq(pobreza, weights = fac00)           
Pobreza_sa 
#================================================================La Libertad
Pobreza_la <-la_h %>% 
  frq(pobreza, weights = fac00)           
Pobreza_la 
#================================================================San Miguel
Pobreza_sm<-sm_h %>% 
  frq(pobreza, weights = fac00)           
Pobreza_sm 