

#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-01----------------------------------------------------------
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


#--------------------------Ayudas Librerias
?tidyverse
?sjmisc
?haven
?codebook
#=======================================================================EndChunk

#=======================================================================StartChunk
#--------------------------Abriendo y explorando la EHPM

DF<-read_sav("Bases/EHPM 2022.sav")#---Se lee EHPM de datos
#Dic_00<-codebook_table(EHPM)#---Construyendo el diccionario
#write_excel_csv(Dic_00,"EHPMs/Diccionario.csv")#---Se guarda diccionario
Dic <- read_csv("Bases/Diccionario.csv")#---Se lee diccionario
View(Dic)#---Ver Diccionario
View(EHPM)#---Ver EHPM de datos

#------------------------Explorando el contenido de la EHPM

dplyr::glimpse(EHPM) #---Estructura del dataframe
head(EHPM,5) #-----Visualización
tail(EHPM,20) #-----Visualización
nrow(EHPM) #---número de lineas o casos
ncol(EHPM) #---número de columnas o variables
dim(EHPM) #---Dimension de la EHPM: lineas - columnas
length(EHPM) #---número de columnas o variables
colnames(EHPM) #----Nombre de las columnas
names(EHPM) #----Nombre de las columnas
str(EHPM) #--Estructura del dataframe

#------------------------Explorando variables

str(EHPM$region) #--Estructura de la variable
dplyr::glimpse(EHPM$region) #--Estructura de la variable

#=======================================================================EndChunk
