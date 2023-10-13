

#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-01----------------------------------------------------------
#------------------------------Análisis Demográfico-----------------------------------------------
#/////////////////////////////////////////////////////////////////////////////////////////////////
#---Por Elias Preza

#=======================================================================StartChunk
#install.packages("nombre_del_paquete") 

#--------------------------Librerias
library(tidyverse)#---Wrangling data
library(sjmisc)#---Frecuencias
library(haven)#---Abrir base en formatos: SPSS,STATA y SAS
library(codebook)#--Uso de frecuencias
library(githubinstall)#--Uso de Github
library(data.table)#--Uso de data table
library(httr)#--Uso sitio web
library(zipR)#--Uso Zip

#--------------------------Ayudas Librerias
?tidyverse
?dplyr
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
View(DF)#---Ver EHPM de datos

#------------------------Explorando el contenido de la EHPM

dplyr::glimpse(DF) #---Estructura del dataframe
head(DF,5) #-----Visualización
tail(DF,20) #-----Visualización
nrow(DF) #---número de lineas o casos
ncol(DF) #---número de columnas o variables
dim(DF) #---Dimension de la EHPM: lineas - columnas
length(DF) #---número de columnas o variables
colnames(DF) #----Nombre de las columnas
names(DF) #----Nombre de las columnas
str(DF) #--Estructura del dataframe

#------------------------Explorando variables

str(DF$region) #--Estructura de la variable
dplyr::glimpse(DF$region) #--Estructura de la variable

#=======================================================================EndChunk
#=======================================================================StartChunk
#--------------------------Haciendo consultas


#==========================
#--Usando dplyr
#=========================

dplyr::glimpse(DF)#---explorando las variables del Data Frame 

C1<-DF %>%
    dplyr::select(edicion,lote,tipo,folio,viv,r101,idboleta,fac00,area,region,correlativo)

#---Creando la seccion 1

EHPM_S01<-DF %>% 
  dplyr::select(r101,r103,r103otr,r104,r105m,r105a,r106,r107,r108a,r108b,fac00) #---Select dplyr

dplyr::glimpse(EHPM_S01)

EHPM_S01.1<-DF %>%
  select(r101,r103,r103otr,r104,r105m,r105a,r106,r107,r108a,r108b,fac00)

dplyr::glimpse(EHPM_S01.1)

#---Construyendo un ID
EHPM_S01<-DF %>% 
  dplyr::select(r101,r103,r103otr,r104,r105m,r105a,r106,r107,r108a,r108b,fac00)%>%#---Select dplyr
  dplyr::mutate(id2=c(1:nrow(DF))) %>%  #--Se construye un ID
  dplyr::mutate(id3=seq(1,60339, by=1))   #--Se construye un ID 

#==========================
#--Usando el vectorizado
#=========================

c101<-DF[,1:20]
c101<-DF[1:50,1:20]
c101<-DF[1:10,c("r101","r103")]

#==========================
#--Usando datatable
#=========================

#---Creando la seccion 1

DT<-as.data.table(DF) #---se transforma la base a datatable

EHPM_S01_2<-DT[,.(r101,r103,r103otr,r104,r105m,r105a,r106,r107,r108a,r108b,fac00)]

EHPM_S01_2<-DT[,.(id=seq(1,60339,by=1),r101,r103,r103otr,r104,
                  r105m,r105a,r106,r107,r108a,r108b,fac00)] #---Construir un id

EHPM_S01_2<-DT[,.(id2= c(1:nrow(DT)),r101,r103,r103otr,r104,
                  r105m,r105a,r106,r107,r108a,r108b,fac00)] #---Construir un id

#=======================================================================StartChunk
#==========================
#--Conectando a la 
#=========================

#---Tabla miembros del hogar
edesaH<-read_sav("https://www.inide.gob.ni/docs/dataBases/Endesa11_12/BaseDatos/ENDESA2011%20miembros%20del%20hogar.sav")

dplyr::glimpse(edesaH)#---llamando a la base

Dic_00<-codebook_table(edesaH)#---Construyendo el diccionario

frq(edesaH$QHS3P04, weights = edesaH$PesoHogar)

frq(edesaH$QHS3P05, weights = edesaH$PesoHogar)

frq(edesaH$QHS3P10, weights = edesaH$PesoHogar)

frq(edesaH$QHS3P14N, weights = edesaH$PesoHogar)

frq(edesaH$QHS3P14G, weights = edesaH$PesoHogar)

frq(edesaH$HHCLUST)
frq(edesaH$HHNUMBV)

sum(edesaH$PesoHogar)

miem<-edesaH %>% 
      group_by(Clave1 ) %>% summarise(Miemb=sum(PesoHogar*HHNUMBV))

sum(miem$Miemb)

miem2<-edesaH %>% 
  group_by(QHS3P05)%>% 
  dplyr::summarise(Miemb=sum(PesoHogar*HHNUMBV)) %>% 
  dplyr::mutate(PobT=sum(Miemb)) %>% 
  dplyr::summarise(Por=Miemb/PobT)


#===============================================================================================Descargando la base Directo
#=======================================================================Forma 2
#--Descargando base de datos directamente de la ONEC

# Descargar el archivo zip
url <- "https://onec.bcr.gob.sv/Repositorio_archivos/files/bd/EHPM%20SPSS%202022.zip"
download.file(url, destfile = "Bases/EHPM%20SPSS%202022.zip")

# Descomprimir el archivo zip
unzip("Bases/EHPM%20SPSS%202022.zip",exdir = "Bases")

df<- read_sav("Bases/EHPM 2022.sav")


#=======================================================================Forma 3
#--Descargando base de datos directamente de OneDrive
df2<- read_sav("https://public.bl.files.1drv.com/y4mwX-VlE1fyxd0PB7GDyAGVC4xzTL4XhGX6Rtxm-eR1_FawHhOfRvzhV5Uo9elCIe2hBggA4ovYQyOoBfjIXpYS82Y9iybKKNi5sq3NDZ6lJcHPBkXOHSrOOsRls4-h3ctbnbz4pMiqvpz7vBeUawV1mY3_TnF6-wiexIVi9Kjqmsu6fSfDOQ1gOnvbpbV_fWI1KrwrgfKIzYWCrUaSX98d_OEe7BcFQTt3GdZPuuDW74?AVOverride=1")


#================================================================Descargando de Hacienda

# Descargar el archivo zip
url <- "https://www.transparenciafiscal.gob.sv/downloads/zip/700-DGICP-DA-2023-csv.zip"
download.file(url, destfile = "700-DGICP-DA-2023-csv.zip")

# Descomprimir el archivo zip
unzip("700-DGICP-DA-2023-csv.zip")


df<- read_delim("DGICP_PAIP1696354351776-012023-122023.csv", 
                delim = "|", escape_double = FALSE, trim_ws = TRUE)


#================================================================Descargando de Nicaragua
#---Tabla miembros del hogar
edesaH<-read_sav("https://www.inide.gob.ni/docs/dataBases/Endesa11_12/BaseDatos/ENDESA2011%20miembros%20del%20hogar.sav")
