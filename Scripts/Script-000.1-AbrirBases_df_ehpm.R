#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-00.1----------------------------------------------------------
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


#=======================================================================StartChunk
#--Descargando base de datos directamente de la ONEC

# Descargar el archivo zip
url <- "https://onec.bcr.gob.sv/Repositorio_archivos/files/bd/EHPM%20SPSS%202022.zip"
download.file(url, destfile = "/media/elias/ELIAS TRABAJO/CONSULTORIAS/IdeaData/Temas Investigacion/Docencia/ad-uca-r-2023/Bases/EHPM%20SPSS%202022.zip")

# Descomprimir el archivo zip
unzip("/media/elias/ELIAS TRABAJO/CONSULTORIAS/IdeaData/Temas Investigacion/Docencia/ad-uca-r-2023/Bases/EHPM%20SPSS%202022.zip",exdir = "Bases")


df<- read_sav("Bases/EHPM 2022.sav")


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
