


#/////////////////////////////////////////////////////////////////////////////////////////////////
#------------------------------Script-04----------------------------------------------------------
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

#=======================================================================EndChunk


#=======================================================================StartChunk
#--------------------------Abriendo y explorando la EHPM

DF<-read_sav("Bases/EHPM 2022.sav")#---Se lee EHPM de datos
Dic <- read_csv("Bases/Diccionario.csv")#---Se lee diccionario
View(Dic)#---Ver Diccionario
View(EHPM)#---Ver EHPM de datos

#=======================================================================EndChunk

#====Se llama la base de datos

#b31 <- read.table("https://data.princeton.edu/eco572/datasets/prestonb31.dat",
#                           col.names = c("age", "N", "D"))
b31<-read.csv("Bases/prestonb31.csv")
b31 <- mutate(b31, n = c(diff(age), 0))

head(b31,10)


#=======================================================================StartChunk

#Paso 1 y 2: Se calculan las tasas de mortalidad y los factores para las edades de 0-1 y 1-4
#años retomadas de Coale-Demeny

#==1. Calcular las tasas de mortalidad dividiendo los eventos por exposición===
b31 <- mutate(b31, m = D/N)

#==2. A continuación necesitamos el tiempo vivido por las muertes unnx. 
#Preston et al. toman prestados estos valores para las edades de 5 a 75 años de Keyfitz y Flieger (1971), p.21.

#kfnax <- read.dta("https://data.princeton.edu/eco572/datasets/kfnax.dta")
kfnax <- read.csv("Bases/TablaKeyfitz.csv")
b31 <- inner_join(b31, kfnax, by="age")

#--Los factores para las edades de 0-1 y 1-4 se basan en las ecuaciones de 
#Coale-Demeny menores de 5 años, que dependen de la tasa de mortalidad a los 0 años. 
#No se utiliza el valor de la última edad, pero lo reemplazamos para evitar confusiones.


b31 <- rename(b31, a = nax)  # for simplicity

cond <- rep(b31[1,"m"] >= 0.107, 2) # condition must be a vector here

b31[1:2,"a"] <- ifelse(cond, c(0.330, 1.352) ,
                       b31[1:2,"a"] <- c(0.045, 1.651) + c(2.684, -2.816)* b31[1,"m"])
last <- nrow(b31)

b31[last,"a"] <- 1/ b31[last,"m"]

kable(b31)

#=======================================================================StartChunk

#Pasos 3 y 4: Se construyen las tasas de mortalidad en probabilidades y 
#el cálculo de la probabilidad de supervivencia


#===3. Convierta las tasas de mortalidad en probabilidades utilizando el unnx factores, y
#===4. Calcular probabilidades de supervivencia condicional como complementos

b31 <- mutate(b31, q = n * m/(1 + (n - a) * m), p = 1 - q)
b31[last, c("q","p")] = c(1, 0)

kable(b31)

#=======================================================================StartChunk
#Paso 5: Se genera la función de supervivencia a partir de un radio de 100,000
#(población hipotética)

b31 <- mutate(b31, lx = 100000 * cumprod( c(1, p[-last])))

kable(b31)


#=======================================================================StartChunk
#Paso 6: Se calculan las muertes, asumiendo que al final toda la población muere.

#===6. Calcular las muertes diferenciándose la función de supervivencia y 
#observando que al final todos mueren

b31 <- mutate(b31, d = c(-diff(lx), lx[last]))

kable(b31)


#=======================================================================StartChunk
#Paso 7 y 8: Se calculan los años por personas vividos y se acumulan para cada edad

#===7. Calcular los años-persona vividos en cada grupo de edad, que es n para aquellos que sobreviven 
#al grupo de edad y unnx para los que mueren, y
#===8. Acumular de abajo hacia arriba, lo que hacemos restando una suma corrida del total

b31 <- mutate(b31, L =  (lx - d) * n +  d * a,T = sum(L) - cumsum(L) + L)

kable(b31)

#=======================================================================StartChunk
#Paso 9: Finalmente se calcula la esperanza de vida:

#===9. Finalmente calcular la expectativa de vida dividiendo el tiempo vivido después de cada edad 
#por los sobrevivientes al comienzo de la edad

b31 <- mutate(b31, e = T/lx)

kable(b31)



#=======================================================================StartChunk
#==Graficando

#===Mortalidad

#--Forma ggplot
ggplot(b31, aes(age, log(m))) +
  geom_line()+
  labs(title ="Tasa Mortalidad Hombres",
       subtitle ="(Austria 1992, en Log)",
       x = "Edades", y = "Tasa Mortalidad (Log)",caption = "Retomado de=https://data.princeton.edu/eco572/periodlt")

#--Forma dygraph

b31.1<-b31 %>% mutate(mLog=log(m)) %>% select(age,mLog)

dygraph(b31.1,main = "Tasa Mortalidad Hombres, Austria 1992 (Log)",
        xlab = "Edades",
        ylab = "Tasa de Mortalidad (Log)") %>%
  dyShading(from = "5", to = "10",color = "#FFE6E6") %>%
  dyShading(from = "20", to = "40",color = "#D1F2EB") %>%
  dyShading(from = "60", to = "80",color = "#CCEBD6")


#===Esperanza de Vida

#--Forma ggplot

ggplot(b31, aes(age, lx/1000))+
  geom_line()+
  labs(title ="Esperanza de vida",
       subtitle ="(Austria 1992, en Miles de Personas)",
       x = "Edades", y = "En miles de Personas",caption = "Retomado de= https://data.princeton.edu/eco572/periodlt")

#--Forma dygraph

b31.2<-b31 %>% mutate(lxM=lx/1000) %>% select(age,lxM)

dygraph(b31.2,main = "Esperanza de vida, Austria 1992 (Miles de Personas)",
        xlab = "Edades",
        ylab = "Miles de Personas") %>%
  dyShading(from = "5", to = "10",color = "#FFE6E6") %>%
  dyShading(from = "20", to = "40",color = "#D1F2EB") %>%
  dyShading(from = "60", to = "80",color = "#CCEBD6")


#=======================================================================EndChunk

#====Se llama la base de datos



#====================================================================================================
#=======================Aplicando los pasos a EHPM 2012-2016 MUJERES, ejercicio con base ya preparada
#====================================================================================================

b32<- as.data.frame(read_csv("Bases/Mortalidad.csv"))#--Base preparada

b32 <- mutate(b32, n = c(diff(Edad), 0))

#==1. Calcular las tasas de mortalidad dividiendo los eventos por exposición===
b32 <- mutate(b32, m = D/N)

#==2. A continuación necesitamos el tiempo vivido por las muertes unnx. 
#Preston et al. toman prestados estos valores para las edades de 5 a 75 años de Keyfitz y Flieger (1971), p.21.

#kfnax <- read.dta("https://data.princeton.edu/eco572/datasets/kfnax.dta")
kfnax <- read.csv("Bases/TablaKeyfitz.csv")

b32 <- inner_join(b32, kfnax, by=c("Edad"="age"))

#--Los factores para las edades de 0-1 y 1-4 se basan en las ecuaciones de 
#Coale-Demeny menores de 5 años, que dependen de la tasa de mortalidad a los 0 años. 
#No se utiliza el valor de la última edad, pero lo reemplazamos para evitar confusiones.


b32 <- rename(b32, a = nax)  # for simplicity

cond <- rep(b32[1,"m"] >= 0.107, 2) # condition must be a vector here

b32[1:2,"a"] <- ifelse(cond, c(0.330, 1.352) ,
                       b32[1:2,"a"] <- c(0.045, 1.651) + c(2.684, -2.816)* b32[1,"m"])
last <- nrow(b32)

b32[last,"a"] <- 1/ b32[last,"m"]

#===3. Convierta las tasas de mortalidad en probabilidades utilizando el unnx factores, y
#===4. Calcular probabilidades de supervivencia condicional como complementos

b32 <- mutate(b32, q = n * m/(1 + (n - a) * m), p = 1 - q)
b32[last, c("q","p")] = c(1, 0)

#===5. Generar la función de supervivencia a partir de un radio de 100.000. Tenga en cuenta 
#que cada valor de lx depende del valor anterior

b32 <- mutate(b32, lx = 100000 * cumprod( c(1, p[-last])))

#===6. Calcular las muertes diferenciándose la función de supervivencia y 
#observando que al final todos mueren

b32 <- mutate(b32, d = c(-diff(lx), lx[last]))

#===7. Calcular los años-persona vividos en cada grupo de edad, que es n para aquellos que sobreviven 
#al grupo de edad y unnx para los que mueren, y
#===8. Acumular de abajo hacia arriba, lo que hacemos restando una suma corrida del total

b32 <- mutate(b32, L =  (lx - d) * n +  d * a,T = sum(L) - cumsum(L) + L)

#===9. Finalmente calcular la expectativa de vida dividiendo el tiempo vivido después de cada edad 
#por los sobrevivientes al comienzo de la edad

b32 <- mutate(b32, e = T/lx)



#=====================================================================================
#------------------Graficando
#=====================================================================================


#===============================
#===Graficando con ggplot

#==Graficando
transmute(b32, Edad = Edad + ifelse(n>0, n/2, 5), m = m) %>%
  ggplot(aes(Edad,log(m))) + geom_line()+
  labs(title ="TM según Sexo y Edad",
       subtitle ="(El Salvador 2012-2016, en Log, m.x(H),m.y(M))",
       x = "Edades", y = "Tasa Mortalidad (Log)",caption = "Elaboración propia con información de la Base EHPM DIGESTYC")


#==Graficando
transmute(b32, Edad = Edad, lx = lx) %>%
  ggplot(aes(Edad,log(lx))) + geom_line() +
  labs(title ="LX según Sexo y Edad",
       subtitle ="(El Salvador 2012-2016, en Log, m.x(H),m.y(M))",
       x = "Edades", y = "Tasa Mortalidad (Log)",caption = "Elaboración propia con información de la Base EHPM DIGESTYC")




#===============================
#===Graficando con dygraph
#---enlace a la libreria: dygraph= http://rstudio.github.io/dygraphs/


df<-b32 %>% select(Edad,m) %>% mutate(m=log(m))

dygraph(df)%>% dyRangeSelector()

dygraph(df, main="Mortalidad")%>%dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
  dyShading(from = "5", to = "10",color = "#FFE6E6") %>%
  dyShading(from = "40", to = "55",color = "#CCEBD6")

#===Grficando con dygraph
b32.2<-b32 %>% mutate(lxM=lx/1000) %>% select(Edad,lxM)

dygraph(b32.2,main = "Esperanza de vida, Austria 1992 (Miles)",
        xlab = "Edades",
        ylab = "Miles de Personas") %>%
  dyShading(from = "5", to = "10",color = "#FFE6E6") %>%
  dyShading(from = "20", to = "40",color = "#D1F2EB") %>%
  dyShading(from = "60", to = "80",color = "#CCEBD6")



#===========================================================================
#=======================Aplicando los pasos a EHPM 2012-2016 HOMBRES
#===========================================================================

b33<- as.data.frame(read_csv("Bases/MortalidadH.csv"))#--Base preparada

b33 <- mutate(b33, n = c(diff(Edad), 0))

#==1. Calcular las tasas de mortalidad dividiendo los eventos por exposición===
b33 <- mutate(b33, m = D/N)

#==2. A continuación necesitamos el tiempo vivido por las muertes unnx. 
#Preston et al. toman prestados estos valores para las edades de 5 a 75 años de Keyfitz y Flieger (1971), p.21.

kfnax <- read.csv("Bases/TablaKeyfitz.csv")

b33 <- inner_join(b33, kfnax, by=c("Edad"="age"))

#--Los factores para las edades de 0-1 y 1-4 se basan en las ecuaciones de 
#Coale-Demeny menores de 5 años, que dependen de la tasa de mortalidad a los 0 años. 
#No se utiliza el valor de la última edad, pero lo reemplazamos para evitar confusiones.


b33 <- rename(b33, a = nax)  # for simplicity

cond <- rep(b33[1,"m"] >= 0.107, 2) # condition must be a vector here

b33[1:2,"a"] <- ifelse(cond, c(0.330, 1.352) ,
                       b33[1:2,"a"] <- c(0.045, 1.651) + c(2.684, -2.816)* b33[1,"m"])
last <- nrow(b33)

b33[last,"a"] <- 1/ b33[last,"m"]

#===3. Convierta las tasas de mortalidad en probabilidades utilizando el unnx factores, y
#===4. Calcular probabilidades de supervivencia condicional como complementos

b33 <- mutate(b33, q = n * m/(1 + (n - a) * m), p = 1 - q)
b33[last, c("q","p")] = c(1, 0)

#===5. Generar la función de supervivencia a partir de un radio de 100.000. Tenga en cuenta 
#que cada valor de lx depende del valor anterior

b33 <- mutate(b33, lx = 100000 * cumprod( c(1, p[-last])))

#===6. Calcular las muertes diferenciándose la función de supervivencia y 
#observando que al final todos mueren

b33 <- mutate(b33, d = c(-diff(lx), lx[last]))

#===7. Calcular los años-persona vividos en cada grupo de edad, que es n para aquellos que sobreviven 
#al grupo de edad y unnx para los que mueren, y
#===8. Acumular de abajo hacia arriba, lo que hacemos restando una suma corrida del total

b33 <- mutate(b33, L =  (lx - d) * n +  d * a,T = sum(L) - cumsum(L) + L)

#===9. Finalmente calcular la expectativa de vida dividiendo el tiempo vivido después de cada edad 
#por los sobrevivientes al comienzo de la edad

b33 <- mutate(b33, e = T/lx)

#==Graficando
transmute(b33, Edad = Edad + ifelse(n>0, n/2, 5), m = m) %>%
  ggplot(aes(Edad,log(m))) + geom_line() + ggtitle("El Salvador, Mortalidad Masculina 2012-2016")

tv<-b33 %>% select(Edad,m) %>% mutate(m=log(m))

dygraph(tv)%>% dyRangeSelector()

dygraph(tv, main="Mortalidad")%>%dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
  dyShading(from = "5", to = "10",color = "#FFE6E6") %>%
  dyShading(from = "40", to = "55",color = "#CCEBD6")


