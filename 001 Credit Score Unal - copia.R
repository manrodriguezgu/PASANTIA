#####      ----------- ooo -- ooo ---------     #######
###                    CASH  -- BLOCK
###                 Credit Score Script
###                  	Exploracion
###                     ooo -- ooo
rm(list = ls()) # Limpiar entorno
#####      ----------- ooo -- ooo ---------     #######
### Directorio
setwd("C:/Users/PERSONAL/OneDrive - Universidad Nacional de Colombia/Escritorio/PASANTIA")
#####      ----------- ooo -- ooo ---------     #######
#### Librerias
#install.packages("VIM") # kNN imputation
library(ggplot2)       
library(VIM)
library(dplyr)
library(cluster)
library(FactoMineR)
library(factoextra)
#####      ----------- ooo -- ooo ---------     #######
####Lectura
cred<-read.csv("BASE.csv", header = TRUE, row.names = NULL, sep=";", dec=",",na.strings=-999.)
dim(cred)
names(cred)
summary(cred)
#####      ----------- ooo -- ooo ---------     #######
####Categoricas
####Algunas variables se depurarón en R
names(table(cred$labor))
cred$labor[cred$labor== "EMPLEADO "] <- "EMPLEADO"
cred$labor[cred$labor== "INDEPENDIETE"] <- "INDEPENDIENTE"

cred$genero[cred$genero== "  HOMBRE"] <- "HOMBRE"
cred$genero[cred$genero== "HOMBRE  "] <- "HOMBRE"
cred$genero[cred$genero== "  MUJER"] <- "MUJER"
cred$genero[cred$genero== "MUJER  "] <- "MUJER"
cred$genero <- trimws(cred$genero)


cred$genero[cred$labor== "INDEPENDIETE"] <- "INDEPENDIENTE"
names(table(cred$genero))
names(table(cred$co))
cred$co[cred$co== "NO "] <- "0"
cred$co[cred$co== "NO"] <- "0"
cred$co[cred$co== "X"] <- "1"
cred$co[cred$co== "SI"] <- "1"

####Categorización
cred$labor<-as.factor(cred$labor)
cred$calificacion<-as.factor(cred$calificacion)
cred$co<-as.factor(cred$co)
cred$genero<-as.factor(cred$genero)
cred$estrato<-as.factor(cred$estrato)
cred$tipo_vivienda<-as.factor(cred$tipo_vivienda)
cred$nivel_educativo<-as.factor(cred$nivel_educativo)
cred$eactual<-as.factor(cred$eactual)

#####      ----------- ooo -- ooo ---------     #######
####Escalares
mediacredito <- mean(cred$credito)
dscredito<- sd(cred$credito)
liminferiorc <- mediacredito-(3*dscredito)
limsuperiorc <- mediacredito+(3*dscredito)
atipicoscredito <- subset(cred, (cred$credito< liminferiorc | cred$credito > limsuperiorc))
dim(atipicoscredito)

mediaingresos <- mean(cred$ingresos)
dsingresos<- sd(cred$ingresos)
liminferioringresos <- mediaingresos-(3*dsingresos)
limsuperioringresos <- mediaingresos+(3*dsingresos)
atipicosingresos <- subset(cred, (cred$ingresos< liminferioringresos | cred$ingresos > limsuperioringresos))
dim(atipicosingresos)

mediaIBC <- mean(cred$IBC)
dsIBC<- sd(cred$IBC)
liminferiorIBC <- mediaIBC-(3*dsIBC)
limsuperiorIBC <- mediaIBC+(3*dsIBC)
atipicosIBC <- subset(cred, (cred$IBC< liminferiorIBC| cred$IBC > limsuperiorIBC))
dim(atipicosIBC)

iguales <- subset(atipicosIBC, number %in% atipicoscredito$number)
dim(iguales)

cred$atipicoscredito<-ifelse((cred$credito < liminferiorc | cred$credito > limsuperiorc),1,0)
cred$atipicosIBC<-ifelse((cred$IBC < liminferiorIBC | cred$IBC > limsuperiorIBC),1,0)
credlimpio <- subset(cred,(cred$atipicoscredito==0 & cred$atipicosIBC==0))
dim(credlimpio)

dim(cred)

bp<-boxplot(cred$ingreso)
bp


bp<-boxplot(cred$credito)
bp
