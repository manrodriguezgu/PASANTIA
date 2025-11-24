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
install.packages("FinCal")# FinCal
install.packages("corrplot") # corrplot
install.packages("Information") #IV


library(VIM)
library(cluster)
library(FinCal)
library(corrplot)
library(Information)
library(pROC)
library(dplyr)

#####      ----------- ooo -- ooo ---------     #######
####Lectura
cred<-read.csv("BASE.csv", header = TRUE, row.names = NULL, sep=";", na.strings=-999.)
cred[1:2,]
dim(cred)
names(cred)
#####      ----------- ooo -- ooo ---------     #######
####Categoricas

names(table(cred$labor))
#La variable “labor" tien errores de digitación en el registro de las categorias
cred$labor[cred$labor== "EMPLEADO "] <- "EMPLEADO"
cred$labor[cred$labor== "INDEPENDIETE"] <- "INDEPENDIENTE"


names(table(cred$calificacion))
#Calificacion es correcta


names(table(cred$co))
#Nuevamente encontramos errores de digitación, ademas sabemos que la X implica que si existe la garantia
cred$co[cred$co== "NO "] <- "0"
cred$co[cred$co== "NO"] <- "0"
cred$co[cred$co== "X"] <- "1"
cred$co[cred$co== "SI"] <- "1"
#para eliminar los Nulos de esta variable debemos cruzarlo con la variable dtscoreco

names(table(cred$yeardde))
#varible con categorias correctas


names(table(cred$genero))
#Nuevamente encontramos errores de digitación
cred$genero[cred$genero== "HOMBRE"] <- "MASCULINO"
cred$genero[cred$genero== "HOMBRE "] <- "MASCULINO"
cred$genero[cred$genero== " HOMBRE"] <- "MASCULINO"
cred$genero[cred$genero== "MUJER"] <- "MUJER"
cred$genero[cred$genero== " MUJER"] <- "MUJER"
cred$genero[cred$genero== "MUJER "] <- "MUJER"


names(table(cred$tipo_vivienda))
#varible con categorias correctas

names(table(cred$nivel_educativo))
#varible con categorias correctas



names(table(cred$eactual))
#eactual esta correctamente categorizada, sin embargo para la construccion de modelos reduciremos las categorias a 1 (impago) y 0 (pago)

#para evitar incluir eventos situacionales en el comportamiento de impago se considerara impago partir de los 60 días

#categorizacion
cred$labor<-as.factor(cred$labor)
cred$calificacion<-as.factor(cred$calificacion)
cred$co<-as.factor(cred$co)
cred$yeardde<-as.factor(cred$yeardde)
cred$genero<-as.factor(cred$genero)
cred$tipo_vivienda<-as.factor(cred$tipo_vivienda)
cred$nivel_educativo<-as.factor(cred$nivel_educativo)
cred$eactual<-as.factor(cred$eactual)

cred$dtscore_aux<-as.factor(cred$dtscore_aux)
cred$dtscoreco_aux<-as.factor(cred$dtscoreco_aux)



summary(cred)

#####      ----------- ooo -- ooo ---------     #######
####Escalares

#detección de valores atipicos en variable "creddtscore"
min(cred$dtscore, na.rm = TRUE)
max(cred$dtscore, na.rm = TRUE)
#todos los valores estan dentro de un rango normal



#detección de valores atipicos en variable "creddtscoreco"
min(cred$dtscoreco, na.rm = TRUE)
max(cred$dtscoreco, na.rm = TRUE)
cred<- cred[-(which(cred$dtscoreco == 909)),]
#existe un score de 909 cuando el valor maximo es de 900



#detección de valores atipicos en variable "creddtscoreco"
min(cred$edad, na.rm = TRUE)
max(cred$edad, na.rm = TRUE)
#no hay valores fuera de un rango normal para la varaible



#deteccion de atipicos para IBC (primero la paso a numerica)
cred$IBC <- as.numeric(cred$IBC)   
min(cred$IBC, na.rm = TRUE)
max(cred$IBC, na.rm = TRUE)
cred<- cred[-(which(cred$IBC == 18.8)),]
#existe un al menos 1 valor atipico



#detección de valores atipicos en variable "antiguedad_laboral"
min(cred$antiguedad_laboral, na.rm = TRUE)
max(cred$antiguedad_laboral, na.rm = TRUE)
#todos los valores estan dentro de un rango normal



#detección de valores atipicos en variable "ncuotas"
min(cred$ncuotas, na.rm = TRUE)
max(cred$ncuotas, na.rm = TRUE)
#todos los valores estan dentro de un rango normal



#detección de valores atipicos en variable "estrato"
min(cred$estrato, na.rm = TRUE)
max(cred$estrato, na.rm = TRUE)
#todos los valores estan dentro de un rango normal



#detección de valores atipicos en variable "ingresos"
bp<-boxplot(cred$ingreso)
bp
#Es posible que existan valores atipicos pero no se evidencia claramente
mediaingresos <- mean(cred$ingresos)
dsingresos<- sd(cred$ingresos)
liminferioringresos <- mediaingresos-(3*dsingresos)
limsuperioringresos <- mediaingresos+(3*dsingresos)
atipicosingresos <- subset(cred, (cred$ingresos< liminferioringresos | cred$ingresos > limsuperioringresos))
dim(atipicosingresos)
#La regla empirica descarta la existencia de valores atipicos en la variable



#detección de valores atipicos en variable "ingresospendeuda"
bp<-boxplot(cred$pendeuda)
bp
#Es posible que existan valores atipicos
mediapendeuda <- mean(cred$pendeuda)
dspendeuda<- sd(cred$pendeuda)
liminferiorpendeuda<- mediapendeuda-(3*dspendeuda)
limsuperiorpendeuda<- mediapendeuda+(3*dspendeuda)
atipicospendeuda<- subset(cred, (cred$pendeuda< liminferiorpendeuda | cred$pendeuda > limsuperiorpendeuda))
dim(atipicospendeuda)
#La regla empirica confirma la existencia de 10 valores atipicos en la variable



#detección de valores atipicos en variable "credito"
bp<-boxplot(cred$credito)
bp
#Es posible que existan valores atipicos
cred$credito<- as.numeric(cred$credito)
mediacredito<- mean(cred$credito)
dscredito<- sd(cred$credito)
liminferiorcredito<- mediacredito-(3*dscredito)
limsuperiorcredito<- mediacredito+(3*dscredito)
atipicoscredito<- subset(cred, (cred$credito< liminferiorcredito | cred$credito > limsuperiorcredito))
dim(atipicoscredito)



#detección de valores atipicos en variable "tinteres"
bp<-boxplot(cred$tinteres)
bp
#no se evidencian valores atipicos se omite regla empirica



#detección de valores atipicos en variable "capacidad"
bp<-boxplot(cred$capacidad)
bp
#es posible que existan valores atipicos
mediacapacidad <- mean(cred$capacidad)
dscapacidad<- sd(cred$capacidad)
liminferiorcapacidad <- mediacapacidad-(3*dscapacidad)
limsuperiorcapacidad <- mediacapacidad+(3*dscapacidad)
atipicoscapacidad <- subset(cred, (cred$capacidad< liminferiorcapacidad | cred$capacidad > limsuperiorcapacidad))
dim(atipicoscapacidad)
#se detectan 4 valores atipicos


#detección de valores atipicos en variable "estrato"
bp<-boxplot(cred$estrato)
bp
#no se evidencian valores atipicos se omite regla empirica


#detección de valores atipicos en variable "creditos_activos"
bp<-boxplot(cred$creditos_activos)
bp
#no se evidencian valores atipicos se omite regla empirica


#detección de valores atipicos en variable "personas_hogar"
bp<-boxplot(cred$personas_hogar)
bp
#no se evidencian valores atipicos se omite regla empirica


cred$atipicospendeuda<-ifelse((cred$pendeuda< liminferiorpendeuda | cred$pendeuda > limsuperiorpendeuda),1,0)
cred$atipicoscapacidad<-ifelse((cred$capacidad < liminferiorcapacidad | cred$capacidad > limsuperiorcapacidad),1,0)
credlimpio <- subset(cred,(cred$atipicospendeuda==0 & cred$atipicoscapacidad==0))
dim(credlimpio)
dim(cred)

cred<-credlimpio


summary(cred)

## --    ### -- ### -------------
##-- Imputation : KNN algorithm
pred <- c("ingresos","estrato", "edad","dtscore", "cuota", "antiguedad_laboral")# seleccion de predictores
cred_se <- cred[, pred]
names(cred_se)
summary(cred_se)


# identificar N/A
uv <- c(which(is.na(cred_se$ingresos)), which(is.na(cred_se$estrato)), which(is.na(cred_se$dtscore)))


### ---- determinar numero de grupos
tem <- scale(cred_se[-uv,]) # estandarizar variables
dim(tem)
### Matriz de distancias
cdistan<- dist(tem, method = "euclidean")## method=manhattan, sorensen, bray-curtis, gower
## estimar k optimos
fviz_nbclust(tem, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

fviz_nbclust(tem, kmeans, method = "silhouette", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

#KNN estimacion
cred_iset <- kNN(cred_se, variable = "ingresos", k = 8, imp_var = FALSE)

# Replace the original dtscore in cred with the imputed values
cred$ingresosimp <- cred_iset$ingresos
names(cred)
summary(cred)


cred[,c(10,29)]# comparacion
sd(cred[,10], na.rm=TRUE); sd(cred[,29])# Las varianzas son similares,mantuvo la estructura de la distribución


-------------------------------------



pred <- c("estrato","ingresosimp", "edad","dtscore", "cuota", "antiguedad_laboral","personas_hogar")# seleccion de predictores
cred_se <- cred[, pred]
names(cred_se)
summary(cred_se)


# identificar N/A
uv <- c(which(is.na(cred_se$estrato)), which(is.na(cred_se$dtscore)))


### ---- determinar numero de grupos
tem <- scale(cred_se[-uv,]) # estandarizar variables
dim(tem)
### Matriz de distancias
cdistan<- dist(tem, method = "euclidean")## method=manhattan, sorensen, bray-curtis, gower
## estimar k optimos
fviz_nbclust(tem, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

fviz_nbclust(tem, kmeans, method = "silhouette", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

#KNN estimacion
cred_iset <- kNN(cred_se, variable = "estrato", k = 8, imp_var = FALSE)

# Replace the original dtscore in cred with the imputed values
cred$estratoimp <- cred_iset$estrato
names(cred)
summary(cred)


cred[,c(21,30)]# comparacion
sd(cred[,21], na.rm=TRUE); sd(cred[,30])# Las varianzas son similares, mantuvo la estructura de la distribución


---------------------------------------------------
cred<-cred[,-c(10,21)]
summary(cred)
	

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
cred<-read.csv("BASE.csv", header = TRUE, row.names = NULL, sep=";", na.strings=-999.)
cred[1:2,]
dim(cred)
names(cred)
#####      ----------- ooo -- ooo ---------     #######
####Categoricas

names(table(cred$labor))
#La variable “labor" tien errores de digitación en el registro de las categorias
cred$labor[cred$labor== "EMPLEADO "] <- "EMPLEADO"
cred$labor[cred$labor== "INDEPENDIETE"] <- "INDEPENDIENTE"


names(table(cred$calificacion))
#Calificacion es correcta


names(table(cred$co))
#Nuevamente encontramos errores de digitación, ademas sabemos que la X implica que si existe la garantia
cred$co[cred$co== "NO "] <- "0"
cred$co[cred$co== "NO"] <- "0"
cred$co[cred$co== "X"] <- "1"
cred$co[cred$co== "SI"] <- "1"
#para eliminar los Nulos de esta variable debemos cruzarlo con la variable dtscoreco

names(table(cred$yeardde))
#varible con categorias correctas


names(table(cred$genero))
#Nuevamente encontramos errores de digitación
cred$genero[cred$genero== "HOMBRE"] <- "MASCULINO"
cred$genero[cred$genero== "HOMBRE "] <- "MASCULINO"
cred$genero[cred$genero== " HOMBRE"] <- "MASCULINO"
cred$genero[cred$genero== "MUJER"] <- "MUJER"
cred$genero[cred$genero== " MUJER"] <- "MUJER"
cred$genero[cred$genero== "MUJER "] <- "MUJER"


names(table(cred$tipo_vivienda))
#varible con categorias correctas

names(table(cred$nivel_educativo))
#varible con categorias correctas



names(table(cred$eactual))
#eactual esta correctamente categorizada, sin embargo para la construccion de modelos reduciremos las categorias a 1 (impago) y 0 (pago)

#para evitar incluir eventos situacionales en el comportamiento de impago se considerara impago partir de los 60 días

#categorizacion
cred$labor<-as.factor(cred$labor)
cred$calificacion<-as.factor(cred$calificacion)
cred$co<-as.factor(cred$co)
cred$yeardde<-as.factor(cred$yeardde)
cred$genero<-as.factor(cred$genero)
cred$tipo_vivienda<-as.factor(cred$tipo_vivienda)
cred$nivel_educativo<-as.factor(cred$nivel_educativo)
cred$eactual<-as.factor(cred$eactual)

cred$dtscore_aux<-as.factor(cred$dtscore_aux)
cred$dtscoreco_aux<-as.factor(cred$dtscoreco_aux)



summary(cred)

#####      ----------- ooo -- ooo ---------     #######
####Escalares

#detección de valores atipicos en variable "creddtscore"
min(cred$dtscore, na.rm = TRUE)
max(cred$dtscore, na.rm = TRUE)
#todos los valores estan dentro de un rango normal



#detección de valores atipicos en variable "creddtscoreco"
min(cred$dtscoreco, na.rm = TRUE)
max(cred$dtscoreco, na.rm = TRUE)
cred<- cred[-(which(cred$dtscoreco == 909)),]
#existe un score de 909 cuando el valor maximo es de 900



#detección de valores atipicos en variable "creddtscoreco"
min(cred$edad, na.rm = TRUE)
max(cred$edad, na.rm = TRUE)
#no hay valores fuera de un rango normal para la varaible



#deteccion de atipicos para IBC (primero la paso a numerica)
cred$IBC <- as.numeric(cred$IBC)   
min(cred$IBC, na.rm = TRUE)
max(cred$IBC, na.rm = TRUE)
cred<- cred[-(which(cred$IBC == 18.8)),]
#existe un al menos 1 valor atipico



#detección de valores atipicos en variable "antiguedad_laboral"
min(cred$antiguedad_laboral, na.rm = TRUE)
max(cred$antiguedad_laboral, na.rm = TRUE)
#todos los valores estan dentro de un rango normal



#detección de valores atipicos en variable "ncuotas"
min(cred$ncuotas, na.rm = TRUE)
max(cred$ncuotas, na.rm = TRUE)
#todos los valores estan dentro de un rango normal



#detección de valores atipicos en variable "estrato"
min(cred$estrato, na.rm = TRUE)
max(cred$estrato, na.rm = TRUE)
#todos los valores estan dentro de un rango normal



#detección de valores atipicos en variable "ingresos"
bp<-boxplot(cred$ingresos)
bp
#Es posible que existan valores atipicos pero no se evidencia claramente
mediaingresos <- mean(cred$ingresos)
dsingresos<- sd(cred$ingresos)
liminferioringresos <- mediaingresos-(3*dsingresos)
limsuperioringresos <- mediaingresos+(3*dsingresos)
atipicosingresos <- subset(cred, (cred$ingresos< liminferioringresos | cred$ingresos > limsuperioringresos))
dim(atipicosingresos)
#La regla empirica descarta la existencia de valores atipicos en la variable



#detección de valores atipicos en variable "ingresospendeuda"
bp<-boxplot(cred$pendeuda)
bp
#Es posible que existan valores atipicos
mediapendeuda <- mean(cred$pendeuda)
dspendeuda<- sd(cred$pendeuda)
liminferiorpendeuda<- mediapendeuda-(3*dspendeuda)
limsuperiorpendeuda<- mediapendeuda+(3*dspendeuda)
atipicospendeuda<- subset(cred, (cred$pendeuda< liminferiorpendeuda | cred$pendeuda > limsuperiorpendeuda))
dim(atipicospendeuda)
#La regla empirica confirma la existencia de 10 valores atipicos en la variable



#detección de valores atipicos en variable "credito"
bp<-boxplot(cred$credito)
bp
#Es posible que existan valores atipicos
cred$credito<- as.numeric(cred$credito)
mediacredito<- mean(cred$credito)
dscredito<- sd(cred$credito)
liminferiorcredito<- mediacredito-(3*dscredito)
limsuperiorcredito<- mediacredito+(3*dscredito)
atipicoscredito<- subset(cred, (cred$credito< liminferiorcredito | cred$credito > limsuperiorcredito))
dim(atipicoscredito)



#detección de valores atipicos en variable "tinteres"
bp<-boxplot(cred$tinteres)
bp
#no se evidencian valores atipicos se omite regla empirica



#detección de valores atipicos en variable "capacidad"
bp<-boxplot(cred$capacidad)
bp
#es posible que existan valores atipicos
mediacapacidad <- mean(cred$capacidad)
dscapacidad<- sd(cred$capacidad)
liminferiorcapacidad <- mediacapacidad-(3*dscapacidad)
limsuperiorcapacidad <- mediacapacidad+(3*dscapacidad)
atipicoscapacidad <- subset(cred, (cred$capacidad< liminferiorcapacidad | cred$capacidad > limsuperiorcapacidad))
dim(atipicoscapacidad)
#se detectan 4 valores atipicos


#detección de valores atipicos en variable "estrato"
bp<-boxplot(cred$estrato)
bp
#no se evidencian valores atipicos se omite regla empirica


#detección de valores atipicos en variable "creditos_activos"
bp<-boxplot(cred$creditos_activos)
bp
#no se evidencian valores atipicos se omite regla empirica


#detección de valores atipicos en variable "personas_hogar"
bp<-boxplot(cred$personas_hogar)
bp
#no se evidencian valores atipicos se omite regla empirica


cred$atipicospendeuda<-ifelse((cred$pendeuda< liminferiorpendeuda | cred$pendeuda > limsuperiorpendeuda),1,0)
cred$atipicoscapacidad<-ifelse((cred$capacidad < liminferiorcapacidad | cred$capacidad > limsuperiorcapacidad),1,0)
credlimpio <- subset(cred,(cred$atipicospendeuda==0 & cred$atipicoscapacidad==0))
dim(credlimpio)
dim(cred)

cred<-credlimpio


summary(cred)

## --    ### -- ### -------------
##-- Imputation : KNN algorithm
pred <- c("ingresos","estrato", "edad","dtscore", "cuota", "antiguedad_laboral")# seleccion de predictores
cred_se <- cred[, pred]
names(cred_se)
summary(cred_se)


# identificar N/A
uv <- c(which(is.na(cred_se$ingresos)), which(is.na(cred_se$estrato)), which(is.na(cred_se$dtscore)))


### ---- determinar numero de grupos
tem <- scale(cred_se[-uv,]) # estandarizar variables
dim(tem)
### Matriz de distancias
cdistan<- dist(tem, method = "euclidean")## method=manhattan, sorensen, bray-curtis, gower
## estimar k optimos
fviz_nbclust(tem, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

fviz_nbclust(tem, kmeans, method = "silhouette", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

#KNN estimacion
cred_iset <- kNN(cred_se, variable = "ingresos", k = 8, imp_var = FALSE)

# Replace the original dtscore in cred with the imputed values
cred$ingresosimp <- cred_iset$ingresos
names(cred)
summary(cred)


cred[,c(10,29)]# comparacion
sd(cred[,10], na.rm=TRUE); sd(cred[,29])# Las varianzas son similares,mantuvo la estructura de la distribución


-------------------------------------



pred <- c("estrato","ingresosimp", "edad","dtscore", "cuota", "antiguedad_laboral","personas_hogar")# seleccion de predictores
cred_se <- cred[, pred]
names(cred_se)
summary(cred_se)


# identificar N/A
uv <- c(which(is.na(cred_se$estrato)), which(is.na(cred_se$dtscore)))


### ---- determinar numero de grupos
tem <- scale(cred_se[-uv,]) # estandarizar variables
dim(tem)
### Matriz de distancias
cdistan<- dist(tem, method = "euclidean")## method=manhattan, sorensen, bray-curtis, gower
## estimar k optimos
fviz_nbclust(tem, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

fviz_nbclust(tem, kmeans, method = "silhouette", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

#KNN estimacion
cred_iset <- kNN(cred_se, variable = "estrato", k = 8, imp_var = FALSE)

# Replace the original dtscore in cred with the imputed values
cred$estratoimp <- cred_iset$estrato
names(cred)
summary(cred)


cred[,c(21,30)]# comparacion
sd(cred[,21], na.rm=TRUE); sd(cred[,30])# Las varianzas son similares, mantuvo la estructura de la distribución


---------------------------------------------------


cred$credito<- ifelse(is.na(cred$credito),cred$cuota*((1-(1+cred$tinteres)^(-cred$ncuotas))/(cred$tinteres)),cred$credito) 


summary(cred)

plot(cred$credito)


cred<-cred[,-c(10,21)]
summary(cred)
cred<-cred[,-c(25,26)]
cred<-cred[,-c(13)]
summary(cred)

dim(cred)	
cred_full<-cred

names(cred)

#####      ----------- ooo -- ooo ---------     #######
####Seleccion de variables Escalar-Escalar
number<-cred[,c(2,4,6,7,10,12,14,15,17,18,19,22,23,25,26)]
number <- number[, sapply(number, function(x) sd(na.omit(x)) > 0.001 | is.factor(x))]
M = cor(number,use = "complete.obs") #"complete.obs" excluye NA
png(filename = "mycorrplot.png", width=4000, height=3400,res=300)
corrplot(M, method = 'number') # colorful number
dev.off()
#relación positiva fuerte entre estrato y antiguedad_laboral, se puede elimina una de ellas
#relación positiva fuerte entre credito y cuota, se puede eliminar una
#relación positiva fuerte entre edad y antiguedad_laboral


####Seleccion de variables Escalar-Categorica
number<-cred[,c(6,7,10,12,14,15,17,18,19,22,23,25,26)]#redefinir sin NA
fisher_score <- function(escalar, interes) {
  media_no_interes <- mean(escalar[interes == 0])
  media_interes <- mean(escalar[interes == 1])
  # Desviaciones estándar
  sd_no_interes <- sd(escalar[interes == 0])
  sd_interes <- sd(escalar[interes == 1])
  # Cálculo Fisher Score
  fisher <- abs(media_no_interes - media_interes) / sqrt(sd_no_interes^2 + sd_interes^2)
  return(fisher)
}

scores<-sapply(number, fisher_score, interes=cred$eactual)
scores

#las variables con Fisher Score < 0,2 son; edad, pendeuda, credito, tinteres, ncuotas, cuota, capacidad, antiguedad_laboral, ingresosimp, estratoimp,
#teniendo en cuenta los resultados de la matriz de correlación y de la prueba Fisher Score se determina eliminar credito, y edad. 
#tambien se elimina ingresosimp ya su valor prediccivo es poco significativo
names(cred)
cred<-cred[,-c(6,12,25)]

summary(cred)
####Seleccion de variables Categorica-Categorica

iv<-cred[,c(3,5,7,8,10,11,14,18,19,22)]
iv$eactual <- as.numeric(as.character(iv$eactual))
IV <- create_infotables(data = iv, y = "eactual")
print(IV$Summary)
# el criterio establece eliminar IV<0,02, vamos a mantener las siguientes variables: nivel_educativo,yeardde, genero 
# tambien mantendermo co aún cuando no cumple con el criterio

names(cred)
cred<-cred[,-c(1,3,5,7,8,18)]

#resultados
summary(cred)
dim(cred)

#####      ----------- ooo -- ooo ---------     #######
####Construccion de modelos

#Separación de la base
names(cred)
cred_train<-sample_frac(cred[,-c(1,2)],0.75)
cred_test<-setdiff(cred[,-c(1,2)],cred_train)
dim(cred_train)
dim(cred_test)

#Modelo logistico con todas las varaibles
M1<-glm(eactual ~ .-eactual, data = cred_train, family = binomial(link="logit"))
summary(M1)


#Modelo logistico con menos varaibles
anova(M1,test="Chisq")
M2<-glm(eactual ~ genero+co+tinteres+tipo_vivienda+personas_hogar, data = cred_train, family = binomial(link="logit"))
summary(M2)

#Modelo logistico con genero y personas_hogar
M3<-glm(eactual ~ genero+personas_hogar, data = cred_train, family = binomial(link="logit"))
summary(M3)


#Otros modelo
#install.packages("caret")
#install.packages("Metrics")
#install.packages("randomForest")
#install.packages("e1071")
#install.packages("kernlab")
#install.packages("nnet")  # si no lo tienes
#install.packages("xgboost")


library(caret)
library(Metrics)
library(randomForest)
library(e1071)
library(kernlab)
library(nnet)
library(xgboost)


#Random Forests
M4 <- randomForest(eactual ~ ., data = cred_train, proximity=TRUE,classwt = c("0" = 1, "1" = 50))
print(M4)



#a partir de acá convertiremos las categoricas en numericas menos eactual
cred_train_num <- cred_train
cred_train_num$co <- as.numeric(cred_train$co)-1       
cred_train_num$genero <- as.numeric(cred_train$genero) - 1  
cred_train_num$tipo_vivienda <- as.numeric(cred_train$tipo_vivienda) - 1 


cred_test_num <- cred_test
cred_test_num$co <- as.numeric(cred_test$co)-1       
cred_test_num$genero <- as.numeric(cred_test$genero) - 1  
cred_test_num$tipo_vivienda <- as.numeric(cred_test$tipo_vivienda) - 1 
 

# Support Vector Machines
M5 <- svm(eactual ~ ., data = cred_train_num, scale = TRUE, kernel = "radial", cost = 10,probability = TRUE)
summary(M5)

M6 = ksvm(eactual ~ ., data = cred_train_num, scale = TRUE, kernel = "rbfdot",prob.model = TRUE)
summary(M6)

#a partir de acá convertiremos las categoricas en numericas incluida eactual
cred_train_num_2 <- cred_train
cred_train_num_2$co <- as.numeric(cred_train$co)-1       
cred_train_num_2$genero <- as.numeric(cred_train$genero) - 1  
cred_train_num_2$tipo_vivienda <- as.numeric(cred_train$tipo_vivienda) - 1 
cred_train_num_2$eactual <- as.numeric(cred_train$eactual) - 1 


cred_test_num_2 <- cred_test
cred_test_num_2$co <- as.numeric(cred_test$co)-1       
cred_test_num_2$genero <- as.numeric(cred_test$genero) - 1  
cred_test_num_2$tipo_vivienda <- as.numeric(cred_test$tipo_vivienda) - 1
cred_test_num_2$eactual <- as.numeric(cred_test$eactual) - 1  

#escalado train
x_train_num_2 <- cred_train_num_2[, setdiff(names(cred_train_num_2), "eactual")]
y_train_num_2 <- cred_train_num_2$eactual 
x_train_scaled <- as.data.frame(scale(x_train_num_2))
cred_train_scaled <- cbind(x_train_scaled, eactual = y_train_num_2)
summary(cred_train_scaled)

#escalado test
# Guardamos media y desviación de cada columna
train_means <- apply(x_train_num_2, 2, mean)
train_sds   <- apply(x_train_num_2, 2, sd)
x_test_num_2 <- cred_test_num_2[, setdiff(names(cred_test_num_2), "eactual")]
x_test_scaled <- as.data.frame(scale(x_test_num_2, center = train_means, scale = train_sds))
cred_test_scaled <- cbind(x_test_scaled, eactual = cred_test_num_2$eactual)

# RED
M7<- nnet(eactual ~ ., data = cred_train_scaled, size = 5, maxit = 500, decay = 0.01)
summary(M7)

#Xgboost
x_train <- as.matrix(cred_train_scaled[, setdiff(names(cred_train_scaled), "eactual")])
x_test  <- as.matrix(cred_test_scaled[, setdiff(names(cred_test_scaled), "eactual")])

y_train <- as.numeric(cred_train_scaled$eactual)
y_test  <- as.numeric(cred_test_scaled$eactual)

M8 <- xgboost(data = x_train, label = y_train, objective = "binary:logistic",eval_metric = "error",nrounds = 100, eta = 0.1,max_depth = 3,verbose = 1)

#####      ----------- ooo -- ooo ---------     #######
####Comparacion de modelos
pred1 <- predict(M1, cred_test, type = "response")
pred2 <- predict(M2, cred_test, type = "response")
pred3 <- predict(M3, cred_test, type = "response")
pred4 <- predict(M4, cred_test, type = "prob")[,2]
pred5 <- attr(predict(M5, cred_test_num, probability = TRUE), "probabilities")[,2]
pred6 <- predict(M6, cred_test_num, type = "probabilities")[,2]
pred7 <- as.numeric(predict(M7, cred_test_scaled))
pred8 <- predict(M8, x_test)

y_real <- cred_test$eactual

data.frame(
  Modelo = c("M1 GLM completo",
             "M2 GLM reducido", 
             "M3 GLM mínimo",
             "M4 Random Forest",
             "M5 SVM e1071",
             "M6 SVM kernlab",
             "M7 Red Neuronal",
             "M8 XGBoost"),
  AUC = round(c(
    auc(roc(y_real, pred1)),
    auc(roc(y_real, pred2)),
    auc(roc(y_real, pred3)),
    auc(roc(y_real, pred4)),
    auc(roc(y_real, pred5)),
    auc(roc(y_real, pred6)),
    auc(roc(y_real, pred7)),
    auc(roc(y_real, pred8))
  ), 4)
) %>% arrange(desc(AUC)) %>% print()

