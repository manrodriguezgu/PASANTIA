######      ------------ ooo -- ooo ---------     #######
###                    CASH  -- BLOCK
###                 Credit Score Script
###                       July 03
###                  Initial Learning
###                     ooo -- ooo
rm(list=ls())
######      ----------- ooo -- ooo ---------     #######
## Fixing set working directory
setwd("D:/CEAM/2025/Semestre I/Advising/FinTech/Datasets")
#### Some Libraries
library(ggplot2)# Plotting
#install.packages("VIM")       # kNN imputation
library(VIM)
library(dplyr)# data manipulation
library(cluster)
library(FactoMineR)
library(factoextra)
# --- Reading
cred<-read.csv("Credito.csv", header = TRUE, na.strings=-999.)
cred[1:2,]
dim(cred)
table(cred$genero)
table(cred$ocupacion)
table(cred$calificacion)
## Total Observa
tapply(cred$dtscore, list(cred$calificacion),  length)
## Total Non-missing Observations
table(!is.na(cred$dtscore), cred$calificacion)
## Mean dtscore Non-missing - Observations
tapply(cred$dtscore, cred$calificacion, mean, na.rm=TRUE)
## Standard deviation of dtscore Non-missing - Observations
tapply(cred$dtscore, cred$calificacion, sd, na.rm=TRUE)

## -- Missing Value Effect
names(cred)
tapply(cred$dtscore, cred$genero, mean, na.rm=TRUE)
tapply(cred$dtscore, cred$genero, sd, na.rm=TRUE)
tapply(cred$ingresos, cred$genero, sd, na.rm=TRUE)
###---
tapply(cred$credito, cred$genero, mean, na.rm=TRUE)
tapply(cred$credito, cred$calificacion, mean, na.rm=TRUE)

## Transforming Variables
cred$Gender<- recode(cred$genero,FEMENINO = 1, MASCULINO = 0)
table(cred$genero, cred$Gender)
summary(cred$Gender)
cred$Cali<- recode(cred$calificacion,B1 = 0, C1 = 1)
table(cred$Cali, cred$calificacion)

## --    ### -- ### -------------
##-- Imputation : KNN algorithm
pred <- c("dtscore", "edad","Gender", "ingresos", "Cali", "pendeuda", "credito")# Select predictors, variables likely related to dtscore, e.g., edad, ingresos, calificacion, etc.
# Subset the data to these columns
cred_se <- cred[, pred]
names(cred_se)
summary(cred_se)
##is.na(cred_se$pendeuda) ## Just checking
##is.na(cred_se$dtscore) ## Just checking

# Now, identifying na's position
uv<-c(which(is.na(cred_se$dtscore), arr.ind=TRUE), which(is.na(cred_se$pendeuda), arr.ind=FALSE))
### ---- Setting k-value in kNN
## We have to delete missing value to determine k- of kNN
tem <- scale(cred_se[-uv,]) # Standardize the variables
dim(tem)
### Matrix of distance
cdistan<- dist(tem, method = "euclidean")## method=manhattan, sorensen, bray-curtis, gower
## Estimating the optimal k "silhouette"= average silhouette width, "wss"=for total within sum of square
fviz_nbclust(tem, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

fviz_nbclust(tem, kmeans, method = "silhouette", k.max=12) +
  geom_vline(xintercept = 8, linetype = 2)

# Perform kNN imputation only on the subset, imputing missing dtscore values
# imp_var=FALSE avoids creating extra columns indicating imputation
cred_iset <- kNN(cred_se, variable = "dtscore", k = 5, imp_var = FALSE)

# Replace the original dtscore in cred with the imputed values
cred$dtscoreimp <- cred_iset$dtscore
names(cred)
cred[,c(2,32)]# Comparing
sd(cred[,2], na.rm=TRUE); sd(cred[,32])# Variance has decreased, problems
# Check that missing values in dtscore are imputed
summary(cred$dtscore)
summary(cred$dtscoreimp)


