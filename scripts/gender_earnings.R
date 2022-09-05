rm(list = ls())

library(pacman)
library(scales)
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr,readxl)
set.seed(10101)

# 1. Carga de información----------------------------------------------------------------
path <- here()
setwd(path)
Data_gfg <- read_excel("./datos/geih_2018_v1-9-22.xlsx")
head(Data_gfg$sex)
View(Data_gfg)

# 2. Definición de X y Y------------------------------------------------------------------

# 2.1. Validacion de outliers

X_Nulos=sum(is.na(df$ingtot)) #validacion de nulos en variable objetivo

X_cero=Data_gfg[Data_gfg$ingtot==0,]$female #validacion de nulos en variable objetivo
df<-Data_gfg[Data_gfg$ingtot!=0,]

g_caja<-boxplot(df$ingtot, col="skyblue", frame.plot=F)
df<-df[!(df$ingtot %in% g_caja$out),] # eliminacion de outliers
g_caja<-boxplot(df$ingtot, col="skyblue", frame.plot=F) # Datos ajasutados

X_inicial =Data_gfg$female
X =as.factor(df$female)
V1 = df[, c('age')]
Y = df$ingtot+1 #re escalando los datos
dat = as.data.frame(cbind(Y,X))
dat2 = as.data.frame(cbind(Y,X,V1))
skim(dat2)
glimpse(dat2)

#View(dat)

# 3. Analisis exploratorio de datos------------------------------------------------------------------


# 3.1 Grafico de distribución de Genero con ingreso mayor a 0
etiquetas <- paste0(c('Masculino','Femenino'), " = ", round(100 * table(X_inicial)/sum(table(X_inicial)), 2), "%")
pie(table(X_inicial), labels = etiquetas,clockwise = TRUE, main = 'Distribución de genero inicial')

# 3.1 Grafico de distribución de Genero con ingreso mayor a 0
etiquetas <- paste0(c('Masculino','Femenino'), " = ", round(100 * table(X)/sum(table(X)), 2), "%")
pie(table(X), labels = etiquetas,clockwise = TRUE, main = 'Distribución de genero sin ingrso 0')

# 3.1 Grafico de distribución de Genero ingreso 0
etiquetas <- paste0(c('Masculino','Femenino'), " = ", round(100 * table(X_cero)/sum(table(X_cero)), 2), "%")
pie(table(X_cero), labels = etiquetas,clockwise = TRUE, main = 'Distribución de genero (Ingreso 0)')

# 3.2 Grafico de distribución de edades

# 3.3 Correlación de edad vs ingresos



# 4. Regresión Lineal de ingreso por genero---------------------------------------------------------

# 4.1 Regresión lineal con log
mod <- lm("I(log(Y)) ~ X", data = dat)
summary(mod)
tidy(mod)
summ(mod, Robust='HC1')

mean(df$ingtot)

# 4.2 Regresión lineal

mod <- lm("Y ~ X", data = dat)
summary(mod)
tidy(mod)
summ(mod, Robust='HC1')


# 4.2. Analisis de las variables---------------------------------------------------------------------


# 5. Regresión Lineal de ingreso por genero utilizando edades y genero----------------------------------


# 5.1 Regresión lineal

mod <- lm("I(log(Y)) ~ X + age", data = dat2)
summary(mod)
tidy(mod)
summ(mod, Robust='HC1')


# 5.2. Analisis de las variables---------------------------------------------------------------------


# 6. Regresión Lineal incluyendo variables de trabajos similares----------------------------------

# 6.1 Regresión lineal
mod <- lm("Y ~ X", data = dat)
summary(mod)

# 6.2. Analisis de las variables---------------------------------------------------------------------

# 6.3. modelo con ajustE FLW---------------------------------------------------------------------

