
# LIBRERIAS ---------------------------------------------------------------

rm(list = ls())

library(pacman)
library(scales)
library(sandwich)
library(fastDummies)
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr,readxl,here)
set.seed(10101)

# 1. Carga de información----------------------------------------------------------------
Data_gfg <- read_excel(here("stores","geih_final.xlsx"))
head(Data_gfg$sex)
#View(Data_gfg)

# 2. Definición de X y Y------------------------------------------------------------------

# 2.1. Validacion de outliers

X_Nulos=sum(is.na(Data_gfg$ingtot)) #validacion de nulos en variable objetivo

X_cero=Data_gfg[Data_gfg$ingtot==0,]$female #validacion de nulos en variable objetivo
df<-Data_gfg[Data_gfg$ingtot!=0,]

g_caja<-boxplot(df$ingtot, col="skyblue", frame.plot=F)
df<-df[!(df$ingtot %in% g_caja$out),] # eliminacion de outliers
g_caja<-boxplot(df$ingtot, col="skyblue", frame.plot=F) # Datos ajasutados

X_inicial =Data_gfg$female
X =as.factor(df$female)
V1 = df[, c('age')]
W = df[, c('oficio')]
Y = df$ingtot+1 #re escalando los datos
dat = as.data.frame(cbind(Y,X))
dat2 = as.data.frame(cbind(Y,X,V1))
dat3 = as.data.frame(cbind(Y,X,V1,W))
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


plot_summs(mod, colors = "black", rescale.distributions = TRUE,
           plot.distributions = TRUE, robust = TRUE)


mean_dat <- apply(select_if(dat2, is.numeric), 2, mean)
mean_dat <- data.frame(t(mean_dat))
mode_dat <- apply(select_if(dat2, is.factor), 2,mode)
mode_dat <- data.frame(t(mode_dat))

mean_obs <- cbind(mode_dat, mean_dat)
mean_obs2 <- mean_obs[rep(1, 40),]

mean_obs2$temp <- seq(-5, 35, length = 40)
#mean_obs2$y_hat <- predict(mod, mean_obs2)


ggplot(dat2, aes(y = Y, x = age)) +
  geom_point() +
  geom_line(data = mean_obs2, aes(x = age, y = Y, 
                                  color = "with controls"), size = 1) +
  stat_smooth(formula = 'y ~ x', method = lm, se = FALSE, 
              aes(color = "without controls"), 
              size = 1) +
  theme_bw() +
  labs(x = "Temperature in Celsius", 
       y = "Number of bicycles rented",
       title = "Predicted values with changes in temperature") +
  scale_color_manual(name = "Model", values = c("red", "blue"))



# 6. Regresión Lineal incluyendo variables de trabajos similares----------------------------------

# 6.1 Generación de las variables dummies


dat3=dat3[!is.na(dat3$oficio),]

dat4 <- dummy_cols(dat3, select_columns = 'oficio')
dat4=dat4[,-4]


# 6.1 Regresión lineal
mod <- lm("I(log(Y)) ~ .", data = dat4)
summary(mod)

# 6.2. Analisis de las variables---------------------------------------------------------------------

skim(dat2)
glimpse(dat4)

# 6.3. modelo con ajustE FLW---------------------------------------------------------------------

