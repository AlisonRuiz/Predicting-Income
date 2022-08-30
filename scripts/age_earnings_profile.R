rm(list = ls())
library(pacman)
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr, readxl)
set.seed(10101)

# Carga de información
Data_gfg <- read_excel("C:/Users/aliso/Downloads/geih_clean_BDML.xlsx")
head(Data_gfg)

# Definición de X y Y
X = Data_gfg[, c('age')]
Y = Data_gfg$y_ingLab_m
dat = cbind(Y,X)
skim(dat)

# Regresión lineal
mod <- lm("Y ~ age+ I(age^2)",        data = dat)
summary(mod)

# Gráfica de distribución de datos
ggplot(dat, aes(y = Y, x = age)) +
  geom_point() +
  theme_bw() +
  labs(x = "Temperature in Celsius", 
       y = "Number of bicycles rented",
       title = "Predicted values with changes in temperature")