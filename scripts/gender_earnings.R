rm(list = ls())
library(pacman)
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr,readxl)
set.seed(10101)

# Carga de información
path <- here()
setwd(path)
Data_gfg <- read_excel("./datos/geih_2018_v1-9-22.xlsx")
head(Data_gfg)

# Definición de X y Y
X = Data_gfg[, c('age')]
Y = Data_gfg$y_ingLab_m
dat = cbind(Y,X)
skim(dat)

# Regresión lineal sin escalar datos
mod <- lm("Y ~ age+ I(age^2)", data = dat)
summary(mod)


# Gráfica de distribución de datos
ggplot(dat, aes(y = Y, x = age)) +
  geom_point() +
  theme_bw() +
  labs(x = "Age", 
       y = "Earning",
       title = "Age-earnings profile distribution")

# caja y bigotes datos sin escalar
ggplot(data=dat , mapping = aes(age , Y)) + 
  geom_boxplot()

# Graficas distribución de datos
ggplot() + geom_histogram(data=dat , aes(x=age) , fill="#99FF33" , alpha=0.5)
ggplot() + geom_histogram(data=dat , aes(x=Y) , fill="#99FF33" , alpha=0.5)


# Ajuste del modelo en la media
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

mean_dat <- apply(select_if(dat, is.numeric), 2, mean)
mean_dat <- data.frame(t(mean_dat))
mode_dat <- apply(select_if(dat, is.factor), 2, Modes)
mode_dat <- data.frame(t(mode_dat))
mean_obs <- cbind(mode_dat, mean_dat)
mean_obs2 <- mean_obs[rep(1, 40),]
mean_obs2$temp <- seq(-5, 35, length = 40)
mean_obs2$y_hat <- predict(mod, mean_obs2)


# Gráfica regresión lineal
ggplot(dat, aes(y = Y, x = age)) +
  geom_point() +
  stat_smooth(formula = 'y ~ x + I(x^2)', method = lm, se = FALSE, 
              size = 1) +
  theme_bw() +
  labs(x = "Age", 
       y = "Earning",
       title = "Age-earnings profile linear regression")