# Problem Set 1: Predicting Income
Desarrollado por John Daniel Delgado Vargas, José Julian Parra Montoya y Alison Gissell Ruiz Ruiz.

### Introducción

El objetivo de este modelo es entender las principales variables relevantes que permiten predecir el ingreso de la población, lo que facilita proponer nuevas estrategias de política pública para mejorar las condiciones de una sociedad. Se utiliza La Gran Encuesta Integrada de Hogares (GEIH) realizada en 2018. Para el analisis se restringe a individuos mayores de 18 años. 

El análisis se desarrolla en 5 etapas:

* Web scraping: obtención de los datos del link https://ignaciomsarmiento.github.io/GEIH2018_sample/
* Limpieza y entendimiento de datos
* Análisis de las edades
* Analisis de Genero
* Construcción del modelo de predicción.

### Tabla de contenido
-  [Install](#install)
-  [Data](#data)
-  [Scripts](#scripts)
-  [Informe](#informe)

### Install

Este proyecto requiere R y las siguientes librerias instaladas

* library(pacman)
* library(ggplot2)#graficar
* library(tidyverse)#organizar datos
* library(xtable)#tablas
* library(openxlsx)#lectura xlsx
* library(janitor)#tabla de frecuencia
* library(data.table)#manejo de dataframes
* library(kableExtra)#tablas
* library(haven)#lectura de dta
* library(scales)#graficos
* library(boot)#bootstrap

Para instalarlas se debe correr el comando install.packages, a continuación un ejemplo de esto.

```bash
install.packages("sandwich")
```

### Data

En la carpeta [`stores`](https://github.com/AlisonRuiz/Predicting-Income/tree/main/stores) se encuentra el set de datos en excel procesado por los siguienets scripts

* web_scrapping.R
* funcion_descriptivas.R

La Descipción de las variables se puede encontrar en:  https://www.dane.gov.co/index.php/estadisticas-por-tema/mercado-laboral/empleo-y-desempleo/geih-historicos


### Scripts

El proyecto cuenta con los siguientes scripts de R:

* [`web_scrapping.R`](https://github.com/AlisonRuiz/Predicting-Income/blob/main/scripts/web_scrapping.R)
* [`funcion_descriptivas.R`](https://github.com/AlisonRuiz/Predicting-Income/blob/main/scripts/funcion_descriptivas.R)
* [`funcion_diferencia_medias.R`](https://github.com/AlisonRuiz/Predicting-Income/blob/main/scripts/funcion_diferencia_medias.R)
* [`age_earnings_profile.R`](https://github.com/AlisonRuiz/Predicting-Income/blob/main/scripts/age_earnings_profile.R)
* [`analisis_descriptivo.R`](https://github.com/AlisonRuiz/Predicting-Income/blob/main/scripts/analisis_descriptivo.R)
* [`gender_earnings.R`](https://github.com/AlisonRuiz/Predicting-Income/blob/main/scripts/gender_earnings.R)

### Informe

El informe se encuentra en la carpeta [`document`](https://github.com/AlisonRuiz/Predicting-Income/blob/main/document/solucion_taller_1.tex) se encuentra en formato .tex y .pdf. En este archivo se resumen los resultados y se explica su interpretación.
