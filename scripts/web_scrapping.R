
# LIBRERIAS ---------------------------------------------------------------

library(rvest)
library(tidyverse)
library(data.table)


# LOOP --------------------------------------------------------------------

#Esta es la página inicial del prof. Ignacio. Debería ser la única url que se le provea al scrapper.
url_home <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

#Leer la página, tomar las etiquetas "a" que contienen enlaces y traer los atributos que tengan un hipervinculo.
urls_tables_page <- read_html(url_home) %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  grep("page",.,value=T)


urls_tablas <- list()
#Iterar sobre los enlaces que nos da la página de Ignacioy extraer los enlaces de las tablas.
for (i in 1:length(urls_tables_page)){
  
  #Url de la página donde está embebida la tabla
  url_table_page <- paste0(url_home,urls_tables_page[[i]])
  #se extrae parte de la Url de la ubicación de la tabla
  html_table_page <- read_html(url_table_page)
  urls_tablas[i] <- html_table_page %>% 
    html_elements("div") %>% 
    html_attr("w3-include-html") %>% 
    grep("page",.,value=T)
  
  
}

#Inicializar tibble vacio.
geih <- tibble()
#Leer las tablas embebidas en las páginas a las que lleva la página principal
for (i in 1:length(urls_tablas)){
  #url de la tabla
  url_table <- paste0(url_home,urls_tablas[[i]])
  html_tabla <- read_html(url_table)
  #tomar trozo de la tabla
  table_part <- html_tabla %>% html_table()
  #pegar a la totalidad
  geih <- geih %>% bind_rows(table_part)
  
  print(i)
}

