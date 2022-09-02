library(rvest)
library(tidyverse)
library(data.table)


#Esta es la página inicial de Ignacio. Debería ser la única url que se le provea al scrapper
#https://ignaciomsarmiento.github.io/GEIH2018_sample

#Esta es una de las páginas que no cargan, que contiene el dataset
url_1="https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html"
html_1=read_html(url_1)


#Esta es la forma de extraer el complemento de la url
tabla_1_url <- html_1 %>% 
  html_elements("div") %>% 
  html_attr("w3-include-html")
tabla_1_url[8]


#Una vez se pegue el complemento, esta es la página que contiene la tabla
url_1="https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
html_1=read_html(url_1)
tabla_1 <- html_1 %>% html_table()
tabla_1[[1]]


#Esta es el código que lee las tablas.
geih <- tibble()

for(i in 1:10){

  url=paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  html=read_html(url)
  table_part <- html %>% html_table()
  geih <- geih %>% bind_rows(table_part)
  
}




#VARIABLES REQUERIDAS.

#SALARIO:
#y_salary_m: contiene solo salario de primera ocupacion
#y_ingLab_m: contiene viaticos, auxilio de transporte, auxilio de alimentos y salario de todas las ocupaciones
#(algunas casillas de los viaticos etc, estan anuales)
#INGRESO:
#ingtot (contiene suma de variables incluyendo valor imputado)
#EDAD:
#age
#GENERO:
#sex


#¿Qué compone la variable ingtot?
geih_2 %>% select(id_tabla,y_total_m,ingtot,impa,isa,ie,imdi,iof1,iof2,iof3h,iof3i,iof6,
                iof1es,iof2es,iof3hes,iof3ies,iof6es,impaes,isaes,iees,imdies) %>%
  mutate(suma=rowSums(across(c(impa,isa,ie,imdi,iof1,iof2,iof3h,iof3i,iof6,iof1es,
                               iof2es,iof3hes,iof3ies,iof6es,impaes,isaes,iees,imdies)),na.rm=T) %>% round(.,1),
         ingtot=ingtot %>% round(.,1),
         suma_ing=suma==ingtot
         
         ) %>%  
  view


geih_2$y_gan

geih_2 %>% select(id_tabla,y_salary_m,y_viaticos_m,y_ingLab_m,y_auxilioTransp_m,y_gananciaIndep_m,ingtot,impa) %>% 
#  filter(id_tabla%in%c(12860)) %>%
#  mutate(resta=y_ingLab_m-y_salary_m) %>% 
  view


#geih_2 %>% select(id_tabla,contains("y")) %>% view

#geih_2 %>% filter(id_tabla%in%c(12860)) %>% view
#geih_2 %>% filter(id_tabla%in%c(2217)) %>% view



