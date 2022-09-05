
# LIBRERIAS ---------------------------------------------------------------

rm(list=ls())

library(ggplot2)#graficar
library(tidyverse)#organizar datos
library(xtable)#tablas
library(openxlsx)#lectura xlsx
library(janitor)#tabla de frecuencia
library(here)

# SOURCE SCRIPT DE FUNCIONES ----------------------------------------------


source("funcion_descriptivas.R")
source("funcion_diferencia_medias.R")


# LECTURA DE DATOS --------------------------------------------------------


geih <-read.xlsx(here("stores","geih.xlsx"))

#Filtrado por edad y creacion de la variable female
geih_2 <- geih %>% 
  filter(age>=18) %>% 
  mutate(female=ifelse(sex==0,1,0)) %>% 
  rowid_to_column("id_tabla") %>% 
  select(-...1) 



# COMPOSICIÓN DE LAS VARIABLES ----------------------------------------------------

# Ingtot ---------------------------------------------------

#¿Qué compone la variable ingtot?
geih_2 %>% select(id_tabla,y_total_m,ingtot,impa,isa,ie,imdi,iof1,iof2,iof3h,iof3i,iof6,
                  iof1es,iof2es,iof3hes,iof3ies,iof6es,impaes,isaes,iees,imdies) %>%
  #crear una nueva variable que sume atraves de las filas de estas variables para verificar si la suma es igual a ingtot
  mutate(suma=rowSums(across(c(impa,isa,ie,imdi,iof1,iof2,iof3h,iof3i,iof6,iof1es,
                               iof2es,iof3hes,iof3ies,iof6es,impaes,isaes,iees,imdies)),na.rm=T) %>% round(.,1),
         ingtot=ingtot %>% round(.,1),
         suma_ing=suma==ingtot
         
  ) %>%  
  filter(!(suma-ingtot<abs(1)))

#Por tanto está compuesta de estas variables.


# y_ingLab_m --------------------------------------------------------------

#comparando impa cuando isa=0 y y_ingLab_m no es NA.
#Se concluye que impa es y_ingLab_m=impa+isa-aux alimento mensual- aux accidentes mensual
geih_2 %>% 
  filter((isa==0|is.na(isa))&(!is.na(y_ingLab_m))) %>% 
  #Se debe usar case_when porque sumar cuando hay nas produce nas. Entonces se debe cuidar caso por caso.
  mutate(impa_menosauxalim=
           case_when(
             
             impa>0&!is.na(impa)&!is.na(y_auxilioAliment_m)&!is.na(y_accidentes_m)~(impa-y_auxilioAliment_m-(y_accidentes_m/12)) %>% round(.,0),
             impa>0&!is.na(impa)&is.na(y_auxilioAliment_m)&!is.na(y_accidentes_m)~(impa-y_auxilioAliment_m-(y_accidentes_m/12)) %>% round(.,0),
             impa>0&!is.na(impa)&!is.na(y_auxilioAliment_m)&is.na(y_accidentes_m)~(impa-y_auxilioAliment_m) %>% round(.,0),
             is.na(y_auxilioAliment_m)~round(impa,0),
             TRUE~0
             
           ),
         
         y_ingLab_m=round(y_ingLab_m,0),
         impa=round(impa,0)
         
  ) %>% 
  select(id_tabla,impa,impa_menosauxalim,y_ingLab_m,isa,y_auxilioAliment_m,y_accidentes_m) %>% 
  #se verifica si la suma es consistente omitiendo errores de redondeo
  
  filter(!(impa_menosauxalim-y_ingLab_m<=abs(1))) 

#cuando isa>0
# se verifica que cuando isa>0 e y_ingLab_m no es NA, entonces y_ingLab=impa+isa-aux alimento mensual- aux accidentes mensual
geih_2 %>% 
  filter(isa>0&!is.na(isa)&(!is.na(y_ingLab_m))) %>% 
  mutate(impa_isa_menos_auxalim_acc=
           case_when(
             
             impa>0&!is.na(impa)&!is.na(y_auxilioAliment_m)&!is.na(y_accidentes_m)~(isa+impa-y_auxilioAliment_m-(y_accidentes_m/12)) %>% round(.,0),
             impa>0&!is.na(impa)&is.na(y_auxilioAliment_m)&!is.na(y_accidentes_m)~(isa+impa-y_auxilioAliment_m-(y_accidentes_m/12)) %>% round(.,0),
             impa>0&!is.na(impa)&!is.na(y_auxilioAliment_m)&is.na(y_accidentes_m)~(isa+impa-y_auxilioAliment_m) %>% round(.,0),
             is.na(y_auxilioAliment_m)~round(isa+impa,0),
             TRUE~isa
             
           ),
         
         y_ingLab_m=round(y_ingLab_m,0),
         impa=round(impa,0)
         
  ) %>% 
  select(id_tabla,impa,isa,impa_isa_menos_auxalim_acc,y_ingLab_m,y_auxilioAliment_m,y_accidentes_m,y_salary_m,p6500) %>% 
  #se verifica si la suma es consistente omitiendo errores de redondeo
  
  filter(!(impa_isa_menos_auxalim_acc-y_ingLab_m<=abs(1)))


#cuando no hay ganancia de independientes ( no todos los cuenta propia generan ganancia, algunos tienen ingreso laboral), 
#entonces se pueden utilizar impa, impaes, isa, isaes para imputar y_ingLab_m
geih_2 %>% 
  filter(is.na(y_ingLab_m)&!is.na(y_gananciaIndep_m)) %>% 
  filter(!is.na(impa)|!is.na(impaes)) %>% 
  select(id_tabla,impa,isa,impaes,isaes,y_ingLab_m,
         y_auxilioAliment_m,y_accidentes_m,y_salary_m,p6500,ends_with("_m"),cuentaPropia)


#confirmacion: crear una variable ing_lab que contiene la suma de todas las variables anteriores cuidando cuando alguna sea na.
geih_2 %>% 
  filter(!is.na(y_ingLab_m)) %>% 
  mutate(
    #se construye una variable que suma las variables previamente encontradas omitiendo los faltantes en cada caso.
    ing_lab=
      case_when(
        
        !is.na(isa)&!is.na(impa)&!is.na(y_auxilioAliment_m)&!is.na(y_accidentes_m)&is.na(y_gananciaIndep_m)~(isa+impa-y_auxilioAliment_m-(y_accidentes_m/12)) %>% round(.,0),
        !is.na(isa)&!is.na(impa)&!is.na(y_auxilioAliment_m)&!is.na(y_accidentes_m)&!is.na(y_gananciaIndep_m)~(isa+impa-y_auxilioAliment_m-(y_accidentes_m/12)-y_gananciaIndep_m) %>% round(.,0),
        
        !is.na(isa)&!is.na(impa)&is.na(y_auxilioAliment_m)&!is.na(y_accidentes_m)&is.na(y_gananciaIndep_m)~(isa+impa-y_auxilioAliment_m-(y_accidentes_m/12)) %>% round(.,0),
        !is.na(isa)&!is.na(impa)&is.na(y_auxilioAliment_m)&!is.na(y_accidentes_m)&!is.na(y_gananciaIndep_m)~(isa+impa-y_auxilioAliment_m-(y_accidentes_m/12)-y_gananciaIndep_m) %>% round(.,0),
        
        
        !is.na(isa)&!is.na(impa)&!is.na(y_auxilioAliment_m)&is.na(y_accidentes_m)&is.na(y_gananciaIndep_m)~(isa+impa-y_auxilioAliment_m) %>% round(.,0),
        !is.na(isa)&!is.na(impa)&!is.na(y_auxilioAliment_m)&is.na(y_accidentes_m)&!is.na(y_gananciaIndep_m)~(isa+impa-y_auxilioAliment_m-y_gananciaIndep_m) %>% round(.,0),
        
        
        !is.na(isa)&!is.na(impa)&is.na(y_auxilioAliment_m)&is.na(y_accidentes_m)&is.na(y_gananciaIndep_m)~round(isa+impa,0),
        !is.na(isa)&!is.na(impa)&is.na(y_auxilioAliment_m)&is.na(y_accidentes_m)&!is.na(y_gananciaIndep_m)~round(isa+impa-y_gananciaIndep_m,0),
        
        is.na(isa)&is.na(y_auxilioAliment_m)&is.na(y_accidentes_m)&is.na(y_gananciaIndep_m)~round(impa,0),
        is.na(isa)&is.na(y_auxilioAliment_m)&is.na(y_accidentes_m)&!is.na(y_gananciaIndep_m)~round(impa-y_gananciaIndep_m,0),
        
        
        TRUE~0
        
      )) %>%
  #se verifica si la suma es consistente omitiendo errores de redondeo
  filter(!(y_ingLab_m-ing_lab<=abs(1))) %>% view

#Quedan solo 7 filas aparentemente inconsistentes porque tienen ingreso laboral pero no reportan ocupación primaria. Como esto es menos del 0.01% de los datos
#se puede afirmar que la construccion es correcta.

#Nueva versión de los datos, utilizando ingresos imputados previamente.

geih_3 <- geih_2 %>% 
  mutate(
    impa_imptd=ifelse(is.na(impa)|impa==0,impaes,impa),
    isa_imptd=ifelse(is.na(isa)|isa==0,isaes,isa)
    
    
    
  ) %>% 
  
  rowwise() %>% 
  
  mutate(
    
    sum_impaisa=sum(impa_imptd,isa_imptd,na.rm=T),
    
    y_ingLab_m_2=ifelse(is.na(y_ingLab_m)&(is.na(y_gananciaIndep_m)|y_gananciaIndep_m==0),
                        sum_impaisa,y_ingLab_m)
  )




# college ----------------------------------------

#La variable college está construida de forma tal que es 1 si los individuos adquirieron educación secundaria.
#no refleja una escolaridad universitaria.

geih_3 %>% 
  filter(college==1) %>% 
  select(p6210,maxEducLevel) %>% table()

#Según el diccionario de variables maxEducLevel==6 consiste en "secondary complete" y la variable
#p6210==5 consiste en "Media". Incluso si estos individuos fueran estudiantes, la variable deberia 
#incluir individuos que ya realizaron sus estudios universitarios por lo que deberíamos observar 
#también las categorías 6 y 7 de ambas variables.


geih_4 <- geih_3 %>% mutate(
  college_2=ifelse(p6210%in%6|maxEducLevel%in%7,1,0)
  
)

# TABLAS ------------------------------------------------------------------



#Función para crear la moda

getmode <- function(v) {
  uniqv <- unique(v)[!is.na(unique(v))]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Función para tablas de faltantes
na_table <- function(x){
  
  x=as.numeric(x)
  missing=sum(is.na(x))
  total=length(x)
  return(c(missing,total))
  
}

#Función para descriptivas de variables dependientes
descriptives_table_dependent <- function(x){
  x=as.numeric(x)
  mean=mean(x,na.rm=T)
  median=median(x,na.rm=T)
  sd=sd(x,na.rm=T)
  pct90=quantile(x,0.9,na.rm=T)
  return(c(mean,median,sd,pct90))
  
}

#Función para descriptivas de variables independientes
descriptives_table_independent <- function(x){
  x=as.numeric(x)
  mean=mean(x,na.rm=T)
  median=median(x,na.rm=T)
  mode=getmode(x)
  sd=sd(x,na.rm=T)
  return(c(mean,median,mode,sd))
  
}



#Creación de la tabla de faltantes. Output es código latex
createDescriptiveTable(list(geih_4),
                       summary_function = na_table,
                       column_names=c("Faltantes","Total"),
                       variable_names=list(c("ingtot","y_ingLab_m","y_ingLab_m_2")),
                       variable_labels=list(c("Ingreso total","Ingreso laboral","Ingreso Laboral Imputado")),
                       arraystretch=1.3,
                       title="Datos faltantes",
                       label="tab:missing",
)

#Creación de la tabla de de descriptivas Output es código latex
createDescriptiveTable(list(geih_4),
                       summary_function = descriptives_table_dependent,
                       column_names=c("Media","Mediana","D.E.","Percentil 90"),
                       variable_names=list(c("ingtot","y_ingLab_m","y_ingLab_m_2")),
                       variable_labels=list(c("Ingreso total","Ingreso laboral","Ingreso Laboral imputado")),
                       arraystretch=1.3,
                       title="Estadísticas descriptivas variables dependientes",
                       label="tab:descriptive_dependent",
)

#Creación de la tabla de de descriptivas Output es código latex
createDescriptiveTable(list(geih_4),
                       summary_function = descriptives_table_independent,
                       column_names=c("Media","Mediana","Moda","D.E."),
                       variable_names=list(c("age","female","college_2","cuentaPropia","formal","oficio")),
                       variable_labels=list(c("Edad","Mujer","Universidad","Cuenta Propia","Formal","Oficio")),
                       arraystretch=1.3,
                       title="Estadísticas descriptivas variables independientes",
                       label="tab:descriptive_independent",
)

#Creación de la tabla de de descriptivas Output es código latex

createDescriptiveTable(list(geih_4),
                       summary_function = descriptives_mean_dif,
                       column_names=c("Media"),
                       variable_names=list(c("ingtot","y_ingLab_m_2")),
                       variable_labels=list(c("Ingreso total","Ingreso laboral")),
                       arraystretch=1.3,
                       group_variable = "female",
                       title="Medias por grupo",
                       label="tab:mean_group",
)

#Creación de la tabla de diferencia de medias. Output es código latex

tabla_diferencia_medias(geih_4,
                        c("Mujer","Universitario"),
                        c("Ingreso total","Ingreso laboral"),
                        c("ingtot","y_ingLab_m_2"),
                        c("female","college_2")
                        
                        )

#Creación de la tabla de diferencia de medias. Output es código latex

tabla_diferencia_medias(geih_4,
                        c("Cuenta Propia","Informal"),
                        c("Ingreso total","Ingreso laboral"),
                        c("ingtot","y_ingLab_m_2"),
                        c("cuentaPropia","informal")
                        
)




#Para crear la tabla de frecuencia de oficios se usa la funcion tabyl del paquete janitor y esto se convierte a codigo
#latex usando la funcion kable


geih_3 %>% 
  filter(!is.na(oficio)) %>% 
  select(oficio) %>% 
  tabyl(oficio) %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10) %>% kable("latex")


# Graficos ----------------------------------------------------------------

#Agrupacion de edades por itnervalos

dist_ingresos <- geih_4 %>% 
  select(valor=y_ingLab_m_2,age) %>% 
  mutate(ingreso="Ingreso Laboral") %>% 
  bind_rows(
    tibble(valor=geih_4$ingtot,ingreso="Ingreso Total",age=geih_4$age)) %>% 
  filter(!is.na(age)&!is.na(valor)) %>%    
  mutate(
    age_interval=case_when(
      age>=18&age<30~"18-30",
      age>=30&age<45~"30-45",
      age>=45&age<60~"45-60",
      age>=60&age<75~"60-75",
      age>=75&age<90~"75-90",
      age>=90&age<110~"90-110",
      TRUE~""))
  

#Creacion de tabla de frecuencias por ingreso
dist_ingresos %>% 
  ggplot(., aes(x=age_interval, y=valor))+
  #geometria
  geom_bar(stat='summary',fun="mean", fill="steelblue")+
  #division de gráficos por tipo de ingreso
  facet_wrap(~ingreso)+
  xlab("")+
  ylab("Ingreso promedio")+
  #Opciones estéticas, tabla minimalista
  theme_bw()+
  theme(text=element_text(size=14, color="black"),
        panel.grid=element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position=c(0.85,0.85),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face="bold"))

write.xlsx(geih_4,"geih_final.xlsx")
