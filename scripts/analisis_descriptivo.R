# Analisis descriptivo ----------------------------------------------------


source("funcion_descriptivas.R")


geih_3 <- geih_2 %>% 
  filter(age>18) %>% 
  mutate(female=ifelse(sex==0,1,0)) %>% 
  select(-...1)


#comparando impa cuando isa=0 y y_ingLab_m no es NA.
#Se concluye que impa es y_ingLab_m=impa+isa-aux alimento mensual- aux accidentes mensual
geih_3 %>% 
  filter((isa==0|is.na(isa))&(!is.na(y_ingLab_m))) %>% 
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
  filter(!(impa_menosauxalim-y_ingLab_m<=abs(1))) %>% 
  view

#cuando isa>0
# se verifica que cuando isa>0 e y_ingLab_m no es NA, entonces y_ingLab=impa+isa-aux alimento mensual- aux accidentes mensual
geih_3 %>% 
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
  filter(!(impa_isa_menos_auxalim_acc-y_ingLab_m<=abs(1))) %>% 
  view


#cuando no hay ganancia de independientes ( no todos los cuenta propia generan ganancia, algunos tienen ingreso laboral), 
#entonces se pueden utilizar impa, impaes, isa, isaes para imputar y_ingLab_m
geih_3 %>% 
  filter(is.na(y_ingLab_m)&!is.na(y_gananciaIndep_m)) %>% 
  filter(!is.na(impa)|!is.na(impaes)) %>% 
  select(id_tabla,impa,isa,impaes,isaes,y_ingLab_m,
         y_auxilioAliment_m,y_accidentes_m,y_salary_m,p6500,ends_with("_m"),cuentaPropia) %>% view

geih_3 %>% 
  filter(is.na(y_ingLab_m)&!is.na(y_gananciaIndep_m)&cuentaPropia==1) %>%
  filter(y_gananciaNeta_m!=y_gananciaIndep_m) %>% 
  view


#confirmacion
geih_3 %>% 
  filter(!is.na(y_ingLab_m)) %>% 
  mutate(
    
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
        
      ),
    
    ing_lab_equal_y_inglab_m=y_ingLab_m-ing_lab<=abs(1),
    
    
  ) %>% 
  select(id_tabla,ing_lab,ing_lab_equal_y_inglab_m,impa,isa,impaes,isaes,y_ingLab_m,
         y_auxilioAliment_m,y_accidentes_m,y_salary_m,p6500,ends_with("_m")) %>% view





#Nueva versión de los datos.

geih_4 <- geih_3 %>% 
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


library(openxlsx)
write.xlsx(geih_4,"geih_2018_v1-9-22.xlsx")

dna_table <- function(x){
  
  x=as.numeric(x)
  missing=sum(is.na(x))
  total=length(x)
  return(c(missing,total))
  
}

descriptives_table <- function(x){
  x=as.numeric(x)
  mean=mean(x,na.rm=T)
  median=median(x,na.rm=T)
  sd=sd(x,na.rm=T)
  pct90=quantile(x,0.9,na.rm=T)
  return(c(mean,median,sd,pct90))
  
}


createDescriptiveTable(list(geih_4),
                       summary_function = na_table,
                       column_names=c("Faltantes","Total"),
                       variable_names=list(c("ingtot","y_ingLab_m","y_ingLab_m_2")),
                       variable_labels=list(c("Ingreso total","Ingreso laboral","Ingreso Laboral Imputado")),
                       arraystretch=1.3,
                       title="Datos faltantes",
                       label="tab:missing",
                       file="faltantes.tx"
)


createDescriptiveTable(list(geih_4),
                       summary_function = descriptives_table,
                       column_names=c("Media","Mediana","D.E.","Percentil 90"),
                       variable_names=list(c("ingtot","y_ingLab_m","y_ingLab_m_2","age","female")),
                       variable_labels=list(c("Ingreso total","Ingreso laboral","Ingreso Laboral imputado","Edad","Mujer")),
                       arraystretch=1.3,
                       title="Estadísticas descriptivas",
                       label="tab:missing",
                       file="faltantes.tx"
)
