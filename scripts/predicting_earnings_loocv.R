#####CUARTO PUNTO###############################################################
################################################################################

####LIBRERIAS###################################################################

rm(list=ls())
library(tidyverse)

####Numeral A###################################################################

geih <-read.xlsx(here("stores","geih_final.xlsx"))

#Se usan solo las filas que contienen todas las variables. Esto es lo que hace LM por debajo. Pero para dividir correctamente trainign/test 
#y comparar con los valores reales, se debe hacer desde el inciio.
geih_earnings_cmplt <- geih[complete.cases(geih[,c("y_ingLab_m_2","female","age","cuentaPropia","college_2","formal","oficio")]),] %>% 
  filter(oficio!=78)

#Semilla
set.seed(10101)

#Muestra de entrenamiento
id_test <- sample(1:nrow(geih_earnings_cmplt),nrow(geih_earnings_cmplt)*0.3)


test_set <- geih_earnings_cmplt[id_test,]
training_set <- geih_earnings_cmplt[-id_test,]

####Numeral B###################################################################

#Especificacion 1: earnings~age+age^2
lm_spc1 <- lm(y_ingLab_m_2~age+I(age^2),training_set)
pred_lm_spc1 <- predict.lm(lm_spc1,test_set)


#Especificacion 2: log(earnings)~female
lm_spc2 <- lm(log(y_ingLab_m_2+1)~female,training_set)
pred_lm_spc2 <- predict.lm(lm_spc2,test_set)


#Especificacion 3: 
lm_spc3 <- lm(y_ingLab_m_2~female+age+I(age^2),training_set)
pred_lm_spc3 <- predict.lm(lm_spc3,test_set)


#Especificacion 4: 
lm_spc4 <- lm(y_ingLab_m_2~female+age+I(age^2)+cuentaPropia,training_set)
pred_lm_spc4 <- predict.lm(lm_spc4,test_set)


#Especificacion 5: 
lm_spc5 <- lm(y_ingLab_m_2~female+age+I(age^2)+cuentaPropia+college_2,training_set)
pred_lm_spc5 <- predict.lm(lm_spc5,test_set)

#Especificacion 6: 
lm_spc6 <- lm(y_ingLab_m_2~female+age+I(age^2)+cuentaPropia+college_2+formal,training_set)
pred_lm_spc6 <- predict.lm(lm_spc6,test_set)


#Especificacion 7: 
lm_spc7 <- lm(y_ingLab_m_2~female+age+I(age^2)+cuentaPropia+
                college_2+formal+I(college_2*formal),training_set)
pred_lm_spc7 <- predict.lm(lm_spc7,test_set)


#Especificacion 8: 
lm_spc8 <- lm(y_ingLab_m_2~female+age+I(age^2)+
                cuentaPropia+college_2+formal+I(college_2*formal)+factor(oficio),training_set)
pred_lm_spc8 <- predict.lm(lm_spc8,test_set)


#calculo del RMSE para todas las especificaciones

rmse=tibble()
models <- list(lm_spc1,lm_spc2,lm_spc3,lm_spc4,lm_spc5,lm_spc6,lm_spc7,lm_spc8)

for (i in 1:8){

  if(i==2){
    
    pred <- predict.lm(models[[i]],test_set)
    rmse <- tibble("RMSE"=sqrt(sum((test_set$y_ingLab_m_2-exp(pred))^2)),
                   modelo=paste("Modelo",i)) %>% 
      bind_rows(rmse)
    
  }else{
    
    pred <- predict.lm(models[[i]],test_set)
    rmse <- tibble("RMSE"=sqrt(sum((test_set$y_ingLab_m_2-pred)^2)),
                   modelo=paste("Modelo",i)) %>% 
      bind_rows(rmse)
    
  }
  
}

#El mejor modelo es el modelo 8 y el siguiente mejor el modelo 7.
rmse %>% 
  arrange(rmse) %>% 
  slice_head(n=2)


####Numeral C###################################################################


#Se escoge RMSE porque penaliza más los errores más grandes lo cual es deseable
#al momento de predecir ingresos: un error más grande es peor que un error más
#pequeño. Adicionalmente se prefiere esta a MSE pues las unidades son las originales.

#La especificación con el menor error de predicción es la que incluye información
#sobre el sector ocupacional. Este es el modelo más complejo pero es razonable
#pues incluye elementos que intuitivamente son predictores del comportamiento del salario.



#La partición de datos de prueba en un formato similar al utilizado en la estimación está dada por:
x_test <- model.matrix(y_ingLab_m_2~female+age+I(age^2)+
               cuentaPropia+college_2+formal+I(college_2*formal)+factor(oficio),test_set)

#Los residuales de la predicción de los datos de prueba es:
residuals_test <- test_set$y_ingLab_m_2-pred_lm_spc8

#La matrix proyectada para la partición de datos de prueba es:
hat_matrix <- x_test %*% solve( t(x_test) %*% x_test ) %*% t(x_test)

#Los estadísticos de leverage son entonces:
h_coef <- diag(hat_matrix)


#Entonces los estadísticos de influencia son:
influence_coef <- residuals_test/(1-h_coef)


#Haciendo un diagrama de dispersión
influence_coef %>%
  tibble("influencia"=.,"y_true"=test_set$y_ingLab_m_2) %>% 
  rowid_to_column("id_prediccion") %>%
  mutate(abs_influencia=abs(influencia)) %>% 
  
  ggplot(.)+
  geom_point(aes(x=log(abs_influencia),y=log(y_true+1)),color="darkred")+
  xlab("Logaritmo del valor absoluto del estadístico de influencia")+
  ylab("Logaritmo del ingreso laboral")+
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

#ggsave("analisis_influencia.png",width=8,height = 5,dpi="retina")


#No se observa un patrón creciente.

####Numeral D###################################################################



#LOOCV para los dos mejores modelos

#Se inicializa tabla de datos vacía que contendrá la predicción para cada estimación y el valor real
pred_true <- tibble()
for(i in 1:nrow(geih_earnings_cmplt)){
  
  #partición de los datos dejando un dato afuera
  test_set_loocv<-geih_earnings_cmplt[i,]
  training_set_loocv<-geih_earnings_cmplt[-i,]
  
  #Estimación del modelo 8
  best_model <- lm(y_ingLab_m_2~female+age+I(age^2)+
                  cuentaPropia+college_2+formal+I(college_2*formal)+factor(oficio),training_set)
  #Estimación del modelo 7
  second_best_model <- lm(y_ingLab_m_2~female+age+I(age^2)+cuentaPropia+
                                       college_2+formal+I(college_2*formal),training_set)
  
  #Recolección de los resultados
  pred_true <- tibble("id"=i,"y_pred_bm"=predict.lm(best_model, test_set),
                      "y_pred_sbm"=predict.lm(second_best_model, test_set),
                      "y"=test_set$y_ingLab_m_2) %>% 
    bind_rows(pred_true)
  
  
  
  print(i)
  
  
  
  
}


#RMSE de LOOCV para el modelo 8 
sqrt(sum((pred_true$y-pred_true$y_pred_bm)^2))
#RMSE de LOOCV para el modelo 7
sqrt(sum((pred_true$y-pred_true$y_pred_sbm)^2))


