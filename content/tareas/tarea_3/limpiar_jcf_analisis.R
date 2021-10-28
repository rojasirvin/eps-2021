# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

setwd("C:/Users/rojas/Dropbox/Evaluación de Programas Sociales/2021/tareas/Tarea 3")

library(tidyverse)
library(pacman)
library(janitor)
library(sandwich)
library(AER)
library(estimatr)
library(clubSandwich)
library(data.table)
library(MatchIt)
library(Zelig)
library(collapse)
library(Matching)
library(cobalt)
library(summarytools)

#Descriptor de archivos:
#https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/889463901242.pdf

#Ingresos de JCF
ing.jcf <- read_csv(
  "./ingresos_jcf.csv",
  locale = locale(encoding = "latin1")) %>% 
  rename(ingjcf_1=ing_6, #Arreglar nombres para que los ingresos estén en orden cronológico
         ingjcf_2=ing_5,
         ingjcf_3=ing_4,
         ingjcf_4=ing_3,
         ingjcf_5=ing_2,
         ingjcf_6=ing_1,
         ingjcf_tri=ing_tri,
         mesjcf_1=mes_6,
         mesjcf_2=mes_5,
         mesjcf_3=mes_4,
         mesjcf_4=mes_3,
         mesjcf_5=mes_2,
         mesjcf_6=mes_1) %>% 
  dplyr::select(-c(clave))

#Ingreso laboral
ing.lab <- read_csv(
  "./ingresos.csv",
  locale = locale(encoding = "latin1")) %>% 
  filter(clave %in% c("P001","P002","P003","P004",
                      "P005","P006","P007","P010",
                      "P011","P012","P013","P014",
                      "P018","P019","P020","P021","P022")) 


ing.lab.fuentes <- ing.lab %>%
  dplyr::select(folioviv,foliohog,numren,ing_tri,
                ing_1=ing_6, #Arreglar nombres para que los ingresos estén en orden cronológico
                ing_2=ing_5,
                ing_3=ing_4,
                ing_4=ing_3,
                ing_5=ing_2,
                ing_6=ing_1) %>% 
  group_by(folioviv,foliohog,numren) %>%
  fsum

ing.lab.meses <- ing.lab %>% 
  dplyr::select(folioviv,foliohog,numren,
                mes_1=mes_6,
                mes_2=mes_5,
                mes_3=mes_4,
                mes_4=mes_3,
                mes_5=mes_2,
                mes_6=mes_1) %>% 
  mutate(mes_1=as.numeric(mes_1),
         mes_2=as.numeric(mes_2),
         mes_3=as.numeric(mes_3),
         mes_4=as.numeric(mes_4),
         mes_5=as.numeric(mes_5),
         mes_6=as.numeric(mes_6)) %>% 
  group_by(folioviv,foliohog,numren) %>%
  fmean

ing.todos <- ing.lab.fuentes %>% 
  left_join(ing.lab.meses, by=c("folioviv","foliohog","numren")) %>% 
  full_join(ing.jcf, by=c("folioviv","foliohog","numren")) %>% 
  mutate(ing_1=ifelse(is.na(ing_1),0,ing_1),
         ing_2=ifelse(is.na(ing_2),0,ing_2),
         ing_3=ifelse(is.na(ing_3),0,ing_3),
         ing_4=ifelse(is.na(ing_4),0,ing_4),
         ing_5=ifelse(is.na(ing_5),0,ing_5),
         ing_6=ifelse(is.na(ing_6),0,ing_6),
         ingjcf_1=ifelse(is.na(ingjcf_1),0,ingjcf_1),
         ingjcf_2=ifelse(is.na(ingjcf_2),0,ingjcf_2),
         ingjcf_3=ifelse(is.na(ingjcf_3),0,ingjcf_3),
         ingjcf_4=ifelse(is.na(ingjcf_4),0,ingjcf_4),
         ingjcf_5=ifelse(is.na(ingjcf_5),0,ingjcf_5),
         ingjcf_6=ifelse(is.na(ingjcf_6),0,ingjcf_6),
         ct_futuro=ifelse(is.na(ct_futuro),0,ct_futuro),
         ing_1=ifelse(ct_futuro!=9,ing_1-ingjcf_1,ing_1),
         ing_2=ifelse(ct_futuro!=9,ing_2-ingjcf_2,ing_2),
         ing_3=ifelse(ct_futuro!=9,ing_3-ingjcf_3,ing_3),
         ing_4=ifelse(ct_futuro!=9,ing_4-ingjcf_4,ing_4),
         ing_5=ifelse(ct_futuro!=9,ing_5-ingjcf_5,ing_5),
         ing_6=ifelse(ct_futuro!=9,ing_6-ingjcf_6,ing_6),
         ing_tri=ifelse(is.na(ing_tri),0,ing_tri),
         ingjcf_tri=ifelse(is.na(ingjcf_tri),0,ingjcf_tri),
         ing_tri=ifelse(ct_futuro!="9",ing_tri-ingjcf_tri,ing_tri),
         ingtot_tri=ingjcf_tri+ing_tri)


poblacion <- read_csv(
  "./poblacion.csv",
  locale = locale(encoding = "latin1"),
  col_types = list(min_8 = col_double())) %>% 
  mutate(mujer=ifelse(sexo==2,1,0),
         indigena=ifelse(etnia==1,1,0),
         analfabeta=ifelse(alfabetism==2,1,0),
         joven=ifelse(edad>=18 & edad <=29,1,0),
         jnc=ifelse(edad>=18 & edad <=29 & asis_esc==2 & trabajo_mp==2,1,0),
         jtrabaja=ifelse(edad>=18 & edad <=29 & trabajo_mp==1,1,0),
         escoacum=case_when(nivelaprob==0 | nivelaprob==1 ~ 0,
                            nivelaprob==2 ~ gradoaprob + 1,
                            nivelaprob==3 ~ gradoaprob + 6,
                            nivelaprob==4 ~ gradoaprob + 9,
                            nivelaprob==5 & antec_esc==1 ~ gradoaprob + 6,
                            nivelaprob==5 & antec_esc==2 ~ gradoaprob + 9,
                            nivelaprob==5 & antec_esc==3 ~ gradoaprob + 12,
                            nivelaprob==6 & antec_esc==1 ~ gradoaprob + 6,
                            nivelaprob==6 & antec_esc==2 ~ gradoaprob + 9,
                            nivelaprob==6 & antec_esc==3 ~ gradoaprob + 12,
                            nivelaprob==7 ~ gradoaprob + 12,
                            nivelaprob==8 ~ gradoaprob + 16,
                            nivelaprob==9 ~ gradoaprob + 18),
         casadounion=ifelse(edo_conyug==1 | edo_conyug==2,1,0),
         mintrab=ifelse(is.na(hor_1) | is.na(min_1),0,(hor_1*60)+min_1),
         minest=ifelse(is.na(hor_2) | is.na(min_2),0,(hor_2*60)+min_2),
         mincuid=ifelse(is.na(hor_4) | is.na(min_4),0,(hor_4*60)+min_4),
         minqueh=ifelse(is.na(hor_6) | is.na(min_6),0,(hor_6*60)+min_6),
         minrecr=ifelse(is.na(hor_8) | is.na(min_8),0,(as.numeric(hor_8)*60)+min_8),
         afilss=ifelse(pop_insabi==1 | atemed==1, 1, 0),
         jefehog=ifelse(parentesco==101,1,0))


viviendas <- read_csv("./viviendas.csv",
                      col_types = list(mat_pisos = col_character()),
                      locale = locale(encoding = "latin1")) %>% 
  clean_names() %>% 
  mutate(pisotierra=ifelse(mat_pisos==1, 1 , 0),
         aguaent=ifelse(disp_agua==1 | disp_agua==2,1,0),
         drenfosa=ifelse(drenaje==1 | drenaje==2,1,0),
         sinelec=ifelse(disp_elect==5,1,0))

hogares <- read_csv(
  "./hogares.csv",
  locale = locale(encoding = "latin1")) %>% 
  mutate(sincomida=ifelse(acc_alim2==1,1,0),
         internet=ifelse(conex_inte==1,1,0))

concentrado <- read_csv(
  "./concentradohogar.csv",
  locale = locale(encoding = "latin1")) %>% 
  mutate(cve_ent=substr(ubica_geo,1,2),
         cve_mun=substr(ubica_geo,3,6),
         rural=ifelse(tam_loc==4,1,0),
         haymenores=ifelse(menores!=0,1,0))



#Un solo conjunto de datos

data.jcf <- poblacion %>% 
  dplyr::select(-ct_futuro) %>% 
  left_join(viviendas, by=c("folioviv")) %>% 
  left_join(hogares, by=c("folioviv", "foliohog")) %>% 
  left_join(concentrado, by=c("folioviv", "foliohog")) %>% 
  left_join(ing.todos, by=c("folioviv","foliohog","numren")) %>% 
  mutate(ing_1=ifelse(is.na(ing_1),0,ing_1),
         ing_2=ifelse(is.na(ing_2),0,ing_2),
         ing_3=ifelse(is.na(ing_3),0,ing_3),
         ing_4=ifelse(is.na(ing_4),0,ing_4),
         ing_5=ifelse(is.na(ing_5),0,ing_5),
         ing_6=ifelse(is.na(ing_6),0,ing_6),
         ingjcf_1=ifelse(is.na(ingjcf_1),0,ingjcf_1),
         ingjcf_2=ifelse(is.na(ingjcf_2),0,ingjcf_2),
         ingjcf_3=ifelse(is.na(ingjcf_3),0,ingjcf_3),
         ingjcf_4=ifelse(is.na(ingjcf_4),0,ingjcf_4),
         ingjcf_5=ifelse(is.na(ingjcf_5),0,ingjcf_5),
         ingjcf_6=ifelse(is.na(ingjcf_6),0,ingjcf_6),
         ingtot_tri=ifelse(is.na(ingtot_tri),0,ingtot_tri),
         ing_tri=ifelse(is.na(ing_tri),0,ing_tri),
         ingjcf_tri=ifelse(is.na(ingjcf_tri),0,ingjcf_tri))


#Definición de grupos de comparación
data.jcf <- data.jcf %>% 
  mutate(jcf=ifelse(ct_futuro %in% c(1,2,3,8,9),1,0)) %>% 
  filter((edad>=18 & edad <=29) | jcf==1) %>% 
  mutate(ingbene=ifelse(ct_futuro==9,bene_gob-ingjcf_tri,NA),
         ingbene=ifelse(jcf==0,bene_gob,ingbene),
         ingbene=ifelse(ingbene<0 | is.na(ingbene),0,ingbene),
         proggob=ifelse(ingbene>0,1,0)) %>% 
  mutate(jcf2=ifelse(jnc==1,0,NA),
         jcf2=ifelse(jcf==1,1,jcf2)) %>% 
  mutate(jcf3=ifelse(jtrabaja==1 & is.na(jcf2),0,NA),
         jcf3=ifelse(jcf==1,1,jcf3)) %>% 
  mutate(trabajo1=ifelse(ing_1>0 & !is.na(ing_1),1,0),
         trabajo2=ifelse(ing_2>0 & !is.na(ing_2),1,0),
         trabajo3=ifelse(ing_3>0 & !is.na(ing_3),1,0),
         trabajo4=ifelse(ing_4>0 & !is.na(ing_4),1,0),
         trabajo5=ifelse(ing_5>0 & !is.na(ing_5),1,0),
         trabajo6=ifelse(ing_6>0 & !is.na(ing_6),1,0)) %>% 
  mutate(benef1=ifelse(ingjcf_1>0 & !is.na(ingjcf_1),1,0),
         benef2=ifelse(ingjcf_2>0 & !is.na(ingjcf_2),1,0),
         benef3=ifelse(ingjcf_3>0 & !is.na(ingjcf_3),1,0),
         benef4=ifelse(ingjcf_4>0 & !is.na(ingjcf_4),1,0),
         benef5=ifelse(ingjcf_5>0 & !is.na(ingjcf_5),1,0),
         benef6=ifelse(ingjcf_6>0 & !is.na(ingjcf_6),1,0))


#Transiciones de empleo

data.jcf <- data.jcf %>%
  mutate(jcf_a_emp=0,
         jcf_a_emp=ifelse(trabajo2==1 & benef1==1,1,jcf_a_emp),
         jcf_a_emp=ifelse(trabajo3==1 & (benef1==1 | benef2==1),1,jcf_a_emp),
         jcf_a_emp=ifelse(trabajo4==1 & (benef1==1 | benef2==1 | benef3==1),1,jcf_a_emp),
         jcf_a_emp=ifelse(trabajo5==1 & (benef1==1 | benef2==1 | benef3==1 | benef4==1),1,jcf_a_emp),
         jcf_a_emp=ifelse(trabajo6==1 & (benef1==1 | benef2==1 | benef3==1 | benef4==1 | benef5==1),1,jcf_a_emp)) %>% 
  mutate(desemp_a_emp=0,
         desemp_a_emp=ifelse(trabajo1==0 & (trabajo2==1 | trabajo3==1 | trabajo4==1 | trabajo5==1 | trabajo6==1) & benef1==0 & jcf==0,1,desemp_a_emp),
         desemp_a_emp=ifelse(trabajo2==0 & (trabajo3==1 | trabajo4==1 | trabajo5==1 | trabajo6==1) & benef2==0 & jcf==0,1,desemp_a_emp),
         desemp_a_emp=ifelse(trabajo3==0 & (trabajo4==1 | trabajo5==1 | trabajo6==1) & benef3==0 & jcf==0,1,desemp_a_emp),
         desemp_a_emp=ifelse(trabajo4==0 & (trabajo5==1 | trabajo6==1) & benef4==0 & jcf==0,1,desemp_a_emp),
         desemp_a_emp=ifelse(trabajo5==0 & (trabajo6==1) & benef5==0 & jcf==0,1,desemp_a_emp)) %>% 
  mutate(siempre_jcf=ifelse(benef1==1 & benef2==1 & benef3==1 & benef4==1 & benef5==1 & benef6==1,1,0),
         siempre_emp=ifelse((trabajo1==1 & trabajo2==1 & trabajo3==1 & trabajo4==1 & trabajo5==1 & trabajo6==1) & jcf==0,1,0),
         encontro=ifelse(jcf_a_emp==1 | desemp_a_emp==1,1,0)) %>% 
  mutate(transicion=ifelse(benef6==1 | siempre_jcf==1| siempre_emp==1,0,1))


write.csv(data.jcf, file = "datos_jcf_analisis.csv", row.names=FALSE)
