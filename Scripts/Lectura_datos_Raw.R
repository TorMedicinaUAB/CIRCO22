if(!require(pacman)){install.packages(pacman)}
pacman::p_load( haven, tidyverse, compareGroups, tableone, table1, survey)

datos <- read_sav("Datos/raw/V3_IQ_DataBase_IPTWOct23_Compensats_IQvsNoIQ.sav") %>% 
  mutate(identificador= 1:371)





