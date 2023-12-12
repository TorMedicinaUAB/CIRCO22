if(!require(pacman)){install.packages(pacman)}
pacman::p_load(haven, tidyverse, compareGroups, tableone,table1)

datos <- read_sav("Datos/raw/IQ_DataBase_IPTWDic23_Compensats_IQvsNoIQ.sav")




