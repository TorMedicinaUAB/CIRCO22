if(!require(pacman)){install.packages(pacman)}
pacman::p_load(haven, tidyverse, compareGroups, tableone,table1)

datos <- read_sav("Datos/raw/V1_IQ_DataBase_IPTWOct23_Compensats_IQvsNoIQ.sav")

datos_basales <-   datos %>% 
    select(
      # grupos a diferencias
      Grup_IQ,
      #identificadoras
      edat_IQ:IMC,
      Enol_Actiu,
      DIabetes,
      HTA,
      # crear variable nueva para displipemia que contenga los valores : 6, 13,17 (básicamente los que tengan dlp, el resto es que no tienen dislipemia)
      malalties_associades1,
      plaquetes_preIQ:K_preIQ,
      Pughpunts_basal,
      Pughclasse_basal,
      etiologiaCH,
      #vareice esfofagicas basales
      VE_basal,
      # varices grastricas
      VG_fúndiques,
      # rigidez de higado
      matches('FsC'),
      MELD_basal,
      HCC_prev,
      MELD_basal,
      MELD_1anyspostIQ,
      matches('HVPG'),
      respostHDK_aguda,
      respostHDK_crònica) %>% 
  mutate(
    dislipemias = as_factor(case_when(
      malalties_associades1 %in% c(6,13,17) ~ 'Si',
      !malalties_associades1 %in% c(6,13,17) ~ 'No' ))) %>% 
  select(-malalties_associades1) %>% 
  mutate_if(function(x) inherits(x, "haven_labelled"), ~ haven::as_factor(.))
  

# descrTable(Grup_IQ~., datos_basales)


Tabla_basalVarFactores  <- datos_basales %>% 
  select(where(is.factor )) %>%
  select(-Grup_IQ) %>% 
  names()

Tabla_basalVarFactores

CreateTableOne(
    data = datos_basales , 
    vars= datos_basales %>% select(-Grup_IQ) %>%   names(), 
    strata= c("Grup_IQ"), 
    factorVars= Tabla_basalVarFactores,
    testExact = fisher.test,
    includeNA = T) %>% 
  print(
    . ,
    nonnormal = datos_basales %>% select(where(is.numeric)) %>%  names(),
    showAllLevels = T ,
    noSpaces = F, 
    smd = T,
        )



