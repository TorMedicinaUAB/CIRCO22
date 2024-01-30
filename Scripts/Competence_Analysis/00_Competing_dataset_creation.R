pacman::p_load('haven')
source('Scripts/Lectura de datos.R')
Datos_propensity <- readRDS('Datos/Datos_propensity.rds')


# Arreglament de variables ----

Datos_propensity_2 <- Datos_propensity %>%  
  select(identificador, prediciones, standarized_weights) %>% 
  rename('prob_competing'='prediciones' )

datos_competing <- datos %>%
  mutate(identificador = 1:371) %>% 
  select(
    NHC,identificador,Grup_IQ,
    TH,Temps_finsaeventTH_mesos, # muerte
    mort,Temps_finseventMORT_mesos, # trasplantamiento
    Descompensació_seguiment_global ,Temps_RIMERADESCOMPENSACIO_mesos, # descompensación
    ascitis_seguimentPostAlta, te_ascites,  # Ascitis
    HDAxHTP,te_1ra_HDA, # Gastrointestinal bleeding
    EH_seguimentPostAlta,te_eh, # Overt hepatic encephalopathy
    PBE_seguimentPostAlta,te_ascites, # SBP , tiene el "mismo tiempo que ascitis"
    HCCdeNOVO_seguimentPostAlta,temps_finseventHCCdenovo, # Hepatocellular carcinoma
    Infeccio_PostIQ,Temps_ingrés_dies #Other Bacterial infections, comprobar si hay en los dos grupos
    )    %>% 
  inner_join( ., Datos_propensity_2 , by= c('identificador'='identificador')) %>% 
  mutate( Temps_SBP = te_ascites) %>% 
  rename(
    'Temps_TH'='Temps_finsaeventTH_mesos',
    'Mort'='mort',
    'Temps_Mort'='Temps_finseventMORT_mesos',
    'Descompensacio'='Descompensació_seguiment_global',
    'Temps_Descompensacio'='Temps_RIMERADESCOMPENSACIO_mesos',
    'Ascitis'='ascitis_seguimentPostAlta',
    'Temps_Ascitis'= 'te_ascites',
    'Sagnat_Gastrointestinal'='HDAxHTP',
    'Temps_Sagnat_Gastrointestinal'='te_1ra_HDA',
    'Encefalopatia_hepatica'= 'EH_seguimentPostAlta',
    'Temps_Encefalopatia_hepatica'='te_eh',
    'SBP'= 'PBE_seguimentPostAlta',
    #'Temps_SBP'='te_ascites', está en el mutate de justo antes del rename
    'Carcinoma_Hepatocelular' ='HCCdeNOVO_seguimentPostAlta',
    'Temps_Carcinoma_Hepatocelular' ='temps_finseventHCCdenovo',
    'Infeccions_bacterianes' = 'Infeccio_PostIQ',
    'Temps_Infeccions_bacterianes' = 'Temps_ingrés_dies'
  )

datos_competing 
# Competing preparaion ------
## TH competing amb Mort ----


Competing_TH <- datos_competing %>% 
  select(TH,Temps_TH,Mort, Temps_Mort) %>%  
  mutate(
    Competing_TH = dplyr::case_when( 
      (TH == 0 & Mort == 0) ~ 0,
      (TH == 0 & Mort == 1) ~ 1,
      (TH == 1 & Mort == 0) & (Temps_TH<=Temps_Mort) ~ 2,
      (TH == 1 & Mort == 1) & (Temps_TH<=Temps_Mort) ~ 2,
      TRUE ~ NA_integer_
    )) %>% 
  mutate(
    Temps_Comp_TH = dplyr::case_when( 
    (TH == 0 & Mort == 0) ~ Temps_Mort,
    (TH == 0 & Mort == 1) ~ Temps_Mort,
    (TH == 1 & Mort == 0) & (Temps_TH<=Temps_Mort) ~ Temps_TH,
    (TH == 1 & Mort == 1) & (Temps_TH<=Temps_Mort) ~ Temps_TH,
    TRUE ~ NA_integer_)) %>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))



  
Competing_Mort <- datos_competing %>% 
  select(TH,Temps_TH,Mort, Temps_Mort) %>%  
  mutate(
    Competing_Mort = dplyr::case_when( 
      (TH == 0 & Mort == 0) ~ 0,
      (TH == 0 & Mort == 1) ~ 1,
      (TH == 0 & Mort == 1) & (Temps_Mort<=Temps_TH) ~ 2,
      (TH == 1 & Mort == 1) & (Temps_Mort<=Temps_TH) ~ 2,
      TRUE ~ NA_integer_
    )) %>% 
  mutate(
    Temps_Comp_Mort = dplyr::case_when( 
      (TH == 0 & Mort == 0) ~ Temps_Mort,
      (TH == 0 & Mort == 1) ~ Temps_Mort,
      (TH == 0 & Mort == 1) & (Temps_Mort<=Temps_TH) ~ Temps_Mort,
      (TH == 1 & Mort == 1) & (Temps_Mort<=Temps_TH) ~ Temps_Mort,
      TRUE ~ NA_integer_)) %>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))

  
## Descompensacio Competing VS TH/Mort ----

Competing_Descompensacio <- datos_competing %>% 
  select(Descompensacio,Temps_Descompensacio,TH,Temps_TH,Mort, Temps_Mort) %>%  
  mutate(
    Comp_Descompensacio= dplyr::case_when( 
       Descompensacio == 0 &  TH == 0 & Mort == 0                                       ~ 0,
      (Descompensacio == 1 & (TH == 1 | Mort == 1)) & (Temps_Descompensacio<= Temps_TH) ~ 1,
      (Descompensacio == 1 & (TH == 0 | Mort == 0)) & (Temps_Descompensacio<= Temps_TH) ~ 1,
       Descompensacio == 1 &  TH == 1 & (Temps_TH <= Temps_Descompensacio)              ~ 2,
      (Descompensacio == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)          ~ 2,
      TRUE ~ NA_integer_)) %>%
  rowwise %>% 
  mutate(
    Temps_Comp_Descompensacio = dplyr::case_when( 
       Descompensacio == 0 &  TH == 0 & Mort == 0                                       ~ max(Temps_Mort,Temps_TH, Temps_Descompensacio),
      (Descompensacio == 1 & (TH == 1 | Mort == 1)) & (Temps_Descompensacio<= Temps_TH) ~ Temps_Descompensacio,
      (Descompensacio == 1 & (TH == 0 | Mort == 0))                                     ~ Temps_Descompensacio,
       Descompensacio == 1 &  TH == 1 & (Temps_TH <= Temps_Descompensacio)              ~ Temps_TH,
      (Descompensacio == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)          ~ Temps_TH,
      TRUE ~ NA_integer_)) %>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))



## Ascitis Competing VS TH/Mort ----

Competing_Ascitis <- datos_competing %>% 
  select(Ascitis,Temps_Ascitis,TH,Temps_TH,Mort, Temps_Mort) %>%  
  mutate(
    Comp_Ascitis= dplyr::case_when( 
      Ascitis == 0 &  TH == 0 & Mort == 0                                  ~ 0,
      (Ascitis == 1 & (TH == 1 | Mort == 1)) & (Temps_Ascitis<= Temps_TH)  ~ 1,
      (Ascitis == 1 & (TH == 0 | Mort == 0)) & (Temps_Ascitis<= Temps_TH)  ~ 1,
      Ascitis == 1 &  TH == 1 & (Temps_TH <= Temps_Ascitis)                ~ 2,
      (Ascitis == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)    ~ 2,
      TRUE ~ NA_integer_)) %>%
  rowwise %>% 
  mutate(
    Temps_Comp_Ascitis= dplyr::case_when( 
      Ascitis == 0 &  TH == 0 & Mort == 0                                       ~ max(Temps_Mort,Temps_TH, Temps_Ascitis),
      (Ascitis == 1 & (TH == 1 | Mort == 1)) & (Temps_Ascitis<= Temps_TH)       ~ Temps_Ascitis,
      (Ascitis == 1 & (TH == 0 | Mort == 0))                                    ~ Temps_Ascitis,
      Ascitis == 1 &  TH == 1 & (Temps_TH <= Temps_Ascitis)                     ~ Temps_TH,
      (Ascitis == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)         ~ Temps_TH,
      TRUE ~ NA_integer_))%>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))


## Sangnat Gastrointestinal Competing VS TH/Mort ----

Competing_Sagnat_Gastrointestinal <- datos_competing %>% 
  select( Sagnat_Gastrointestinal,Temps_Sagnat_Gastrointestinal,TH,Temps_TH,Mort, Temps_Mort) %>%  
  mutate(
    Comp_Sagnat_Gastrointestinal= dplyr::case_when( 
       Sagnat_Gastrointestinal == 0 &  TH == 0 & Mort == 0                                                 ~ 0,
      (Sagnat_Gastrointestinal == 1 & (TH == 1 | Mort == 1)) & (Temps_Sagnat_Gastrointestinal<= Temps_TH)  ~ 1,
      (Sagnat_Gastrointestinal == 1 & (TH == 0 | Mort == 0)) & (Temps_Sagnat_Gastrointestinal<= Temps_TH)  ~ 1,
       Sagnat_Gastrointestinal == 1 &  TH == 1 & (Temps_TH <= Temps_Sagnat_Gastrointestinal)               ~ 2,
      (Sagnat_Gastrointestinal == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)                    ~ 2,
      TRUE ~ NA_integer_)) %>%
  rowwise %>% 
  mutate(
    Temps_Comp_Sagnat_Gastrointestinal= dplyr::case_when( 
       Sagnat_Gastrointestinal == 0 &  TH == 0 & Mort == 0                                                ~ max(Temps_Mort,Temps_TH, Temps_Sagnat_Gastrointestinal),
      (Sagnat_Gastrointestinal == 1 & (TH == 1 | Mort == 1)) & (Temps_Sagnat_Gastrointestinal<= Temps_TH) ~ Temps_Sagnat_Gastrointestinal,
      (Sagnat_Gastrointestinal == 1 & (TH == 0 | Mort == 0))                                              ~ Temps_Sagnat_Gastrointestinal,
       Sagnat_Gastrointestinal == 1 &  TH == 1 & (Temps_TH <= Temps_Sagnat_Gastrointestinal)              ~ Temps_TH,
      (Sagnat_Gastrointestinal == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)                   ~ Temps_TH,
      TRUE ~ NA_integer_)) %>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))

## Encefalopatia hepatica Competing VS TH/Mort ----

Competing_Encefalopatia_hepatica <- datos_competing %>% 
  select(Encefalopatia_hepatica,Temps_Encefalopatia_hepatica,TH,Temps_TH,Mort, Temps_Mort) %>%  
  mutate(
    Comp_Encefalopatia_hepatica= dplyr::case_when( 
       Encefalopatia_hepatica == 0 &  TH == 0 & Mort == 0                                                ~ 0,
      (Encefalopatia_hepatica == 1 & (TH == 1 | Mort == 1)) & (Temps_Encefalopatia_hepatica<= Temps_TH)  ~ 1,
      (Encefalopatia_hepatica == 1 & (TH == 0 | Mort == 0)) & (Temps_Encefalopatia_hepatica<= Temps_TH)  ~ 1,
       Encefalopatia_hepatica == 1 &  TH == 1 & (Temps_TH <= Temps_Encefalopatia_hepatica)               ~ 2,
      (Encefalopatia_hepatica == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)                   ~ 2,
      TRUE ~ NA_integer_)) %>%
  rowwise %>% 
  mutate(
    Temps_Comp_Encefalopatia_hepatica= dplyr::case_when( 
       Encefalopatia_hepatica == 0 &  TH == 0 & Mort == 0                                                 ~ max(Temps_Mort,Temps_TH, Temps_Encefalopatia_hepatica),
      (Encefalopatia_hepatica == 1 & (TH == 1 | Mort == 1)) & (Temps_Encefalopatia_hepatica<= Temps_TH)   ~ Temps_Encefalopatia_hepatica,
      (Encefalopatia_hepatica == 1 & (TH == 0 | Mort == 0))                                               ~ Temps_Encefalopatia_hepatica,
       Encefalopatia_hepatica == 1 &  TH == 1 & (Temps_TH <= Temps_Encefalopatia_hepatica)                ~ Temps_TH,
      (Encefalopatia_hepatica == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)                    ~ Temps_TH,
      TRUE ~ NA_integer_)) %>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))


## SBP Competing VS TH/Mort ----


Competing_SBP <-  datos_competing %>% 
  select(SBP,Temps_SBP, TH , Temps_TH , Mort, Temps_Mort) %>%  
  mutate(
    Comp_SBP= dplyr::case_when( 
       SBP == 0 &  TH == 0 & Mort == 0                                                ~ 0,
      (SBP == 1 & (TH == 1 | Mort == 1)) & (Temps_SBP<= Temps_TH)  ~ 1,
      (SBP == 1 & (TH == 0 | Mort == 0)) & (Temps_SBP<= Temps_TH)  ~ 1,
       SBP == 1 &  TH == 1 & (Temps_TH <= Temps_SBP)               ~ 2,
      (SBP == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)                   ~ 2,
      TRUE ~ NA_integer_)) %>%
  rowwise %>% 
  mutate(
    Temps_Comp_SBP= dplyr::case_when( 
       SBP == 0 &  TH == 0 & Mort == 0                                                 ~ max(Temps_Mort,Temps_TH, Temps_SBP),
      (SBP == 1 & (TH == 1 | Mort == 1)) & (Temps_SBP<= Temps_TH)   ~ Temps_SBP,
      (SBP == 1 & (TH == 0 | Mort == 0))                                               ~ Temps_SBP,
       SBP == 1 &  TH == 1 & (Temps_TH <= Temps_SBP)                ~ Temps_TH,
      (SBP == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)                    ~ Temps_TH,
      TRUE ~ NA_integer_)) %>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))


## Carcinoma Hepatocelular Competing VS TH/Mort ----

Competing_Carcinoma_Hepatocelular <- datos_competing %>% 
  select(Carcinoma_Hepatocelular,Temps_Carcinoma_Hepatocelular, TH , Temps_TH , Mort, Temps_Mort) %>%  
  mutate(
    Comp_Carcinoma_Hepatocelular= dplyr::case_when( 
       Carcinoma_Hepatocelular == 0 &  TH == 0 & Mort == 0                                  ~ 0,
      (Carcinoma_Hepatocelular == 1 & (TH == 1 | Mort == 1)) & (Temps_Carcinoma_Hepatocelular<= Temps_TH)  ~ 1,
      (Carcinoma_Hepatocelular == 1 & (TH == 0 | Mort == 0)) & (Temps_Carcinoma_Hepatocelular<= Temps_TH)  ~ 1,
       Carcinoma_Hepatocelular == 1 &  TH == 1 & (Temps_TH <= Temps_Carcinoma_Hepatocelular)                ~ 2,
      (Carcinoma_Hepatocelular == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)    ~ 2,
      TRUE ~ NA_integer_)) %>%
  rowwise %>% 
  mutate(
    Temps_Comp_Carcinoma_Hepatocelular= dplyr::case_when( 
       Carcinoma_Hepatocelular == 0 &  TH == 0 & Mort == 0                                       ~ max(Temps_Mort,Temps_TH, Temps_Carcinoma_Hepatocelular),
      (Carcinoma_Hepatocelular == 1 & (TH == 1 | Mort == 1)) & (Temps_Carcinoma_Hepatocelular<= Temps_TH)       ~ Temps_Carcinoma_Hepatocelular,
      (Carcinoma_Hepatocelular == 1 & (TH == 0 | Mort == 0))                                    ~ Temps_Carcinoma_Hepatocelular,
       Carcinoma_Hepatocelular == 1 &  TH == 1 & (Temps_TH <= Temps_Carcinoma_Hepatocelular)                     ~ Temps_TH,
      (Carcinoma_Hepatocelular == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)         ~ Temps_TH,
      TRUE ~ NA_integer_)) %>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))


## Other Bacterial infections Competing VS TH/Mort ----

Competing_Infeccions_bacterianes <- datos_competing %>% 
  select(Infeccions_bacterianes,Temps_Infeccions_bacterianes, TH, Temps_TH, Mort, Temps_Mort) %>%  
  mutate(
    Comp_Infeccions_bacterianes= dplyr::case_when( 
      Infeccions_bacterianes == 0 &  TH == 0 & Mort == 0                                  ~ 0,
      (Infeccions_bacterianes == 1 & (TH == 1 | Mort == 1)) & (Temps_Infeccions_bacterianes<= Temps_TH)  ~ 1,
      (Infeccions_bacterianes == 1 & (TH == 0 | Mort == 0)) & (Temps_Infeccions_bacterianes<= Temps_TH)  ~ 1,
      Infeccions_bacterianes == 1 &  TH == 1 & (Temps_TH <= Temps_Infeccions_bacterianes)                ~ 2,
      (Infeccions_bacterianes == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)    ~ 2,
      TRUE ~ NA_integer_)) %>%
  rowwise %>% 
  mutate(
    Temps_Comp_Infeccions_bacterianes= dplyr::case_when( 
      Infeccions_bacterianes == 0 &  TH == 0 & Mort == 0                                       ~ max(Temps_Mort,Temps_TH, Temps_Infeccions_bacterianes),
      (Infeccions_bacterianes == 1 & (TH == 1 | Mort == 1)) & (Temps_Infeccions_bacterianes<= Temps_TH)       ~ Temps_Infeccions_bacterianes,
      (Infeccions_bacterianes == 1 & (TH == 0 | Mort == 0))                                    ~ Temps_Infeccions_bacterianes,
      Infeccions_bacterianes == 1 &  TH == 1 & (Temps_TH <= Temps_Infeccions_bacterianes)                     ~ Temps_TH,
      (Infeccions_bacterianes == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)         ~ Temps_TH,
      TRUE ~ NA_integer_)) %>% 
  select(-c(TH,Temps_TH,Mort, Temps_Mort))


# Dataset competing analysis ----

Competing_dataset <- list(
  datos_competing %>% select(NHC,identificador,Grup_IQ),
  Competing_TH,
  Competing_Mort,
  Competing_Descompensacio,
  Competing_Ascitis,
  Competing_Carcinoma_Hepatocelular,
  Competing_Encefalopatia_hepatica,
  Competing_Sagnat_Gastrointestinal,
  Competing_SBP,
  Competing_Infeccions_bacterianes)  %>%  
  reduce(bind_cols) %>% 
  mutate_at(vars(starts_with('Comp')), ~haven::labelled(
    .x, c('Censura'= 0,'Event'= 1,'Competing'=2), 
    label = 'a' ))

Competing_dataset

# rm(list=setdiff(ls(), "Competing_dataset"))
# 
# writexl::write_xlsx(Competing_dataset,'Competing_dataset.xlsx')

