pacman::p_load('haven')
source('Scripts/Lectura de datos.R')

# HCC_prev
# 
# SignesIndirectes_HTP;VE_basal;CSHP 


datos_competing <- datos %>%  
  select(
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
  mutate(Temps_SBP=te_ascites) %>% 
  rename(
    'Temps_TH'='Temps_finsaeventTH_mesos',
    'Mort'='mort',
    'Temps_Mort'='Temps_finseventMORT_mesos',
    'Descompensacio'='Descompensació_seguiment_global',
    'Temps_Descompensacio'='Temps_RIMERADESCOMPENSACIO_mesos',
    'Ascitis'='ascitis_seguimentPostAlta',
    'Temps_Ascitis'= 'te_ascites',
    'Gastrointestinal_bleeding'='HDAxHTP',
    'Temps_sagnat_Gastrointestinal'='te_1ra_HDA',
    'Encefalopatia_hepatica'= 'EH_seguimentPostAlta',
    'Temps_Encefalopatia_hepatica'='te_eh',
    'SBP'= 'PBE_seguimentPostAlta',
    # 'Temps_SBP'='te_ascites',
    'Carcinoma_Hepatocelular' ='HCCdeNOVO_seguimentPostAlta',
    'Temps_Carcinoma_Hepatocelular' ='temps_finseventHCCdenovo',
    'Infeccions_bacterianes' = 'Infeccio_PostIQ',
    'Temps_Infeccions_bacterianes' = 'Temps_ingrés_dies'
  )
 
datos_competing %>% 
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
    TRUE ~ NA_integer_
  ))
  

datos_competing %>% 
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
    Temps_Comp_Descompensacio= dplyr::case_when( 
       Descompensacio == 0 &  TH == 0 & Mort == 0                                       ~ max(Temps_Mort,Temps_TH, Temps_Descompensacio),
      (Descompensacio == 1 & (TH == 1 | Mort == 1)) & (Temps_Descompensacio<= Temps_TH) ~ Temps_Descompensacio,
      (Descompensacio == 1 & (TH == 0 | Mort == 0))                                     ~ Temps_Descompensacio,
       Descompensacio == 1 &  TH == 1 & (Temps_TH <= Temps_Descompensacio)              ~ Temps_TH,
      (Descompensacio == 0 & (TH == 1 | Mort == 1)) & (Temps_TH <= Temps_Mort)          ~ Temps_TH,
      TRUE ~ NA_integer_))




  

"TH"; "Temps_finsaeventTH_mesos" 
"mort" ; "Temps_finseventMORT_mesos"    

"ascitis_seguimentPostAlta" ; "te_ascites"  

"Gastrointestinal bleeding";"HDAxHTP" ; "te_1ra_HDA"

"Overt hepatic encephalopathy";"EH_seguimentPostAlta"; "te_eh"
"SBP" ; "PBE_seguimentPostAlta";"te_ascites"; "mismo tiempo que ascitis"
"Hepatocellular carcinoma" ; "HCCdeNOVO_seguimentPostAlta";"temps_finseventHCCdenovo"
"Other Bacterial infections"; "Infeccio_PostIQ";"Temps_ingrés_dies" #comprobar si hay en los dos grupos 



