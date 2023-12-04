source('Scripts/Lectura de datos.R')

datos_basales <- datos %>% 
  select(
    # Grupos a diferenciar en el experimento CIRCO22, esta es la variable clave.
    Grup_IQ,
    #identificadoras
    edat_IQ:IMC,
    # Bioquimicas
    Enol_Actiu,
    DIabetes,
    HTA,
    # las variable dislipemia hay que crearla a partir de enfermedades asociadas
    # hay que crear desde malalties_associades1 que contengan los valores: 6, 13,17 (los que contienen dlp (codigo de dislipemia), 
    # el resto de codigos es que no tienen dislipemia
    malalties_associades1,
    plaquetes_preIQ:K_preIQ,
    etiologiaCH,
    #vareice esfofagicas basales
    VE_basal,
    # varices grastricas
    VG_fúndiques,
    # rigidez de higado
    matches('FsC'),
    matches('HVPG'),
    Pughpunts_basal,
    Pughclasse_basal,
    HCC_prev,
    MELD_basal,
    MELD_1anyspostIQ,
    respostHDK_aguda, respostHDK_crònica) %>% 
  mutate(
    # Creamos las dislipemias:
    dislipemias = as_factor(case_when(
      malalties_associades1 %in% c(6,13,17) ~ 'Si',
      !malalties_associades1 %in% c(6,13,17) ~ 'No' ))) %>% 
  # eliminamos esta variable, que ahora ya no nos sirve
  select(-malalties_associades1) %>% 
  # Extraemos los los factores, en lugar de dejarlos como variables numericas (si hace falta ya haremos el One_hot encoding más tarde)
  mutate_if(function(x) inherits(x, "haven_labelled"), ~ haven::as_factor(.)) %>% 
  # reordenamos un poco el data.frame para que sea fácil de seguir luego
  select( Grup_IQ,edat_IQ,sexe_home,Pes,Talla_m,IMC, where(is.numeric), where(is.factor))


datos_basales_sin_NA_extremos <- datos_basales %>%  
  select(-c(FA_preIQ,FsC_Elastografia,GGT_preIQ,HVPG_basal,INR_preIQ,K_preIQ,Leucos_preIQ,MELD_1anyspostIQ,NA_preIQ_,PCR_preIQ))

