source('Scripts/Lectura de datos.R')

datos_tabla_basal <-   datos %>% 
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
  mutate_if(function(x) inherits(x, "haven_labelled"), ~ haven::as_factor(.)) %>% 
  select( Grup_IQ,edat_IQ,sexe_home,Pes,Talla_m,IMC, where(is.numeric), where(is.factor))




CreateTableOne(
  data = datos_tabla_basal , 
  # Ponemos los nombres de todas las variables, excepto la de estratificación
  vars= datos_tabla_basal %>% select(-Grup_IQ) %>%   names(), 
  # Variable de estratificación
  strata= c("Grup_IQ"), 
  # Extraemos los nombres de las variables factor, EXCEPTO la variable por la que estratificaremos
  factorVars= datos_tabla_basal %>% select(where(is.factor )) %>% select(-Grup_IQ) %>%  names(),
  testExact = fisher.test,
  # Muestra los NA en cada categoría
  includeNA = T) %>% 
  print(
    . ,
    # el parametro nonnormal permite ajustar por mediana y IQR
    nonnormal = datos_tabla_basal %>% select(where(is.numeric)) %>%  names(),
    # de base, TableOne crea una categoria basal, con este argument, muestra todos los niveles
    showAllLevels = T ,
    noSpaces = F, 
    smd = T,
  ) %>% 
  write.csv(., file = "Outputs/Tablas_basales/Tabla_basal.csv")


datos_tabla_basal %>% 
  select(Grup_IQ, Leucos_preIQ) %>% 
  filter(Grup_IQ=='No IQ') %>%  view()
