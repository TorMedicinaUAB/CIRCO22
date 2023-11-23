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




Tabla_basal <- CreateTableOne(
  data = datos_tabla_basal , 
  # Ponemos los nombres de todas las variables, excepto la de estratificación
  vars= datos_tabla_basal %>% select(-Grup_IQ) %>%   names(), 
  # Variable de estratificación
  strata= c("Grup_IQ"), 
  # Extraemos los nombres de las variables factor, EXCEPTO la variable por la que estratificaremos
  factorVars= datos_tabla_basal %>% select(where(is.factor )) %>% select(-Grup_IQ) %>%  names(),
  testExact = fisher.test,
  addOverall = TRUE, 
  argsApprox = list(correct = T), 
  # Muestra los NA en cada categoría
  includeNA = T) %>% 
  print(
    . ,
    # el parametro nonnormal permite ajustar por mediana y IQR
    nonnormal = datos_tabla_basal %>% select(where(is.numeric)) %>%  names(),
    # de base, TableOne crea una categoria basal, con este argument, muestra todos los niveles
    showAllLevels = T ,
    noSpaces = F, 
    smd = T
  ) %>%  
  as_tibble(., rownames = "Variables") %>% 
  mutate(aux = str_extract(Variables,"(.*) \\(" ) ) %>% 
  mutate(aux = str_remove(aux," \\(" ) )

Missings <- datos_tabla_basal %>% 
  select(Grup_IQ,where(is.numeric))  %>% 
  group_by(Grup_IQ) %>% 
  summarize_all(~sum(is.na(.))) %>% 
  pivot_longer(-Grup_IQ, names_to = "Variables", values_to = "Missings") %>% 
  pivot_wider(names_from = Grup_IQ , values_from = Missings) %>% 
  mutate(Variables= paste(Variables, '(missing values)' )) %>% 
  mutate(aux = str_extract(Variables,"(.*) \\(" ) ) %>% 
  mutate(aux = str_remove(aux," \\(" ) ) %>% 
  mutate_all(as.character )

Lista_basal <- Tabla_basal %>%  
  # fill(aux,.direction = 'down') %>% 
  full_join(Missings, by= c('aux','No IQ','Si IQ', 'Variables')) %>% 
  mutate(aux_2 = str_extract(Variables,"^.{1,}")) %>% 
  fill(aux,.direction = 'down') %>%
  fill(aux_2,.direction = 'down') %>% 
  mutate(aux_3 = case_when(
    Variables == 'n'~1,
    str_detect(aux_2,'Pes|sexe_home|edat|Talla|IMC') ~ 2,
    str_detect(aux_2,'IQR|missing') ~ 3, 
    !str_detect(aux_2,'IQR|missing')|is.na(Variables) ~ 4)) %>%
  arrange(aux_3, aux, aux_2,level) %>%
  group_split(aux_3) %>%  
  as.list()

Lista_basal[1:3] <- Lista_basal[1:3] %>% 
  map(~ mutate_all(..1, ~replace_na(.,'')))

Tabla_basal <- Lista_basal %>%  
  bind_rows() %>% 
  select(-matches('aux'))

Tabla_basal %>% 
  writexl::write_xlsx(.,'Outputs/Tablas_basales/Tabla_basal_con_missings.xlsx')
