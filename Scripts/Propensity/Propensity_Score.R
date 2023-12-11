source('Scripts/Lectura_datos_basales.R')

datos_imputados <- readRDS('Datos/Imputados/Datos_imputados.rds')

datos_imputados

CreateTableOne(
  # Argumento para poner de donde extraremos los datos
  data = datos_imputados , 
  # Ponemos los nombres de todas las variables, excepto la de estratificación
  vars= datos_imputados %>% select(-Grup_IQ) %>%   names(), 
  # Variable de estratificación
  strata= c("Grup_IQ"), 
  # Extraemos los nombres de las variables factor, EXCEPTO la variable por la que estratificaremos
  factorVars= datos_imputados %>% select(where(is.factor )) %>% select(-Grup_IQ) %>%  names(),
  # fijamos que test queremos para las variables numéricas.
  testExact = fisher.test,
  # Este argumento es para poner el total, y no solo los estratificados
  addOverall = TRUE, 
  argsApprox = list(correct = T), 
  # Muestra los NA en cada categoría
  includeNA = T) %>% 
  print(
    . ,
    # el parametro nonnormal permite extraer la mediana y IQR, en vez de solo la media.
    nonnormal = datos_imputados %>% select(where(is.numeric)) %>%  names(),
    # de base, TableOne crea una categoria basal, con este argument, muestra todos los niveles
    showAllLevels = T ,
    noSpaces = F, 
    smd = T
  ) %>%  
  as_tibble(., rownames = "Variables") %>% 
  mutate(aux = str_extract(Variables,"(.*) \\(" ) ) %>% 
  mutate(aux = str_remove(aux," \\(" ) ) %>%  
  select(Variables,SMD) %>%  
  mutate(SMD=as.numeric(SMD)) %>% 
  mutate(Variables = str_extract(Variables, '^[^ (]+')) %>% 
  filter(!is.na(SMD)) %>% 
  filter(SMD<0.1)
  
