source('Scripts/Lectura_datos_basales.R')


datos_imputados <- readRDS('Datos/Imputados/Datos_imputados.rds')



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
  filter(SMD>0.1) %>% 
  arrange(desc(SMD))-> var_descompensades

datos_imputados$VE_basal
var_descompensades
datos_imputados$etiol_OH

# 'I(^2)'
# I(VE_basal^2)

formula_1 <- as.formula(
  'Grup_IQ ~ VE_basal+I(edat_IQ^2)+
  SignesIndirectes_HTP+
  HVPG_10+Charlson_Index+etiol_OH'
)

mod_propensity <- glm(
  formula = formula_1, 
  data = datos_imputados, 
  family=binomial(link="logit") )

datos_imputados_propensity <- datos_imputados %>% 
  mutate(prediciones=predict(mod_propensity,datos_imputados,type="response") )

datos_imputados_propensity$prediciones

datos_imputados %>% 
  group_by(Grup_IQ) %>% 
  count()

datos_imputados %>% 
  count()

datos_imputados_propensity <- datos_imputados_propensity %>% 
  mutate(standarized_weights = case_when(
    Grup_IQ == 'Si IQ' ~ (177/(371))/ (  prediciones),
    Grup_IQ == 'No IQ' ~ (194/(371))/ (1-prediciones) 
  )) 


datos_imputados_propensity$standarized_weights

iptwdatos <- svydesign(
  ids = ~ 1, 
  data = datos_imputados_propensity,
  strata = ~Grup_IQ ,
  weights = ~ datos_imputados_propensity$standarized_weights)

tabWeighted <- svyCreateTableOne(
  vars= datos_imputados %>% select(-Grup_IQ) %>% names(),
  strata = "Grup_IQ", 
  data = iptwdatos, 
  smd =TRUE)

print(tabWeighted, smd = TRUE)

