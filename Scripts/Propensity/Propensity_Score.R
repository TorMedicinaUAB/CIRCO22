source('Scripts/Lectura_datos_basales.R')
library('survey')

datos_imputados <- readRDS('Datos/Imputados/Datos_imputados.rds')

# CreateTableOne(
#   # Argumento para poner de donde extraremos los datos
#   data = datos_imputados , 
#   # Ponemos los nombres de todas las variables, excepto la de estratificación
#   vars= datos_imputados %>% select(-Grup_IQ) %>%   names(), 
#   # Variable de estratificación
#   strata= c("Grup_IQ"), 
#   # Extraemos los nombres de las variables factor, EXCEPTO la variable por la que estratificaremos
#   factorVars= datos_imputados %>% select(where(is.factor )) %>% select(-Grup_IQ) %>%  names(),
#   # fijamos que test queremos para las variables numéricas.
#   testExact = fisher.test,
#   # Este argumento es para poner el total, y no solo los estratificados
#   addOverall = TRUE, 
#   argsApprox = list(correct = T), 
#   # Muestra los NA en cada categoría
#   includeNA = T) %>% 
#   print(
#     . ,
#     # el parametro nonnormal permite extraer la mediana y IQR, en vez de solo la media.
#     nonnormal = datos_imputados %>% select(where(is.numeric)) %>%  names(),
#     # de base, TableOne crea una categoria basal, con este argument, muestra todos los niveles
#     showAllLevels = T ,
#     noSpaces = F, 
#     smd = T
#   ) %>%  
#   as_tibble(., rownames = "Variables") %>% 
#   mutate(aux = str_extract(Variables,"(.*) \\(" ) ) %>% 
#   mutate(aux = str_remove(aux," \\(" ) ) %>%  
#   select(Variables,SMD) %>%  
#   mutate(SMD=as.numeric(SMD)) %>% 
#   mutate(Variables = str_extract(Variables, '^[^ (]+')) %>% 
#   filter(!is.na(SMD)) %>% 
#   filter(SMD>0.1) %>% 
#   arrange(desc(SMD))-> var_descompensades

# var_descompensades %>%  view()

datos_imputados_transformados_propensity <- datos_imputados %>% 
  mutate(
    DIabetes = droplevels(DIabetes,c('Metformina', 'Altres','ADO', 'dieta', 'ADO+Insulina','metformina+altres','ADO'))
  )



variables_propensity <- c(
  'edat_IQ','Charlson_Index',
  'plaquetes_preIQ','FsC_Elastografia', 
  'MidaMelsa_mm', 'Pughpunts_basal',
  'Pes','Creat_mgdL_preIQ','IMC','VG_fúndiques','TTO_Estatinas','ALT_preIQ',
  'AST_preIQ', 'DIabetes')

mod_propensity <- glm(
  formula = Grup_IQ ~  
    # DIabetes +
    I(edat_IQ^2) +
    log(edat_IQ) +
    # HVPG_basal+
    I(Charlson_Index) +
    log(plaquetes_preIQ) +
    # I(FsC_Elastografia^(2)) + exceso de missings 
    log(MidaMelsa_mm) +
    Pughpunts_basal+
    log(Pes)+
    IMC+
    VG_fúndiques+
    I(Creat_mgdL_preIQ^(2)) +
    ALT_preIQ +
    TTO_Estatinas+
    log(AST_preIQ) + I(AST_preIQ^(2))
  , 
  data = datos_imputados_transformados_propensity, 
  family=binomial(link="logit") )

datos_imputados_propensity <- datos_imputados %>% 
  mutate(prediciones=predict(mod_propensity,datos_imputados,type="response") )

# datos_imputados_propensity$prediciones

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


# datos_imputados_propensity$standarized_weights

iptwdatos <- svydesign(
  ids = ~ 1, 
  data = datos_imputados_transformados_propensity,
  strata = ~Grup_IQ,
  weights = ~ datos_imputados_propensity$standarized_weights)

tabWeighted <- svyCreateTableOne(
  vars= datos_imputados_transformados_propensity %>% select(-Grup_IQ) %>% names(),
  strata = "Grup_IQ", 
  data = iptwdatos, 
  smd =TRUE)

print(tabWeighted, smd = TRUE)


datos_imputados_propensity %>% 
  ggplot(aes(standarized_weights, fill= Grup_IQ )) +
  geom_density()


datos_imputados_propensity %>% 
  ggplot(aes(prediciones, fill= Grup_IQ )) +
  geom_histogram(color='black',alpha=0.8)

