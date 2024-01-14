source('Scripts/Lectura_datos_basales.R')
library('survey')

datos_imputados <- readRDS('Datos/Imputados/Datos_imputados.rds')

datos_imputados_transformados_propensity <- datos_imputados %>% 
  mutate(
    DIabetes = droplevels(DIabetes,c('Metformina', 'Altres','ADO', 'dieta', 'ADO+Insulina','metformina+altres'))
  )

datos_imputados_transformados_propensity_2 <- datos_imputados %>% 
  mutate(
    DIabetes = droplevels(DIabetes,c('Metformina', 'Altres','ADO', 'dieta', 'ADO+Insulina','metformina+altres','ADO')),
    SignesIndirectes_HTP = droplevels(SignesIndirectes_HTP,c('dubtós'))
  ) %>%
  select('Grup_IQ','edat_IQ','sexe_home',"IMC",'etiol_OH','Enol_Actiu',"Charlson_Index","plaquetes_preIQ","DIabetes",'Pughpunts_basal',
         'colaterals_shunts','MELD_basal','Creat_mgdL_preIQ','Alb_gL_preIQ','BB_mgdL_preIQ','INR_preIQ')


variables_propensity <- c(
  'edat_IQ','sexe_home','IMC', 'etiol_OH','Enol_Actiu','Charlson_Index','plaquetes_preIQ','DIabetes','Pughpunts_basal',
  'colaterals_shunts','MELD_basal','Creat_mgdL_preIQ', 'Alb_gL_preIQ','BB_mgdL_preIQ','INR_preIQ'
  
  # 'MidaMelsa_mm', 'Pughpunts_basal',
  # ,'Creat_mgdL_preIQ','IMC','VG_fúndiques','TTO_Estatinas','ALT_preIQ',
  # 'AST_preIQ', 'DIabetes'
)

mod_propensity <- glm(
  formula = Grup_IQ ~  
    # DIabetes +
    I(edat_IQ^2) + sexe_home + IMC + log(plaquetes_preIQ) + Pughpunts_basal + Charlson_Index + 
    colaterals_shunts + MELD_basal + Alb_gL_preIQ + Creat_mgdL_preIQ + BB_mgdL_preIQ + INR_preIQ+ 
    
    DIabetes + etiol_OH + Enol_Actiu
  # log(MidaMelsa_mm) +
  # Pughpunts_basal+
  
  # IMC+
  # VG_fúndiques+
  # I(Creat_mgdL_preIQ^(2)) +
  # ALT_preIQ +
  # TTO_Estatinas+
  # log(AST_preIQ) + I(AST_preIQ^(2))
  , 
  data = datos_imputados_transformados_propensity_2, 
  family=binomial(link="logit") )

datos_imputados_propensity <- datos_imputados %>% 
  mutate(prediciones=predict(mod_propensity,datos_imputados,type="response") )

datos_imputados_propensity <- datos_imputados_propensity %>% 
  mutate(standarized_weights = case_when(
    Grup_IQ == 'Si IQ' ~ (177/(371))/ (  prediciones),
    Grup_IQ == 'No IQ' ~ (194/(371))/ (1-prediciones) 
  )) 



iptwdatos <- svydesign(
  ids = ~ 1, 
  data = datos_imputados_transformados_propensity_2,
  strata = ~Grup_IQ,
  weights = ~ datos_imputados_propensity$standarized_weights)

tabWeighted <- svyCreateTableOne(
  vars= datos_imputados_transformados_propensity_2 %>% select(-Grup_IQ) %>% names(),
  strata = "Grup_IQ", 
  data = iptwdatos_propensity, 
  smd =TRUE)

print(tabWeighted, smd = TRUE)


# datos_imputados_propensity$standarized_weights

iptwdatos_propensity <- svydesign(
  ids = ~ 1, 
  data = datos_imputados_transformados_propensity,
  strata = ~Grup_IQ,
  weights = ~ datos_imputados_propensity$standarized_weights)

tabWeighted_full <- svyCreateTableOne(
  vars= datos_imputados_transformados_propensity %>% select(-Grup_IQ) %>% names(),
  strata = "Grup_IQ", 
  data = iptwdatos, 
  smd =TRUE,
  addOverall = TRUE)

print(tabWeighted_full, smd = TRUE)

# print(tabWeighted, smd = TRUE)


Tabla_basal_propensity <- print(
  tabWeighted_full ,
  # el parametro nonnormal permite extraer la mediana y IQR, en vez de solo la media.
  nonnormal = datos_imputados %>% select(where(is.numeric)) %>%  names(),
  # de base, TableOne crea una categoria basal, con este argument, muestra todos los niveles
  showAllLevels = T ,
  noSpaces = F,
  smd = T) %>%
  as_tibble(., rownames = "Variables") %>%
  mutate(aux = str_extract(Variables, "(.*) \\(")) %>%
  mutate(aux = str_remove(aux, " \\("))

Tabla_basal_propensity

# Añadiendo missings a TableOne ----
# para sacar los missing values, haremos dos tablas de missings, una con los totales y otra stratificada por el grupo clave

Missings_overall <- datos_imputados %>% 
  select(Grup_IQ,where(is.numeric))  %>% 
  summarize_all(~sum(is.na(.))) %>%  
  mutate(Grup_IQ = as.factor('Overall'))

Missings_stratified <- datos_imputados %>% 
  select(Grup_IQ,where(is.numeric))  %>% 
  group_by(Grup_IQ) %>% 
  summarize_all(~sum(is.na(.)))

Missings <-  Missings_overall %>% 
  bind_rows(Missings_stratified) %>% 
  pivot_longer(-Grup_IQ, names_to = "Variables", values_to = "Missings") %>% 
  pivot_wider(names_from = Grup_IQ , values_from = Missings) %>% 
  mutate(Variables= paste(Variables, '(missing values)' )) %>% 
  mutate(aux = str_extract(Variables,"(.*) \\(" ) ) %>% 
  mutate(aux = str_remove(aux," \\(" ) ) %>% 
  mutate_all(as.character)

# Reajuste de la tabla basal----
## lo complicado aquí es unir los missings a las variables numerics y hacer que se mantenga la estructura con los factores
## creamos 3 variables auxiliares para ayudarnos con los órdenes y que todo quede en su sitio:P
# aux_3: 1º prioridad a la hora de ordenar, 
## es una variable que contendrá del 1 al 4 siendo respectivamente
## recuento total, variables identificadores, variables numericas y variables factor,variable 
# aux_2: 2º prioridad: es el nombre de la variable en si mismo, sin la etiqueta de identificador de que funcion se está aplicando.
# aux: variable que contiene la funcion para las variables numericas

Lista_propensity_imputada <- Tabla_basal_propensity %>%  
  full_join(Missings, by= c('aux','No IQ','Si IQ', 'Variables', 'Overall')) %>% 
  # Hacemos un variable que sea metanombre, es decir nombre de la variable numerica pero sin la funcion
  mutate(aux_2 = str_extract(Variables,"^.{1,}")) %>% 
  # rellenamos los NA en las variables auxiliar 
  fill(aux,.direction = 'down') %>%
  fill(aux_2,.direction = 'down') %>% 
  # ahora aquí creamos la auxiliar 3 que contendrá un índice numerico para ordenar las variables
  mutate(aux_3 = case_when(
    # mantenemosun orden a la hora de ordenar la tabla
    ## primero los recuentos totales
    Variables == 'n'~1,
    # Luego las variables identificadoras
    str_detect(aux_2,'Pes|sexe_home|edat|Talla|IMC') ~ 2,
    ## las variables numericas
    str_detect(aux_2,'IQR|missing') ~ 3, 
    ## Finalmente las variables factor
    !str_detect(aux_2,'IQR|missing')|is.na(Variables) ~ 4)) %>%
  # ordenamos por; grupos, variable, subfncion de variable (numericas) y nivel (para las factor)
  arrange(aux_3, aux, aux_2) %>% 
  group_split(aux_3) %>%  
  as.list()

# arreglamos la parte numerica
Lista_propensity_imputada[1:3] <- Lista_propensity_imputada[1:3] %>% 
  map(~ mutate_all(..1, ~replace_na(.,'')))

# arreglamos la parte factor
Lista_propensity_imputada[[4]] <- Lista_propensity_imputada[[4]] %>%
  mutate(Variables=aux_2) %>% 
  group_by(Variables) %>% 
  mutate(Variables= case_when(row_number(Variables) == 1 ~Variables, T~NA_character_ ))

# finalmete volvemos a unificar la tabla y eliminamos las variables auxiliares.
Tabla_basal_propensity_final <- Lista_propensity_imputada %>%  
  bind_rows() %>% 
  select(-matches('aux'))

Tabla_basal_propensity_final

# Exportamos la tabla final conseguida----

Tabla_basal_propensity_final %>%
  writexl::write_xlsx(.,'Outputs/Tablas_basales/Tabla_basal_propensity.xlsx')




