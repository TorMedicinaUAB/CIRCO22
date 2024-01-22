source('Scripts/Propensity/Propensity_Score.R')
# print(tabWeighted, smd = TRUE)

Propensity_table_Weighted

Tabla_basal_propensity <- print(
  Propensity_table_Weighted,
  # el parametro nonnormal permite extraer la mediana y IQR, en vez de solo la media.
  nonnormal = datos_imputados %>% select(where(is.numeric)) %>%  names(),
  # de base, TableOne crea una categoria basal, con este argument, muestra todos los niveles
  showAllLevels = T ,
  noSpaces = F,
  addOverall =T,
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




