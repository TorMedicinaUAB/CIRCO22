# lectura de datos CIRCO22----
# Llamamos a los datos automatica con el script lectura de datos, estos serán leídos de la versión Raw
source('Scripts/Lectura de datos.R')

# para las variables basales nos interesan las variables pre intervencion algunas identificaddoras y sobretodo las pre-intervenciónquirurgica 
datos_tabla_basal <-   datos %>% 
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
    SignesIndirectes_HTP,
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

# Creando la primera tabla basal ----
# En este primer fragmento de coedigo se crea una tabla basal con la funcion CreateTableOne, 
# que has estado experimentando un poco en todo el mes de noviembre de 2023
# recuerda que la función CreateTableOne crea un objeto de tipo Table que puedes pasar a tibble, es donde está la clave

Tabla_basal <- CreateTableOne(
  # Argumento para poner de donde extraremos los datos
  data = datos_tabla_basal , 
  # Ponemos los nombres de todas las variables, excepto la de estratificación
  vars= datos_tabla_basal %>% select(-Grup_IQ) %>%   names(), 
  # Variable de estratificación
  strata= c("Grup_IQ"), 
  # Extraemos los nombres de las variables factor, EXCEPTO la variable por la que estratificaremos
  factorVars= datos_tabla_basal %>% select(where(is.factor )) %>% select(-Grup_IQ) %>%  names(),
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
    nonnormal = datos_tabla_basal %>% select(where(is.numeric)) %>%  names(),
    # de base, TableOne crea una categoria basal, con este argument, muestra todos los niveles
    showAllLevels = T ,
    noSpaces = F, 
    smd = T
  ) %>%  
  as_tibble(., rownames = "Variables") %>% 
  mutate(aux = str_extract(Variables,"(.*) \\(" ) ) %>% 
  mutate(aux = str_remove(aux," \\(" ) )

# Añadiendo missings a TableOne ----
# para sacar los missing values, haremos dos tablas de missings, una con los totales y otra stratificada por el grupo clave

Missings_overall <- datos_tabla_basal %>% 
  select(Grup_IQ,where(is.numeric))  %>% 
  summarize_all(~sum(is.na(.))) %>%  
  mutate(Grup_IQ = as.factor('Overall'))

Missings_stratified <- datos_tabla_basal %>% 
  select(Grup_IQ,where(is.numeric))  %>% 
  group_by(Grup_IQ) %>% 
  summarize_all(~sum(is.na(.)))

Missings <-  Missings_overall%>% 
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

Lista_basal <- Tabla_basal %>%  
  
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
  arrange(aux_3, aux, aux_2,level) %>% 
  group_split(aux_3) %>%  
  as.list()

# arreglamos la parte numerica
Lista_basal[1:3] <- Lista_basal[1:3] %>% 
  map(~ mutate_all(..1, ~replace_na(.,'')))

# arreglamos la parte factor
Lista_basal[[4]] <- Lista_basal[[4]] %>%
  mutate(Variables=aux_2) %>% 
  group_by(Variables) %>% 
  mutate(Variables= case_when(row_number(Variables) == 1 ~Variables, T~NA_character_ ))

# finalmete volvemos a unificar la tabla y eliminamos las variables auxiliares.
Tabla_basal <- Lista_basal %>%  
  bind_rows() %>% 
  select(-matches('aux'))

Tabla_basal

# Exportamos la tabla final conseguida----

# Tabla_basal %>%
#   writexl::write_xlsx(.,'Outputs/Tablas_basales/Tabla_basal_con_missings.xlsx')
