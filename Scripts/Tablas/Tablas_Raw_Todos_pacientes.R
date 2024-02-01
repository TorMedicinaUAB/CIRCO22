source('Scripts/Lectura_datos_basales.R')

rm(list=setdiff(ls(),c('Pacientes_excluidos_propensity','datos_basales')))

datos_basales

# Datos propensity ----


Tabla_pacientes_raw_Todos <- CreateTableOne(
  data = datos_basales,
  vars = datos_basales %>%  select(-c(NHC,identificador, Grup_IQ)) %>%  names(),
  strata = "Grup_IQ",
  includeNA = T,
  addOverall = T,
  smd = T) %>% 
  print(.,add_overall= T,smd = TRUE) %>% 
  as_tibble(., rownames = "Variables")


writexl::write_xlsx(Tabla_pacientes_raw_Todos, 'Outputs/Tablas_check/Tabla_pacientes_raw_Todos.xlsx' )