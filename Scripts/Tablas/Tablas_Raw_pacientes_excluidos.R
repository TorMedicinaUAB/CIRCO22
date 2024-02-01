source('Scripts/Lectura_datos_basales.R')
source('Scripts/Propensity/Propensity_Score.R')

rm(list=setdiff(ls(),c('Pacientes_excluidos_propensity','datos_basales')))

# Datos propensity ----

Tabla_pacientes_raw_excluidos <- CreateTableOne(
  data = datos_basales %>% filter(identificador %in% Pacientes_excluidos_propensity$identificador),
  vars = datos_basales %>%  select(-c(NHC,identificador, Grup_IQ)) %>%  names(),
  strata = "Grup_IQ",
  smd = T) %>% 
  print(., smd = TRUE) %>% 
  as_tibble(., rownames = "Variables")


Tabla_pacientes_raw_excluidos

writexl::write_xlsx(Tabla_pacientes_raw_excluidos, 'Outputs/Tablas_check/Tabla_pacientes_raw_excluidos.xlsx' )
