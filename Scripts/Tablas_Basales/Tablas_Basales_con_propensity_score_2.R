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
  smd = T) %>%
  as_tibble(., rownames = "Variables") %>%
  mutate(aux = str_extract(Variables, "(.*) \\(")) %>%
  mutate(aux = str_remove(aux, " \\("))

Tabla_basal_propensity %>% 
  writexl::write_xlsx(.,'Outputs/Tablas_basales/Tabla_basal_propensity.xlsx')

