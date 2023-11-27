source('Scripts/Lectura de datos.R')

datos <- datos %>% 
  mutate_if(function(x) inherits(x, "haven_labelled"), ~ haven::as_factor(.))

datos %>%  names()

datos %>%  select(mort,perdua_seguiment)
datos$Tipus_descompensacióSIMPLIFICADA
datos$HDAxHTP
datos$PBE_seguimentPostAlta
datos$te_ascites
