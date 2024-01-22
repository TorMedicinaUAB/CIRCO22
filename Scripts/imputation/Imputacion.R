source('Scripts/Lectura_datos_basales.R')

pacman::p_load(mice)

datos_basales %>% 
  select(Grup_IQ, FA_preIQ,FsC_Elastografia,GGT_preIQ,HVPG_basal,INR_preIQ,K_preIQ,Leucos_preIQ,MELD_1anyspostIQ,NA_preIQ_,PCR_preIQ) %>% 
  group_by(Grup_IQ) %>% 
  summarize_all(~sum(is.na(.x)))
  
datos_basales_sin_NA_extremos <- datos_basales %>%
  select(-c(HVPG_basal,K_preIQ,Leucos_preIQ,MELD_1anyspostIQ,NA_preIQ_,PCR_preIQ))

# Creating an imputation model

imputation_model <- mice(
  data = datos_basales_sin_NA_extremos, 
  method = '2l.bin', 
  maxit = 1000, 
  m = 1,
  verbose=F)

# Imputing missing values

imputed_data <- complete(imputation_model) %>% as_tibble()

# imputed_data %>% naniar::vis_miss()
# 
# imputed_data %>%  write_rds(., 'Datos/Imputados/Datos_imputados.rds')
