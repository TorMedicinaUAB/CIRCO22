source('Scripts/Lectura_datos_basales.R')
library('survey')

datos_imputados <- readRDS('Datos/Imputados/Datos_imputados.rds')


# HCC
# SignesIndirectes_HTP; ve; presenciaCSPH

datos_imputados_transformados <- datos_imputados %>% 
  mutate(
    DIabetes = droplevels(DIabetes,c('Metformina', 'Altres','ADO', 'dieta', 'ADO+Insulina','metformina+altres','ADO')),
    SignesIndirectes_HTP = droplevels(SignesIndirectes_HTP,c('dubtós'))
  ) %>%
  select('Grup_IQ','edat_IQ','sexe_home',"IMC",'etiol_OH','Enol_Actiu',"Charlson_Index","plaquetes_preIQ","DIabetes",'Pughpunts_basal',
         'colaterals_shunts','MELD_basal','Creat_mgdL_preIQ','Alb_gL_preIQ','BB_mgdL_preIQ','INR_preIQ',
         'HCC_prev',
         'SignesIndirectes_HTP' )

datos_imputados_transformados %>% writexl::write_xlsx('datos_imputados.xlsx')



variables_propensity <- c(
 'SignesIndirectes_HTP','HCC_prev','edat_IQ','sexe_home','etiol_OH','IMC','Charlson_Index', 'Pughpunts_basal','Enol_Actiu','DIabetes',
 'colaterals_shunts','MELD_basal','plaquetes_preIQ','Alb_gL_preIQ','INR_preIQ'
)

mod_propensity <- glm(
  formula = Grup_IQ ~  
    SignesIndirectes_HTP + I(edat_IQ^2) +HCC_prev +sexe_home +etiol_OH +log(IMC) +
    log(Charlson_Index) +
    I(Pughpunts_basal^3)+log(Pughpunts_basal) +Enol_Actiu +DIabetes +colaterals_shunts +
    log(MELD_basal) + I(MELD_basal^3) +
    I(plaquetes_preIQ^-3)+
    Alb_gL_preIQ

  , 
  data = datos_imputados_transformados, 
  family=binomial(link="logit") )

datos_imputados_propensity <- datos_imputados %>% 
  mutate(prediciones=predict(mod_propensity,datos_imputados,type="response") )

datos_imputados_propensity <- datos_imputados_propensity %>% 
  mutate(standarized_weights = case_when(
    Grup_IQ == 'Si IQ' ~ (177/(371))/ (  prediciones),
    Grup_IQ == 'No IQ' ~ (194/(371))/ (1-prediciones) 
  )) 


iptwdatos_propensity <- svydesign(
  ids = ~ 1, 
  data = datos_imputados_transformados,
  strata = ~Grup_IQ,
  weights = ~ datos_imputados_propensity$standarized_weights)

Propensity_table_Weighted <- svyCreateTableOne(
  vars= datos_imputados_transformados %>% select(-Grup_IQ) %>% names(),
  strata = "Grup_IQ", 
  data = iptwdatos_propensity, 
  smd =TRUE)

print(Propensity_table_Weighted, smd = TRUE)

# datos_imputados_propensity %>%
#   ggplot(aes(standarized_weights, fill= Grup_IQ )) +
#   geom_density()
# 
# 
# datos_imputados_propensity %>%
#   ggplot(aes(prediciones, fill= Grup_IQ )) +
#   geom_histogram(color='black',alpha=0.8)
