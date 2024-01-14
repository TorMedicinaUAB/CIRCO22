source('Scripts/Lectura_datos_basales.R')
library('survey')

datos_imputados <- readRDS('Datos/Imputados/Datos_imputados.rds')

datos_imputados_transformados_propensity <- datos_imputados %>% 
  mutate(
    DIabetes = droplevels(DIabetes,c('Metformina', 'Altres','ADO', 'dieta', 'ADO+Insulina','metformina+altres','ADO'))
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
  smd =TRUE)

print(tabWeighted_full, smd = TRUE)

# datos_imputados_propensity %>% 
#   ggplot(aes(standarized_weights, fill= Grup_IQ )) +
#   geom_density()
# 
# 
# datos_imputados_propensity %>% 
#   ggplot(aes(prediciones, fill= Grup_IQ )) +
#   geom_histogram(color='black',alpha=0.8)

