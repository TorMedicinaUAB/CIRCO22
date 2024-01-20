pacman::p_load('haven','survival')
source('Scripts/Competence_Analysis/0_Competing_dataset_creation.R')

Competing_dataset %>%  writexl::write_xlsx("Competing_dataset.xlsx")
# Assuming 'datos' is your dataframe containing the variables


Competing_dataset_2 <- Competing_dataset %>%  mutate_at(vars(-starts_with('Temps')),as.factor )

Competing_dataset_2 %>%  select(Comp_Descompensacio , Temps_Comp_Descompensacio )


# Surv(
#   Competing_dataset_2$Temps_Comp_TH, 
#   event = Competing_dataset_2$Competing_TH==1,
#   type = 'interval')
# Fit Cox proportional hazards model with competing risks
cox_model <- coxph(Surv(Temps_Comp_Descompensacio, as.double(Comp_Descompensacio)) ~ Grup_IQ, data = Competing_dataset_2)
cox_model <- coxph(Surv(Temps_Descompensacio, as.double(Descompensacio)) ~ Grup_IQ, data = Competing_dataset_2)
# Obtain parameter estimates
PE <- coef(cox_model)

# Obtain Type 3 ANOVA
Type3 <- anova(cox_model, test = "Chisq")

# Obtain Hazard Ratios
HR <- exp(coef(cox_model))

# Print the results
print(PE)   # Parameter estimates
print(Type3) # Type 3 ANOVA
print(HR)    # Hazard Ratios

SURV

# Here, Surv(DeathCompeteTM, DeathCompeteYN == 1) indicates a survival object where:
#   - DeathCompeteTM is the time variable.
#   - DeathCompeteYN == 1 represents the event of interest (competes with other events).
