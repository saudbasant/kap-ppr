#Load required packages----
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here, tidyverse, gt, gtsummary, cardx, easystats, broom.helpers)

#Import data
KAP_binary <- read_csv(here("clean_data/kap_coded_binary.csv"))


#Data wrangling for logistic regression----
#First convert the binary outcomes into codes
KAP_reg <- KAP_binary |> 
  mutate(Knowledge_level_binary_code = case_when(knowledge_level == "Good" ~ '1',
                                                 knowledge_level == "Poor" ~ '0'),
         Attitude_level_binary_code = case_when(attitude_level == "Good" ~ '1',
                                                attitude_level == "Poor" ~ '0'),
         Practice_level_binary_code = case_when(practice_level == "Good" ~ "1",
                                                practice_level == "Poor" ~ "0"))


#Convert character variables to factor
KAP_reg_final <- KAP_reg |> 
  mutate(across(where(is.character), as.factor))

str(KAP_reg_final)

#Export the processed excel sheet
write_csv(KAP_reg_final, here("clean_data/KAP_reg_final.csv"))

names(KAP_reg_final)

#Univariate logistic regression: Outcome must be binary, Predictor only one---- 
#Factors associated with good knowledge in the univariate logistic regressions analysis
##Univariate model
uv_tbl <- KAP_reg_final |>
  select(2,4:13, Knowledge_level_binary_code) |>
  tbl_uvregression(
    method = glm,
    y = Knowledge_level_binary_code,
    method.args = list(family = binomial),
    exponentiate = T
  ) |>
  add_global_p() |>  #add overall p value for the whole variable
  bold_p(t = 0.05) |> 
  add_significance_stars(
    hide_p = F, hide_se = F, hide_ci = F) |>  #add stars to significant values
  modify_header(label = "**Predictor**") 

report_model(uv_tbl)


#multivariate logistic regression (outcome=binary or categorical)----
mv_logR <- glm(Knowledge_level_binary_code ~ Education_level +  Family_size + 
                 Religion + Sheep_goat_rearing_experience +
                 Annual_income_from_sheep_goat_farming + flock_size_group +
                 Received_awareness_about_PPR_previously + 
                 Lost_shep_goat_by_PPR_last_year, 
               data = KAP_reg_final,
               family = binomial(link = "logit")) #Select the predictors that have p vaue <= 0.25

report(mv_logR)

mv_tbl <- mv_logR |> 
  tbl_regression(exponentiate = TRUE) |> 
  add_global_p() |>  #add overall p value for the whole variable
  bold_p(t = 0.05) |> 
  add_significance_stars(
    hide_p = F, hide_se = F, hide_ci = F) |> #add stars to significant values
  modify_header(label = "**Predictor**") 


#MERGE tables
tbl_merge(
  tbls = list(uv_tbl, mv_tbl),
  tab_spanner = c("**Univariate**", "**Multivariate**")
) |>  
  as_gt() |>
  gtsave(here("tables/Factors_affecting_knowledge_reg.docx"))





##Regression attitude----
# Factors associated with good attitude in the univariate logistic regressions analysis
##Univariate model
uv_tblA <- KAP_reg_final |>
  select(2,4:13, Attitude_level_binary_code) |>
  tbl_uvregression(
    method = glm,
    y = Attitude_level_binary_code,
    method.args = list(family = binomial),
    exponentiate = T
  ) |>
  add_global_p() |>  #add overall p value for the whole variable
  bold_p(t = 0.05) |> 
  add_significance_stars(
    hide_p = F, hide_se = F, hide_ci = F) |>  #add stars to significant values
  modify_header(label = "**Predictor**") 

report_model(uv_tblA)


#multivariate logistic regression (outcome=binary or categorical)
mv_logR <- glm(Attitude_level_binary_code ~ age_group + Education_level +  
                 Family_size + Religion + Sheep_goat_rearing_experience + 
                 Sheep_goat_rearing_system + Annual_income_from_sheep_goat_farming + 
                 flock_size_group + animal_ownership +
                 Received_awareness_about_PPR_previously + 
                 Lost_shep_goat_by_PPR_last_year, 
               data = KAP_reg_final,
               family = binomial(link = "logit")) #Select the predictors that have p vaue <= 0.25

report(mv_logR)

mv_tbl_A <- mv_logR |> 
  tbl_regression(exponentiate = TRUE) |> 
  add_global_p() |>  #add overall p value for the whole variable
  bold_p(t = 0.05) |> 
  add_significance_stars(
    hide_p = F, hide_se = F, hide_ci = F) |> #add stars to significant values
  modify_header(label = "**Predictor**") 


#MERGE tables
tbl_merge(
  tbls = list(uv_tblA, mv_tbl_A),
  tab_spanner = c("**Univariate**", "**Multivariate**")
) |>  
  as_gt() |>
  gtsave(here("tables/Factors_affecting_attitude_regr.docx"))





##Regression practice----
# Factors associated with good practice in the univariate logistic regressions analysis
##Univariate model
uv_tbl_P <- KAP_reg_final |>
  select(2,4:13, Practice_level_binary_code) |>
  tbl_uvregression(
    method = glm,
    y = Practice_level_binary_code,
    method.args = list(family = binomial),
    exponentiate = T
  ) |>
  add_global_p() |>  #add overall p value for the whole variable
  bold_p(t = 0.05) |> 
  add_significance_stars(
    hide_p = F, hide_se = F, hide_ci = F) |>  #add stars to significant values
  modify_header(label = "**Predictor**") 

report_model(uv_tbl)


#multivariate logistic regression (outcome=binary or categorical)
mv_logR_p <- glm(Practice_level_binary_code ~ age_group +Education_level +  
                 Religion + Sheep_goat_rearing_experience + 
                 Annual_income_from_sheep_goat_farming +
                 Received_awareness_about_PPR_previously + 
                 Lost_shep_goat_by_PPR_last_year, 
                 data = KAP_reg_final,
               family = binomial(link = "logit")) #Select the predictors that have p vaue <= 0.25

report(mv_logR)

mv_tbl_P <- mv_logR_p |> 
  tbl_regression(exponentiate = TRUE) |> 
  add_global_p() |>  #add overall p value for the whole variable
  bold_p(t = 0.05) |> 
  add_significance_stars(
    hide_p = F, hide_se = F, hide_ci = F) |> #add stars to significant values
  modify_header(label = "**Predictor**") 


#MERGE tables
tbl_merge(
  tbls = list(uv_tbl_P, mv_tbl_P),
  tab_spanner = c("**Univariate**", "**Multivariate**")
) |>  
  as_gt() |>
  gtsave(here("tables/Factors_affecting_practice_regr.docx"))
