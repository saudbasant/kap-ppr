#Load requied packages----
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here, tidyverse, gt, gtsummary, cardx, easystats, broom.helpers,
               metan)


#import data----
raw_KAP <- read_csv(here("clean_data/KAP_csv.csv"))


#Data wrangling----
#Check data
glimpse(raw_KAP)


#Add age category
KAP_new <- raw_KAP |> 
  mutate(age_group = case_when(Age < 30 ~ "18-29 years",
                              Age >= 30 & Age <= 45 ~ "30-45 years",
                              Age > 45 ~ ">45 years"))

#Add flock size
KAP_new <- KAP_new |> 
  mutate(flock_size = Number_of_goats_in_herd + Number_of_sheep_in_herd,
         flock_size_group = case_when(flock_size <=3 ~ "1 to 3",
                                      flock_size > 3 & flock_size <= 10 ~ "4 to 10",
                                      flock_size > 10  ~ ">10"),
   animal_ownership = case_when(
    Number_of_goats_in_herd != 0 & Number_of_sheep_in_herd == 0 ~ "Goat",
    Number_of_goats_in_herd == 0 & Number_of_sheep_in_herd != 0 ~ "Sheep",
    Number_of_goats_in_herd != 0 & Number_of_sheep_in_herd != 0 ~ 
      "Sheep and Goat"))
 

  names(KAP_new)

#Rearrange columns
KAP_new2 <- KAP_new |> 
  select(1:4,106,5:12,107:109, everything())

names(KAP_new2)

#Demographic----
KAP_new2 |> 
  select(2,5:9, 11,12,15,16,90,91 ) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Socio-demographic characteristics**") |>
  modify_caption("**TABLE 1. Socio-demographic characteristics of farmers (n = 309)**") |> 
  as_gt() |> 
  gtsave(here("Tables/demographics.docx"))

names(KAP_new2)


#KAP Descriptive----
#Knowledge descriptive
KAP_new2 |>
  select(47:50, 58:65) |> #position of the variables changed now due to rearrangement
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Knowledge questions**") |> 
  as_gt() |> 
  gtsave(here("Tables/Knowledge_descriptive.docx"))


#Attitude descriptive
KAP_new2 |>
  select(66:76) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Attitude questions**") |> 
  as_gt() |> 
  gtsave(here("Tables/Attitudes_descriptive.docx"))


#Practice descriptive
KAP_new2 |>
  select(77, 80:87) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Practice questions**") |> 
  as_gt() |> 
  gtsave(here("Tables/Practices_descriptive.docx"))

#Data wrangling for KAP level----
#Save the processed csv file
write_csv(KAP_new2, here("Data/KAP/KAP_csv2_(processed).csv"))


#Import coded data
KAP_coded <- read_csv(here("Data/KAP/KAP_csv3_(processed_coded).csv"))


#categorize KAP level
KAP_csv4_coded_final <- KAP_coded |> 
  mutate(Knowledge_level = case_when(knowledge_score_PCT < 75 ~ "Poor",
                                     knowledge_score_PCT >= 75 ~ "Good"),
         attitude_level = case_when(attitude_score_PCT < 75 ~ "Poor",
                                    attitude_score_PCT >= 75 ~ "Good"),
         practice_level = case_when(practice_score_PCT < 75 ~ "Poor",
                                    practice_score_PCT >= 75 ~ "Good"))

#Save the categorized data
write_csv(KAP_csv4_coded_final, here("Data/KAP/KAP_csv4_(coded_final).csv"))

names(KAP_csv4_coded_final)

##KAP level(binary)
KAP_csv4_coded_final |> 
  select(110:112) |> 
  tbl_summary(digits = everything() ~ 2) |>
  add_ci() |>
  as_gt() |> 
  gtsave(here("Tables/KAP_level_binary.docx"))

  
  
  

#Data wrangling for logistic regression----
#First convert the binary outcomes into codes
  KAP_csv5 <- KAP_csv4_coded_final |> 
    mutate(Knowledge_level_binary_code = case_when(Knowledge_level == "Good" ~ '1',
                                                   Knowledge_level == "Poor" ~ '0'),
          Attitude_level_binary_code = case_when(attitude_level == "Good" ~ '1',
                                                 attitude_level == "Poor" ~ '0'),
          Practice_level_binary_code = case_when(practice_level == "Good" ~ "1",
                                                 practice_level == "Poor" ~ "0"))

  
  #Convert character variables to factor
  KAP_csv5_final <- KAP_csv5 |> 
    mutate(across(where(is.character), as.factor))
  
  str(KAP_csv5_final)
  
  #Export the processed excel sheet
  write_csv(KAP_csv5_final, here("Data/KAP/KAP_csv5_(final).csv"))

names(KAP_csv5_final)

#Univariate logistic regression: Outcome must be binary, Predictor only one---- 
#Factors associated with good knowledge in the univariate logistic regressions analysis
  ##Univariate model
uv_tbl <- KAP_csv5_final |>
    select(5,7:9,11:17, Knowledge_level_binary_code) |>
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
                 Sheep_goat_rearing_experience + Sheep_goat_rearing_system +
                 Annual_income_from_sheep_goat_farming + flock_size_group +
                 Received_awareness_about_PPR_previously + 
                 Lost_shep_goat_by_PPR_last_year, 
                 data = KAP_csv5_final,
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
    gtsave(here("Tables/Factors_affecting_knowledge_UVreg.docx"))





##Regression attitude----
# Factors associated with good attitudein the univariate logistic regressions analysis
##Univariate model
uv_tbl <- KAP_csv5_final |>
  select(5,7:9,11:17, Attitude_level_binary_code) |>
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

report_model(uv_tbl)


#multivariate logistic regression (outcome=binary or categorical)
mv_logR <- glm(Attitude_level_binary_code ~ age_group + Education_level +  
                 Family_size + Religion + Sheep_goat_rearing_experience + 
                 Sheep_goat_rearing_system + Annual_income_from_sheep_goat_farming + 
                 flock_size_group + animal_ownership +
                 Received_awareness_about_PPR_previously + 
                 Lost_shep_goat_by_PPR_last_year, 
               data = KAP_csv5_final,
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
  gtsave(here("Tables/Factors_affecting_attitude_UVreg.docx"))





##Regression practice----
# Factors associated with good practice in the univariate logistic regressions analysis
##Univariate model
uv_tbl <- KAP_csv5_final |>
  select(5,7:9,11:17, Practice_level_binary_code) |>
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
mv_logR <- glm(Practice_level_binary_code ~ Education_level +  
                 Religion + Sheep_goat_rearing_experience + 
                 Annual_income_from_sheep_goat_farming + 
                 Received_awareness_about_PPR_previously + 
                 Lost_shep_goat_by_PPR_last_year, 
               data = KAP_csv5_final,
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
  gtsave(here("Tables/Factors_affecting_practice_UVreg.docx"))





#Correlation between KAP----
# import data
data <- read_csv(here("Data/KAP/KAP_csv5_(final).csv")) #check.names = FALSE, to avoid dots in between variable names

names(data)

#create data for correlation
corr_data <-data |> 
  select(62,75,85)

view(corr_data)

#calculate corr coefficient and store the output in a variable
M <- corr_coef(corr_data)

#plot the output
plot(M)


# Save a plot as TIFF
ggsave("R/SAURES_2023//FMD/Figures/correlation_plotFMD.tiff", units="in", 
       width=5, height=2, dpi=900, compression = 'lzw')

ggsave("R/SAURES_2023/FMD/Figures/correlation_plotFMD.tiff", 
       width = 10, height = 6, dpi = 300, units = "in")
dev.off()

#export the correlation chart as png
png(filename='Figures/correlation_plot.png', res=1200, units = 'cm',
    width = 15, height = 10)
plot(M)
dev.off() #to close the export process