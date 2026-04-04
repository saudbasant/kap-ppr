#load packages----
source("scripts/00-setup.R")


#import data----
data_coded <- read_csv(here("data/processed-339/coded339.csv"))


#Dicotomize-KAP level
kap_coded_binary <- data_coded |> 
  mutate(
    #min 5 correct answers
    knowledge_level = case_when(Knowledge_score < 5 ~ "Poor",
                                Knowledge_score >= 5 ~ "Good"),
    #median-cut-off
    attitude_level = case_when(Attitude_score < 27 ~ "Negative",
                               Attitude_score >= 27 ~ "Positive"),
    #minimum 3 practices adopted
    practice_level = case_when(Practice_score < 3 ~ "Poor",
                               Practice_score >= 3 ~ "Good"))

# #Save the categorized data
# write_csv(kap_coded_ordinal, here("clean_data/kap_coded_binary.csv"))


##KAP level(binary)
kap_coded_binary |> 
  select(59:61) |> 
  tbl_summary(digits = everything() ~ 2) |>
  add_ci() |>
  modify_header(label ~ "**Domain**") |> 
  modify_caption("**KAP level categories**") |> 
  as_gt() |> 
  gtsave(here("outputs/tables/KAP_level_binary.docx"))


#Ordinal KAP level
#Dicotomize-KAP level
kap_coded_ordinal <- data_coded |> 
  mutate(
    #ordinal: 3 scale
    knowledge_level = case_when(Knowledge_score < 5 ~ "Poor",
                                Knowledge_score >= 5 & Knowledge_score < 10 ~"Moderate",
                                Knowledge_score >= 10 ~ "Good"),
    #median-cut-off
    attitude_level = case_when(Attitude_score < 27 ~ "Negative",
                               Attitude_score >= 27 ~ "Positive"),
    #minimum 3 practices adopted
    practice_level = case_when(Practice_score < 3 ~ "Poor",
                               Practice_score >= 3 ~ "Good"))

#summary
kap_coded_ordinal |> 
  select(59:61) |> 
  tbl_summary(digits = everything() ~ 2) |>
  add_ci() |>
  modify_header(label ~ "**Domain**") |> 
  modify_caption("**KAP level categories**") |> 
  as_gt() |> 
  gtsave(here("outputs/tables/KAP_level_domain_specific.docx"))

