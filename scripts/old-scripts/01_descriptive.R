#Load required packages----
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here, tidyverse, gt, gtsummary, cardx, easystats, broom.helpers)


#Import data
KAP_processed <- read_csv(here("clean_data/KAP_processed.csv"))
names(KAP_processed)

#Descriptive Demographic----
KAP_processed |> 
  select(1:14) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Socio-demographic characteristics**") |>
  modify_caption("**TABLE 1. Socio-demographic characteristics of farmers (n = 309)**") |> 
  as_gt() |> 
  gtsave(here("tables/demographics338.docx"))



#KAP Descriptive----
#Knowledge descriptive
KAP_processed |>
  select(15:26) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Knowledge questions**") |> 
  as_gt() |> 
  gtsave(here("tables/Knowledge_descriptive338.docx"))


names(KAP_processed)
#Attitude descriptive
KAP_processed |>
  select(27:37) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Attitude questions**") |> 
  as_gt() |> 
  gtsave(here("tables/Attitudes_descriptive.docx"))


#Practice descriptive
KAP_processed |>
  select(38:46) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Practice questions**") |> 
  as_gt() |> 
  gtsave(here("tables/Practices_descriptive.docx"))
