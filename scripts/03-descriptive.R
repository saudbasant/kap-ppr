#load packages----
source("scripts/00-setup.R")
library(DataExplorer)

#Import data
data <- read_csv(here("data/processed-339/processed339.csv"))
names(data)

data <- read_csv(here("data/processed-339/coded339.csv"))

#Descriptive Demographic----
data |> 
  select(3:21,63) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  modify_header(label ~ "**Socio-demographic characteristics**") |>
  modify_caption("**TABLE 1. Socio-demographic characteristics of farmers (n = 339)**") |> 
  as_gt() |> 
  gtsave(here("outputs/tables/socio-demographics.docx"))



#KAP Descriptive----
#Knowledge descriptive
data |>
  select(20:23,25:32) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Knowledge related questions**") |> 
  modify_caption("**TABLE 2. Knowledge of the farmers regarding PPR (n = 339)**") |> 
  as_gt() |> 
  gtsave(here("outputs/tables/Knowledge_summary.docx"))



# Table is new format
library(binom) #To create Wilson Confidence Interval

# store original column order
question_order <- names(data |> select(20:23,25:32))

kap_table <- data |>
  select(all_of(question_order)) |>
  
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  ) |>
  
  # preserve question order
  mutate(question = factor(question, levels = question_order)) |>
  
  count(question, response) |>
  
  group_by(question) |>
  mutate(
    total = sum(n),
    percent = (n/total)*100
  ) |>
  
  mutate(
    ci = binom.confint(n, total, method = "wilson"),
    ci_low = round(ci$lower*100,1),
    ci_high = round(ci$upper*100,1),
    percent = round(percent,2)
  ) |>
  
  mutate(
    Percent_CI = paste0(percent, " (", ci_low, "–", ci_high, ")")
  ) |>
  
  # # sort responses within each question
  # arrange(question, desc(n)) |>
  
  select(question, response, n, Percent_CI)

kap_table |>
  gt(groupname_col = "question") |>
  cols_label(
    response = "Knowledge questions",
    n = "Frequency",
    Percent_CI = "% (95% CI)"
  ) |>
  tab_caption("Table 2. Knowledge of the farmers regarding PPR (n = 339)") |> 
  gtsave(here("outputs/tables/descriptive-wilsonCI.docx"))




names(data)
#Attitude descriptive
data |>
  select(33:43) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Attitude related questions**") |> 
  modify_caption("**TABLE 3. Attitudes of the farmers regarding PPR (n = 339)**") |> 
  as_gt() |> 
  gtsave(here("outputs/tables/Attitudes_summary.docx"))


#Practice descriptive
data |>
  select(44:52) |> 
  tbl_summary(type = everything() ~ "categorical",
              digits = everything() ~ 2,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_ci() |> 
  modify_header(label ~ "**Practice related questions**") |> 
  modify_caption("**TABLE 4. Practices of the farmers regarding PPR (n = 339)**") |> 
  as_gt()|> 
  gtsave(here("outputs/tables/Practices_summary.docx"))











