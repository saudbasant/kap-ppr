#load packages----
source("scripts/00-setup.R")


#import data----
data_coded <- read_csv(here("data/processed-339/coded339.csv"))
is.na(data_coded)
vis_miss(data_coded)
#categorize KAP level
kap_coded_binary <- kap_coded |> 
  mutate(knowledge_level = case_when(knowledge_pct < 75 ~ "Poor",
                                     knowledge_pct >= 75 ~ "Good"),
         attitude_level = case_when(attitude_pct < 75 ~ "Poor",
                                    attitude_pct >= 75 ~ "Good"),
         practice_level = case_when(practice_pct < 75 ~ "Poor",
                                    practice_pct >= 75 ~ "Good"))

#Save the categorized data
write_csv(kap_coded_binary, here("clean_data/kap_coded_binary.csv"))

names(kap_coded_binary)

##KAP level(binary)
kap_coded_binary |> 
  select(53:55) |> 
  tbl_summary(digits = everything() ~ 2) |>
  add_ci() |>
  as_gt() |> 
  gtsave(here("tables/KAP_level_binary.docx"))
