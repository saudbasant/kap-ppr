#Load required packages----
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
KAP_new1 <- KAP_new |> 
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
KAP_processed <- KAP_new1 |> 
  select(1:4,106,5:12,107:109, everything())

names(KAP_processed)

#Save processed data----
write_csv(KAP_processed,here("clean_data/KAP_processed.csv"))

