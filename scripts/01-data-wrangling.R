#Deletes everything from the current R session workspace
rm(list = ls())  #Never run this inside interactive exploration if you still need objects.

#load packages----
source(here("scripts/00-setup.R")) #Executes all commands inside 00-setup.R


#import data----
raw_KAP <- read_csv(here("data/processed-339/processed339.csv"))
                                         

#Data wrangling----
#Check data
glimpse(raw_KAP)


names(raw_KAP)

#Rearrange columns
KAP_processed339 <- raw_KAP |> 
  select(1,3,2,4:9,12:19,10,11, everything())

names(KAP_processed)

#Remove empty row
KAP_processed339 <- KAP_processed339 |> 
  filter(!if_all(everything(), is.na))

#Save processed data----
write_csv(KAP_processed339,here("data/processed-339/processed339.csv"))
       #This replaced the imported file.

