#Load requied packages----
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here, tidyverse, likert, ggpubr, ggthemes, RColorBrewer)

# import data
data <- read_csv(here("clean_data/KAP_csv.csv"))


# Visualize attitude response 
attitude_df <- data %>% 
  select(63:73) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(across(everything(), ~ fct_relevel(.x, "Agree", "Neutral", "Disagree"))) |> 
  as.data.frame() 


#Export
png(filename='figures/attitude_likert_chart.png', res=900, units = 'cm',
    width = 20, height = 20)

# Plot in likert scale 
plot(likert(attitude_df), 
           ordered = T, 
           group.order = names(attitude_df), 
           center = 2) 

dev.off() #to close the export process