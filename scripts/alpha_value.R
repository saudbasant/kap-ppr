#Load required packages
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here, tidyverse, psych)

#Load data
data <- read_csv(here("clean_data/KAP_processed.csv"))
names(data)

#Knowledge----
#subset knowledge data
k_data <- data[, 15:27]

#Calculate Cronbach's alpha
psych::alpha(k_data)


#Attitude----
#subset knowledge data
a_data <- data[, 30:40]

#Calculate Cronbach's alpha
psych::alpha(a_data)


#Practice----
#subset knowledge data
p_data <- data[, 43:50]

#Calculate Cronbach's alpha
psych::alpha(p_data)