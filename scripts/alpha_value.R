#load packages----
source("scripts/00-setup.R")


#Load data
dat <- read_csv(here("data/processed-339/coded339.csv"))
names(data)

#Knowledge----
#subset knowledge data
k_data <- dat[, 20:32]

#Calculate Cronbach's alpha
psych::alpha(k_data)


#Attitude----
#subset knowledge data
a_data <- dat[, 35:45]

#Calculate Cronbach's alpha
psych::alpha(a_data)


#Practice----
#subset knowledge data
p_data <- dat[, 48:56]

#Calculate Cronbach's alpha
psych::alpha(p_data)
