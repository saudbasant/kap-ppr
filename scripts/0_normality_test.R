#load basic packages
library(gt)
library(gtsummary)
library(tidyverse)
library(readxl)



#import dataset
data <- read.csv("R/SAURES_2023/FMD/Clean_data/Cronbachs_alpha.csv")


#calculate normality of all columns at a time
apply(data, 2, function(column) shapiro.test(column)$p.value)

#nterpret Results
##For the Shapiro-Wilk test:
###p > 0.05: The column is approximately normal.
###p ≤ 0.05: The column is not normal



#Visualize Each Column
##For a more intuitive understanding, plot histograms or Q-Q plots:

par(mfrow = c(2, 2)) # Set 2x2 layout for multiple plots
for (col in colnames(data)) {
  qqnorm(data[[col]], main = paste("Q-Q Plot for", col))
  qqline(data[[col]], col = "red")
}


#Pattern	Interpretation
## Straight diagonal line ->	Data is approximately normal.
## S-shaped curve	->	Indicates heavy or light tails.
## Positive skew (right-skewed)	->	Points rise above the line on the right.
## Negative skew (left-skewed)	->	Points rise above the line on the left.
## Extreme outliers	->	Individual points far from the line.

