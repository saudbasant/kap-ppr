#Load required packages----
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here,
               tidyverse,
               gt,
               gtsummary)


#Import data
data <- read_csv(here("clean_data/multiple_res_signs.csv"))

## Code to import sheet in xlsx format
MR <- read_xlsx('Clean_data/FMD_KAP_Farmers.xlsx', sheet = 7)

#data structure
str(data)

# Calculate frequency and percentage
frequency <- colSums(data[-1])   # Exclude the first column (if it's a label column)
percentage <- colMeans(data[-1]) * 100

# Create a summary table
summary_table <- data.frame(
  Option = names(frequency),
  Frequency = frequency,
  Percentage = round(percentage, 2)  # Round percentages to 2 decimal places
)

print(summary_table)  #run this after running tbl_summary function


# Create a gtsummary table
summary_table <- tbl_summary(
  data[-1],  # Exclude the first column
  statistic = list(all_categorical() ~ "{n} ({p}%)"),  # Frequency and percentage
  digits = all_categorical() ~ 2 ) |>                  # Round percentages to 2 decimal places
  add_ci() |> 
  as_gt() |> 
  gtsave(here("tables/clinical_signs_summary.docx"))
