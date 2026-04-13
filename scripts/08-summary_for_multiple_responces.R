#Load required packages----
if(!require("pacman")) intall.packages(pacman)
pacman::p_load(here,
               tidyverse,
               gt,
               gtsummary)


#Import data
data <- read_csv(here("data/processed-339/clinical-signs.csv"))

## Code to import sheet in xlsx format
data <- read_xlsx('data/raw/master_data.xlsx', sheet = 5)

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
tbl_summary(
  data[-1],  # Exclude the first column
  statistic = list(all_categorical() ~ "{n} ({p}%)"),  # Frequency and percentage
  digits = all_categorical() ~ 2 ) |>                  # Round percentages to 2 decimal places
  add_ci() |> 
  as_gt() |> 
  gtsave(here("outputs/tables/clinical_signs_summary.docx"))
