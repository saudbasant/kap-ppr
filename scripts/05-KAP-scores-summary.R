#load packages----
source("scripts/00-setup.R")

#Import data
data_codeded <- read_csv(here("data/processed-339/coded339.csv"))
names(data_codeded)
str(data_codeded)

#Check distribution of Knowledge scores
hist(data_codeded$knowledge_PCT)
# Distribution = right-skewed (many low values, long tail to high)
# Best reporting:  Median (IQR)
# You may also report mean ± SD as supplementary

hist(data_codeded$attitude_PCT)
# Distribution = left-skewed but clustered at high values (ceiling effect)
# Here both mean and median will be high and similar.
# Best reporting:  Mean ± SD is acceptable
# Median (IQR) can also be reported
# Either is fine.

hist(data_codeded$practice_PCT)
# Distribution = strongly right-skewed (many very low scores)
# Best reporting:  Median (IQR) is strongly recommended
# Mean alone will be misleading

#Convert to numeric data
data_codeded <- data_codeded |>
  mutate(
    across(
      c(knowledge_PCT, attitude_PCT, practice_PCT),
      ~ as.numeric(as.character(.x))
    )
  )

#check structure
str(data_codeded[, c("knowledge_PCT",
                     "attitude_PCT",
                     "practice_PCT")])


#calculate KAP domain mean, median
data_codeded |>
  select(knowledge_PCT, attitude_PCT, practice_PCT) |>
  ## Create a formatted summary table using the gtsummary package
  tbl_summary(                                     
    # Define how each variable should be treated statistically
    # "continuous2" allows displaying MULTIPLE statistics rows per variable
    # (unlike "continuous" which shows only one row)
    type = list(
      knowledge_PCT ~ "continuous2",
      attitude_PCT  ~ "continuous2",
      practice_PCT  ~ "continuous2"
    ),
    # Define which statistics to display, and in what format
    # This applies to all continuous variables (matched by all_continuous())
    statistic = all_continuous() ~ c(
        # Row 1: Median with interquartile range (non-parametric summary)
       "{median} ({p25}, {p75})",
       # Row 2: Mean ± standard deviation (parametric summary)
       "{mean} ± {sd}"
    )
  ) |> 
  as_gt() |> 
#Save the table
gtsave(here("outputs/tables/KAPs_averages.docx"))












