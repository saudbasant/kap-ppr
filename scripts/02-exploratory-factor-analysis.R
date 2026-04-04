#load packages----
source("scripts/00-setup.R")
library(DataExplorer)  #used to explore data
library(psych)
library(GPArotation)
library(janitor)  #for instant summarisation of observations


#Import data
data_processed <- read_csv(here("data/processed-339/processed339.csv"))

#Structural integrity check
any(duplicated(data_processed))
str(data_processed)
summary(data_processed)

#Range and outliers check
boxplot(data_processed$`goat numbers`)

#Create raw reports
create_report(data_codeded)  #Creates extensive report of whole data
plot_missing(data)     #plots missing data

# Compare mean knowledge score by location (here by Upazila)

aggregate(knowledge_score ~ Upazila, data = data_codeded, mean)
boxplot(knowledge_score ~ Upazila, data = data_codeded)


#import coded data
data <- read_csv(here("data/processed-339/coded339.csv"))


#KNOWLEDGE-DOMAIN-QUALITY-CHECK----
#select knowledge dataframe
knowl_dat <- data |> select(20:32)

#calculate item difficulty index
# Calculate proportion of correct responses for each item
item_difficulty <- colMeans(knowl_dat, na.rm = TRUE)

# Display results
item_difficulty


#Reliability test
alpha(knowl_dat)


# Knowledge score diagnostics
#Distribution visualization
hist(data$Knowledge_score)
boxplot(data$Knowledge_score)
plot(density(data$Knowledge_score))

#Normality-test (important for modelling choice)
# This determines parametric vs non-parametric vs count models.
shapiro.test(data$Knowledge_score)

#Interpretation
# Shapiro p-value:
# p > 0.05 → approximately normal
# p < 0.05 → non-normal distribution
# But remember. Large sample → almost always non-normal.

plot_qq(data$Knowledge_score)   #Quantile-Quantile plot


#Dispersion assessment of knowledge score:used to select regression model
#mean
mean(data$Knowledge_score)  #output: 4.3

#variance
var(data$Knowledge_score)  #output 12.27
       #Interpretation: Variance > mean → overdispersion 
      # This means:
      #  Poisson assumption (mean = variance) is violated 
      # •	Using Poisson regression will: 
      #   o	Underestimate standard errors 
      # 	Inflate Type-I error 
      # 	Produce misleading significance 
      # Therefore:
      #   ❗ Poisson regression is NOT appropriate.
      # Correct modelling choice for knowledge score
      # Because:
      #   Count outcome 
      # 	Right-skewed distribution 
      # 	Strong overdispersion 
      # The correct primary model is:
      #   ✅ Negative Binomial Regression


# Calculate the median knowledge score
median_knowl <- median(data$`Knowledge score`)
median_knowl
#median = 4, it will be used as cut-off value to categorise the knowledge scores into good and poor


#Create categorical attitude level variable
data_knowl_bin <- data |> 
  mutate(knowledge_level = case_when(
    `Knowledge score` >= 4 ~ "adequate",
    `Knowledge score` < 4 ~ "poor"))


# Create numeric binary variable (useful for logistic regression)
data_knowl_bin_coded <- data_knowl_bin |> 
  mutate(knowledge_level_coded = case_when(
    knowledge_level == "adequate" ~ "1",
    knowledge_level == "poor" ~ "0"))


#visualize the knowledge category distribution
data_knowl_bin_coded |> 
  tidyplot(x = knowledge_level) |> 
  add_count_bar() |> 
  add_count_value()



#Summary using janitor package  
data_knowl_bin_coded |> 
  tabyl(knowledge_level)





#ATTITUDES-DOMAIN-QUALITY-CHECK----
#select attitudes data frame
#total questions 11 # 3 points likert s0cring is done: 3 for agree, 2 for neutral,
# 1 for disagree
attit_dat <- data |> select(35:45)



#Attitude-scale-validation
##Step1-construct-validity
###Pearson-based-EFA/Classical Factor Analysis/
  #Computes Pearson correlation matrix among items.
    # 'Meaning it assumes:
    # Data are continuous
    # Distance between 1 → 2 = distance between 2 → 3
    # Variables follow approximate normal distribution
  #Then factor analysis is performed on this Pearson correlation matrix.

#KMO-test
KMO(attit_dat)

#Bartlett-test
cortest.bartlett(attit_dat)

# Determine number of factors
fa.parallel(attit_dat, fa = "fa")



###Polychoric correlation based EFA
#Suitable for 3 points likert
#More accurate for ordinal psychometric data
#Report: “Exploratory factor analysis was performed using polychoric correlations appropriate for ordinal Likert-type items.”

#Polychoric correlation
poly_mat <- polychoric(attit_dat)$rho

#check KMO
KMO(poly_mat)

#Bartlett test
cortest.bartlett(poly_mat, n = 339)

# Determine number of factors
fa.parallel(poly_mat, n.obs = 339, fa = "fa")

#EFA: Model 1. One-factor solution
fa(poly_mat,
   nfactors = 1,
   rotate = "none",
   fm = "ml",
   n.obs = 339)

#EFA: Model 2. three-factor solution
fa(poly_mat,
   nfactors = 3,
   rotate = "varimax",
   fm = "ml",
   n.obs = 339)


##Step2-Reliability testing
#Cronbach alpha: shows internal consistency 
alpha(attit_dat)
#Additional test: McDonald omega


#Distribution assessment of total attitude scores
# Visual assessment:Identify skewness, floor or ceiling effects.
hist(data$Attitude_score)
boxplot(data$Attitude_score)
plot(density(data$Attitude_score))

#Normality-test (important for modelling choice)
# This determines parametric vs non-parametric vs count models.
shapiro.test(data$Attitude_score)

#Interpretation
# Shapiro p-value:
# p > 0.05 → approximately normal
# p < 0.05 → non-normal distribution
# But remember. Large sample → almost always non-normal.

plot_qq(data$Attitude_score)   #Quantile-Quantile plot



# Calculate the median attitude score
median_att <- median(data$`Attitude score`)
median_att
    #median = 27, it will be used as cut-off value to categorise the attitude scores into positive and negative

#Create categorical attitude level variable
data_att_bin <- data |> 
  mutate(attitude_level = case_when(
                `Attitude score` >= 27 ~ "positive",
                `Attitude score` < 27 ~ "negative"))
       

# Create numeric binary variable (useful for logistic regression)
data_att_bin_coded <- data_att_bin |> 
  mutate(attitude_level_coded = case_when(
    attitude_level == "positive" ~ "1",
    attitude_level == "negative" ~ "0"))



#visualize the attitude category distribution
data_att_bin_coded |> 
  tidyplot(x = attitude_level) |> 
  add_count_bar() |> 
  add_count_value()



#Summary using janitor package  
data_att_bin_coded |> 
  tabyl(attitude_level)



#PRACTICES-DOMAIN-QUALITY-CHECK----
#select practices dataframe
prac_dat <- data |> select(51:57)

#calculate item difficulty index
# Calculate proportion of correct responses for each item
item_difficulty <- colMeans(prac_dat, na.rm = TRUE)

# Display results
item_difficulty


#Reliability test
alpha(prac_dat)


# Knowledge score diagnostics
#Distribution visualization
hist(data$Practice_score)
boxplot(data$Practice_score)
plot(density(data$Practice_score))

#Normality-test (important for modelling choice)
# This determines parametric vs non-parametric vs count models.
shapiro.test(data$Practice_score)

#Interpretation
# Shapiro p-value:
# p > 0.05 → approximately normal
# p < 0.05 → non-normal distribution
# But remember. Large sample → almost always non-normal.

plot_qq(data$Practice_score)   #Quantile-Quantile plot


#Dispersion assessment of knowledge score
#mean
mean(data$Practice_score)  #output: 1.799

#variance
var(data$Practice_score)  #output 2.421


#proportion of farmers with 0 practice score
mean(data$`Practice score` == 0)
#If 20–25% farmers have score = 0
# Then:
#   👉 Consider Zero-inflated Negative Binomial
# Otherwise:
#   👉 Standard Negative Binomial is sufficient.
# 



# Calculate the median knowledge score
median_knowl <- median(data$`Practice score`)
median_knowl
#median = 4, it will be used as cut-off value to categorise the knowledge scores into good and poor

#Create categorical attitude level variable
data_knowl_bin <- data |> 
  mutate(knowledge_level = case_when(
    `Knowledge score` >= 4 ~ "adequate",
    `Knowledge score` < 4 ~ "poor"))


# Create numeric binary variable (useful for logistic regression)
data_knowl_bin_coded <- data_knowl_bin |> 
  mutate(knowledge_level_coded = case_when(
    knowledge_level == "adequate" ~ "1",
    knowledge_level == "poor" ~ "0"))


#visualize the knowledge category distribution
data_knowl_bin_coded |> 
  tidyplot(x = knowledge_level) |> 
  add_count_bar() |> 
  add_count_value()


#Summary using janitor package  
data_knowl_bin_coded |> 
  tabyl(knowledge_level)



#KAP-SCORES-DESCREPTIVE-SUMMARY----
#Convert character or factor data type to numeric data: 
data2 <- data |>
  mutate(
    across(
      c(`Knowledge score`, `Attitude score`, `Practice score`),
      ~ as.numeric(as.character(.x))
    )
  )

#Force the data types to continuous
#Gtsummary treats a numeric data frame as categorical it it has fewer unique observations
vars <- c("Knowledge score", "Attitude score", "Practice score")

#Create mean summary
t_mean <- data2 |>
  select(all_of(vars)) |>
  tbl_summary(
    type = list(
      `Knowledge score` ~ "continuous",
      `Attitude score` ~ "continuous",
      `Practice score` ~ "continuous"
    ),
    statistic = all_continuous() ~ "{mean} ± {sd}",
    digits = all_continuous() ~ 2
  ) |>
  modify_header(stat_0 ~ "**Mean ± SD**")

#Create median summary
t_median <- data2 |>
  select(all_of(vars)) |>
  tbl_summary(
    type = everything() ~ "continuous",
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    digits = all_continuous() ~ 2
  ) |>
  modify_header(stat_0 ~ "**Median (IQR)**")

#Create range summary
t_range <- data2 |>
  select(all_of(vars)) |>
  tbl_summary(
    type = everything() ~ "continuous",
    statistic = all_continuous() ~ "{min}–{max}",
    digits = all_continuous() ~ 2
  ) |>
  modify_header(stat_0 ~ "**Range**")

#merge summary tables
tbl_merge(
  tbls = list(t_mean, t_median, t_range)
) |>
  bold_labels() |>
  as_gt() |> 
  gtsave(here("outputs/tables/KAPs-summary.docx"))









#VISUALIZATION-OF-KAP-SCORES----
# Convert wide data to long format for ggplot
#Creates two columns/long format: this format is required for combined plotting/.
kap_long <- data %>%
  select(Knowledge_score, Attitude_score, Practice_score) %>%
  pivot_longer(cols = everything(),
               names_to = "Domain",
               values_to = "Score")

#create plot
ggplot(kap_long, aes(x = Domain, y = Score, fill = Domain)) +
  geom_boxplot(width = 0.6, alpha = 0.7) +
  labs(title = "Distribution of Knowledge, Attitude and Practice Scores",
       x = "KAP Domain",
       y = "Score") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


ggplot(kap_long, aes(x = Domain, y = Score, fill = Domain)) +
  geom_boxplot(width = 0.5, alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  labs(title = "Distribution of KAP Scores",
       x = "Domain",
       y = "Score") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")



ggplot(kap_long, aes(x = Domain, y = Score)) +
  geom_boxplot(fill = "lightblue", width = 0.6) +
  labs(title = "Distribution of KAP Scores",
       x = "",
       y = "Score") +
  theme_bw(base_size = 14)


#violin plot
ggplot(kap_long, aes(x = Domain, y = Score, fill = Domain)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.2, size = 0.8) +
  labs(title = "Distribution of Knowledge, Attitude and Practice Scores",
       x = "",
       y = "Score") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")


#Scores standardization
data$knowledge_z <- scale(data$`Knowledge score`)
data$attitude_z  <- scale(data$`Attitude score`)
data$practice_z  <- scale(data$`Practice score`)

#Convert to numeric vector

data$knowledge_z <- as.numeric(data$knowledge_z)
data$attitude_z  <- as.numeric(data$attitude_z)
data$practice_z  <- as.numeric(data$practice_z)

# Create long dataset for plotting
kap_long_z <- data %>%
  select(knowledge_z, attitude_z, practice_z) %>%
  pivot_longer(cols = everything(),
               names_to = "Domain",
               values_to = "Zscore")


#plot
ggplot(kap_long_z, aes(x = Domain, y = Zscore, fill = Domain)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA) +
  labs(title = "Standardized Distribution of KAP Scores",
       x = "",
       y = "Standardized score (z)") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")


#VISUALIZATION-OF-KAP-SCORE-PCT----
#select and pivot longer the kap score pct
kap_pct_long <- data |> 
  select(knowledge_pct, Attitude_pct, Practice_pct) |> 
  pivot_longer(cols = everything(),
               names_to = "Domain",
               values_to = "Percentage")
  
# Clean domain labels
kap_pct_long$Domain <- factor(kap_pct_long$Domain,
                              levels = c("knowledge_pct", "Attitude_pct", "Practice_pct"),
                              labels = c("Knowledge score", "Attitude score", "Practice score"))


# Create boxplot
ggplot(kap_pct_long, aes(x = Domain, y = Percentage, fill = Domain)) +
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.shape = 16, outlier.size = 1.5) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = c("#009E73", "#E69F00", "#56B4E9")) +
  labs(x = "",
       y = "Percentage (%)") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))

#save
ggsave(here("outputs/figures/kap-distribution-boxplot.png"), width = 8, height = 6, 
       dpi = 300)


ggsave(here("outputs/figures/kap-distribution-boxplot2.jpeg"), width = 8, height = 6, 
       dpi = 300)





