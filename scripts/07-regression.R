#load packages----
source("scripts/00-setup.R")



#import data
data <- read_csv(here("data/processed-339/coded339.csv"))


#Count the zero knowledge scores
sum(data$Knowledge_score == 0)
#Count the zero knowledge scores
mean(data$Knowledge_score == 0)  #>10–15% zeros → consider zero-inflated models


#convert all predictors to factors
data <- data |> 
  mutate(across(c(
    age, Education_level,
    Family_size, Primary_Income_source, herd_size,
    livestock_ownership, Sheep_goat_rearing_experience, Sheep_goat_rearing_system, 
    Annual_income, received_training, herd_previously_affected, disease_reporting
  ), as.factor))

#District as predictor has been removed.

#Order the ordinal variables
data <- data %>%
  mutate(
    # Age (young → old)
    age = factor(age,
                 levels = c("18-29 years", "30-45 years", "46-70 years")
    ),
    # Education (low → high)
    Education_level = factor(Education_level,
                             levels = c("No formal education", "Primary", "Secondary and above")
    ),
    # Family size (small → large)
    Family_size = factor(Family_size,
                         levels = c("1 to 6", "More than 6")
    ),
    # Income (low → high)
    Annual_income = factor(Annual_income,
                           levels = c("Upto 30,000Tk", "31,000-70,000Tk", "More than 70,000Tk")
    ),
    # Experience (low → high)
    Sheep_goat_rearing_experience = factor(Sheep_goat_rearing_experience,
                                           levels = c("1 to 6 years", "> 6 years")
    ),
    # Herd size (small → large)
    herd_size = factor(herd_size,
                       levels = c("1 to 10", "11 to 32")
    ),
    #disease reporting (Self care -> Veterinarian)
    disease_reporting = factor(disease_reporting,
                               levels = c("self care", "Consult vet technician", 
                               "Consult veterinarian")
  )
)



#create predictor list
predictors <- c(
  "age", "Education_level",
  "Family_size", "Primary_Income_source", "herd_size",
  "livestock_ownership", "Sheep_goat_rearing_experience", "Sheep_goat_rearing_system", 
  "Annual_income", "received_training", "herd_previously_affected", "disease_reporting"
)



##KNOWLEDGE-REGRESSION----
#Bivariate-screening
#A.Using traditional non-parametric tests for categorical variables----
#summary function for median and IQR calculation
get_summary <- function(data, var, outcome) {
  data %>%
    group_by(.data[[var]]) %>%
    summarise(
      median = median(.data[[outcome]], na.rm = TRUE),
      Q1 = quantile(.data[[outcome]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[outcome]], 0.75, na.rm = TRUE),
      n = n(), #add sample size per category
      .groups = "drop"
    ) %>%
    mutate(
      Category = as.character(.data[[var]]),
      
      Summary = sprintf("%.0f (%.0f–%.0f), n=%d", median, Q1, Q3, n),
  
      Variable = var
    ) %>%
    dplyr::select(Variable, Category, Summary)
}

#apply to all predictors/calculate median and IQR for each category
summary_table <- map_dfr(predictors, ~get_summary(data, .x, "Knowledge_score"))


#Bivariate screening function
# For Knowledge (non-parametric because the knowledge score is right skewed)
bivariate_nonparam <- function(outcome, predictors, data) {
  purrr::map_dfr(predictors, function(var) {
    
    formula <- as.formula(paste(outcome, "~", var))
    
    test <- if (nlevels(data[[var]]) == 2) {
      wilcox.test(formula, data = data)
    } else {
      kruskal.test(formula, data = data)
    }
    
    tibble::tibble(
      Variable = var,
      P_value = test$p.value
    )
  })
}


#Run bivariate analysis
# Knowledge
biv_knowledge <- bivariate_nonparam("Knowledge_score", predictors, data)



#Add p values to the bivariate screening table
pvals <- biv_knowledge %>%
  mutate(
    p_value = ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))
  ) %>%
  dplyr::select(Variable, p_value)

#Merge the summary table of median(IQR) with p-value table
final_table <- summary_table %>%
  left_join(pvals, by = "Variable") %>%
  group_by(Variable) %>%
  mutate(p_value = ifelse(row_number() == 1, p_value, "")) %>%
  ungroup()


#gt formatting of the final table
final_table %>%
  gt(groupname_col = "Variable") %>%
  cols_label(
    Category = "Category",
    Summary = "Knowledge score (median, IQR)",
    p_value = "p-value"
  ) %>%
  tab_header(
    title = "Bivariate Analysis of Factors Associated with Knowledge Score"
  ) |>
  tab_source_note(
    source_note = "Values are median (IQR). P-values from Mann–Whitney U test (2 groups) or Kruskal–Wallis test (>2 groups)."
  ) |> 
  gtsave(here("outputs/tables/knowledge-bivariate2.docx"))


#select variables from bivariate screening with P < 0.20
vars_knowledge <- biv_knowledge$Variable[biv_knowledge$P_value < 0.20]

#add theory driven variables
# Always include these
key_vars <- c("Education_level", "received_training")

# Knowledge model
vars_knowledge <- unique(c(vars_knowledge, key_vars))

#build formula
form_knowledge <- as.formula(
  paste("Knowledge_score ~", paste(vars_knowledge, collapse = " + "))
)


#Check multicollinearity (before final models)
library(car)
# Temporary linear models
vif(lm(form_knowledge, data = data))


# Fit negative binomial model
library(MASS) # Regression models (negative binomial)
model_knowledge <- glm.nb(form_knowledge, data = data)


#B.Using Modern Method (Univariate Regression)----
# # Univariate regression (using your tbl_uvregression code) is the better choice 
# for screening because it directly aligns with the multivariate model you plan to run.
# # While Mann-Whitney and Kruskal-Wallis are great for testing differences in medians, 
# using regression for screening is more consistent for the following reasons:
# # 1. Direction and Magnitude
#  Regression provides coefficients and 95% Confidence Intervals, which tell you 
#   the direction (positive or negative) and the strength of the association.
# Non-parametric tests only provide a p-value, which tells you if a difference
# exists, but not how much.
# # 2. Consistency of Assumptions
# # Since you are screening for a Multivariate Linear Regression (or a GLM), using 
# a Bivariate Linear Regression ensures that the screening process uses the 
# same underlying logic as your final model.
# # 3. Handling Skewness in Screening

# Univariate negative binomial regression (bivariate screening)
library(MASS)

univariate_knowledge <- tbl_uvregression(
  data = data,
  method = MASS::glm.nb,
  y = Knowledge_score,
  include = all_of(predictors),
  exponentiate = TRUE   # converts coefficients to IRR
) |>
  add_global_p() |>
  bold_p() |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)
  ) |>
  modify_header(
    label = "**Predictor**",
    estimate ~ "**IRR (95% CI)**",
    p.value ~ "**p-value**"
  )


# Select predictors with p < 0.20
vars_p20_knowledge <- univariate_knowledge$table_body |>
  dplyr::filter(row_type == "label") |>
  dplyr::filter(p.value < 0.20) |>
  dplyr::pull(variable) |>
  unique()

#PROCEED-to-MULTIVARIATE ANALYSIS----
#Based on the assessment of knowledge score: right skewed, over-dispersion, selected
#model for regression is Negative Binomial Regression

#Negative Binomial Regression table (Traditional)----
tbl_regression(
  model_knowledge,
  exponentiate = TRUE
) |>
  add_global_p() |>
  bold_p() |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)   # prevents reference rows from showing parentheses
  ) |>
  modify_header(
    estimate ~ "IRR (95% CI)",
    p.value ~ "Global p-value"
  ) |>
  modify_caption(
    "Factors Associated with Knowledge Score"
  ) |>
  as_gt() |> 
  gtsave(here("outputs/tables/knowledge-regression-nb-global-p(no-district).docx"))


#Forest-plot-of-the-regression-output  
library(broom)
tidy(model_knowledge, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    x = "Adjusted IRR (95% CI)",
    y = "Predictors"
  ) +
  theme_minimal()  




# Zero Inflated Negative Binomial Reg Model----
library(pscl)

zinb_model <- zeroinfl(
  Knowledge_score ~ District + Education_level + Family_size +
    Primary_Income_source + herd_size + Sheep_goat_rearing_experience +
    Sheep_goat_rearing_system + Annual_income + received_training +
    herd_previously_affected + disease_reporting |
    Education_level + Sheep_goat_rearing_experience + received_training + 
    herd_previously_affected + disease_reporting,
  data = data,
  dist = "negbin"
)
# Left side = count model (Knowledge score ~ predictors)
# (Variables were selected based on 0.2 p-value cutoff from bivariate screening)
# Right side = zero model (Probability of zero knowledge ~ predictors)
# Selected variables are theoretical predictors of zero knowledge, e.g. education, training, disease exposure

#Examine model output
summary(zinb_model)
#Outputs
# Count model coefficients
# Zero-inflation model coefficients



# Build regression table

# ---- COUNT MODEL (IRR)
tbl_count <- tbl_regression(
  zinb_model,
  exponentiate = TRUE,
  intercept = FALSE,
  tidy_fun = broom.helpers::tidy_zeroinfl,
  component = "conditional" #count model
) |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)   # prevents reference rows from showing parentheses
  ) |>
  modify_header(
    estimate ~ "IRR (95% CI)",
    p.value ~ "p-value"
  ) |>
  modify_caption(
    "Determinants of Knowledge Score (Zero-Inflated Negative Binomial Model)"
  ) |> 
  as_gt() |> 
  gtsave(here("outputs/tables/knowledge-regression-zinb-count-model.docx"))



# ---- ZERO-INFLATION MODEL (OR)
tbl_zero <- tbl_regression(
  zinb_model,
  exponentiate = TRUE,
  intercept = FALSE,
  tidy_fun = broom.helpers::tidy_zeroinfl,
  component = "zero_inflated" # zero model
) |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)   # prevents reference rows from showing parentheses
  ) |>
  modify_header(
    estimate ~ "Odds Ratio (95% CI)",
    p.value ~ "p-value"
  ) |>
  modify_caption(
    "Predictors of Zero Knowledge (ZINB Zero-Inflation Model)"
  ) |> 
  as_gt() |> 
  gtsave(here("outputs/tables/knowledge-regression-zinb-zero-model.docx"))


# Combine both tables
tbl_combined <-
  tbl_stack(
    list(tbl_count, tbl_zero),
    group_header = c(
      "Count model: Determinants of knowledge score",
      "Zero-inflation model: Predictors of zero knowledge"
    )
  )

#The ZINB model gave similar output to that of NB model, so NB model was prefered for easiar interpretation.









#MODEL DIAGNOSTICS----
##Check overdispersion (to justify Negative bionmial vs Poisson)
  #First fit Poisson model
  model_pois <- glm(form_knowledge, family = poisson, data = data)
  
  # Compare AIC
  AIC(model_pois, model_knowledge)
    #interpretation: Lower AIC = better model
    #If NB << Poison -> overdispersion confirmed.
  
  
  
  #Formal Overdispersion Test
  library(AER) 
  dispersiontest(model_pois)
  # Result	Interpretation
  # p < 0.05	Overdispersion present
  # p ≥ 0.05	Poisson acceptable
  
  
#Goodness of fit
  #Log-likelihood
  logLik(model_knowledge)
  
  #AIC
  AIC(model_knowledge)
  
#Pseudo R²
  library(pscl) # pseudo R2 and Vuong test
  pR2(model_knowledge)
  #Report: McFadden R² (most common)

#Multicollinearity
  library(car)  # VIF
  vif(lm(form_knowledge, data = data))
  # VIF measures correlation among predictors only, not the outcome.
  # So computing it using lm() instead of glm.nb() is fine.
  #Interpretation
    # GVIF^(1/(2*Df)) < 2 → good
    # 2–5 → acceptable
    # >5 → problematic

#Influential observations
  # Cook's distance
  cooks <- cooks.distance(model_knowledge)
  
  # Plot
  plot(cooks, type = "h", main = "Cook's Distance")
  abline(h = 4/length(cooks), col = "red")
  # If points > threshold:
    # check data
    # do NOT blindly remove
  
#Residual Diagnosis-(OLD METHOD)
  # Deviance residuals
  res <- residuals(model_knowledge, type = "deviance")
  
  # Plot
  plot(res, main = "Deviance residuals")
  
  # Histogram
  hist(res, breaks = 20)
  
#Residual Diagnosis-(DHARMa: MODERN METHOD)
  library(DHARMa)      # residual diagnostics: currently best residual diagnostic for GLM and GLMM models.
  sim_res <- simulateResiduals(model_knowledge)
  
  plot(sim_res)
  
  testDispersion(sim_res)
  
  testZeroInflation(sim_res)
  
  testOutliers(sim_res)
  
  # What it checks
  # Test------------------------------What it detects
  # Residual==========================uniformity	model fit
  # Dispersion========================over/under dispersion
  # Zero inflation====================excess zeros
  # Outliers==========================extreme cases
  
  #DHARMa test for the negative binomial test gave following warnings in the Residual vs predicted plot
    #Quantile deviations detected
    #Combined adjusted quantile test significant
        # Interpretation:
        #   Residuals show systematic structure with predictions.
        # This means:
        #   The model does not capture all patterns in the data.
        # Possible reasons:
          # Missing predictor variables
          # Nonlinear relationships
          # Interaction effects
          # Model misspecification
 
   # DHARMa Dispersion Test
  # Plot result:
    # p-value = 0
  # Interpretation:
    #   The fitted model still shows dispersion problems.
  # This means:
     #   Even the Negative Binomial model does not fully explain the variance.
  # Possible causes:
    # unmodelled heterogeneity
    # missing predictors
    # clustering in data
  
  # Zero Inflation Test
  # Plot result:p-value = 0
  # Interpretation:
      #   The model predicts far fewer zeros than observed.
  # Observed zeros:
       #   ≈ 63
  # Simulated expected zeros:
       #   ≈ 25–45
  # Conclusion:
         #   Your data contains excess zeros.
  # Therefore:
      # A standard Negative Binomial model is likely inappropriate.
      # You should test a Zero-Inflated Negative Binomial (ZINB) model.

  
  
  ##SO, I need to compare three more models, Negative binomial, zero inflated binomial and hurdle model

  #Fit ZINB model
  library(pscl)
  
  zinb_model <- zeroinfl(
    form_knowledge,
    data = data,
    dist = "negbin"
  )
  #compare NB and ZINB
  AIC(model_knowledge, zinb_model)
  
  vuong(model_knowledge, zinb_model)
 
   #Interpretation of the output
   # ΔAIC ≈ 56, which is extremely strong evidence that the ZINB model fits the data much better.
   # Evidence for ZINB:
      # AIC strongly favors ZINB (ΔAIC = 56)
      # Raw Vuong significant
      # AIC-corrected Vuong significant
      # DHARMa detected zero inflation
      # DHARMa detected dispersion
  #So, I need to proceed to ZIBN model 
  #In ZINB two models are fitted; count model for Factors affecting knowledge score level
      #and zero model to identify Factors affecting probability of zero knowledge
  
  
  

  
  
  
  
##Multivariate negative binomial regression model (Modern)----
mv_knowledge_model <- MASS::glm.nb(
    as.formula(
      paste("Knowledge_score ~", paste(vars_p20_knowledge, collapse = " + "))
    ),
    data = data
  )
  
#Multivariate  regression_table----
multivariate_knowledge <- tbl_regression(
    mv_knowledge_model,
    exponentiate = TRUE
  ) |>
    add_global_p() |>
    bold_p() |>
    bold_labels() |>
    modify_column_merge(
      pattern = "{estimate} ({conf.low}–{conf.high})",
      rows = !is.na(estimate)
    ) |>
    modify_header(
      estimate ~ "**Adjusted IRR (95% CI)**",
      p.value ~ "**p-value**"
    )

# Merge univariate and multivariable tables
tbl_merge(
  tbls = list(univariate_knowledge, multivariate_knowledge),
  tab_spanner = c("**Univariate**", "**Multivariate**")
) |>  
  as_gt() |>
  gtsave(here("outputs/tables/knowledge-regression-negative-binomial.docx"))  
  



##ATTITUDES-REGRESSION----
#Bivariate-screening----
#A.Bivariate screening (Traditional Method)----
library(dplyr)
library(purrr)
library(broom)  #contains tidy function

bivariate_results <- map_df(
  predictors,
  function(var) {
    
    formula <- as.formula(
      paste("Attitude_score ~", var)
    )
    
    model <- lm(formula, data = data)
    
    broom::tidy(anova(model)) %>%
      slice(1) %>%
      mutate(variable = var) %>%
      dplyr::select(variable, p.value)
  }
)

bivariate_results


#variable screening
selected_vars <- bivariate_results %>%
  filter(p.value < 0.20) %>%
  pull(variable)


#regression formula
formula_multivariable <- as.formula(
  paste(
    "Attitude_score ~",
    paste(selected_vars, collapse = " + ")
  )
)

#multivariate linear regression model
mv_attitude_model <- lm(
  formula_multivariable,
  data = data
)




#B.Bivariate screening (Univariate regression Method)----
# This automatically runs bivariate linear regression for each predictor
univariate_attitude <- tbl_uvregression(
  data = data,
  method = lm,
  y = Attitude_score,
  include = all_of(predictors)
) |> 
  add_global_p() |> 
  bold_p(t = 0.05)|> 
  bold_labels() |>
  # Merge estimate and CI into one column for publication style
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)
  ) |>
  # Rename headers for clarity
  modify_header(
    label = "**Predictor**",
    estimate ~ "**β (95% CI)**",
    p.value ~ "**p-value**"
  ) 

# Extract significant predictors (p < 0.20)
# This pulls the variable names directly from the gtsummary object
vars_p20_att <- univariate_attitude$table_body |>
  dplyr::filter(row_type == "label") |>
  dplyr::filter(p.value < 0.20) |>
  dplyr::pull(variable) |>
  unique()



#multivariate linear regression model
mv_attitude_model <- lm(
  as.formula(
    paste("Attitude_score ~", paste(vars_p20_att, collapse = " + "))
  ),
  data = data
)

summary(mv_attitude_model)



#MV LR Model diagnostics (Attitude model)----
      #check multicollinearity
      library(car)
      
      vif(mv_attitude_model)
      
      # Residual diagnostics
      par(mfrow = c(2,2))
      plot(mv_attitude_model)
      shapiro.test(residuals(mv_attitude_model))
      
      # Influential observation check
      plot(cooks.distance(mv_attitude_model))
      
      # Check heteroscedasticity formally
        # Breusch–Pagan test
          library(lmtest)
          bptest(mv_attitude_model)
                #p =  0.05 | Homoscedasticity satisfied |
                # p < 0.05 | Heteroscedasticity present |
      
      # Model fit
          summary(mv_attitude_model)$r.squared
          

          
          
#Multivariate  regression_table----
multivariate_attitude <- tbl_regression(mv_attitude_model) |>
  add_global_p() |> 
  bold_p() |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)   # prevents reference rows from showing parentheses
  ) |>
  modify_header(
    estimate ~ "**Adjusted β (95% CI)**",
    p.value ~ "**p-value**"
  ) 


#MERGE regression tables
tbl_merge(
  tbls = list(univariate_attitude, multivariate_attitude),
  tab_spanner = c("**Univariate**", "**Multivariate**")
) |>  
  as_gt() |>
  gtsave(here("outputs/tables/attitudes-linear-regression.docx"))

































# 
# #Bivariate screening function
# # For Knowledge & Practice (non-parametric)
# bivariate_nonparam <- function(outcome, predictors, data) {
#   results <- data.frame()
# 
#   for (var in predictors) {
#     formula <- as.formula(paste(outcome, "~", var))
# 
#     test <- if (nlevels(data[[var]]) == 2) {
#       wilcox.test(formula, data = data)
#     } else {
#       kruskal.test(formula, data = data)
#     }
# 
#     results <- rbind(results,
#                      data.frame(Variable = var,
#                                 P_value = test$p.value))
#   }
#   return(results)
# }
# 
# 
# # For Attitude (parametric)
# bivariate_param <- function(outcome, predictors, data) {
#   results <- data.frame()
# 
#   for (var in predictors) {
#     formula <- as.formula(paste(outcome, "~", var))
# 
#     test <- if (nlevels(data[[var]]) == 2) {
#       t.test(formula, data = data)
#     } else {
#       summary(aov(formula, data = data))[[1]][["Pr(>F)"]][1]
#     }
# 
#     pval <- ifelse(is.list(test), test$p.value, test)
# 
#     results <- rbind(results,
#                      data.frame(Variable = var,
#                                 P_value = pval))
#   }
#   return(results)
# }
# 
# 
# 
# 
# 
# #select variables
# vars_knowledge <- biv_knowledge$Variable[biv_knowledge$P_value < 0.20]
# 
# vars_attitude  <- biv_attitude$Variable[biv_attitude$P_value < 0.20]
# 
# vars_practice  <- biv_practice$Variable[biv_practice$P_value < 0.20]
# 
# 
# #add theory driven variables
# # Always include these
# key_vars <- c("Education_level", "received_training")
# 
# # Knowledge model
# vars_knowledge <- unique(c(vars_knowledge, key_vars))
# 
# # Attitude model (add knowledge)
# vars_attitude <- unique(c(vars_attitude, key_vars, "Knowledge_score"))
# 
# # Practice model (add knowledge + attitude)
# vars_practice <- unique(c(vars_practice, key_vars, "Knowledge_score",
#                           "Attitude_score"))
# 
# 
# 
# #build formula
# form_knowledge <- as.formula(
#   paste("Knowledge_score ~", paste(vars_knowledge, collapse = " + "))
# )
# 
# form_attitude <- as.formula(
#   paste("Attitude_score ~", paste(vars_attitude, collapse = " + "))
# )
# 
# form_practice <- as.formula(
#   paste("Practice_score ~", paste(vars_practice, collapse = " + "))
# )
# 
# #Check multicollinearity (before final models)
# library(car)
# # Temporary linear models
# vif(lm(form_knowledge, data = data))
# 
# vif(lm(form_attitude, data = data))
# 
# vif(lm(form_practice, data = data))
# 
# 
# 
# # Fit final models
# library(MASS) # Regression models (negative binomial)
# 
# model_knowledge <- glm.nb(form_knowledge, data = data)
# 
# model_attitude <- lm(form_attitude, data = data)
# 
# model_practice <- glm.nb(form_practice, data = data)
# 
# #EXTRACT RESULTS
# summary(model_knowledge)
# summary(model_attitude)
# summary(model_practice)
# 
# # IRR for NB models
# exp(coef(model_knowledge))
# exp(coef(model_attitude))
# exp(coef(model_practice))
# 
# 
# #PUBLICATION READY REGRESSION TABLE USING gt
# 
# # Function to create Negative Binomial regression table (IRR)
# format_nb_table <- function(model) {
#   broom::tidy(model, conf.int = TRUE) %>%
#     dplyr::mutate(
#       IRR = exp(estimate),
#       CI_low = exp(conf.low),
#       CI_high = exp(conf.high),
#       IRR_CI = sprintf("%.2f (%.2f–%.2f)", IRR, CI_low, CI_high),
#       p_value = sprintf("%.3f", p.value)
#     ) %>%
#     dplyr::select(term, IRR_CI, p_value)
# }
# 
# 
# # Function for linear regression table (β)
# format_lm_table <- function(model) {
#   broom::tidy(model, conf.int = TRUE) %>%
#     dplyr::mutate(
#       Beta = estimate,
#       CI = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
#       p_value = sprintf("%.3f", p.value)
#     ) %>%
#     dplyr::select(term, CI, p_value)
# }
# 
# #Generate Tables
# # Knowledge (NB)
# tab_knowledge <- format_nb_table(model_knowledge)
# 
# # Attitude (LM)
# tab_attitude <- format_lm_table(model_attitude)
# 
# # Practice (NB)
# tab_practice <- format_nb_table(model_practice)
# 
# 
# #Create GT table
# gt_knowledge <- tab_knowledge %>%
#   gt() %>%
#   cols_label(
#     term = "Variable",
#     IRR_CI = "IRR (95% CI)",
#     p_value = "p-value"
#   ) %>%
#   tab_header(
#     title = "Determinants of Knowledge Score"
#   )





















##PRACTICE-REGRESSION----
#Count the zero knowledge scores
sum(data$Practice_score == 0)
#Count the zero knowledge scores
mean(data$Practice_score == 0)  #>10–15% zeros → consider zero-inflated models


#Bivariate-screening
#B.Using Modern Method (Univariate Regression)----
# Univariate negative binomial regression (bivariate screening)----
library(gtsummary)
library(MASS)

univariate_practice <- tbl_uvregression(
  data = data,
  method = MASS::glm.nb,
  y = Practice_score,
  include = all_of(predictors),
  exponentiate = TRUE   # converts coefficients to IRR
) |> 
  add_global_p() |>
  bold_p() |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)
  ) |>
  modify_header(
    label = "**Predictor**",
    estimate ~ "**IRR (95% CI)**",
    p.value ~ "**p-value**"
  )  
#Model diagnostics showed that Poisson regression fits better than negative 
# binomial, and binomial logistic regression for practice, so,

# Univariate Poisson regression----
univariate_practice_P <- tbl_uvregression(
  data = data,
  method = glm,
  y = Practice_score,
  method.args = list(family = poisson(link = "log")),
  include = all_of(predictors),
  exponentiate = TRUE
) |>
  add_global_p() |>
  bold_p() |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)
  ) |>
  modify_header(
    label = "**Predictor**",
    estimate ~ "**IRR (95% CI)**",
    p.value ~ "**p-value**"
  )


# Select predictors with p < 0.20
vars_p20_practice_P <- univariate_practice_P$table_body |>
  dplyr::filter(row_type == "label") |>
  dplyr::filter(p.value < 0.20) |>
  dplyr::pull(variable) |>
  unique()


#PROCEED-to-MULTIVARIATE ANALYSIS----
#Based on the assessment of knowledge score: right skewed, over-dispersion, selected
#model for regression is Negative Binomial Regression

#Negative Binomial Regression table (Traditional)----
tbl_regression(
  model_knowledge,
  exponentiate = TRUE
) |>
  add_global_p() |>
  bold_p() |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)   # prevents reference rows from showing parentheses
  ) |>
  modify_header(
    estimate ~ "IRR (95% CI)",
    p.value ~ "Global p-value"
  ) |>
  modify_caption(
    "Factors Associated with Knowledge Score"
  ) |>
  as_gt() |> 
  gtsave(here("outputs/tables/knowledge-regression-nb-global-p(no-district).docx"))


#Forest-plot-of-the-regression-output  
library(broom)
tidy(model_knowledge, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    x = "Adjusted IRR (95% CI)",
    y = "Predictors"
  ) +
  theme_minimal()  




#Zero Inflated Negative Binomial Reg Model----
library(pscl)

zinb_model <- zeroinfl(
  Knowledge_score ~ District + Education_level + Family_size +
    Primary_Income_source + herd_size + Sheep_goat_rearing_experience +
    Sheep_goat_rearing_system + Annual_income + received_training +
    herd_previously_affected + disease_reporting |
    Education_level + Sheep_goat_rearing_experience + received_training + 
    herd_previously_affected + disease_reporting,
  data = data,
  dist = "negbin"
)
# Left side = count model (Knowledge score ~ predictors)
# (Variables were selected based on 0.2 p-value cutoff from bivariate screening)
# Right side = zero model (Probability of zero knowledge ~ predictors)
# Selected variables are theoretical predictors of zero knowledge, e.g. education, training, disease exposure

#Examine model output
summary(zinb_model)
#Outputs
# Count model coefficients
# Zero-inflation model coefficients



# Build regression table

# ---- COUNT MODEL (IRR)
tbl_count <- tbl_regression(
  zinb_model,
  exponentiate = TRUE,
  intercept = FALSE,
  tidy_fun = broom.helpers::tidy_zeroinfl,
  component = "conditional" #count model
) |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)   # prevents reference rows from showing parentheses
  ) |>
  modify_header(
    estimate ~ "IRR (95% CI)",
    p.value ~ "p-value"
  ) |>
  modify_caption(
    "Determinants of Knowledge Score (Zero-Inflated Negative Binomial Model)"
  ) |> 
  as_gt() |> 
  gtsave(here("outputs/tables/knowledge-regression-zinb-count-model.docx"))



# ---- ZERO-INFLATION MODEL (OR)
tbl_zero <- tbl_regression(
  zinb_model,
  exponentiate = TRUE,
  intercept = FALSE,
  tidy_fun = broom.helpers::tidy_zeroinfl,
  component = "zero_inflated" # zero model
) |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)   # prevents reference rows from showing parentheses
  ) |>
  modify_header(
    estimate ~ "Odds Ratio (95% CI)",
    p.value ~ "p-value"
  ) |>
  modify_caption(
    "Predictors of Zero Knowledge (ZINB Zero-Inflation Model)"
  ) |> 
  as_gt() |> 
  gtsave(here("outputs/tables/knowledge-regression-zinb-zero-model.docx"))


# Combine both tables
tbl_combined <-
  tbl_stack(
    list(tbl_count, tbl_zero),
    group_header = c(
      "Count model: Determinants of knowledge score",
      "Zero-inflation model: Predictors of zero knowledge"
    )
  )

#The ZINB model gave similar output to that of NB model, so NB model was prefered for easiar interpretation.









#MODEL DIAGNOSTICS----
##Check overdispersion (to justify Negative bionmial vs Poisson)
#First fit Poisson model
model_pois <- glm(form_knowledge, family = poisson, data = data)

# Compare AIC
AIC(model_pois, model_knowledge)
#interpretation: Lower AIC = better model
#If NB << Poison -> overdispersion confirmed.


#Formal Overdispersion Test
library(AER) 
dispersiontest(model_pois)
# Result	Interpretation
# p < 0.05	Overdispersion present
# p ≥ 0.05	Poisson acceptable


#Goodness of fit
#Log-likelihood
logLik(mv_practice_model)

#AIC
AIC(model_knowledge)

#Pseudo R²
library(pscl) # pseudo R2 and Vuong test
pR2(mv_practice_model)
#Report: McFadden R² (most common)

#Multicollinearity
library(car)  # VIF
vif(lm(mv_practice_model, data = data))
# VIF measures correlation among predictors only, not the outcome.
# So computing it using lm() instead of glm.nb() is fine.
#Interpretation
# GVIF^(1/(2*Df)) < 2 → good
# 2–5 → acceptable
# >5 → problematic

#Influential observations
# Cook's distance
cooks <- cooks.distance(mv_practice_model)

# Plot
plot(cooks, type = "h", main = "Cook's Distance")
abline(h = 4/length(cooks), col = "red")
# If points > threshold:
# check data
# do NOT blindly remove

#Residual Diagnosis-(OLD METHOD)
# Deviance residuals
res <- residuals(mv_practice_model, type = "deviance")

# Plot
plot(res, main = "Deviance residuals")

# Histogram
hist(res, breaks = 20)

#Residual Diagnosis-(DHARMa: MODERN METHOD)
library(DHARMa)      # residual diagnostics: currently best residual diagnostic for GLM and GLMM models.
sim_res <- simulateResiduals(mv_practice_model)

plot(sim_res)

testDispersion(sim_res)

testZeroInflation(sim_res)

testOutliers(sim_res)

# What it checks
# Test------------------------------What it detects
# Residual==========================uniformity	model fit
# Dispersion========================over/under dispersion
# Zero inflation====================excess zeros
# Outliers==========================extreme cases

#DHARMa test for the negative binomial test gave following warnings in the Residual vs predicted plot
#Quantile deviations detected
#Combined adjusted quantile test significant
# Interpretation:
#   Residuals show systematic structure with predictions.
# This means:
#   The model does not capture all patterns in the data.
# Possible reasons:
# Missing predictor variables
# Nonlinear relationships
# Interaction effects
# Model misspecification

# DHARMa Dispersion Test
# Plot result:
# p-value = 0
# Interpretation:
#   The fitted model still shows dispersion problems.
# This means:
#   Even the Negative Binomial model does not fully explain the variance.
# Possible causes:
# unmodelled heterogeneity
# missing predictors
# clustering in data

# Zero Inflation Test
# Plot result:p-value = 0
# Interpretation:
#   The model predicts far fewer zeros than observed.
# Observed zeros:
#   ≈ 63
# Simulated expected zeros:
#   ≈ 25–45
# Conclusion:
#   Your data contains excess zeros.
# Therefore:
# A standard Negative Binomial model is likely inappropriate.
# You should test a Zero-Inflated Negative Binomial (ZINB) model.



##SO, I need to compare three more models, Negative binomial, zero inflated binomial and hurdle model

#Fit ZINB model
library(pscl)

zinb_model <- zeroinfl(
  form_knowledge,
  data = data,
  dist = "negbin"
)
#compare NB and ZINB
AIC(model_knowledge, zinb_model)

vuong(model_knowledge, zinb_model)

#Interpretation of the output
# ΔAIC ≈ 56, which is extremely strong evidence that the ZINB model fits the data much better.
# Evidence for ZINB:
# AIC strongly favors ZINB (ΔAIC = 56)
# Raw Vuong significant
# AIC-corrected Vuong significant
# DHARMa detected zero inflation
# DHARMa detected dispersion
#So, I need to proceed to ZIBN model 
#In ZINB two models are fitted; count model for Factors affecting knowledge score level
#and zero model to identify Factors affecting probability of zero knowledge





##Multivariate negative binomial regression model (Modern)----
mv_practice_model <- MASS::glm.nb(
  as.formula(
    paste("Practice_score ~", paste(vars_p20_practice, collapse = " + "))
  ),
  data = data
)

#Multivariate  negative binomial regression_table----
multivariate_practice <- tbl_regression(
  mv_practice_model,
  exponentiate = TRUE
) |>
  add_global_p() |>
  bold_p() |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)
  ) |>
  modify_header(
    estimate ~ "**Adjusted IRR (95% CI)**",
    p.value ~ "**p-value**"
  )

# Merge univariate and multivariable tables
tbl_merge(
  tbls = list(univariate_knowledge, multivariate_knowledge),
  tab_spanner = c("**Univariate**", "**Multivariate**")
) |>  
  as_gt() |>
  gtsave(here("outputs/tables/knowledge-reg1.docx"))  


##Multivariate Binomial Logistic Regression----
# Create binomial outcome variables
data$practice_success <- data$Practice_score
data$practice_failure <- 7 - data$Practice_score

# Check quickly:
summary(data$practice_success)
summary(data$practice_failure)


#Fit binomial logistic regression model
practice_binom_model <- glm(
  as.formula(
    paste(
      "cbind(practice_success, practice_failure) ~",
      paste(vars_p20_practice, collapse = " + ")
    )
  ),
  family = binomial(link = "logit"),
  data = data
)


#compare models
AIC(mv_practice_model, practice_binom_model)
#                     df      AIC
# mv_practice_model    15 1079.695
# practice_binom_model 14 1085.483
#Lower AIC = better model fit, so NB better fits my practice data

# > 


##Poisson Logistic Regression----
# Fit Multivariate Poisson regression model
mv_poisson_practice_model <- glm(
  as.formula(
    paste("Practice_score ~", paste(vars_p20_practice_P, collapse = " + "))
  ),
  family = poisson(link = "log"),
  data = data
)

# View Poisson model summary
summary(poisson_practice_model)

# Check overdispersion in Poisson model
library(performance)

check_overdispersion(poisson_practice_model)

# If output shows:
# dispersion ratio > 1
# p < 0.05
# Then Poisson is overdispersed, meaning Poisson is inappropriate and NB is preferred.

# Likelihood ratio test (formal comparison)
library(lmtest)
lrtest(poisson_practice_model, mv_practice_model)

#AIC
AIC(poisson_practice_model, mv_practice_model)
#                        df      AIC
# poisson_practice_model 14 1077.691
# mv_practice_model      15 1079.695
# Lower AIC = better model, Poisson regression fits your data best among the three models.


#Multivariate  Poisson regression table----
MV_practice <- tbl_regression(
  mv_poisson_practice_model,
  exponentiate = TRUE
) |>
  add_global_p() |>
  bold_p() |>
  bold_labels() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}–{conf.high})",
    rows = !is.na(estimate)
  ) |>
  modify_header(
    estimate ~ "**IRR (95% CI)**",
    p.value ~ "**p-value**"
  )

# Merge univariate and multivariable tables
tbl_merge(
  tbls = list(univariate_practice_P, MV_practice),
  tab_spanner = c("**Univariate**", "**Multivariate**")
) |>  
  as_gt() |>
  gtsave(here("outputs/tables/practice-poisson-regrssion.docx"))  





