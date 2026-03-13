# Load required packages
library(here)
library(tidyverse)
library(gt)
library(gtsummary)
library(naniar)

# Global plotting theme
theme_set(theme_bw())

# Avoid scientific notation
options(scipen = 999)

# Optional: set reproducible seed
set.seed(123)