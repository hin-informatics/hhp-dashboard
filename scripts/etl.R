# What is this script?
# What does it do?
# Who authored it?
# When was it created?

# Have you:
# 1. Pulled the changes?
# 2. Open the .Rproj file.
# 3. Ran renv::restore().

source('scripts/ini.R')
test_mode <- T
tic('ETL Process complete')

# EXTRACTION ----
# Step 1: Extract data from shared location

FILENAME <- "Healthy Hearts Evaluation Report 1 - patient level data.csv"
PATH <- paste0(USERPROFILE, LOCATION, FILENAME)
extract <- read.csv(PATH, skip = 9) # Remove rows not needed
extract <- clean_names(extract) # Ensure clean headers

# Step 2: Clean
# Create clean patient data table by selecting relevants columns
use_cols <- c(
  "emis_number",
  "age",
  "lower_layer_area_2011",
  "gender",
  "ethnic_origin"
)

patient_dt <- select(extract, paste0(use_cols))

View(patient_dt) # View table

# TRANSFORMATION ----
# Categorisation
## EMIS (please delete)

patient_dt <- patient_dt %>% 
  mutate(emis_cat = case_when(
    emis_number >= 0 & emis_number <= 9 ~ "0-9",
    between(emis_number, 100, 200) ~ "100-200", 
    emis_number >= 201 ~ "> 200",
    TRUE  ~ "Unknown"
  ))

# Data Table option: 
setDT(patient_dt) # You have to set it as a data table to work.

patient_dt[, emis_cat2 := fcase(
  emis_number %between% c(0,10), "0-10",
  emis_number %between% c(100,200), "100-200",
  emis_number %between% c(201,300), "100-200",
  default = "Unknown"
)]

View(patient_dt)

tabyl(patient_dt$emis_cat)

## Step 3: Create 10-year age bands groupings
# Use the age column
# Age bands should be 0-9, 10-19, 20-29, 30-39...90-99, > 100.

## YOUR CODE START HERE ##

patient_dt

## YOUR CODE ENDS HERE ##

## Step 4: Create ethnicity groups

## YOUR CODE START HERE ##

## YOUR CODE ENDS HERE ##

# Step 5: Link LSOA to lookup





##

# LOAD ----

toc()