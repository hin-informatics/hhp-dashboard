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

patient_dt



# HELPER FUNCTIONS ----
"C:\\Users\\k2481754\\King's College London\\HIN - Informatics - Informatics sensitive data\\Healthy Hearts\\Healthy Hearts Evaluation Report 1 - patient level data.csv"
"C:\\Users\\k1505767\\King's College London\\HIN - Informatics - Informatics sensitive data\\Healthy Hearts\\Healthy Hearts Evaluation Report 1 - patient level data.csv"




##