# HHP TRANSFORMATION FUNCTIONS
# alan.rajan@nhs.net

# Instructions:
# 1. Git Pull
# 2. Make adjustments between start and end code blocks
# 3. Test it works
# 4. Add, Commit and push back to Git.


# Age band Function ----
# Creates 10-year age band column with data and variable input.

create_age_bands <- function(data, age_var){
  message('Creating 10-year age band column: ')
  
  d = data
  
  ## Start code here ##
  d <- d %>%
    mutate(grouped_age_bands = case_when(
      #list of cases below
      between(age,0,9) ~ "0 to 9",
      between(age,10,19) ~ "10 to 19",
      between(age,20,29) ~ "20 to 29",
      between(age,30,39) ~ "30 to 39",
      between(age,40,49) ~ "40 to 49",
      between(age,50,59) ~ "50 to 59",
      between(age,60,69) ~ "60 to 69",
      between(age,70,79) ~ "70 to 79",
      between(age,80,89) ~ "80 to 89",
      between(age,90,99) ~ "90 to 99",
      #Spec says patients aged 18+ only but leaving the below in the code
      age < 0 ~ "Infant",
      age >= 100 ~ "Above 100",
      TRUE ~ "Unknown"
  ))
  ## End code here ##
  
  return(d)
}

# Geography Assignment Function ----
# Creates useful geography and IMD columns with data and variable input.

create_geo_grps <- function(data){
  message('Creating geographies and IMD: ')
  
  d = data
  
  ## Start code here ##
  # IMPORTANT! Check to see these columns and addresses are right
  # Extract the look-up table from source
  github_imd =read.csv("https://raw.githubusercontent.com/hin-informatics/hhp-dashboard/refs/heads/main/data/IMD_2010.csv")
  
  # Assign IMD and other geographies to d from look-up table.
  d <- d %>%
    left_join(
      github_imd %>% select(lsoa_code, la_name),
      by = c("lower_layer_area_2011" = "lsoa_code")
    )
  ## End code here ##
  
  return(d)
}