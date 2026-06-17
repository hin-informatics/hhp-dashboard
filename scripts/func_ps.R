# HHP TRANSFORMATION FUNCTIONS
# polly.sinclar@nhs.net

# Instructions:
# 1. Git Pull
# 2. Make adjustments between start and end code blocks
# 3. Test it works
# 4. Add, Commit and push back to Git.


# Ethnic Grouping Function ----
# Creates major ethnic categories column with data and variable input.

create_ethnic_grps <- function(data, ethnic_var){
  message('Creating major ethnic groups: ')
  
  d = data
   
  ## Start code here ##
  
  ## End code here ##
  
  return(d)
}

## Test: Create EMIS categories ----

create_emis_group <- function(data){
  
  
  d = data
  
  ## Start code here ##
  
  d <- d %>% 
    mutate(emis_cat1 = case_when(
      emis_number >= 0 & emis_number <= 9 ~ "0-9",
      between(emis_number, 100, 200) ~ "100-200", 
      emis_number >= 201 ~ "> 200",
      TRUE  ~ "Unknown"
    ))
  
  ## End code here ##
  
  return(d)
}