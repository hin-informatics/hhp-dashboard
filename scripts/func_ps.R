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
  
  # Vectors for Ethnicity
  African = c(
    "African - ethnic category 2001 census",
    "African: African, African Scottish or African British - Scotland ethnic category 2011 census"
    
    )
  Asian = c(
    "Asian and Chinese - ethnic category 2001 census",
    "Asian or Asian British: Chinese - England and Wales ethnic category 2011 census"
    )
  Mixed = c(
    ""
    )
  OtherWhites = c(
    "Albanian - ethnic category 2001 census"
    
    )
  WhiteBritish = c(
    ""
    )
  Others = c(
    "Any other group - ethnic category 2001 census",
    "Arab - ethnic category 2001 census"
    )
  
  d <- d %>% 
    mutate(ethnic_group = case_when(
      ethnic_origin %in% African ~ "African",
      ethnic_origin %in% Mixed ~ "Mixed",
      TRUE  ~ "Unknown"
    ))
  
  ## End code here ##
  
  return(d)
}