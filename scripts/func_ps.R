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
  
  ### Asian or Asian British Groups ###
  d <- d %>% 
    mutate(grouped_ethnic_origin = case_when(
      ethnic_origin %in% c(
        "Asian and Chinese - ethnic category 2001 census",
        "Asian or Asian British: any other Asian background - England and Wales ethnic category 2011 census",
        "Bangladeshi",
        "Bangladeshi or British Bangladeshi - ethnic category 2001 census",
        "Filipino - ethnic category 2001 census",
        "Indian or British Indian - ethnic category 2001 census",
        "Other Asian background - ethnic category 2001 census",
        "Other Asian ethnic group",
        "Other Asian or Asian unspecified - ethnic category 2001 census",
        "Pakistani",
        "Pakistani or British Pakistani - ethnic category 2001 census",
        "Vietnamese") ~ "Asian or Asian British",
  
      ### Black, African, Caribbean or Black British Groups ###
      ethnic_origin %in% c(
        "African - ethnic category 2001 census",
        "African: African, African Scottish or African British - Scotland ethnic category 2011 census",
        "Black African",
        "Black British - ethnic category 2001 census",
        "Black Caribbean",
        "Black Caribbean/W.I./Guyana",
        "Black or African or Caribbean or Black British: African - England and Wales ethnic category 2011 census",
        "Black or African or Caribbean or Black British: Caribbean - England and Wales ethnic category 2011 census",
        "Black or African or Caribbean or Black British: other Black or African or Caribbean background - England and Wales ethnic category 2011 census",
        "Caribbean - ethnic category 2001 census",
        "Nigerian - ethnic category 2001 census",
        "North African - ethnic category 2001 census",
        "Other Black background - ethnic category 2001 census",
        "Somali - ethnic category 2001 census") ~ "Black, African, Caribbean or Black British",
  
  ### Mixed / Multiple Ethnic Groups ###
      ethnic_origin %in% c(
        "Black Caribbean and White",
        "Black and Asian - ethnic category 2001 census",
        "Black and White - ethnic category 2001 census",
        "Mixed Asian - ethnic category 2001 census",
        "Mixed multiple ethnic groups: any other Mixed or multiple ethnic background - England and Wales ethnic category 2011 census",
        "Other Mixed background - ethnic category 2001 census",
        "Other Mixed or Mixed unspecified - ethnic category 2001 census",
        "Other ethnic, Asian/White orig",
        "Other ethnic, mixed origin",
        "White and Asian - ethnic category 2001 census",
        "White and Black African - ethnic category 2001 census",
        "White and Black Caribbean - ethnic category 2001 census") ~ "Mixed / Multiple Ethnicity",
  
  ### Not Stated Groups ###
      ethnic_origin %in% c(
        "Ethnic category not stated - 2001 census",
        "Ethnicity and other related nationality data",
        "Patient ethnicity unknown",
        "Refusal by patient to provide information about ethnic group") ~ "Not Stated",
  
  ### Other Ethnic Groups ###
      ethnic_origin %in% c(
        "Any other group - ethnic category 2001 census",
        "Arab - ethnic category 2001 census",
        "Asian or Asian British: Chinese - England and Wales ethnic category 2011 census",
        "Chinese - ethnic category 2001 census",
        "Iranian - ethnic category 2001 census",
        "Latin American - ethnic category 2001 census",
        "Other - ethnic category 2001 census",
        "South and Central American - ethnic category 2001 census") ~ "Other Ethnic Group",
  
  ### White Groups ###
      ethnic_origin %in% c(
        "Albanian - ethnic category 2001 census",
        "British or mixed British - ethnic category 2001 census",
        "Bulgarian",
        "Greek - ethnic category 2001 census",
        "Gypsy/Romany - ethnic category 2001 census",
        "Irish - ethnic category 2001 census",
        "Other White European or European unspecified or Mixed European - ethnic category 2001 census",
        "Other White background - ethnic category 2001 census",
        "Other White or White unspecified - ethnic category 2001 census",
        "Other mixed White - ethnic category 2001 census",
        "Portuguese",
        "Scottish - ethnic category 2001 census",
        "White",
        "White - ethnic group",
        "White British",
        "White British - ethnic category 2001 census",
        "White Irish - ethnic category 2001 census",
        "White: English or Welsh or Scottish or Northern Irish or British - England and Wales ethnic category 2011 census",
        "White: any other White background - England and Wales ethnic category 2011 census") ~ "White",
  TRUE ~ "Not Coded"
      ))
  
  ## End code here ##
  
  return(d)
}