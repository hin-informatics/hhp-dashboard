# PRACTICE

source('etl.r')

d0 <- dt

# How to create category columns ----
# Tidyverse Option:

d0 <- d0 %>% 
  mutate(emis_cat1 = case_when(
    emis_number >= 0 & emis_number <= 9 ~ "0-9",
    between(emis_number, 100, 200) ~ "100-200", 
    emis_number >= 201 ~ "> 200",
    TRUE  ~ "Unknown"
  ))
tabyl(d0$emis_cat1) # Review

# Data Table option: 

setDT(d0) # You have to set it as a data table to work.

d0[, emis_cat2 := fcase(
  emis_number %between% c(0,10), "0-10",
  emis_number %between% c(100,200), "100-200",
  emis_number %between% c(201,300), "100-200",
  default = "Unknown"
)]
tabyl(d0$emis_cat2) # Review



View(d0)







##