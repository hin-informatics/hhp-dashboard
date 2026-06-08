source('scripts/ini.R')

# Pipeline Settings
TestMode <- T
DataRefresh <- F

tic('ETL Process complete')

# EXTRACTION ----
# Step 1: Extract data from shared location
if(DataRefresh){
  message('DataRefresh set to "T": Fresh data pull.')
  FILENAME <- "Healthy Hearts Evaluation Report 1 - patient level data.csv"
  PATH <- paste0(USERPROFILE, LOCATION, FILENAME)
  extract <- read.csv(PATH, skip = 9) # Remove rows not needed
  
  # CLEANING
  # Headers
  extract <- clean_names(extract) # Ensure clean headers
  
  # Column selection
  use_cols <- c(
    "emis_number",
    "age",
    "lower_layer_area_2011",
    "gender",
    "ethnic_origin"
  )
  
  dt <- select(extract, paste0(use_cols))
  
  saveRDS(dt, paste0('data/wd_',format(Sys.Date(), "%Y%m%d"))) # Save Working data set
}else{
  message('DataRefresh set to "F": Using latest archived data source.')
  file_list <- list.files('data/', pattern = "^wd_", full.names = T)
  file_metadata <- file.info(file_list)
  latest_data <- file_list[which.max(file_metadata$mtime)]
  dt <- readRDS(latest_data)
}

# TRANSFORMATION PIPELINE ----

dt <- create_age_bands(dt)
dt <- create_ethnic_grps(dt)
dt <- create_geo_grps(dt)

# LOAD ----
if(test_mode){
  message("TestMode is set to 'T': No changes made to the output data.")
}
write.csv(dt, 'data/clean.csv')

toc()