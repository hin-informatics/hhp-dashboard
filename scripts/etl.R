source('scripts/ini.R')

# Pipeline Settings
TestMode <- T
DataRefresh <- F

tic('ETL Process complete')

# EXTRACTION ----
# Step 1: Extract data from shared location
if(DataRefresh){
  message('DataRefresh set to "T": Fresh data pull.')
  # Access SharePoint
  site <- get_sharepoint_site(
    site_url = "https://emckclac.sharepoint.com/sites/LSMhini",
    tenant = "emckclac.onmicrosoft.com"
  )
  
  drv <- site$get_drive("Informatics sensitive data") # Get drive
  drv$list_files(path = "Healthy Hearts") # list files in Healthy Heart folder
  file_item <- drv$get_item("Healthy Hearts/Healthy Hearts Evaluation Report 1 - patient level data.csv")
  
  extract <- file_item$load_dataframe()
  
  # CLEANING
  extract <- tail(extract, nrow(extract)-8) # Skip top rows
  extract <- clean_names(extract) # Ensure clean headers
  
  # Column selection
  use_cols <- c(
    "emis_number",
    "age",
    "lower_layer_area_2011",
    "gender",
    "ethnic_origin"
  )
  
  dt <- select(extract, paste0(use_cols)) # Use columns
  
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