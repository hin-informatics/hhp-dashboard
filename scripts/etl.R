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
  extract <- clean_names(extract) # Ensure clean headers
  saveRDS(extract)
}else{
  message('DataRefresh set to "F": Using archived data source.')
  dt <- readRDS(extract)
}

# TRANSFORMATION PIPELINE ----

dt <- create_age_bands(dt)
dt <- create_ethnic_grps(dt)
dt <- create_geo_grps(dt)

# LOAD ----
if(test_mode){
  message("TestMode is set to 'T': No changes made to the output data.")
}
fwrite(dt, )

toc()