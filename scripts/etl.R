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
# Extract data from proxy location on OneD rive
####
# Your Code here ####
PATH <- "C:\\Users\\k2481754\\OneDrive - King's College London\\Remote Projects\\HHP\\Healthy Hearts Evaluation Report 1 - patient level data.csv"
extract <- read.csv(PATH)
###



# TRANSFORMATION ----
# LOAD ----

toc()