# ==============================================================================
# Script: test.R
# Purpose: Calculates patient counts and optimisation numbers by practice
#          across clinical conditions (CKD, T2D, and HYP) using data/facts.csv.
# ==============================================================================

# 1. Locate and load facts.csv -------------------------------------------------
find_facts_path <- function() {
  candidates <- c(
    "data/facts.csv",
    file.path("..", "data", "facts.csv"),
    file.path("hhp-dashboard", "data", "facts.csv")
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  stop("Could not locate 'data/facts.csv'. Ensure scripts/etl.R has been executed.")
}

facts_file <- find_facts_path()

if (requireNamespace("data.table", quietly = TRUE)) {
  facts <- data.table::fread(facts_file, data.table = FALSE)
} else {
  facts <- read.csv(facts_file, stringsAsFactors = FALSE)
}

# 2. Validate required columns -------------------------------------------------
required_cols <- c(
  "organisation_name",
  "ckd_exist", "ckd_optimised",
  "diabetes_exist", "diabetes_optimised",
  "hypertension_exist", "hypertension_optimised"
)

missing_cols <- setdiff(required_cols, colnames(facts))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns in facts.csv: %s", paste(missing_cols, collapse = ", ")))
}

# Standardise practice name if blank/NA
facts$organisation_name[is.na(facts$organisation_name) | trimws(facts$organisation_name) == ""] <- "Unknown Practice"

# 3. Calculate summary metrics by practice and condition -----------------------
conditions <- list(
  CKD = list(exist = "ckd_exist", opt = "ckd_optimised", label = "Chronic Kidney Disease (CKD)"),
  T2D = list(exist = "diabetes_exist", opt = "diabetes_optimised", label = "Type 2 Diabetes (T2D)"),
  HYP = list(exist = "hypertension_exist", opt = "hypertension_optimised", label = "Hypertension (HYP)")
)

practices <- sort(unique(facts$organisation_name))

calc_condition_stats <- function(df, practice_name) {
  out <- list()
  for (c_code in names(conditions)) {
    e_col <- conditions[[c_code]]$exist
    o_col <- conditions[[c_code]]$opt
    
    n_pts <- sum(df[[e_col]] == 1, na.rm = TRUE)
    n_opt <- sum(df[[e_col]] == 1 & df[[o_col]] == 1, na.rm = TRUE)
    pct   <- if (n_pts > 0) (n_opt / n_pts) * 100 else 0
    
    out[[length(out) + 1]] <- data.frame(
      Practice      = practice_name,
      Condition     = c_code,
      Patients      = n_pts,
      Optimised     = n_opt,
      Pct_Optimised = round(pct, 1),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

summary_list <- list()
for (p in practices) {
  p_df <- facts[facts$organisation_name == p, ]
  summary_list[[length(summary_list) + 1]] <- calc_condition_stats(p_df, p)
}

summary_df <- do.call(rbind, summary_list)

# If multiple practices exist, add an "ALL PRACTICES (TOTAL)" row set
if (length(practices) > 1) {
  total_df <- calc_condition_stats(facts, "ALL PRACTICES (TOTAL)")
  summary_df <- rbind(summary_df, total_df)
}

# 4. Formatted Console Output --------------------------------------------------
cat("\n")
cat("========================================================================================\n")
cat("                HEALTHY HEARTS COHORT OPTIMISATION AUDIT REPORT                         \n")
cat("                             (Source: data/facts.csv)                                   \n")
cat("========================================================================================\n\n")

# Long Table: By Practice and Condition
summary_display <- summary_df
summary_display$Pct_Optimised <- sprintf("%.1f%%", summary_display$Pct_Optimised)
colnames(summary_display) <- c("Practice", "Condition", "# Patients", "# Optimised", "% Optimised")

if (requireNamespace("knitr", quietly = TRUE)) {
  cat(knitr::kable(summary_display, format = "simple", align = c("l", "c", "r", "r", "r")), sep = "\n")
} else {
  print(summary_display, row.names = FALSE)
}

cat("\n========================================================================================\n")
cat(sprintf("Total registered cohort size: %d patients across %d practice(s).\n", nrow(facts), length(practices)))
cat("========================================================================================\n\n")

# Return summary data frame invisibly
invisible(summary_df)
