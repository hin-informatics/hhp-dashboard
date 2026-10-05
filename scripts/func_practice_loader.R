## PRACTICE LOADER & VALIDATION MODULE ##
# Discovers practice folders, validates 3-report EMIS extracts against manifest,
# reports audit results, and appends valid practices into unified datasets.

# 1. Get Manifest ----
get_manifest <- function(headers_path = "data/headers.csv") {
  if (!file.exists(headers_path)) {
    stop(paste("Manifest file not found at:", headers_path))
  }
  
  headers <- read.csv(headers_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  clean_col <- function(x) {
    x <- trimws(x)
    x <- x[x != "" & !is.na(x)]
    return(x)
  }
  
  patient_cols <- if ("patient_data" %in% names(headers)) clean_col(headers$patient_data) else character(0)
  appt_cols <- if ("appt_data" %in% names(headers)) clean_col(headers$appt_data) else character(0)
  baseline_cols <- if ("baseline_data" %in% names(headers)) clean_col(headers$baseline_data) else character(0)
  
  list(
    patient = patient_cols,
    appt = appt_cols,
    baseline = baseline_cols
  )
}

# 2. Discover Practices ----
discover_practices <- function(practices_dir = "practices") {
  if (!dir.exists(practices_dir)) {
    stop(paste("Practices directory not found at:", practices_dir))
  }
  
  practice_dirs <- list.dirs(practices_dir, recursive = FALSE, full.names = TRUE)
  
  data.frame(
    practice_name = basename(practice_dirs),
    practice_path = practice_dirs,
    stringsAsFactors = FALSE
  )
}

# 3. Read EMIS Export File & Trim Spacer Columns ----
read_emis_raw <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  
  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel files.")
    }
    raw_df <- suppressMessages(
      readxl::read_excel(file_path, col_names = FALSE, col_types = "text", .name_repair = "minimal")
    )
    raw_df <- as.data.frame(raw_df, stringsAsFactors = FALSE)
  } else if (ext == "csv") {
    raw_df <- suppressMessages(
      data.table::fread(file_path, header = FALSE, colClasses = "character", data.table = FALSE)
    )
  } else {
    stop(paste("Unsupported file extension:", ext))
  }
  
  # Identify completely empty spacer columns (every row is NA or blank)
  is_all_blank <- function(col) {
    all(is.na(col) | trimws(as.character(col)) == "")
  }
  
  blank_cols <- which(vapply(raw_df, is_all_blank, logical(1)))
  if (length(blank_cols) > 0) {
    raw_df <- raw_df[, -blank_cols, drop = FALSE]
  }
  
  return(raw_df)
}

# 4. Validate Single Practice ----
validate_practice <- function(practice_path, manifest) {
  practice_name <- basename(practice_path)
  
  # Target filenames
  f1_name <- "Healthy Hearts Evaluation Report 1.xls"
  f2_name <- "Healthy Hearts Evaluation Report 2.xls"
  f3_name <- "Healthy Hearts Baseline Report 3.xls"
  
  p1 <- file.path(practice_path, f1_name)
  p2 <- file.path(practice_path, f2_name)
  p3 <- file.path(practice_path, f3_name)
  
  validate_file <- function(path, expected_len, report_label) {
    if (!file.exists(path)) {
      return(list(status = "MISSING", ok = FALSE, note = paste(report_label, "missing")))
    }
    
    raw <- tryCatch({
      read_emis_raw(path)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(raw)) {
      return(list(status = "CORRUPT", ok = FALSE, note = paste(report_label, "could not be read")))
    }
    
    col_count <- ncol(raw)
    if (col_count != expected_len) {
      return(list(
        status = paste0("MISMATCH (", col_count, " cols)"),
        ok = FALSE,
        note = paste0(report_label, " has ", col_count, " cols (expected ", expected_len, ")")
      ))
    }
    
    list(status = "PASS", ok = TRUE, note = "")
  }
  
  v1 <- validate_file(p1, length(manifest$patient), "Report 1")
  v2 <- validate_file(p2, length(manifest$appt), "Report 2")
  v3 <- validate_file(p3, length(manifest$baseline), "Report 3")
  
  # Strict all-or-nothing gating: ALL 3 must be valid
  is_valid <- v1$ok && v2$ok && v3$ok
  
  notes <- c(v1$note, v2$note, v3$note)
  notes <- notes[notes != ""]
  
  notes_str <- if (is_valid) {
    "All 3 reports verified"
  } else {
    paste(notes, collapse = "; ")
  }
  
  data.frame(
    practice_name = practice_name,
    report_1 = v1$status,
    report_2 = v2$status,
    report_3 = v3$status,
    status = if (is_valid) "VALID" else "INVALID",
    is_valid = is_valid,
    notes = notes_str,
    stringsAsFactors = FALSE
  )
}

# 5. Print Validation Summary Audit Table ----
report_validation_summary <- function(audit_df) {
  border <- paste(rep("=", 105), collapse = "")
  sub_border <- paste(rep("-", 105), collapse = "")
  
  cat("\n", border, "\n", sep = "")
  cat(" HEALTHY HEARTS PRACTICE VALIDATION AUDIT REPORT\n")
  cat(border, "\n")
  cat(sprintf(" %-30s %-16s %-16s %-16s %-10s %s\n",
              "Practice Name", "Report 1 (85)", "Report 2 (80)", "Report 3 (17)", "Status", "Notes"))
  cat(sub_border, "\n")
  
  for (i in seq_len(nrow(audit_df))) {
    row <- audit_df[i, ]
    cat(sprintf(" %-30s %-16s %-16s %-16s %-10s %s\n",
                substr(row$practice_name, 1, 30),
                substr(row$report_1, 1, 16),
                substr(row$report_2, 1, 16),
                substr(row$report_3, 1, 16),
                row$status,
                row$notes))
  }
  cat(border, "\n")
  cat(sprintf(" Total Practices Discovered: %d | Validated (Accepted): %d | Rejected: %d\n",
              nrow(audit_df), sum(audit_df$is_valid), sum(!audit_df$is_valid)))
  cat(border, "\n\n")
}

# 6. Extract, Validate, and Append Practices ----
extract_and_append_practices <- function(practices_dir = "practices",
                                         headers_path = "data/headers.csv") {
  manifest <- get_manifest(headers_path)
  practices <- discover_practices(practices_dir)
  
  if (nrow(practices) == 0) {
    stop("No practice folders found in: ", practices_dir)
  }
  
  audit_list <- lapply(seq_len(nrow(practices)), function(i) {
    validate_practice(practices$practice_path[i], manifest)
  })
  audit_df <- do.call(rbind, audit_list)
  
  # Display formatted audit table
  report_validation_summary(audit_df)
  
  # Filter to strictly VALID practices
  valid_practices <- practices[audit_df$is_valid, , drop = FALSE]
  
  if (nrow(valid_practices) == 0) {
    stop("No practices passed validation. Pipeline execution stopped.")
  }
  
  list_r1 <- list()
  list_r2 <- list()
  list_r3 <- list()
  
  f1_name <- "Healthy Hearts Evaluation Report 1.xls"
  f2_name <- "Healthy Hearts Evaluation Report 2.xls"
  f3_name <- "Healthy Hearts Baseline Report 3.xls"
  
  for (i in seq_len(nrow(valid_practices))) {
    p_name <- valid_practices$practice_name[i]
    p_path <- valid_practices$practice_path[i]
    
    # Load and clean Report 1
    raw1 <- read_emis_raw(file.path(p_path, f1_name))
    data1 <- raw1[11:nrow(raw1), , drop = FALSE]
    # Filter rows with at least one non-empty value
    valid_rows1 <- apply(data1, 1, function(r) any(!is.na(r) & trimws(r) != ""))
    data1 <- data1[valid_rows1, , drop = FALSE]
    colnames(data1) <- manifest$patient
    data1$practice_source <- p_name
    list_r1[[i]] <- data.table::as.data.table(data1)
    
    # Load and clean Report 2
    raw2 <- read_emis_raw(file.path(p_path, f2_name))
    data2 <- raw2[11:nrow(raw2), , drop = FALSE]
    valid_rows2 <- apply(data2, 1, function(r) any(!is.na(r) & trimws(r) != ""))
    data2 <- data2[valid_rows2, , drop = FALSE]
    colnames(data2) <- manifest$appt
    data2$practice_source <- p_name
    list_r2[[i]] <- data.table::as.data.table(data2)
    
    # Load and clean Report 3
    raw3 <- read_emis_raw(file.path(p_path, f3_name))
    data3 <- raw3[11:nrow(raw3), , drop = FALSE]
    valid_rows3 <- apply(data3, 1, function(r) any(!is.na(r) & trimws(r) != ""))
    data3 <- data3[valid_rows3, , drop = FALSE]
    colnames(data3) <- manifest$baseline
    data3$practice_source <- p_name
    list_r3[[i]] <- data.table::as.data.table(data3)
  }
  
  # Stack datasets across all valid practices
  ptts_dt <- data.table::rbindlist(list_r1, use.names = TRUE, fill = TRUE)
  appt_dt <- data.table::rbindlist(list_r2, use.names = TRUE, fill = TRUE)
  base_dt <- data.table::rbindlist(list_r3, use.names = TRUE, fill = TRUE)
  
  message(sprintf("Successfully appended %d valid practice(s): %d patients, %d appt/med rows, %d baseline rows.",
                  nrow(valid_practices), nrow(ptts_dt), nrow(appt_dt), nrow(base_dt)))
  
  list(
    ptts_dt = ptts_dt,
    appt_dt = appt_dt,
    base_dt = base_dt,
    audit = audit_df
  )
}
