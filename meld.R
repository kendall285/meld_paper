# Install packages and load libraries

if (!requireNamespace("bigrquery", quietly = TRUE)) install.packages("bigrquery")
if (!requireNamespace("cowplot", quietly = TRUE)) install.packages("cowplot")
if (!requireNamespace("gargle", quietly = TRUE)) install.packages("gargle")
if (!requireNamespace("gtsummary", quietly = TRUE)) install.packages("gtsummary")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("magick", quietly = TRUE)) install.packages("magick")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if(!requireNamespace("ResourceSelection", quietly = TRUE)) install.packages("ResourceSelection")

library(bigrquery)
library(cowplot)
library(dplyr)
library(gargle)
library(ggplot2)
library(gtsummary)
library(lubridate)
library(magick)
library(pROC)
library(ResourceSelection)
library(tidyr)
library(readr)


########################################################
###               MIMIC-IV BIGQUERY SETUP            ###
########################################################

########### BIGQUERY CONNECT ############

gcp_project_id <- "<provide your Google Cloud project ID here>"

# Don't let ADC be discovered this session
Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = "")

# Force gargle to use ONLY user OAuth2 (disable all other credential modes)
gargle::cred_funs_set(
  list(credentials_user_oauth2 = gargle::credentials_user_oauth2)
)

# Force account picker and a fresh token (no cache reuse)
options(gargle_oauth_email = TRUE)
bq_auth(email = TRUE)

# Sanity test
bq_table_download(bq_project_query(gcp_project_id, "SELECT 'AUTH_OK' AS status"))

############# MIMIC-IV QUERIES #############
# All admissions in MIMIC-IV (with ethnicity, mortality flag, admission and discharge times)
all_admissions_query <- "
SELECT
  a.race AS ethnicity,
  a.hospital_expire_flag,
  a.admittime,
  a.dischtime
FROM `physionet-data.mimiciv_3_1_hosp.admissions` AS a
;"

# Admissions of interest: cirrhosis cases identified via ICD-9/10 codes
# Joins admissions, diagnoses, and patient demographics
admissions_of_interest_query <- "
SELECT
  a.hadm_id,
  p.subject_id,
  p.anchor_age,
  p.gender,
  a.race AS ethnicity,
  d.icd_code,
  d.icd_version,
  a.hospital_expire_flag,
  a.admittime,
  a.dischtime
FROM `physionet-data.mimiciv_3_1_hosp.diagnoses_icd` AS d
JOIN `physionet-data.mimiciv_3_1_hosp.admissions`   AS a
  ON d.hadm_id = a.hadm_id
JOIN `physionet-data.mimiciv_3_1_hosp.patients`     AS p
  ON d.subject_id = p.subject_id
WHERE
  (d.icd_version = 10 AND d.icd_code IN ('K743','K745','K7031','K7469','K7030','K7460','K717','K744'))
  OR
  (d.icd_version = 9  AND d.icd_code IN ('5716','5715','5712'))
;"

# Lab results relevant for MELD score calculation
# Extracts Bilirubin, Creatinine, Sodium, INR, and Albumin with valid numeric values
labs_query <- "
SELECT
  le.hadm_id,
  le.charttime,
  dl.label,
  le.valuenum
FROM `physionet-data.mimiciv_3_1_hosp.labevents`  AS le
JOIN `physionet-data.mimiciv_3_1_hosp.d_labitems` AS dl
  ON le.itemid = dl.itemid
WHERE
  le.hadm_id IS NOT NULL
  AND le.valuenum IS NOT NULL
  AND le.valuenum > 0
  AND (
    dl.label = 'Bilirubin, Total'
    OR dl.label = 'Creatinine'
    OR dl.label = 'Sodium'
    OR LOWER(dl.label) LIKE '%inr%'
    OR dl.label = 'Albumin'
  )
;"

# Admissions where patients received liver transplants
# ICD-10 code: 0FY00Zx series; ICD-9 code: 505%
liver_transplants_query <- "
SELECT
  *
FROM `physionet-data.mimiciv_3_1_hosp.procedures_icd`
WHERE
  (icd_version = 10 AND icd_code LIKE '0FY00Z%')
  OR
  (icd_version = 9  AND icd_code LIKE '505%')
;"

# Admissions where patients received dialysis / renal replacement therapy
# ICD-9 codes: 3995, 5498; ICD-10 codes: 5A1D%
dialysis_query <- "
SELECT
  *
FROM `physionet-data.mimiciv_3_1_hosp.procedures_icd`
WHERE
  (icd_version = 9  AND (icd_code LIKE '3995%' OR icd_code LIKE '5498%'))
  OR
  (icd_version = 10 AND icd_code LIKE '5A1D%')
;"


############# MIMIC-IV BIGQUERY HELPERS + CSV WRITERS #############
# Assumes these SQL strings are already defined above:
#   all_admissions_query, admissions_of_interest_query, labs_query,
#   liver_transplants_query, dialysis_query

# ---- small utilities ----------------------------------------------------------

# Create parent directory for a file path, if missing.
.ensure_parent_dir <- function(path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# Write CSV safely (no row names, UTF-8).
.write_csv_safe <- function(df, path) {
  .ensure_parent_dir(path)
  tryCatch(
    {
      utils::write.csv(df, file = path, row.names = FALSE, fileEncoding = "UTF-8")
      invisible(TRUE)
    },
    error = function(e) stop("Error: failed to write CSV at '", path, "'. ", e$message)
  )
}

# Run a BigQuery SQL string safely and return a data.frame (tibble).
.run_query_safe <- function(query,
                            project = gcp_project_id,
                            bigint = "integer64",
                            page_size = 1e5,
                            quiet = TRUE) {
  if (is.null(project) || !nzchar(project)) {
    stop("Error: gcp_project_id is not set. Set gcp_project_id <- '<project-id>'.")
  }
  
  job <- tryCatch(
    bigrquery::bq_project_query(project, query, quiet = quiet),
    error = function(e) stop("Error: failed to start BigQuery job. ", e$message)
  )
  
  # Wait for completion and inspect status
  bigrquery::bq_job_wait(job)
  jf <- bigrquery::bq_job_fields(job)
  
  if (!is.null(jf$status$errorResult)) {
    # Collect all errors for better visibility
    errs <- c(
      paste0(jf$status$errorResult$reason, ": ", jf$status$errorResult$message),
      vapply(jf$status$errors %||% list(), function(x) paste0(x$reason, ": ", x$message), character(1))
    )
    stop("BigQuery job failed:\n", paste(unique(errs), collapse = "\n"))
  }
  
  tryCatch(
    bigrquery::bq_table_download(job, bigint = bigint, page_size = page_size),
    error = function(e) stop("Error: failed to download query results. ", e$message)
  )
}
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- fetchers (return data.frames) -------------------------------------------

# Get all admissions (race→ethnicity, expire flag, admit/discharge times).
get_all_admissions_from_mimic_iv <- function() {
  .run_query_safe(all_admissions_query)
}

# Get cirrhosis admissions (ICD-9/10) with demographics.
get_cirrhosis_admissions_from_mimic_iv <- function() {
  .run_query_safe(admissions_of_interest_query)
}

# Get MELD-relevant labs (Bilirubin, Creatinine, Sodium, INR, Albumin).
# If you provide admissions_list, we call your updater hook before running the query.
get_labs_from_mimic_iv <- function(admissions_list = NULL) {
  if (!is.null(admissions_list)) {
    tryCatch(
      update_selected_admissions_bq_table(admissions_list = admissions_list),
      error = function(e) stop("Error: failed to update selected admissions table. ", e$message)
    )
  }
  .run_query_safe(labs_query)
}

# Get liver transplant procedures.
get_liver_transplants_from_mimic_iv <- function() {
  .run_query_safe(liver_transplants_query)
}

# Get dialysis / RRT procedures.
get_dialysis_from_mimic_iv <- function() {
  .run_query_safe(dialysis_query)
}

# ---- one-liners to fetch & save CSVs -----------------------------------------

# Each returns the data.frame *and* writes the CSV to disk.

save_all_admissions_csv <- function(path = "Data/mimic_admissions_all.csv") {
  df <- get_all_admissions_from_mimic_iv()
  .write_csv_safe(df, path)
  df
}

save_cirrhosis_admissions_csv <- function(path = "Data/mimic_admissions_meld.csv") {
  df <- get_cirrhosis_admissions_from_mimic_iv()
  .write_csv_safe(df, path)
  df
}

save_labs_csv <- function(path = "Data/mimic_labs.csv", admissions_list = NULL) {
  df <- get_labs_from_mimic_iv(admissions_list = admissions_list)
  .write_csv_safe(df, path)
  df
}

save_liver_transplants_csv <- function(path = "Data/mimic_liver.csv") {
  df <- get_liver_transplants_from_mimic_iv()
  .write_csv_safe(df, path)
  df
}

save_dialysis_csv <- function(path = "Data/mimic_dialysis.csv") {
  df <- get_dialysis_from_mimic_iv()
  .write_csv_safe(df, path)
  df
}

# ---- convenience: run everything in one shot ---------------------------------

save_all_mimic_extracts <- function(
    admissions_all_path   = "Data/mimic_admissions_all.csv",
    admissions_meld_path  = "Data/mimic_admissions_meld.csv",
    labs_path             = "Data/mimic_labs.csv",
    liver_tx_path         = "Data/mimic_liver.csv",
    dialysis_path         = "Data/mimic_dialysis.csv",
    admissions_list_for_labs = NULL  # optionally scope labs
) {
  df_all   <- save_all_admissions_csv(admissions_all_path)
  df_meld  <- save_cirrhosis_admissions_csv(admissions_meld_path)
  df_labs  <- save_labs_csv(labs_path, admissions_list_for_labs)
  df_tx    <- save_liver_transplants_csv(liver_tx_path)
  df_dial  <- save_dialysis_csv(dialysis_path)
  
  invisible(list(
    admissions_all  = df_all,
    admissions_meld = df_meld,
    labs            = df_labs,
    liver_tx        = df_tx,
    dialysis        = df_dial
  ))
}
########################################################
### [RUN ONCE]     MIMIC-IV DATA FETCH               ###
########################################################
# Run fetchers and write CSVs to disk
save_all_mimic_extracts()


################################################################
###         MIMIC-IV      Helper functions                   ###
################################################################

## 1. Function to enrich missing ethnicities in the dataframe of the admissions of interest. 
# The dataframe of all admissions in MIMIC-IV is used for enrichment.
# The function takes dataframes with unique and all admissions and returns the enriched unique admissions.
enrich_ethnicity <- function(unique_admissions_df, all_admissions_df) {
  # Iterate over the unique_admissions_df rows
  for(i in seq_len(nrow(unique_admissions_df))) {
    # Check if ethnicity is "UNABLE TO OBTAIN" or "UNKNOWN"
    if(unique_admissions_df$ethnicity[i] %in% c("UNABLE TO OBTAIN", "UNKNOWN")) {
      # Get the subject_id of the current row
      current_subject_id <- unique_admissions_df$subject_id[i]
      
      # Find a non-"UNKNOWN" and non-"UNABLE TO OBTAIN" ethnicity in all_admissions_df for the same subject_id
      new_ethnicity <- all_admissions_df %>%
        filter(subject_id == current_subject_id, !ethnicity %in% c("UNABLE TO OBTAIN", "UNKNOWN")) %>%
        select(ethnicity) %>%
        unique() %>%
        slice(1) %>%
        pull(ethnicity)
      
      # If a new ethnicity is found, update the unique_admissions_df
      if(length(new_ethnicity) > 0) {
        unique_admissions_df$ethnicity[i] <- new_ethnicity
      }
    }
  }
  
  return(unique_admissions_df)
}


## 2. Function finds the "best" set of labs in a dataset for each admission.
find_best_labs <- function(dataset, time_window=24, optimize="None") {
  ## time_window defines max datetime range between lab result storetime
  ## optimize = "None" returns all labs 
  ## optimize = "SD" returns the labs with the lowest SD at the earliest storetime
  ## optimize = "storetime" returns the labs with the earliest storetime
  
  # Convert storetime to POSIXct if it's not
  if (!inherits(dataset$storetime, "POSIXct")) {
    # Add time   
    dataset$storetime <- ifelse(grepl(":", dataset$storetime),
                                dataset$storetime, 
                                paste(dataset$storetime, "00:00:00"))
    dataset$storetime <- as.POSIXct(dataset$storetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }

  # Sort the dataframe by storetime
  sorted_df <- dataset[order(dataset$storetime, decreasing = FALSE), ]
  
  # Initiate a dataframe with labs to return
  labs_df <- data.frame(hadm_id=numeric(), 
                        Alb=numeric(), Cr=numeric(), INR=numeric(), Na=numeric(), TBil=numeric(), 
                        storetime=character(), 
                        SD=numeric(),
                        stringsAsFactors = FALSE)
  
  # Iterate over each row of the sorted dataframe
  for (i in 1:nrow(sorted_df)) {
    # Initiate a labs set tracker 
    result <- list(Alb = NA, Cr = NA, INR = NA, Na = NA, TBil = NA, storetime = NA)
    # Initiate a storetimes tracker
    storetimes <- c()    
    
    # Take the row
    row <- sorted_df[i, ]
    # Identify the next non-NA column (excluding 'storetime')
    non_na_column <- names(row)[which(!is.na(row) & names(row) != "storetime" & names(row) != "hadm_id")]
    center_time <- row[["storetime"]]
    
    # Since there's always exactly one non-NA column, directly assign the value.
    if (length(non_na_column) == 1 && is.na(result[[non_na_column]])) { 
      # Store a lab value
      result[[non_na_column]] <- row[[non_na_column]]
      # Store the lab storetime 
      result[["storetime"]] <- row[["storetime"]]
      # Add storetime to a storetime tracker
      storetimes <- c(storetimes, row[["storetime"]])
    }
    
    for (j in i:nrow(sorted_df)) {
      eval_row <- sorted_df[j, ]
      eval_time <- eval_row[["storetime"]]

      if (difftime(eval_time, center_time, units = "hours") < time_window) {
      # Identify the non-NA column (excluding 'storetime')
        eval_non_na_column <- names(eval_row)[which(!is.na(eval_row) & names(eval_row) != "storetime" & names(eval_row) != "hadm_id")]
        if (length(eval_non_na_column) == 1 && is.na(result[[eval_non_na_column]])) {
          result[[eval_non_na_column]] <- eval_row[[eval_non_na_column]]
          storetimes <- c(storetimes, eval_row[["storetime"]])
        }
        } 
      else if (all(!sapply(result, is.na))) {
        break # Exit the inner loop if all values are found 
        } 
      else { # Exit the inner loop if the time window is exceeded
        result <- list(Alb = NA, Cr = NA, INR = NA, Na = NA, TBil = NA, storetime = NA) # Reset result tracker
        storetimes <- c() # Reset storetimes tracker
        break 
      }
    }
      
    if (!any(sapply(result, is.na))) {
      ## All labs are found => add it to a dataframe
      # Calculate SD of a cluster
      SD <- sd(as.numeric(storetimes))
      # Add it to results 
      result$SD <- SD
      
      # Add hadm_id 
      result$hadm_id <- row[["hadm_id"]]
      
      ## Add results to a dataframe 
      # Transform result to a dataframe 
      result_df <- unnest(as.data.frame(t(result)), cols = everything())
      # Ensure the storetime column is of type POSIXct
      #result_df$storetime <- as.POSIXct(result_df$storetime)
      # Merge dataframes 
      labs_df <- rbind(labs_df, result_df)
      # Ensure the dataframe maintains the correct types for each column
      #labs_df$storetime <- as.POSIXct(labs_df$storetime)
      } 
    
  }


  # Return NULL if no labs found 
  if (nrow(labs_df)==0) {
    return(NULL)
  } else {
    # Adjust return based on the requested optimization criteria.
    if (optimize == "SD") {
      ## Find the labs cluster with the lowest SD 
      # Sort the dataframe by SD and storetime  
      labs_df <- labs_df[order(labs_df$SD, labs_df$storetime), ]
      # Select the first row of the sorted dataframe
      labs_df <- labs_df[1, ]
    } else if (optimize == "storetime") {
      ## Find the labs cluster with the earlist storetime 
      # Sort the dataframe by storetime and SD  
      labs_df <- labs_df[order(labs_df$storetime, labs_df$SD), ]
      # Select the first row of the sorted dataframe
      labs_df <- labs_df[1, ]
    }
    return(labs_df)
  }
}

## 3. Function to compute a single p-value for differences across AUROCs
compute_combined_pvalue <- function(df, outcome_col, predictor_col, group_col) {
  # Ensure group_col is a factor
  df[[group_col]] <- as.factor(df[[group_col]])
  group_levels <- levels(df[[group_col]])
  
  # Prepare to store pairwise p-values
  pairwise_p_values <- numeric(0)
  
  # Perform pairwise comparisons
  for (i in 1:(length(group_levels) - 1)) {
    for (j in (i + 1):length(group_levels)) {
      data_i <- df[df[[group_col]] == group_levels[i], ]
      data_j <- df[df[[group_col]] == group_levels[j], ]
      
      roc_i <- roc(data_i[[outcome_col]], data_i[[predictor_col]])
      roc_j <- roc(data_j[[outcome_col]], data_j[[predictor_col]])
      
      test_result <- roc.test(roc_i, roc_j, method="delong")
      pairwise_p_values <- c(pairwise_p_values, test_result$p.value)
    }
  }
  
  # Combine p-values using Fisher's method
  combined_test <- sum(-2*log(pairwise_p_values))
  combined_p_value <- pchisq(combined_test, df = 2 * length(pairwise_p_values), lower.tail = FALSE)
  
  return(round(combined_p_value, 3))
}

## 4. Function that creates dataframes for plotting calibration curves.
create_df_for_calibration_plot <- function(data, mortality_col, outcome_col, bin_size) {

  # Calculate calibration data for a single group
  calculate_calibration_data <- function(data) {
    data$bin <- cut(data[[mortality_col]], breaks = seq(0, 1, by = bin_size), include.lowest = TRUE)
    calibration_data <- data %>%
      group_by(bin) %>%
      summarise(mean_predicted = mean(.data[[mortality_col]], na.rm = TRUE),
                observed = mean(.data[[outcome_col]], na.rm = TRUE))
    return(calibration_data)
  }
  
  # Split data by ethnicity and calculate calibration data
  list_calibration_data <- split(data, data$ethnicity) %>%
    lapply(calculate_calibration_data)
  
  # Combine all calibration data into one dataframe
  calibration_data_combined <- do.call(rbind, list_calibration_data)
  
  # Assuming 'ethnicity' is not a column in the resulting calibration_data,
  # this will correctly append ethnicity information
  calibration_data_combined$ethnicity <- rep(names(list_calibration_data), sapply(list_calibration_data, nrow))
  
  return(calibration_data_combined)
}


## 5. Function to get AUROC and CI
get_auroc_ci <- function(roc_obj) {
  ci <- ci.auc(roc_obj)  # Calculate the confidence interval for AUROC
  return(c(
    round(ci[2], 3),  # Round the AUROC to 3 decimal places and return it first
    round(ci[1], 3),  # Round the lower bound of the confidence interval to 3 decimal places
    round(ci[3], 3)   # Round the upper bound of the confidence interval to 3 decimal places
  ))
}


################################################################
###     MIMIC-IV            Load data                        ###
################################################################

## Load MIMIC-IV

# Load all admissions in MIMIC-IV.
mimic_admissions_all_df <- read.csv("Data/mimic_admissions_all.csv")

# Load the admissions of interest only, based on ICD codes.
# ((icd_version = 10 AND (icd_code = 'K743' OR icd_code = 'K745' OR icd_code = 'K7031' OR icd_code = 'K7469' OR icd_code = 'K7030' OR icd_code = 'K7460' OR icd_code = 'K717' OR icd_code = 'K744')) OR 
# (icd_version = 9 AND (icd_code = '5716' OR icd_code = '5715' OR icd_code = '5712')))
mimic_admissions_meld_df <- read.csv("Data/mimic_admissions_meld.csv")

# Load all lab results available in MIMIC-IV.
mimic_labs_df <- read.csv("Data/mimic_labs.csv")

# Load liver transplants for all patients in MIMIC-IV.
mimic_liver_df <- read.csv("Data/mimic_liver.csv")

# Load all dialysis procedures in MIMIC-IV.
mimic_dialysis_df <- read.csv("Data/mimic_dialysis.csv")

################################################################
###     MIMIC-IV               Data cleaning.                ###
################################################################

# Set a threshold for the max labs range
max_labs_time_range <- 24 # hours

## 1. Clean admissions of interest. 
# 1.1. Remove unneeded columns.
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  select(-admittime_1, -admission_type, -admission_location, -icd_code, -icd_version)

# 1.2. Convert admittime, dischtime, and dod to POSIXct
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(admittime = as.POSIXct(admittime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))

mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(dischtime = as.POSIXct(dischtime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))

mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(dod = if_else(dod == "", NA_character_, dod), # Convert empty strings to NA
         dod = as.POSIXct(dod, format = "%Y-%m-%d", tz = "UTC")) # Then convert to POSIXct

# Check for any NAs and NULLs in admittime and dischtime
has_na <- any(is.na(mimic_admissions_meld_df$admittime))
has_null <- any(is.null(mimic_admissions_meld_df$admittime))

has_na <- any(is.na(mimic_admissions_meld_df$dischtime))
has_null <- any(is.null(mimic_admissions_meld_df$dischtime))

# 1.3. Remove duplicative admissions from the admissions of interest and delete clean icd_code and icd_version
# Remove duplicates from admissions_dd that appeared because patients may have multiple icd codes.
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  distinct(hadm_id, .keep_all = TRUE)

## 2. Clean labs. [Run only once]
# 2.1. Keep only the labs of interest listed in mimic_admissions_meld_df
mimic_labs_df <- mimic_labs_df %>%
  filter(hadm_id %in% unique(mimic_admissions_meld_df$hadm_id))

# 2.2. Delete unneeded columns 
mimic_labs_df <- mimic_labs_df %>%
  select(-labevent_id, -subject_id, -specimen_id, -itemid, -charttime, -value, -valueuom, -ref_range_lower, 
         -ref_range_upper, -flag, -priority, -comments, -itemid_1, -fluid, -category, -loinc_code)

# 2.3. Update lab labels to common terms 
mimic_labs_df$label[mimic_labs_df$label == "Bilirubin, Total"] <- "TBil"
mimic_labs_df$label[mimic_labs_df$label == "Albumin"] <- "Alb"
mimic_labs_df$label[mimic_labs_df$label == "Sodium"] <- "Na"
mimic_labs_df$label[mimic_labs_df$label == "Creatinine"] <- "Cr"
mimic_labs_df$label[mimic_labs_df$label == "INR(PT)"] <- "INR"

# 2.4. Convert to a long format 
mimic_labs_df <- mimic_labs_df %>%
  mutate(Alb = if_else(label == "Alb", valuenum, NA_real_),
         Cr = if_else(label == "Cr", valuenum, NA_real_),
         Na = if_else(label == "Na", valuenum, NA_real_),
         INR = if_else(label == "INR", valuenum, NA_real_),
         TBil = if_else(label == "TBil", valuenum, NA_real_))

mimic_labs_df$label <- NULL
mimic_labs_df$valuenum <- NULL

# 2.5. Check for any NAs and NULLs in storetime
has_na <- any(is.na(mimic_labs_df$storetime))
has_null <- any(is.null(mimic_labs_df$storetime))

# 2.6. Save labs to a csv
write.csv(mimic_labs_df, "Outputs/labs_meld_clean.csv", row.names = FALSE)

## 3. Filter labs that have all five labels and keep the earliest set. [Run only once]
# 3.1. Load cleaned labs from a csv & transform storetime to POSIXct
mimic_labs_df <- read.csv("Outputs/labs_meld_clean.csv")

if (!inherits(mimic_labs_df$storetime, "POSIXct")) {
  # Add time   
  mimic_labs_df$storetime <- ifelse(grepl(":", mimic_labs_df$storetime),
                                    mimic_labs_df$storetime,
                                    paste(mimic_labs_df$storetime, "00:00:00"))
  mimic_labs_df$storetime <- as.POSIXct(mimic_labs_df$storetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

# Check for any NAs and NULLs in storetime
has_na <- any(is.na(mimic_labs_df$storetime))
has_null <- any(is.null(mimic_labs_df$storetime))

# 3.2. Filter labs that have all five labels and keep the earliest set.  [1hr compute time!]
# Get all unique hadm_id values from the labs
unique_hadm_ids <- unique(mimic_labs_df$hadm_id)

# Initiate an empty dataframe
mimic_labs_filtered_df <- data.frame()

# Loop through each unique hadm_id and find the best cluster of labs for each hadm_id
for (id in unique_hadm_ids) {
  # Subset the dataframe based for each hadm_id 
  df_subset <- mimic_labs_df %>% filter(hadm_id == id)
  
  # find_best_labs() in that subset
  process_result <- tryCatch({
    find_best_labs(df_subset, time_window = max_labs_time_range, optimize = "storetime") # Take max_labs_time_range between labs and pick first storetime
  }, error = function(e) {
    # Handle potential errors
    cat("buggy hadm_id:", id, "\n")
    NULL  # Return a safe value or perform another recovery action
  })
  
  # Check if process_result is not null before binding
  if (!is.null(process_result)) {
    mimic_labs_filtered_df <- rbind(mimic_labs_filtered_df, process_result)
    # Unnest the resulting dataframe 
    mimic_labs_filtered_df <- unnest(mimic_labs_filtered_df, cols = everything())
  }
}

# 3.3. Save filtered labs to a csv
write.csv(mimic_labs_filtered_df, "Outputs/mimic_labs_filtered.csv", row.names = FALSE)

## 4. Clean mimic_liver_df.
# 4.1. Delete unneeded columns.
mimic_liver_df <- mimic_liver_df %>%
  select(-seq_num, -icd_code, -icd_version)

# 4.2. Convert chartdate to POSIXct
mimic_liver_df$chartdate <- as.POSIXct(mimic_liver_df$chartdate, format = "%Y-%m-%d", tz = "UTC")

# 4.3. Check if any patients received > 1 transplant and keep only the earliest event.
mimic_liver_df <- mimic_liver_df %>%
  arrange(subject_id, chartdate) %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()


## 5. Clean dialysis. 
# 5.1. Keep only dialysis of interest listed in mimic_admissions_meld_df
mimic_dialysis_df <- mimic_dialysis_df %>%
  filter(hadm_id %in% unique(mimic_admissions_meld_df$hadm_id))

# 5.2. Remove unneded columns 
mimic_dialysis_df <- mimic_dialysis_df %>%
  select(-seq_num, -icd_code, -icd_version)

# 5.3. Convert chartdate to POSIXct
mimic_dialysis_df$chartdate <- as.POSIXct(mimic_dialysis_df$chartdate, format = "%Y-%m-%d", tz = "UTC")

rm(df_subset, mimic_labs_df, has_na, has_null, id, max_labs_time_range, process_result, unique_hadm_ids)

################################################################
###     MIMIC-IV             Data flow.                      ###
################################################################

## Load pre-saved labs data 
# Load filtered labs from cvs 
mimic_labs_filtered_df <- read.csv("Outputs/mimic_labs_filtered.csv")

# Update storetime format to POSIXct if it's not
if (!inherits(mimic_labs_filtered_df$storetime, "POSIXct")) {
  # Add time for cases where 00:00:00 is missing
  mimic_labs_filtered_df$storetime <- ifelse(grepl(":", mimic_labs_filtered_df$storetime),
                                             mimic_labs_filtered_df$storetime,
                                             paste(mimic_labs_filtered_df$storetime, "00:00:00"))
  mimic_labs_filtered_df$storetime <- as.POSIXct(mimic_labs_filtered_df$storetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

# Check for any NAs and NULLs in storetime
has_na <- any(is.na(mimic_labs_filtered_df$storetime))
has_null <- any(is.null(mimic_labs_filtered_df$storetime))


## Set thresholds 
age_min <- 18
age_max <- 89

## 1. Count all unique admissions in MIMIC-IV. 523,740
count_all_admissions <- nrow(mimic_admissions_all_df)

## 2. Count admissions of interest only. 18,054
count_admissions_meld <- nrow(mimic_admissions_meld_df)

## 3. Filter by age 
# 3.1. Filter admissions where age > 18 & age < 89    
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  filter(anchor_age > age_min, anchor_age < age_max)

# 3.2. Count unique admissions of interest in the age range. 17,958
count_admissions_meld_proper_age <- nrow(mimic_admissions_meld_df)

## 4. Filter only admissions that have ethnicity data.
# 4.1. Enrich ethnicity using all admissions. 
mimic_admissions_meld_df <- enrich_ethnicity(mimic_admissions_meld_df, mimic_admissions_all_df)

# 4.2. Filter admissions by the ethnicity of interest: "ASIAN", "BLACK/AFRICAN AMERICAN", "HISPANIC/LATINO", "WHITE".
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  filter(ethnicity %in% c("ASIAN", "BLACK/AFRICAN AMERICAN", "HISPANIC/LATINO", "WHITE"))

# 4.3. Count the admissions with ethnicity. 16,556
count_admissions_meld_proper_age_ethnicity <- nrow(mimic_admissions_meld_df)

## 5. Filter only admissions that have complete labs data.
# 5.1. Get admissions from the labs dataframe
admissions_in_labs <- mimic_labs_filtered_df$hadm_id

# 5.2. Filter admissions that have labs
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  filter(hadm_id %in% admissions_in_labs)

# 5.3 Add labs to admissions_meld_df 
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  left_join(select(mimic_labs_filtered_df, hadm_id, Alb, Cr, INR, Na, TBil, storetime), by = "hadm_id")

# 5.4. Count admissions with labs. 10,500
count_admissions_meld_proper_age_ethnicity_labs <- nrow(mimic_admissions_meld_df)

## 6. Keep only the first admission for each patient.
# 6.1. Filter by unique patients and admittime. 
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  arrange(subject_id, admittime) %>% # Arrange by subject_id and admittime
  group_by(subject_id) %>% # Group by patient ID
  slice(1) %>% # Select the first row for each group (earliest admission)
  ungroup() # Remove the grouping

# 6.2. Count only one admission of interest in the age range for each patient. 4,306
count_admissions_meld_proper_age_ethnicity_labs_unique_patient <- nrow(mimic_admissions_meld_df)

################################################################
###     MIMIC-IV          Add outcomes of interest.          ###
################################################################

# Set expiration threshold 
expiration_timeframe <- as.difftime(90, units = "days")

## 1. Add liver_transplant on the same admission within expiration_timeframe.
# Add an empty 'transplant_adm' column to admissions_meld_df, initializing as FALSE
mimic_admissions_meld_df$transplant_adm <- FALSE

# Iterate over rows in admissions_meld_df and check mimic_liver_df if the patient received a transplant on the same admission 
for (i in 1:nrow(mimic_admissions_meld_df)) {
  adm_id <- mimic_admissions_meld_df$hadm_id[i]
  lab_time <- mimic_admissions_meld_df$storetime[i]
  
  # Find the row from mimic_liver_df for the hadm_id
  liver_row <- mimic_liver_df %>% filter(hadm_id == adm_id)

  # If a row is found, check if the transplant was received within expiration_timeframe
  if (nrow(liver_row) == 1) {
    time_to_outcome <- difftime(liver_row$chartdate, lab_time, units = "days") < expiration_timeframe
    
    # Update transplant_adm to TRUE if within expiration_timeframe
    if (time_to_outcome) {
      mimic_admissions_meld_df$transplant_adm[i] <- TRUE
    }
  }
}

# Count transplant during admission
transplant_adm_counts <- table(mimic_admissions_meld_df$transplant_adm)
print(transplant_adm_counts)

## 2. Add expired on the same admission within expiration_timeframe if not received a transplant on that admission 
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(expired_adm = ifelse(hospital_expire_flag == 1 & 
                                !transplant_adm & 
                                difftime(dischtime, storetime, units = "days") < expiration_timeframe, 
                              TRUE, FALSE))

# Count expired at admission
expired_adm_counts <- table(mimic_admissions_meld_df$expired_adm)
print(expired_adm_counts)

## 3. Add outcome on admission if received a liver transplant or expired.
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(outcome_adm = ifelse(transplant_adm | expired_adm, TRUE, FALSE))

# Count outcome at admission
outcome_adm_counts <- table(mimic_admissions_meld_df$outcome_adm)
print(outcome_adm_counts)

# 4. Add liver_transplant on any admission within expiration_timeframe.
# Add an empty 'transplant' column to admissions_meld_df, initializing as FALSE
mimic_admissions_meld_df$transplant_90_days <- FALSE

# Iterate over rows in admissions_meld_df and check mimic_liver_df if the patient received a transplant 
for (i in 1:nrow(mimic_admissions_meld_df)) {
  patient_id <- mimic_admissions_meld_df$subject_id[i]
  lab_time <- mimic_admissions_meld_df$storetime[i]
  
  # Find the row from mimic_liver_df for the hadm_id
  liver_row <- mimic_liver_df %>% filter(subject_id == patient_id)
  
  # If a row is found, check if the transplant was received within expiration_timeframe
  if (nrow(liver_row) == 1) {
    time_to_outcome <- difftime(liver_row$chartdate, lab_time, units = "days") < expiration_timeframe
    
    # Update transplant_adm to TRUE if within expiration_timeframe
    if (time_to_outcome) {
      mimic_admissions_meld_df$transplant_90_days[i] <- TRUE
    }
  }
}

# Count transplants
transplant_90_days_counts <- table(mimic_admissions_meld_df$transplant_90_days)
print(transplant_90_days_counts)

# 5. Add expired within expiration_timeframe if not received a transplant. 
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(expired_90_days = ifelse(transplant_90_days == FALSE & 
                                    !is.na(dod) & difftime(dod, storetime, units = "days") < expiration_timeframe, 
                                  TRUE, FALSE))

# Count expirations
expired_90_days_counts <- table(mimic_admissions_meld_df$expired_90_days)
print(expired_90_days_counts)

# 6. Add outcome if received a liver transplant or expired within expiration_timeframe.
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(outcome_90_days = ifelse(transplant_90_days | expired_90_days, TRUE, FALSE))

# Count outcomes in 90 days 
outcome_90_days_counts <- table(mimic_admissions_meld_df$outcome_90_days)
print(outcome_90_days_counts)

################################################################
###  MIMIC-IV  Prepare mimic_admissions_meld for analysis    ###
################################################################
## Set thresholds
max_dialysis_range <- 24 # hours

## 1. Add dialysis flag.
mimic_admissions_meld_df$dialysis <- FALSE

# Iterate over rows in admissions_meld_df and check mimic_dialysis_df if the patient received dialysis within 
for (i in 1:nrow(mimic_admissions_meld_df)) {
  patient_id <- mimic_admissions_meld_df$subject_id[i]
  lab_time <- mimic_admissions_meld_df$storetime[i]
  
  # Find the rows from mimic_dialysis_df for the patient_id
  dialysis_rows <- mimic_dialysis_df %>% filter(subject_id == patient_id)
  
  # If a row is found, check if the dialysis was received within max_dialysis_range hours
  if (nrow(dialysis_rows) >= 1) {
    # Calculate time difference in hours between each dialysis chartdate and lab_time
    dialysis_rows <- dialysis_rows %>% 
      mutate(time_difference = abs(as.numeric(difftime(chartdate, lab_time, units = "hours"))))
    
    # Find the minimum time difference and check if it's within the max_dialysis_range
    if (min(dialysis_rows$time_difference, na.rm = TRUE) <= max_dialysis_range) {
      mimic_admissions_meld_df$dialysis[i] <- TRUE
    }
  }
}

# Count dialysis
dialysis_counts <- table(mimic_admissions_meld_df$dialysis)
print(dialysis_counts)

## 2. Delete unneeded columns.
mimic_admissions_meld_df$dod <- NULL
mimic_admissions_meld_df$hospital_expire_flag <- NULL

## 3. Rename columns 
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  rename(
    age = anchor_age,
    labtime = storetime
  )

## 4. Add total expired, transplant, and outcome to align with EMORY 
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(
    transplant = ifelse(transplant_adm | transplant_90_days, TRUE, FALSE),
    expired = ifelse(expired_adm | expired_90_days, TRUE, FALSE),
    outcome = ifelse(outcome_adm | outcome_90_days, TRUE, FALSE)
  )

## 5. Save ready for analysis dataframe to a csv
write.csv(mimic_admissions_meld_df, "Outputs/mimic_admissions_meld_df.csv", row.names = FALSE)

################################################################
###  MIMIC-IV             Analysis                           ###
################################################################
## 1. Load prepared admissions data 
mimic_admissions_meld_df <- read.csv("Outputs/mimic_admissions_meld_df.csv")


## 5. Add length of stay (los) in days.
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(los = as.numeric(difftime(dischtime, admittime, units = "days")))

## 6. Add female as a dichotomous column
mimic_admissions_meld_df$female <- ifelse(mimic_admissions_meld_df$gender == "F", 1, 0)

## 7. Update ethnicities 
mimic_admissions_meld_df$ethnicity[mimic_admissions_meld_df$ethnicity == "BLACK/AFRICAN AMERICAN"] <- "AFRICAN AMERICAN"

## 8. Add MELD scores
# 8.1. Set thresholds 
max_na <- 137
min_na <- 125
min_cr <- 1
min_bl <- 1
min_inr <- 1
max_cr <- 4
max_cr_meld3 <- 3
min_alb <- 1.5
max_alb <- 3.5

# 8.2. Add MELD
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(
    Cr_adjusted = ifelse(Cr <= min_cr, min_cr, Cr), # Ensure minimum creatinine is 1 mg/dL
    Cr_adjusted = ifelse(Cr_adjusted >= max_cr | dialysis == TRUE, max_cr, Cr_adjusted), # Cap creatinine at 4 mg/dL or adjust for dialysis
    TBil_adjusted = pmax(TBil, min_bl), # Ensure minimum bilirubin is 1 mg/dL
    INR_adjusted = pmax(INR, min_inr) # Ensure minimum INR is 1
  ) %>%
  # Calculate MELD score using adjusted values
  mutate(
    MELD = round(9.57 * log(Cr_adjusted) + 3.78 * log(TBil_adjusted) + 11.20 * log(INR_adjusted) + 6.43)
  ) %>%
  select(-c(Cr_adjusted, TBil_adjusted, INR_adjusted)) 

# 8.3. Add MELD Na
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(
    Na_adjusted = pmax(pmin(Na, max_na), min_na), # Adjust Na within the min_na to max_na range
    MELD_Na_temp = round(MELD + 1.32 * (137 - Na_adjusted) - (0.033 * MELD * (137 - Na_adjusted))),
    MELD_Na = ifelse(MELD >= 11, MELD_Na_temp, MELD)
  ) %>%
  select(-c(Na_adjusted, MELD_Na_temp)) 

# 8.4. Add MELD_3
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(
    # Calculate the gender factor (1.33 if female, 0 otherwise)
    gender_factor = ifelse(gender == "F", 1.33, 0),
    Alb_adjusted = ifelse(Alb <= min_alb, min_alb, Alb), # Ensure minimum albumin is 1.5 g/dL
    Alb_adjusted = ifelse(Alb_adjusted >= max_alb, max_alb, Alb_adjusted), # Ensure maximum albumin is 3.5 g/dL
    Cr_adjusted = ifelse(Cr <= min_cr, min_cr, Cr), # Ensure minimum creatinine is 1 mg/dL
    Cr_adjusted = ifelse(Cr_adjusted >= max_cr_meld3 | dialysis == TRUE, max_cr_meld3, Cr_adjusted), # Cap creatinine at 3 mg/dL
    TBil_adjusted = pmax(TBil, min_bl), # Ensure minimum bilirubin is 1 mg/dL
    INR_adjusted = pmax(INR, min_inr), # Ensure minimum INR is 1
    Na_adjusted = ifelse(Na <= min_na, min_na, Na), # Ensure minimum Na is 125 mg/dL
    Na_adjusted = ifelse(Na_adjusted >= max_na, max_na, Na_adjusted), # Ensure max Na is 137 mg/dL
    
    # Calculate loge (natural logarithm) of bilirubin, INR, creatinine, and albumin difference
    loge_bilirubin = log(TBil_adjusted),
    loge_INR = log(INR_adjusted),
    loge_creatinine = log(Cr_adjusted),
    albumin_diff = 3.5 - Alb_adjusted,
    
    MELD_3 = round(gender_factor +
                     4.56 * loge_bilirubin +
                     0.82 * (137 - Na_adjusted) -
                     0.24 * (137 - Na_adjusted) * loge_bilirubin +
                     9.09 * loge_INR +
                     11.14 * loge_creatinine +
                     1.85 * albumin_diff -
                     1.83 * albumin_diff * loge_creatinine +
                     6
    )) %>%
  select(-c(Alb_adjusted, Cr_adjusted, TBil_adjusted, INR_adjusted, Na_adjusted, loge_bilirubin, loge_INR, loge_creatinine, 
            albumin_diff, gender_factor))


## 9. Add COX mortality rate for MELD Na and MELD 3.0 
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(
    # Calculate mortality probability for MELD Na
    mortality_meld = 1 - (0.98465 ^ exp(0.1635 * (MELD_Na - 10))),
    
    # Calculate mortality probability for MELD 3.0
    mortality_meld_3 = 1 - (0.946 ^ exp(0.17698 * MELD_3 - 3.56))
  )


#### Save an interim dataframe as a csv for EMORY
write.csv(mimic_admissions_meld_df, "Outputs/mimic_admissions_meld_df_for_zach.csv", row.names = FALSE)


## 10. Create outcomes as categorical for table 1
mimic_admissions_meld_df <- mimic_admissions_meld_df %>%
  mutate(
    # Support for MIMIC 
    admission_outcome_type = case_when(
      expired_adm ~ "Death", 
      transplant_adm ~ "Liver transplant",
      TRUE ~ NA_character_  # Default case if none of the above is TRUE
    ),
    days_90_outcome_type = case_when(
      expired_90_days ~ "Death",
      transplant_90_days ~ "Liver transplant",
      TRUE ~ NA_character_  # Default case if none of the above is TRUE
    ),
    # Support for EMORY
    outcome_type = case_when(
      expired ~ "Death",
      transplant ~ "Liver transplant",
      TRUE ~ NA_character_  # Default case if none of the above is TRUE
    )
  )


## 11. Table 1
mimic_table_one <- 
  mimic_admissions_meld_df %>%
  select(age, female, ethnicity, los, Alb, Cr, INR, Na, TBil, MELD, MELD_Na, MELD_3, 
         outcome_90_days, days_90_outcome_type, outcome_adm, admission_outcome_type) %>%
  tbl_summary(
    by = ethnicity,
    label = list(age ~ "Age (years)", 
                 Alb ~ "Albumin (g/dL)",
                 Cr ~ "Creatinine (mg/dL)",
                 INR ~ "PT/INR",
                 Na ~ "Sodium (mg/dL)",
                 TBil ~ "Total bilirubin (mg/dL)",
                 female ~ "Female", 
                 MELD_Na ~ "MELD Na",
                 MELD_3 ~ "MELD 3.0",
                 outcome_90_days ~ "Total outcomes within 90 days",
                 days_90_outcome_type ~ "Outcomes within 90 days",
                 outcome_adm ~ "Total outcomes during admission",
                 admission_outcome_type ~ "Outcomes during admission",
                 los ~ "Length of stay (days)"),
    digits = list(age ~ c(0, 0), 
                  los ~ c(0, 0),
                  Na ~ c(0, 0),
                  Alb ~ c(2, 2), 
                  Cr ~ c(2, 2),
                  INR ~ c(2, 2),
                  TBil ~ c(2, 2)),
    type = list(female ~ "dichotomous"),
    missing = "no"
    ) %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>%
  add_overall() %>%
  add_p %>%
  bold_labels() %>%
  italicize_levels()

mimic_table_one
gt::gtsave(as_gt(mimic_table_one), file = "Outputs/mimic_table1.rtf")


## 12. Compute ROCs 
# 12.1. Ensure the outcome variables are binary and has two levels and ethnicity is a factor
mimic_admissions_meld_df$outcome_adm <- factor(mimic_admissions_meld_df$outcome_adm, levels = c(FALSE, TRUE), labels = c("negative", "positive"))
mimic_admissions_meld_df$outcome_90_days <- factor(mimic_admissions_meld_df$outcome_90_days, levels = c(FALSE, TRUE), labels = c("negative", "positive"))
mimic_admissions_meld_df$ethnicity <- factor(mimic_admissions_meld_df$ethnicity)


# 12.2. MELD Na, 90 days 
roc_meld_outcomes_90_races_all <- roc(mimic_admissions_meld_df$outcome_90_days, 
                                      mimic_admissions_meld_df$mortality_meld)
roc_meld_outcomes_90_races_aa <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "AFRICAN AMERICAN") %>% .$outcome_90_days, 
                                      mimic_admissions_meld_df %>% filter(ethnicity == "AFRICAN AMERICAN") %>% .$mortality_meld)
roc_meld_outcomes_90_races_asian <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "ASIAN") %>% .$outcome_90_days, 
                                        mimic_admissions_meld_df %>% filter(ethnicity == "ASIAN") %>% .$mortality_meld)
roc_meld_outcomes_90_races_hispanic <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "HISPANIC/LATINO") %>% .$outcome_90_days, 
                                        mimic_admissions_meld_df %>% filter(ethnicity == "HISPANIC/LATINO") %>% .$mortality_meld)
roc_meld_outcomes_90_races_white <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "WHITE") %>% .$outcome_90_days, 
                                        mimic_admissions_meld_df %>% filter(ethnicity == "WHITE") %>% .$mortality_meld)
# 12.3. MELD 3, 90 days 
roc_meld_3_outcomes_90_races_all <- roc(mimic_admissions_meld_df$outcome_90_days, 
                                        mimic_admissions_meld_df$mortality_meld_3)
roc_meld_3_outcomes_90_races_aa <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "AFRICAN AMERICAN") %>% .$outcome_90_days, 
                                       mimic_admissions_meld_df %>% filter(ethnicity == "AFRICAN AMERICAN") %>% .$mortality_meld_3)
roc_meld_3_outcomes_90_races_asian <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "ASIAN") %>% .$outcome_90_days, 
                                          mimic_admissions_meld_df %>% filter(ethnicity == "ASIAN") %>% .$mortality_meld_3)
roc_meld_3_outcomes_90_races_hispanic <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "HISPANIC/LATINO") %>% .$outcome_90_days, 
                                             mimic_admissions_meld_df %>% filter(ethnicity == "HISPANIC/LATINO") %>% .$mortality_meld_3)
roc_meld_3_outcomes_90_races_white <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "WHITE") %>% .$outcome_90_days, 
                                          mimic_admissions_meld_df %>% filter(ethnicity == "WHITE") %>% .$mortality_meld_3)

# 12.4. MELD Na, during admission
roc_meld_outcomes_adm_races_all <- roc(mimic_admissions_meld_df$outcome_adm, 
                                       mimic_admissions_meld_df$mortality_meld)
roc_meld_outcomes_adm_races_aa <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "AFRICAN AMERICAN") %>% .$outcome_adm, 
                                      mimic_admissions_meld_df %>% filter(ethnicity == "AFRICAN AMERICAN") %>% .$mortality_meld)
roc_meld_outcomes_adm_races_asian <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "ASIAN") %>% .$outcome_adm, 
                                         mimic_admissions_meld_df %>% filter(ethnicity == "ASIAN") %>% .$mortality_meld)
roc_meld_outcomes_adm_races_hispanic <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "HISPANIC/LATINO") %>% .$outcome_adm, 
                                            mimic_admissions_meld_df %>% filter(ethnicity == "HISPANIC/LATINO") %>% .$mortality_meld)
roc_meld_outcomes_adm_races_white <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "WHITE") %>% .$outcome_adm, 
                                         mimic_admissions_meld_df %>% filter(ethnicity == "WHITE") %>% .$mortality_meld)

# 12.5. MELD 3, during admission
roc_meld_3_outcomes_adm_races_all <- roc(mimic_admissions_meld_df$outcome_adm, 
                                         mimic_admissions_meld_df$mortality_meld_3)
roc_meld_3_outcomes_adm_races_aa <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "AFRICAN AMERICAN") %>% .$outcome_adm, 
                                        mimic_admissions_meld_df %>% filter(ethnicity == "AFRICAN AMERICAN") %>% .$mortality_meld_3)
roc_meld_3_outcomes_adm_races_asian <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "ASIAN") %>% .$outcome_adm, 
                                           mimic_admissions_meld_df %>% filter(ethnicity == "ASIAN") %>% .$mortality_meld_3)
roc_meld_3_outcomes_adm_races_hispanic <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "HISPANIC/LATINO") %>% .$outcome_adm, 
                                              mimic_admissions_meld_df %>% filter(ethnicity == "HISPANIC/LATINO") %>% .$mortality_meld_3)
roc_meld_3_outcomes_adm_races_white <- roc(mimic_admissions_meld_df %>% filter(ethnicity == "WHITE") %>% .$outcome_adm, 
                                           mimic_admissions_meld_df %>% filter(ethnicity == "WHITE") %>% .$mortality_meld_3)

# 13. Compute p_values for AUROCs
auroc_p_value_meld_90_days <- compute_combined_pvalue(mimic_admissions_meld_df,
                                                      outcome_col = "outcome_90_days",
                                                      predictor_col = "mortality_meld",
                                                      group_col = "ethnicity")

auroc_p_value_meld_3_90_days <- compute_combined_pvalue(mimic_admissions_meld_df,
                                                      outcome_col = "outcome_90_days",
                                                      predictor_col = "mortality_meld_3",
                                                      group_col = "ethnicity")

auroc_p_value_meld_adm <- compute_combined_pvalue(mimic_admissions_meld_df,
                                                      outcome_col = "outcome_adm",
                                                      predictor_col = "mortality_meld",
                                                      group_col = "ethnicity")

auroc_p_value_meld_3_adm <- compute_combined_pvalue(mimic_admissions_meld_df,
                                                        outcome_col = "outcome_adm",
                                                        predictor_col = "mortality_meld_3",
                                                        group_col = "ethnicity")

# 14. Store AUROCs, CI-s, and p-values to a dataframe and save as an csv
# Create a data frame to store the results
meld_aurocs_ci_df <- data.frame(
  Dataset = rep(c("MIMIC-IV"), each=20),
  Score = rep(c("MELD-Na", "MELD 3.0", "MELD-Na", "MELD 3.0"), each=5),
  Period = rep(c("During admission", "During admission", "90 days", "90 days"), each=5),
  "Race/Ethnicity" = rep(c("All races", "African American", "Asian", "Hispanic/Latino", "White"), 4),
  AUROC = numeric(20),
  CI = character(20),
  p_value = character(20),
  stringsAsFactors = FALSE
)

# List of ROC objects
roc_objects <- list(
  roc_meld_outcomes_adm_races_all,
  roc_meld_outcomes_adm_races_aa,
  roc_meld_outcomes_adm_races_asian,
  roc_meld_outcomes_adm_races_hispanic,
  roc_meld_outcomes_adm_races_white,
  roc_meld_3_outcomes_adm_races_all,
  roc_meld_3_outcomes_adm_races_aa,
  roc_meld_3_outcomes_adm_races_asian,
  roc_meld_3_outcomes_adm_races_hispanic,
  roc_meld_3_outcomes_adm_races_white,
  roc_meld_outcomes_90_races_all,
  roc_meld_outcomes_90_races_aa,
  roc_meld_outcomes_90_races_asian,
  roc_meld_outcomes_90_races_hispanic,
  roc_meld_outcomes_90_races_white,
  roc_meld_3_outcomes_90_races_all,
  roc_meld_3_outcomes_90_races_aa,
  roc_meld_3_outcomes_90_races_asian,
  roc_meld_3_outcomes_90_races_hispanic,
  roc_meld_3_outcomes_90_races_white
)

# Populate the data frame with AUROC and CI values
for (i in 1:length(roc_objects)) {
  auroc_ci <- get_auroc_ci(roc_objects[[i]])
  meld_aurocs_ci_df$AUROC[i] <- auroc_ci[1]
  meld_aurocs_ci_df$CI[i] <- paste0("(", round(auroc_ci[2], 3), ", ", round(auroc_ci[3], 3), ")")
}


# Add p-values to the meld_aurocs_ci_df 
meld_aurocs_ci_df$p_value[1] <- auroc_p_value_meld_adm
meld_aurocs_ci_df$p_value[6] <- auroc_p_value_meld_3_adm
meld_aurocs_ci_df$p_value[11] <- auroc_p_value_meld_90_days  
meld_aurocs_ci_df$p_value[16] <- auroc_p_value_meld_3_90_days

# Save meld_aurocs_ci_df as a csv
write.csv(meld_aurocs_ci_df, "Outputs/meld_aurocs_ci_df.csv", row.names = FALSE)

## 14. Calculate across group differences of AUROCs without Asians 
# 14.1 Filter w/o Asians 
mimic_admissions_meld_df_no_asians <- mimic_admissions_meld_df %>%
  mutate(ethnicity = as.character(ethnicity)) %>%
  filter(ethnicity != "ASIAN") %>%
  mutate(ethnicity = as.factor(ethnicity))

# 14.2 Recompute ROCs with dropped Asians
roc_meld_outcomes_90_races_all_no_asians <- roc(mimic_admissions_meld_df_no_asians$outcome_90_days, 
                                                mimic_admissions_meld_df_no_asians$mortality_meld)

roc_meld_3_outcomes_90_races_all_no_asians <- roc(mimic_admissions_meld_df_no_asians$outcome_90_days, 
                                                  mimic_admissions_meld_df_no_asians$mortality_meld_3)

roc_meld_outcomes_adm_races_all_no_asians <- roc(mimic_admissions_meld_df_no_asians$outcome_adm, 
                                                 mimic_admissions_meld_df_no_asians$mortality_meld)

roc_meld_3_outcomes_adm_races_all_no_asians <- roc(mimic_admissions_meld_df_no_asians$outcome_adm, 
                                                   mimic_admissions_meld_df_no_asians$mortality_meld_3)

# 14.3 Recompute p-values with dropped Asians
auroc_p_value_meld_90_days_no_asians <- compute_combined_pvalue(mimic_admissions_meld_df_no_asians,
                                                      outcome_col = "outcome_90_days",
                                                      predictor_col = "mortality_meld",
                                                      group_col = "ethnicity")

auroc_p_value_meld_3_90_days_no_asians <- compute_combined_pvalue(mimic_admissions_meld_df_no_asians,
                                                        outcome_col = "outcome_90_days",
                                                        predictor_col = "mortality_meld_3",
                                                        group_col = "ethnicity")

auroc_p_value_meld_adm_no_asians <- compute_combined_pvalue(mimic_admissions_meld_df_no_asians,
                                                  outcome_col = "outcome_adm",
                                                  predictor_col = "mortality_meld",
                                                  group_col = "ethnicity")

auroc_p_value_meld_3_adm_no_asians <- compute_combined_pvalue(mimic_admissions_meld_df_no_asians,
                                                    outcome_col = "outcome_adm",
                                                    predictor_col = "mortality_meld_3",
                                                    group_col = "ethnicity")



# 15. Save AUROCS, CIs, and p-values for MIMIC-IV with no Asians into a dataframe and a csv
# Create a data frame to store the results
meld_aurocs_ci_no_asians_df <- data.frame(
  Dataset = rep(c("MIMIC-IV"), each=16),
  Score = rep(c("MELD-Na", "MELD 3.0", "MELD-Na", "MELD 3.0"), each=4),
  Period = rep(c("During admission", "During admission", "90 days", "90 days"), each=4),
  "Race/Ethnicity" = rep(c("All races", "African American", "Hispanic/Latino", "White"), 4),
  AUROC = numeric(16),
  CI = character(16),
  p_value = character(16),
  stringsAsFactors = FALSE
)

# List of ROC objects
roc_objects_no_asians <- list(
  roc_meld_outcomes_adm_races_all_no_asians,
  roc_meld_outcomes_adm_races_aa,
  roc_meld_outcomes_adm_races_hispanic,
  roc_meld_outcomes_adm_races_white,
  roc_meld_3_outcomes_adm_races_all_no_asians,
  roc_meld_3_outcomes_adm_races_aa,
  roc_meld_3_outcomes_adm_races_hispanic,
  roc_meld_3_outcomes_adm_races_white,
  roc_meld_outcomes_90_races_all_no_asians,
  roc_meld_outcomes_90_races_aa,
  roc_meld_outcomes_90_races_hispanic,
  roc_meld_outcomes_90_races_white,
  roc_meld_3_outcomes_90_races_all_no_asians,
  roc_meld_3_outcomes_90_races_aa,
  roc_meld_3_outcomes_90_races_hispanic,
  roc_meld_3_outcomes_90_races_white
)

# Populate the data frame with AUROC and CI values
for (i in 1:length(roc_objects_no_asians)) {
  auroc_ci <- get_auroc_ci(roc_objects_no_asians[[i]])
  meld_aurocs_ci_no_asians_df$AUROC[i] <- auroc_ci[1]
  meld_aurocs_ci_no_asians_df$CI[i] <- paste0("(", round(auroc_ci[2], 3), ", ", 
                                              round(auroc_ci[3], 3), ")")
}

# Add p-values to the meld_aurocs_ci_df 
meld_aurocs_ci_no_asians_df$p_value[1] <- auroc_p_value_meld_adm_no_asians
meld_aurocs_ci_no_asians_df$p_value[5] <- auroc_p_value_meld_3_adm_no_asians
meld_aurocs_ci_no_asians_df$p_value[9] <- auroc_p_value_meld_90_days_no_asians
meld_aurocs_ci_no_asians_df$p_value[13] <- auroc_p_value_meld_3_90_days_no_asians 

# Save meld_aurocs_ci_df as a csv
write.csv(meld_aurocs_ci_no_asians_df, "Outputs/meld_aurocs_ci_no_asians_df.csv", row.names = FALSE)

## 16. Compute AUROCs, CIs, and p-values for Hispanics vs Non-Hispanics and and AA vs Non-AA

# 16.1 Create new dataframes with new ethnicity factors  
mimic_admissions_meld_df_hispanic <- mimic_admissions_meld_df %>%
  mutate(ethnicity = as.character(ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity == "HISPANIC/LATINO", "HISPANIC/LATINO", "NON-HISPANIC/LATINO")) %>%
  mutate(ethnicity = as.factor(ethnicity))

mimic_admissions_meld_df_aa <- mimic_admissions_meld_df %>%
  mutate(ethnicity = as.character(ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity == "AFRICAN AMERICAN", "AFRICAN AMERICAN", "NOT AFRICAN AMERICAN")) %>%
  mutate(ethnicity = as.factor(ethnicity))

# 16.2 Recompute ROCs for NON-HISPANIC/LATINO and NOT AFRICAN AMERICAN 
# NON-HISPANIC/LATINO
roc_meld_outcomes_adm_races_non_hispanic <- roc(mimic_admissions_meld_df_hispanic %>% filter(ethnicity != "HISPANIC/LATINO") %>% .$outcome_adm, 
                                                mimic_admissions_meld_df_hispanic %>% filter(ethnicity != "HISPANIC/LATINO") %>% .$mortality_meld)

roc_meld_3_outcomes_adm_races_non_hispanic <- roc(mimic_admissions_meld_df_hispanic %>% filter(ethnicity != "HISPANIC/LATINO") %>% .$outcome_adm, 
                                                  mimic_admissions_meld_df_hispanic %>% filter(ethnicity != "HISPANIC/LATINO") %>% .$mortality_meld_3)

roc_meld_outcomes_90_races_non_hispanic <- roc(mimic_admissions_meld_df_hispanic %>% filter(ethnicity != "HISPANIC/LATINO") %>% .$outcome_90_days, 
                                               mimic_admissions_meld_df_hispanic %>% filter(ethnicity != "HISPANIC/LATINO") %>% .$mortality_meld)

roc_meld_3_outcomes_90_races_non_hispanic <- roc(mimic_admissions_meld_df_hispanic %>% filter(ethnicity != "HISPANIC/LATINO") %>% .$outcome_90_days, 
                                                 mimic_admissions_meld_df_hispanic %>% filter(ethnicity != "HISPANIC/LATINO") %>% .$mortality_meld_3)

# NOT AFRICAN AMERICAN 
roc_meld_outcomes_adm_races_not_aa <- roc(mimic_admissions_meld_df_aa %>% filter(ethnicity != "AFRICAN AMERICAN") %>% .$outcome_adm, 
                                          mimic_admissions_meld_df_aa %>% filter(ethnicity != "AFRICAN AMERICAN") %>% .$mortality_meld)

roc_meld_3_outcomes_adm_races_not_aa <- roc(mimic_admissions_meld_df_aa %>% filter(ethnicity != "AFRICAN AMERICAN") %>% .$outcome_adm, 
                                            mimic_admissions_meld_df_aa %>% filter(ethnicity != "AFRICAN AMERICAN") %>% .$mortality_meld_3)

roc_meld_outcomes_90_races_not_aa <- roc(mimic_admissions_meld_df_aa %>% filter(ethnicity != "AFRICAN AMERICAN") %>% .$outcome_90_days, 
                                         mimic_admissions_meld_df_aa %>% filter(ethnicity != "AFRICAN AMERICAN") %>% .$mortality_meld)

roc_meld_3_outcomes_90_races_not_aa <- roc(mimic_admissions_meld_df_aa %>% filter(ethnicity != "AFRICAN AMERICAN") %>% .$outcome_90_days, 
                                           mimic_admissions_meld_df_aa %>% filter(ethnicity != "AFRICAN AMERICAN") %>% .$mortality_meld_3)

# 16.3 Recompute p-values Hispanic vs Non-Hispanic
# HISPANIC/LATINO vs NON-HISPANIC/LATINO
auroc_p_value_meld_adm_hispanic <- compute_combined_pvalue(mimic_admissions_meld_df_hispanic,
                                                           outcome_col = "outcome_adm",
                                                           predictor_col = "mortality_meld",
                                                           group_col = "ethnicity")

auroc_p_value_meld_3_adm_hispanic <- compute_combined_pvalue(mimic_admissions_meld_df_hispanic,
                                                             outcome_col = "outcome_adm",
                                                             predictor_col = "mortality_meld_3",
                                                             group_col = "ethnicity")

auroc_p_value_meld_90_days_hispanic <- compute_combined_pvalue(mimic_admissions_meld_df_hispanic,
                                                               outcome_col = "outcome_90_days",
                                                               predictor_col = "mortality_meld",
                                                               group_col = "ethnicity")

auroc_p_value_meld_3_90_days_hispanic <- compute_combined_pvalue(mimic_admissions_meld_df_hispanic,
                                                                 outcome_col = "outcome_90_days",
                                                                 predictor_col = "mortality_meld_3",
                                                                 group_col = "ethnicity")


# AFRICAN AMERICAN vs NOT AFRICAN AMERICAN
auroc_p_value_meld_adm_aa <- compute_combined_pvalue(mimic_admissions_meld_df_aa,
                                                     outcome_col = "outcome_adm",
                                                     predictor_col = "mortality_meld",
                                                     group_col = "ethnicity")

auroc_p_value_meld_3_adm_aa <- compute_combined_pvalue(mimic_admissions_meld_df_aa,
                                                       outcome_col = "outcome_adm",
                                                       predictor_col = "mortality_meld_3",
                                                       group_col = "ethnicity")

auroc_p_value_meld_90_days_aa <- compute_combined_pvalue(mimic_admissions_meld_df_aa,
                                                         outcome_col = "outcome_90_days",
                                                         predictor_col = "mortality_meld",
                                                         group_col = "ethnicity")

auroc_p_value_meld_3_90_days_aa <- compute_combined_pvalue(mimic_admissions_meld_df_aa,
                                                           outcome_col = "outcome_90_days",
                                                           predictor_col = "mortality_meld_3",
                                                           group_col = "ethnicity")


# 16.4. Save AUROCS, CIs, and p-values for MIMIC-IV with HISPANICS vs NON-HISPANICS into a dataframe and a csv
# Create a data frame to store the results
meld_aurocs_ci_hispanics_df <- data.frame(
  Dataset = rep(c("MIMIC-IV"), each=12),
  Score = rep(c("MELD-Na", "MELD 3.0", "MELD-Na", "MELD 3.0"), each=3),
  Period = rep(c("During admission", "During admission", "90 days", "90 days"), each=3),
  "Race/Ethnicity" = rep(c("All races", "Hispanic/Latino", "Non-Hispanic/Latino"), 4),
  AUROC = numeric(12),
  CI = character(12),
  p_value = character(12),
  stringsAsFactors = FALSE
)

# List of ROC objects
roc_objects_hispanics <- list(
  roc_meld_outcomes_adm_races_all,
  roc_meld_outcomes_adm_races_hispanic,
  roc_meld_outcomes_adm_races_non_hispanic,
  roc_meld_3_outcomes_adm_races_all,
  roc_meld_3_outcomes_adm_races_hispanic,
  roc_meld_3_outcomes_adm_races_non_hispanic,
  roc_meld_outcomes_90_races_all,
  roc_meld_outcomes_90_races_hispanic,
  roc_meld_outcomes_90_races_non_hispanic,
  roc_meld_3_outcomes_90_races_all,
  roc_meld_3_outcomes_90_races_hispanic,
  roc_meld_3_outcomes_90_races_non_hispanic
)

# Populate the data frame with AUROC and CI values
for (i in 1:length(roc_objects_hispanics)) {
  auroc_ci <- get_auroc_ci(roc_objects_hispanics[[i]])
  meld_aurocs_ci_hispanics_df$AUROC[i] <- auroc_ci[1]
  meld_aurocs_ci_hispanics_df$CI[i] <- paste0("(", round(auroc_ci[2], 3), ", ", 
                                              round(auroc_ci[3], 3), ")")
}

# Add p-values to the meld_aurocs_ci_hispanics_df 
meld_aurocs_ci_hispanics_df$p_value[1] <- auroc_p_value_meld_adm_hispanic
meld_aurocs_ci_hispanics_df$p_value[4] <- auroc_p_value_meld_3_adm_hispanic
meld_aurocs_ci_hispanics_df$p_value[7] <- auroc_p_value_meld_90_days_hispanic
meld_aurocs_ci_hispanics_df$p_value[10] <- auroc_p_value_meld_3_90_days_hispanic 

# Save meld_aurocs_ci_df as a csv
write.csv(meld_aurocs_ci_hispanics_df, "Outputs/meld_aurocs_ci_hispanics_df.csv", row.names = FALSE)

# 16.5. Save AUROCS, CIs, and p-values for MIMIC-IV with AA vs NOT AA into a dataframe and a csv
# Create a data frame to store the results
meld_aurocs_ci_aa <- data.frame(
  Dataset = rep(c("MIMIC-IV"), each=12),
  Score = rep(c("MELD-Na", "MELD 3.0", "MELD-Na", "MELD 3.0"), each=3),
  Period = rep(c("During admission", "During admission", "90 days", "90 days"), each=3),
  "Race/Ethnicity" = rep(c("All races", "African American", "Not African American"), 4),
  AUROC = numeric(12),
  CI = character(12),
  p_value = character(12),
  stringsAsFactors = FALSE
)

# List of ROC objects
roc_objects_aa <- list(
  roc_meld_outcomes_adm_races_all,
  roc_meld_outcomes_adm_races_aa,
  roc_meld_outcomes_adm_races_not_aa,
  roc_meld_3_outcomes_adm_races_all,
  roc_meld_3_outcomes_adm_races_aa,
  roc_meld_3_outcomes_adm_races_not_aa,
  roc_meld_outcomes_90_races_all,
  roc_meld_outcomes_90_races_aa,
  roc_meld_outcomes_90_races_not_aa,
  roc_meld_3_outcomes_90_races_all,
  roc_meld_3_outcomes_90_races_aa,
  roc_meld_3_outcomes_90_races_not_aa
)

# Populate the data frame with AUROC and CI values
for (i in 1:length(roc_objects_aa)) {
  auroc_ci <- get_auroc_ci(roc_objects_aa[[i]])
  meld_aurocs_ci_aa$AUROC[i] <- auroc_ci[1]
  meld_aurocs_ci_aa$CI[i] <- paste0("(", round(auroc_ci[2], 3), ", ", 
                                              round(auroc_ci[3], 3), ")")
}

# Add p-values to the meld_aurocs_ci_aa 
meld_aurocs_ci_aa$p_value[1] <- auroc_p_value_meld_adm_aa
meld_aurocs_ci_aa$p_value[4] <- auroc_p_value_meld_3_adm_aa
meld_aurocs_ci_aa$p_value[7] <- auroc_p_value_meld_90_days_aa
meld_aurocs_ci_aa$p_value[10] <- auroc_p_value_meld_3_90_days_aa 

# Save meld_aurocs_ci_aa as a csv
write.csv(meld_aurocs_ci_aa, "Outputs/meld_aurocs_ci_aa.csv", row.names = FALSE)


## 17. Plot calibration curves. 
# 17.1. Create a dataframe of MELD scores vs COX. Used to find x values for corresponding MELD scores to add vertical guides. 
meld_to_cox_map_df <- data.frame(
  meld_score = 1:50,
  meld_3_cox_mortality = rep(NA, 50),
  meld_cox_mortality = rep(NA, 50)
)

cox_meld <- function(scores) {
  # Initialize a vector to store the mortality probabilities
  mortality_meld <- numeric(length(scores))
  
  for (i in 1:length(scores)) {
    # Calculate mortality probability for MELD scores
    mortality_meld[i] <- 1 - (0.98465 ^ exp(0.1635 * (scores[i] - 10)))
  }
  
  # Return the calculated mortality probabilities
  return(mortality_meld)
}

cox_meld_3 <- function(scores) {
  mortality_meld_3 <- numeric(length(scores))
  
  for (i in 1:length(scores)) {
    # Calculate mortality probability for MELD 3.0 scores
    mortality_meld_3[i] <- 1 - (0.946 ^ exp(0.17698 * scores[i] - 3.56))
  }
  
  # Return the calculated mortality probabilities
  return(mortality_meld_3)
}


meld_to_cox_map_df$meld_3_cox_mortality <- cox_meld_3(meld_to_cox_map_df$meld_score)
meld_to_cox_map_df$meld_cox_mortality <- cox_meld(meld_to_cox_map_df$meld_score)

# 17.2. Reverse outcomes back to numeric and ethnicity to char.
mimic_admissions_meld_df$outcome_adm <- as.numeric(mimic_admissions_meld_df$outcome_adm) - 1 
mimic_admissions_meld_df$outcome_90_days <- as.numeric(mimic_admissions_meld_df$outcome_90_days) - 1
mimic_admissions_meld_df$ethnicity <- as.character(mimic_admissions_meld_df$ethnicity)

# 17.3. Calibration plot for MELD 3.0 90 days mortality
calibration_df_meld_3_90_days <- create_df_for_calibration_plot(data = mimic_admissions_meld_df,
                                                                mortality_col = "mortality_meld_3",
                                                                outcome_col = "outcome_90_days",
                                                                bin_size = 0.2)

calibration_plot_meld_3_90_days <- ggplot(calibration_df_meld_3_90_days, aes(x = mean_predicted, y = observed, color = ethnicity)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "Mean Predicted Mortality Probability", y = "Observed Outcome Frequency", title = "MELD 3.0, 90 days outcome") +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  guides(color = guide_legend(title = "Ethnicity")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", family = "Arial", hjust = 0.5), # Bold and Arial for title
    axis.title = element_text(size = 16), # Default appearance for axis titles
    axis.text = element_text(size = 14), # Default appearance for axis text
    legend.position = "none" # Remove the legend
  ) +
  annotate("text", x = c(0.052938952, 0.273314849, 0.538599767, 0.846486291), y = 1.05, 
           label = c("20", "30", "35", "40"), size = 5, vjust = 0, hjust = 1.3, color = "black") +
  theme(plot.margin = margin(t = 50, r = 20, b = 40, l = 20)) +
  geom_vline(xintercept = c(0.052938952, 0.273314849, 0.538599767, 0.846486291), linetype = "dotted", color = "blue") # Adding vertical lines

ggsave("Outputs/calibration_plot_meld_3_90_days.jpg", 
       plot = calibration_plot_meld_3_90_days, device = "jpeg", width = 9, height = 6, units = "in", dpi = 300)

# 17.4. Calibration plot for MELD 3.0 at admission outcome
calibration_df_meld_3_outcome_adm <- create_df_for_calibration_plot(data = mimic_admissions_meld_df,
                                                                mortality_col = "mortality_meld_3",
                                                                outcome_col = "outcome_adm",
                                                                bin_size = 0.2)

calibration_plot_meld_3_outcome_adm <- ggplot(calibration_df_meld_3_outcome_adm, aes(x = mean_predicted, y = observed, color = ethnicity)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "Mean Predicted Mortality Probability", y = "Observed Outcome Frequency", title = "MELD 3.0, outcome during admission") +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme(
    plot.title = element_text(size = 16, face = "bold", family = "Arial", hjust = 0.5), # Bold and Arial for the title
    axis.title = element_text(size = 16), # Default appearance for axis titles
    axis.text = element_text(size = 14), # Default appearance for axis text
    legend.position = "none", # This line removes the legend
    plot.margin = margin(t = 50, r = 20, b = 40, l = 20) # Adjusting the margins
  ) +
  annotate("text", x = c(0.052938952, 0.273314849, 0.538599767, 0.846486291), y = 1.05, 
           label = c("20", "30", "35", "40"), size = 5, vjust = 0, hjust = 1.3, color = "black") +
  geom_vline(xintercept = c(0.052938952, 0.273314849, 0.538599767, 0.846486291), linetype = "dotted", color = "blue") # Adding vertical lines


ggsave("Outputs/calibration_plot_meld_3_outcome_adm.jpg", 
       plot = calibration_plot_meld_3_outcome_adm, device = "jpeg", width = 9, height = 6, units = "in", dpi = 300)

calibration_plot_meld_3_outcome_adm

# 17.5. Calibration plot for MELD 90 days mortality
calibration_df_meld_90_days <- create_df_for_calibration_plot(data = mimic_admissions_meld_df,
                                                                mortality_col = "mortality_meld",
                                                                outcome_col = "outcome_90_days",
                                                                bin_size = 0.2)

calibration_plot_meld_90_days <- ggplot(calibration_df_meld_90_days, aes(x = mean_predicted, y = observed, color = ethnicity)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "Mean Predicted Mortality Probability", y = "Observed Outcome Frequency", title = "MELD Na, 90 days outcome") +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme(
    plot.title = element_text(size = 16, face = "bold", family = "Arial", hjust = 0.5), # Bold and Arial for the title
    axis.title = element_text(size = 16), # Default appearance for axis titles
    axis.text = element_text(size = 14), # Default appearance for axis text
    legend.position = "none", # This line removes the legend
    plot.margin = margin(t = 50, r = 20, b = 40, l = 20) # Adjusting the margins
  ) +
  annotate("text", x = c(0.076281349, 0.334363079, 0.602201953, 0.876033688), y = 1.05, 
           label = c("20", "30", "35", "40"), size = 5, vjust = 0, hjust = 1.3, color = "black") +
  geom_vline(xintercept = c(0.076281349, 0.334363079, 0.602201953, 0.876033688), linetype = "dotted", color = "blue") # Adding vertical lines


ggsave("Outputs/calibration_plot_meld_90_days.jpg", 
       plot = calibration_plot_meld_90_days, device = "jpeg", width = 9, height = 6, units = "in", dpi = 300)

calibration_plot_meld_90_days

# 17.6. Calibration plot for MELD at admission outcome
calibration_df_meld_outcome_adm <- create_df_for_calibration_plot(data = mimic_admissions_meld_df,
                                                                    mortality_col = "mortality_meld",
                                                                    outcome_col = "outcome_adm",
                                                                    bin_size = 0.2)

calibration_plot_meld_outcome_adm <- ggplot(calibration_df_meld_outcome_adm, aes(x = mean_predicted, y = observed, color = ethnicity)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "Mean Predicted Mortality Probability", y = "Observed Outcome Frequency", title = "MELD Na, outcome during admission") +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme(
    plot.title = element_text(size = 16, face = "bold", family = "Arial", hjust = 0.5), # Bold and Arial for the title
    axis.title = element_text(size = 16), # Default appearance for axis titles
    axis.text = element_text(size = 14), # Default appearance for axis text
    legend.position = "none", # This line removes the legend
    plot.margin = margin(t = 50, r = 20, b = 40, l = 20) # Adjusting the margins
  ) +
  annotate("text", x = c(0.076281349, 0.334363079, 0.602201953, 0.876033688), y = 1.05, 
           label = c("20", "30", "35", "40"), size = 5, vjust = 0, hjust = 1.3, color = "black") +
  geom_vline(xintercept = c(0.076281349, 0.334363079, 0.602201953, 0.876033688), linetype = "dotted", color = "blue") # Adding vertical lines


ggsave("Outputs/calibration_plot_meld_outcome_adm.jpg", 
       plot = calibration_plot_meld_outcome_adm, device = "jpeg", width = 9, height = 6, units = "in", dpi = 300)

calibration_plot_meld_outcome_adm

# 17.7. Plot the legend 
calibration_plot_meld_outcome_adm_with_legend <- ggplot(calibration_df_meld_outcome_adm, aes(x = mean_predicted, y = observed, color = ethnicity)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Ethnicity") +  # Set the legend title here
  labs(x = "Mean Predicted Mortality Probability", y = "Observed Outcome Frequency", title = "MELD Na, outcome during admission") +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme(
    plot.title = element_text(size = 12, face = "bold", family = "Arial", hjust = 0.5),  # Bold and Arial for the title
    axis.title = element_text(size = 10),  # Default appearance for axis titles
    axis.text = element_text(size = 10),  # Default appearance for axis text
    plot.margin = margin(t = 50, r = 20, b = 40, l = 20),  # Adjusting the margins
    legend.text = element_text(size = 16),  # Increase legend text size
    legend.title = element_text(size = 16, face = "bold")  # Increase legend title size
  ) +
  annotate("text", x = c(0.076281349, 0.334363079, 0.602201953, 0.876033688), y = 1.05,
           label = c("20", "30", "35", "40"), size = 3.7, vjust = 0, hjust = 1.3, color = "black") +
  geom_vline(xintercept = c(0.076281349, 0.334363079, 0.602201953, 0.876033688), linetype = "dotted", color = "blue")  # Adding vertical lines


calibration_plots_legend <- get_legend(calibration_plot_meld_outcome_adm_with_legend)
ggsave("Outputs/calibration_plots_legend.jpg", calibration_plots_legend, width = 5, height = 4)

# 17.8. Plot calibration plots on one plot
image1 <- image_read("Outputs/calibration_plot_meld_outcome_adm.jpg")
image2 <- image_read("Outputs/calibration_plot_meld_3_outcome_adm.jpg")
image3 <- image_read("Outputs/calibration_plot_meld_90_days.jpg")
image4 <- image_read("Outputs/calibration_plot_meld_3_90_days.jpg")
image5 <- image_read("Outputs/emory_calibration_plot_meld_outcome_adm.jpg")
image6 <- image_read("Outputs/emory_calibration_plot_meld_3_outcome_adm.jpg")
legend_image <- image_read("Outputs/calibration_plots_legend.jpg")

# Create a blank image with the same dimensions as the legend for layout purposes
placeholder <- image_blank(width = image_info(legend_image)$width, height = image_info(legend_image)$height, color = "white")


# Combine images horizontally to form two rows
row1 <- image_append(c(image1, image2, placeholder))
row2 <- image_append(c(image3, image4, legend_image))
row3 <- image_append(c(image5, image6, placeholder))


# Combine the two rows vertically to form the final grid
final_image <- image_append(c(row1, row2, row3), stack = TRUE)

# Save the combined image to a JPG file with the specific name
image_write(final_image, "Outputs/all_calibration_plots_in_one.jpg", format = "jpg")

## 18. Plot MELD 3.0 and MELD Na scores vs predicted mortality 
long_meld_to_cox_map_df <- meld_to_cox_map_df %>%
  pivot_longer(cols = c(meld_3_cox_mortality, meld_cox_mortality), 
               names_to = "mortality_type", 
               values_to = "mortality_rate")

# Plot the data
meld_score_mortality_rate_plot <- ggplot(long_meld_to_cox_map_df, aes(x = meld_score, y = mortality_rate, color = mortality_type)) +
  geom_line() +
  labs(x = "MELD 3.0/Na Score", y = "Predicted Mortality Rate", 
       title = "Comparison of Predicted Mortality Rates for MELD 3.0 and MELD Na Scores") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, hjust = 0.5, vjust = 0.5),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")) +
  scale_color_manual(values = c("meld_3_cox_mortality" = "blue", "meld_cox_mortality" = "red"),
                     labels = c("MELD 3.0", "MELD Na")) +
  guides(color = guide_legend(title = "MELD Score"))

ggsave("Outputs/predicted_mortality_meld_3_na_comparison_plot.jpg", 
       plot = meld_score_mortality_rate_plot, device = "jpeg", width = 9, height = 6, units = "in", dpi = 300)

# Display the plot
meld_score_mortality_rate_plot

## 19. Perform the Hosmer-Lemeshow test for all and each ethnicity.
hoslem_test <- function(df, n_bootstraps = 500, fixed_sample_size = 150) {
  # Define the ethnicities and combinations of interest
  ethnicities <- c("AFRICAN AMERICAN", "ASIAN", "HISPANIC/LATINO", "WHITE")
  outcomes <- c("outcome_90_days", "outcome_adm")
  mortalities <- c("mortality_meld_3", "mortality_meld")
  
  # Prepare the results dataframe with an extra row for "ALL"
  results <- setNames(data.frame(matrix(ncol = 4, nrow = length(ethnicities) + 1, byrow = TRUE), 
                                 stringsAsFactors = FALSE), 
                      c("MELD_3_outcome_90_days", "MELD_outcome_90_days", 
                        "MELD_3_outcome_adm", "MELD_outcome_adm"))
  rownames(results) <- c("ALL", ethnicities)
  
  # Initialize an empty list to store HL statistics for each ethnicity and combination
  hl_stats_raw <- list()
  
  # Define a mapping from combinations of outcomes and mortalities to column names
  column_mapping <- list(
    "mortality_meld_3_outcome_90_days" = "MELD_3_outcome_90_days",
    "mortality_meld_outcome_90_days" = "MELD_outcome_90_days",
    "mortality_meld_3_outcome_adm" = "MELD_3_outcome_adm",
    "mortality_meld_outcome_adm" = "MELD_outcome_adm"
  )
  
  # Function to draw a sample from a dataset and keep the proportion of outcomes
  sample_binary_proportion <- function(dataframe, column_name, sample_size) {
    # Check if the column exists in the dataframe
    if (!column_name %in% names(dataframe)) {
      stop("Column name not found in the dataframe.")
    }
    
    # Split the dataframe into TRUE and FALSE subsets
    true_subset <- dataframe[dataframe[[column_name]] == TRUE, ]
    false_subset <- dataframe[dataframe[[column_name]] == FALSE, ]
    
    # Calculate the proportion of TRUE and FALSE
    total_rows <- nrow(dataframe)
    true_prop <- nrow(true_subset) / total_rows
    false_prop <- nrow(false_subset) / total_rows
    
    # Calculate the number of samples to draw from each subset
    true_sample_size <- round(true_prop * sample_size)
    false_sample_size <- sample_size - true_sample_size
    
    # Sample from each subset
    true_sample <- true_subset[sample(nrow(true_subset), true_sample_size, replace = TRUE), ]
    false_sample <- false_subset[sample(nrow(false_subset), false_sample_size, replace = TRUE), ]
    
    # Combine the samples and return
    combined_sample <- rbind(true_sample, false_sample)
    
    return(combined_sample)
  }
  

  # Function to perform Hosmer-Lemeshow test on bootstrapped samples and return all statistics
  perform_hl_test <- function(subset_df, outcome, mortality) {
    hl_statistics <- numeric(n_bootstraps) # Initialize vector to store HL statistics from each bootstrap
    
    for (i in 1:n_bootstraps) {
      # Draw a sample with replacement that keeps the same outcome distribution
      bootstrap_sample <- sample_binary_proportion(subset_df, column_name = outcome, sample_size = fixed_sample_size)
      
      # Perform the Hosmer-Lemeshow test on the bootstrap sample
      test_result <- hoslem.test(x = bootstrap_sample[[outcome]], y = bootstrap_sample[[mortality]], g = 6)
      hl_statistics[i] <- test_result$statistic
    }
    
    # Return all the HL statistics from all bootstrap samples
    return(hl_statistics)
  }
  
  for (ethno in c("ALL", ethnicities)) {
    hl_stats_raw[[ethno]] <- list() # Initialize list for this ethnicity
    if (ethno == "ALL") {
      df_subset <- df # Use the entire dataset for "ALL"
    } else {
      df_subset <- df[df$ethnicity == ethno, ]
    }
    
    for (outcome in outcomes) {
      for (mortality in mortalities) {
        # Perform the Hosmer-Lemeshow test on bootstrapped samples
        raw_stats <- perform_hl_test(df_subset, outcome, mortality)
        
        # Store raw HL statistics
        key <- paste(mortality, outcome, sep = "_")
        if (!is.list(hl_stats_raw[[ethno]][[key]])) {
          hl_stats_raw[[ethno]][[key]] <- list()
        }
        hl_stats_raw[[ethno]][[key]] <- raw_stats
        
        # Use the mapping to get the correct column name and calculate the mean result
        column_name <- column_mapping[[key]]
        results[ethno, column_name] <- mean(raw_stats)
      }
    }
  }
  
  # Return both the original results and the new list of raw HL statistics
  return(list(mean_results = results, raw_statistics = hl_stats_raw))
}

mimic_hoslem_test <- hoslem_test(mimic_admissions_meld_df)
mimic_hoslem_test_table <- mimic_hoslem_test[[1]]
mimic_hoslem_test_raw <- mimic_hoslem_test[[2]]

# Save a table and raw results to csv
write.csv(mimic_hoslem_test_table, "Outputs/mimic_hoslem_test_table.csv", row.names = TRUE)
write.csv(mimic_hoslem_test_raw, "Outputs/mimic_hoslem_test_raw.csv", row.names = FALSE)





  
#########################
## Exclusions graphs 
# MIMIC-IV
# https://mermaid.live/edit#pako:eNqlk0Fvm0AQhf_KaM9rFRIgGEWRGjuVfPCpVS84h-kywKiwS5elrRXy37tgp3VRKkUqp100772PJ-ZJKFOQyERlsavh0_agwT_v8_jqWt5EAWDRct-z0T2whv1uv9usdp8fYbW6G-MglkmaAP1UzVBQkV1O_2BXm8EBQsFYadNzD6YExdbW02WE-zxMZRBHSxW4mv4hejzh3c_x67-SK4JbCFMwdj7fQboeYZOHN3Idp69GYNdZ01lGR5Pk7L2ZvUMZBVcX9hYVvSNXa1bsjlCgQxg0fkdu8EtDU6o2bvb2NU3ufUeKS6ZiqVU-rzKWyVewzcNExnHyFj5AvfQ6I29n5MR3edkIa2XariGv9ozGojP2RD7CQx4GMg6Ct-TKRaicQRaWZ5KHM0m4ji5IvhJ1YHRznO1Ltr37Ewyl745Q1dChY9JuhA95JK-DZDnZz6OD5m8DvQz_P7aQoiXbIhd-C56mzzgIb9fSQWT-qGlwFpuDOOhnP4qDMx-PWonM2YGksGaoapGV2PT-NnTelLb-17XY_n5LBfvA_WnP5nV7_gU6gC_j

# Emory 
# https://mermaid.live/edit#pako:eNqlU09vmzAU_ypPlqJcHC2EpAU0VVqbHnfabqGHV3jA08BmttkWhXz3GSdto6w9zSfb-v21ng-i0CWJTMxmB1bsMjjMXUMdzTOYKxqcwXZ-PM5muaoN9g183-YK_Pqyi-TtzUpGaQpYdmwta2WBFXg6PHba7KFEh89o6QkWi7vR49eRjNMI6E_RDiWV2SXzN7tGDw4QSsZaacsWdAUFG9NMhxHud6ulTJbJNSs4vk96OoW9DwFSuYouzVkVuutbcgQtPmuD7iX0CA-7KJKb9PYfr48oZ6eH4LRexm8-ea4WEG_WsACDBX0i1ygu2J14MCj8hezVWgJtQGkXjM4PaXsquGIqr7kFOqq1YbJBf516eawJPkOUTDrT_g6SdITtVCVK1-8-G_a90b1hrzZR5JWNBFTlB1W3oepGxjcXZeEHUQ9atfsgX7Gx7s0YKp-MsGigR8ek3AiPu41M4ugaaQN0UPxzoBfw_8YWUnRkOuTSz_thKpGLMOu5yPz2PO25yNXRQ3Fw-tteFSJzZiApjB7qRmQVttafht5L0tYPncHu9ZZK9nZfTz8qfKzjXxeKJ6w



