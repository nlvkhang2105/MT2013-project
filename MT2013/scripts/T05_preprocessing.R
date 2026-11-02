# ============================================================
# T05 - Data Preprocessing
# Owner: Tue
# Goal: Prepare data for descriptive and inferential analysis
# ============================================================

source("MT2013/scripts/load_data.R")
df <- load_data()

# Selected variables:
# Width : X1    - Image width
# Class : X1558 - Ad / Non-ad
# F1    : X351  - url*ads
# F2    : X1455 - alt*click+here

# ------------------------------------------------------------
# STEP 1: Select and validate the analysis variables.
# ------------------------------------------------------------

analysis_df <- data.frame(
  Width = df$X1,
  Class = df$X1558,
  F1    = df$X351,
  F2    = df$X1455,
  stringsAsFactors = FALSE
)

# Width: Numeric and positive (NA allowed)
if (!is.numeric(analysis_df$Width)) {
  stop("Width must be numeric.")
}
if (any(analysis_df$Width[!is.na(analysis_df$Width)] <= 0)) {
  stop("Width must be positive.")
}

# Class: Only 'ad.' and 'nonad.' (NA allowed)
valid_classes <- c("ad.", "nonad.")
invalid_classes <- setdiff(
  unique(analysis_df$Class[!is.na(analysis_df$Class)]), 
  valid_classes
)

if (length(invalid_classes) > 0) {
  stop("Invalid Class labels: ", paste(invalid_classes, collapse = ", "))
}

# F1 and F2: Numeric with values 0, 1, or NA
if (!is.numeric(analysis_df$F1) || !is.numeric(analysis_df$F2)) {
  stop("F1 and F2 must be numeric.")
}

valid_binary <- c(0, 1)
invalid_f1 <- setdiff(unique(analysis_df$F1[!is.na(analysis_df$F1)]), valid_binary)
if (length(invalid_f1) > 0) {
  stop("Invalid F1 values: ", paste(invalid_f1, collapse = ", "))
}

invalid_f2 <- setdiff(unique(analysis_df$F2[!is.na(analysis_df$F2)]), valid_binary)
if (length(invalid_f2) > 0) {
  stop("Invalid F2 values: ", paste(invalid_f2, collapse = ", "))
}

# ------------------------------------------------------------
# STEP 2: Assess missing values and duplicate records.
# ------------------------------------------------------------

# Count missing values in each selected variable
missing_counts <- colSums(is.na(analysis_df))

# Summarize Width missingness by Class
n_total <- as.vector(
  table(factor(analysis_df$Class, levels = valid_classes))
  )
  
n_missing <- as.vector(
  table(factor(
    analysis_df$Class[is.na(analysis_df$Width)],
    levels = valid_classes
  ))
)

n_valid <- n_total - n_missing
missing_rate <- round(n_missing / n_total, 4)

missing_by_class <- data.frame(
  Class = valid_classes,
  n_total = n_total,
  n_missing = n_missing,
  missing_rate = missing_rate,
  n_valid = n_valid,
  stringsAsFactors = FALSE
)

# Detect duplicates excluding the first (index) column
# Keep all duplicate records
duplicate_rows <- duplicated(df[, -1])
n_duplicates <- sum(duplicate_rows)

# ------------------------------------------------------------
# STEP 3: Prepare and validate analysis datasets.
# ------------------------------------------------------------

# RQ1 :  Width and Class observations
rq1_data <- analysis_df[
  !is.na(analysis_df$Width) & !is.na(analysis_df$Class), 
  c("Width", "Class")
]

# RQ2: Separate complete datasets for F1 and F2
rq2_data <- list(
  F1 = analysis_df[
    !is.na(analysis_df$Class) & !is.na(analysis_df$F1), 
    c("Class", "F1")
  ],
  F2 = analysis_df[
    !is.na(analysis_df$Class) & !is.na(analysis_df$F2), 
    c("Class", "F2")
  ]
)

# ------------------------------------------------------------
# Summary
# ------------------------------------------------------------

cat("=== T05: DATA PREPROCESSING ===\n\n")

cat("Missing values by variable:\n")
print(missing_counts)

cat("\nWidth missingness by Class:\n")
print(missing_by_class, row.names = FALSE)

cat("\nDuplicate records (excluding index):", n_duplicates, "\n")

cat("\nAnalysis dataset sizes:\n")
cat("analysis_df :", nrow(analysis_df), "rows\n")
cat("rq1_data   :", nrow(rq1_data), "rows\n")
cat("rq2_data$F1:", nrow(rq2_data$F1), "rows\n")
cat("rq2_data$F2:", nrow(rq2_data$F2), "rows\n")

cat("\nT05 completed successfully.\n")
