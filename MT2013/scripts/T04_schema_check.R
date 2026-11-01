# ============================================================
# T04 - Dataset Schema Check
# Owner: Khang
# Goal: Inspect and validate the raw dataset structure
# ============================================================

source("MT2013/scripts/load_data.R")
df <- load_data()

# ------------------------------------------------------------
# STEP 1: Verify dataset dimensions.
# ------------------------------------------------------------

n_rows <- nrow(df)
n_cols <- ncol(df)
n_features <- n_cols - 2 # Exclude index and label columns

if (n_rows != 3279 || n_cols != 1560) {
  stop(
    "Unexpected dimensions: ",
    n_rows, " rows, ", n_cols, " columns. ",
    "Expected: 3279 rows, 1560 columns."
  )
}

# ------------------------------------------------------------
# STEP 2: Inspect sample records and data types.
# ------------------------------------------------------------

cat("Number of observations:", n_rows, "\n")
cat("Number of columns:", n_cols, "\n")
cat("Number of features:", n_features, "\n\n")

cat("First 5 rows (first 10 columns):\n")
print(head(df[, 1:min(10, n_cols)], 5))
cat("\nDatatype distribution:\n")
print(table(sapply(df, class)))

# ------------------------------------------------------------
# STEP 3: Examine Class distribution.
# ------------------------------------------------------------

cat("AD/NON-AD count:\n")
label_col_name <- names(df)[ncol(df)]
label_counts <- table(df[[label_col_name]], useNA = "ifany")
print(label_counts)

# ------------------------------------------------------------
# STEP 4: Check binary feature values.
# ------------------------------------------------------------

cat("Anomaly check:...\n")
binary_cols <- names(df)[5:(ncol(df) - 1)]
anomalous_cols <- list()

for (col in binary_cols) {
  unique_vals <- unique(df[[col]])

  # Identify values outside 0, 1, and NA
  invalid <- unique_vals[!is.na(unique_vals) & !(unique_vals %in% c(0, 1, "0", "1"))]

  if (length(invalid) > 0) {
    anomalous_cols[[col]] <- invalid
  }
}

if (length(anomalous_cols) == 0) {
  cat("No anomalies found. Only 1, 0 and NA.\n")
} else {
  cat("Anomalies found in column(s)\n")
  print(anomalous_cols)
}