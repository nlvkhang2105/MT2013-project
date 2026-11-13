# 1. Load data and select columns
source("MT2013/scripts/load_data.R")
df <- load_data()

feature_cols <- paste0("X", 4:1557)
analysis_df <- df[, paste0("X", 0:1558)] # Exclude the index column
names(analysis_df)[1:4] <- c("Height", "Width", "Aratio", "Local")
names(analysis_df)[1559] <- "Class"

# 2. Check analysis requirements
for (col in c("Height", "Width", "Aratio", "Local", feature_cols)) {
  x <- analysis_df[[col]]
  if (!all(is.na(x)) && !is.numeric(x)) {
    stop("Expected numeric data in column: ", col)
  }
  if (any(!is.na(x) & !is.finite(x))) {
    stop("Non-finite value in column: ", col)
  }
}

valid_classes <- c("ad.", "nonad.")
if (any(!is.na(analysis_df$Class) &
        !(analysis_df$Class %in% valid_classes))) {
  stop("Class must contain ad., nonad., or NA.")
}
if (!all(valid_classes %in% analysis_df$Class)) {
  stop("Both ad. and nonad. classes are required.")
}

for (col in c("Height", "Width", "Aratio")) {
  if (any(analysis_df[[col]] <= 0, na.rm = TRUE)) {
    stop("Inspect non-positive values in column: ", col)
  }
}

for (col in c("Local", feature_cols)) {
  x <- analysis_df[[col]]
  if (any(!is.na(x) & !(x %in% c(0, 1)))) {
    stop("Column must contain only 0, 1, or NA: ", col)
  }
}

# 3. Summarize missingness and duplicate rows; retain duplicates
missing_counts <- colSums(is.na(analysis_df))
missing_summary <- data.frame(
  variable = names(analysis_df),
  n_missing = as.vector(missing_counts),
  missing_percent = round(100 * as.vector(missing_counts) / nrow(analysis_df), 2)
)
duplicate_rows <- duplicated(analysis_df)
n_duplicates <- sum(duplicate_rows)

# 4. Report preprocessing results
cat("=== T05: DATA PREPROCESSING ===\n")
cat("Input rows:", nrow(analysis_df), "\n")
cat("Rows with missing Class:", sum(is.na(analysis_df$Class)), "\n")
cat("Textual indicators:", length(feature_cols), "\n")
cat("Missing textual values:", sum(missing_counts[feature_cols]), "\n")
cat("Duplicate rows after the first occurrence (retained):", n_duplicates, "\n")
cat("\nVariables with missing values:\n")
print(missing_summary[missing_summary$n_missing > 0, ], row.names = FALSE)
