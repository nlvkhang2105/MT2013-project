
file_path <- "MT2013/data/raw/add.csv"

if (!file.exists(file_path)) {
  stop("File not found at: ", file_path)
}
df <- read.csv(file_path, na.strings = c("?", "NA", "", " ?"), stringsAsFactors = FALSE)
n_rows <- nrow(df)
n_cols <- ncol(df)
n_features <- n_cols - 2 # Trừ 1 cột index và 1 cột label

cat("Number of observations:", n_rows, "\n")
cat("Number of columns:", n_cols, "\n")
cat("Number of features:", n_features, "\n\n")

cat("First 5 lines and :\n")
print(head(df[, 1:min(10, n_cols)], 5))
cat("\nDatatype distribution:\n")
print(table(sapply(df, class)))

cat("AD/NON-AD count:\n")
label_col_name <- names(df)[ncol(df)]
label_counts <- table(df[[label_col_name]], useNA = "ifany")
print(label_counts)

cat("Anomaly check:...\n")
binary_cols <- names(df)[5:(ncol(df) - 1)]
anomalous_cols <- list()
for (col in binary_cols) {
  unique_vals <- unique(df[[col]])
  # Lọc ra các giá trị khác 0, 1 và NA
  invalid <- unique_vals[!is.na(unique_vals) & !(unique_vals %in% c(0, 1, "0", "1"))]
  if (length(invalid) > 0) {
    anomalous_cols[[col]] <- invalid
  }
}
if (length(anomalous_cols) == 0) {
  cat("No anomoly found, there is only 1, 0 and NA.\n")
} else {
  cat("Anomoly found in column(s)\n")
  print(anomalous_cols)
}