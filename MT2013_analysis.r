# MT2013 - Internet Advertisements
# Run from the directory containing add.csv: Rscript MT2013_analysis.R

# Load raw data ----
load_data <- function() {
  file_path <- "add.csv"

  if (!file.exists(file_path)) {
    stop("File not found at: ", file_path)
  }

  df <- read.csv(
    file_path,
    na.strings = c("?", "NA", "", " ?"),
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )

  return(df)
}

# T04 - Schema checks ----
# Owner: Khang

df <- load_data()

# Verify dataset dimensions
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

# Inspect sample records and data types
cat("Number of observations:", n_rows, "\n")
cat("Number of columns:", n_cols, "\n")
cat("Number of features:", n_features, "\n\n")

cat("First 5 rows (first 10 columns):\n")
print(head(df[, 1:min(10, n_cols)], 5))
cat("\nDatatype distribution:\n")
print(table(sapply(df, class)))

# Examine Class distribution
cat("AD/NON-AD count:\n")
label_col_name <- names(df)[ncol(df)]
label_counts <- table(df[[label_col_name]], useNA = "ifany")
print(label_counts)

# Check binary feature values
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

# T05 - Data preprocessing ----

feature_cols <- paste0("X", 4:1557)
analysis_df <- df[, paste0("X", 0:1558)] # Exclude the index column
names(analysis_df)[1:4] <- c("Height", "Width", "Aratio", "Local")
names(analysis_df)[1559] <- "Class"

# Check analysis requirements
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

# Summarize missingness and duplicate rows; retain duplicates
missing_counts <- colSums(is.na(analysis_df))
missing_summary <- data.frame(
  variable = names(analysis_df),
  n_missing = as.vector(missing_counts),
  missing_percent = round(100 * as.vector(missing_counts) / nrow(analysis_df), 2)
)
duplicate_rows <- duplicated(analysis_df)
n_duplicates <- sum(duplicate_rows)

# Report preprocessing results
cat("=== T05: DATA PREPROCESSING ===\n")
cat("Input rows:", nrow(analysis_df), "\n")
cat("Rows with missing Class:", sum(is.na(analysis_df$Class)), "\n")
cat("Textual indicators:", length(feature_cols), "\n")
cat("Missing textual values:", sum(missing_counts[feature_cols]), "\n")
cat("Duplicate rows after the first occurrence (retained):", n_duplicates, "\n")
cat("\nVariables with missing values:\n")
print(missing_summary[missing_summary$n_missing > 0, ], row.names = FALSE)
write.csv(analysis_df, "MT2013_cleaned.csv", row.names = FALSE, na = "NA")

# T06 - Descriptive statistics and figures ----
# Owner: Quoc Bao

y <- analysis_df$Class

figure_dir <- "figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Class counts and geometry summary.
class_n <- table(factor(y, levels = c("ad.", "nonad.")))
print(data.frame(Class = c("Ad", "Non-ad"), N = as.integer(class_n),
                 Percent = round(100 * as.integer(class_n) / length(y), 2)))

table1 <- data.frame()
for (g in c("ad.", "nonad.")) {
  for (v in c("Height", "Width", "Aratio")) {
    x <- analysis_df[[v]][y == g]
    z <- x[!is.na(x)]
    table1 <- rbind(table1, data.frame(
      Variable = v, Class = if (g == "ad.") "Ad" else "Non-ad",
      Valid_n = length(z), Missing_pct = round(100 * mean(is.na(x)), 2),
      Mean = round(mean(z), 2), SD = round(sd(z), 2),
      Median = round(median(z), 2), IQR = round(IQR(z), 2)
    ))
  }
}
print(table1, row.names = FALSE)

# Local completeness by class.
cat("\nLocal by Class (0 / 1 / NA):\n")
print(table(y, analysis_df$Local, useNA = "ifany"))

# Support of each textual indicator: number of observed 1s.
X <- as.matrix(analysis_df[, feature_cols, drop = FALSE])
k_j <- colSums(X == 1)
cat("\nFeature support summary:\n")
print(summary(k_j))
cat("SD:", round(sd(k_j), 2), "\n")

support_groups <- cut(k_j, breaks = c(-1, 0, 5, 20, 100, Inf),
                      labels = c("0", "1-5", "6-20", "21-100", ">100"))
support_counts <- table(support_groups)
print(support_counts)

png(file.path(figure_dir, "Figure1_support_feature.png"),
    width = 1800, height = 1000, res = 220)
par(mar = c(5, 5, 2, 1.5))
bp <- barplot(support_counts, col = "#016783", border = NA,
              xlab = "Feature support (observations per indicator)",
              ylab = "Number of textual indicators",
              ylim = c(0, max(support_counts) * 1.15))
text(bp, support_counts, labels = support_counts, pos = 3)
dev.off()

# Rates within each Class; difference is descriptive, not a p-value.
ad_rate <- colMeans(X[y == "ad.", , drop = FALSE])
nonad_rate <- colMeans(X[y == "nonad.", , drop = FALSE])
rate_df <- data.frame(feature = feature_cols, support = k_j,
                      ad_rate = ad_rate, nonad_rate = nonad_rate,
                      difference = ad_rate - nonad_rate, row.names = NULL)
cat("\nFeatures more common in Ad / Non-ad:",
    sum(rate_df$difference > 0), "/", sum(rate_df$difference < 0), "\n")
cat("Both occurrence rates < 5%:",
    sum(ad_rate < 0.05 & nonad_rate < 0.05), "\n")

# Zero-support indicators cannot be shown on a logarithmic x-axis.
show <- rate_df$support > 0
png(file.path(figure_dir, "Figure2_occurence_rates.png"),
    width = 1800, height = 1200, res = 220)
par(mar = c(5, 5, 2, 1.5))
plot(rate_df$support[show], rate_df$difference[show],
     log = "x", pch = 16, cex = 0.65,
     col = rgb(0.12, 0.35, 0.65, 0.45),
     xlab = "Textual feature support (log scale)",
     ylab = "Ad rate - Non-ad rate")
abline(h = 0, col = "#B45143", lty = 2, lwd = 2)
dev.off()

# Width by Class: descriptive only, omitting missing Width.
width_df <- analysis_df[!is.na(analysis_df$Width), c("Width", "Class")]
width_df$Class <- factor(width_df$Class, levels = c("ad.", "nonad."),
                         labels = c("Ad", "Non-ad"))
png(file.path(figure_dir, "Figure3_width_boxplot.png"),
    width = 1600, height = 1100, res = 220)
par(mar = c(5, 5, 2, 1.5))
boxplot(Width ~ Class, data = width_df, col = c("#85B8C8", "#D2DEE5"),
        border = "#50616A", xlab = "Advertisement class",
        ylab = "Image width (pixels)")
dev.off()

cat("\nT06 complete: table1 and 3 PNG figures in", figure_dir, "\n")

# T07 - Statistical inference ----

alpha <- 0.05

# Two-sided Fisher tests; retain rare/constant features and duplicate rows.
class_group <- factor(analysis_df$Class, levels = c("ad.", "nonad."))
results <- data.frame(
  Feature = feature_cols,
  Support = integer(length(feature_cols)),
  Ad_rate = numeric(length(feature_cols)),
  Non_ad_rate = numeric(length(feature_cols)),
  Difference = numeric(length(feature_cols)),
  Raw_p = numeric(length(feature_cols))
)

for (j in seq_along(feature_cols)) {
  # Explicit levels preserve a 2x2 table; table() excludes incomplete pairs.
  tab <- table(class_group,
               factor(analysis_df[[feature_cols[j]]], levels = c(0, 1)))
  n <- rowSums(tab)
  if (any(n == 0)) {
    stop("No observed feature values in one class for ", feature_cols[j])
  }
  rates <- tab[, "1"] / n
  test_result <- fisher.test(tab)
  raw_p <- test_result$p.value
  results[j, 2:6] <- c(
    sum(tab[, "1"]),
    rates[1],
    rates[2],
    rates[1] - rates[2],
    raw_p
  )
}

# Adjust the same complete family of raw p-values.
results$BY_p <- p.adjust(results$Raw_p, method = "BY")
results$BH_p <- p.adjust(results$Raw_p, method = "BH")
results$Bonferroni_p <- p.adjust(results$Raw_p, method = "bonferroni")
results$BY_discovery <- results$BY_p <= alpha
results$BH_discovery <- results$BH_p <= alpha
results$Bonferroni_discovery <- results$Bonferroni_p <= alpha

# Select report features after adjustment across the full family.
comparison <- data.frame(
  Procedure = c("BY", "BH", "Bonferroni"),
  Error_control = c("FDR", "FDR", "FWER"),
  Discoveries = c(sum(results$BY_discovery), sum(results$BH_discovery),
                  sum(results$Bonferroni_discovery))
)

by_results <- results[results$BY_discovery, ]
# Ties: smaller BY-adjusted p, then original feature-column order.
by_results <- by_results[
  order(
    -abs(by_results$Difference),
    by_results$BY_p,
    match(by_results$Feature, feature_cols)
  ),
]
selected_by <- head(by_results[, c("Feature", "Support", "Ad_rate",
                                  "Non_ad_rate", "Difference", "BY_p")], 10)

cat("\nTable 1 - Multiple-Testing Comparison (level = 0.05)\n")
print(comparison, row.names = FALSE)
cat("\nTable 2 - Selected BY Discoveries (rates on the 0-1 scale)\n")
print(selected_by, row.names = FALSE, digits = 5)
cat("\nCommon to all three:", sum(results$BY_discovery & results$BH_discovery &
                                  results$Bonferroni_discovery),
    "\nBH but not BY:", sum(results$BH_discovery & !results$BY_discovery),
    "\nBY but not Bonferroni:",
    sum(results$BY_discovery & !results$Bonferroni_discovery),
    "\nBonferroni but not BY:",
    sum(results$Bonferroni_discovery & !results$BY_discovery), "\n")

# Export all features without rounding the stored results.
output_file <- "outputs/T07_inference_results.csv"
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(results, output_file, row.names = FALSE)
cat("\nFull results saved to:", output_file, "\n")

# BY allows dependence among tests only when individual p-values are valid.
# It does not repair dependent observations; discoveries are marginal associations.
