# T07 - Statistical Inference
# Run from the repository root. Requires the updated T05.
source("MT2013/scripts/T05_preprocessing.R")

alpha <- 0.05

# 1. Two-sided Fisher tests; retain rare/constant features and duplicate rows.
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

# 2. Adjust the same complete family of raw p-values.
results$BY_p <- p.adjust(results$Raw_p, method = "BY")
results$BH_p <- p.adjust(results$Raw_p, method = "BH")
results$Bonferroni_p <- p.adjust(results$Raw_p, method = "bonferroni")
results$BY_discovery <- results$BY_p <= alpha
results$BH_discovery <- results$BH_p <= alpha
results$Bonferroni_discovery <- results$Bonferroni_p <= alpha

# 3. Two report tables; selection is AFTER correction across all features.
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

# 4. Export all features without rounding the stored results.
output_file <- "MT2013/outputs/T07_inference_results.csv"
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(results, output_file, row.names = FALSE)
cat("\nFull results saved to:", output_file, "\n")

# BY allows dependence among tests only when individual p-values are valid.
# It does not repair dependent observations; discoveries are marginal associations.
