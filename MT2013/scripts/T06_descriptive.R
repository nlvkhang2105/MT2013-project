# ============================================================
# T06 - Descriptive Statistics and Visualization
# Owner: Quoc Bao
# Detailed instructions: T06 handoff document.
# ============================================================

source("MT2013/scripts/T05_preprocessing.R")

# Inputs: analysis_df, feature_cols

# TODO 1: Summarize Class distribution and geometric variables.
cat("=== TODO_1 ===\n")
class_levels <- c("ad.", "nonad.")
class_labels <- c("Ad", "NonAd")

labels_df <- analysis_df[!is.na(analysis_df$Class), ]

fmt_n_pct <- function(n, total) {
        sprintf("%d (%.2f%%)", n, 100 * n / total)
}


#A
n_total <- nrow(analysis_df)
n_label <- nrow(labels_df)

class_counts <- table(factor(labels_df$Class, levels = class_levels))

class_summary <- data.frame(Class = class_labels,
                            Instances = as.vector(class_counts),
                            Percentage = round(100 * as.vector(class_counts) / n_label, digits = 3)
                            )

print(class_summary)

#B
geometric_vars <- c("Height", "Width", "Aratio")

geometric_stats <- function(x) {
        x <- x[!is.na(x)]
        q <- quantile(x, prob = c(0.25, 0.75), names = FALSE)
        c(
                valid_n = length(x),
                mean = mean(x),
                sd = sd(x),
                median = median(x),
                Q1 = q[1],
                Q3 = q[2],
                min = min(x),
                max = max(x)
        )
}

table1_rows <- list()
for (i in seq_along(class_levels)) {   # outer loop: Class (Ad first, then Non-ad)
        for (v in geometric_vars) {                # inner loop: Height, Width, Aratio
                x <- labeled_df[[v]][labeled_df$Class == class_levels[i]]
                s <- geometric_stats(x)
                table1_rows[[length(table1_rows) + 1]] <- data.frame(
                        Variable  = v,
                        Class     = class_labels[i],
                        Valid_n   = as.integer(s["valid_n"]),
                        Mean      = round(s["mean"], digits = 3),
                        SD        = round(s["sd"], digits = 3),
                        Median    = round(s["median"], digits = 3),
                        Q1        = round(s["Q1"], digits = 3),
                        Q3        = round(s["Q3"], digits = 3),
                        Min       = round(s["min"], digits = 3),
                        Max       = round(s["max"], digits = 3),
                        row.names = NULL
                )
        }
}

table1 <- do.call(rbind, table1_rows)
print(table1)

# TODO 2: Summarize Local distribution by Class.
cat("=== TODO_2 ===\n")
cell <- function(x) {
        n <- length(x)
        c(
        `Local = 0, n  (%)` = fmt_n_pct(sum(x == 0, na.rm = TRUE), total = n),
        `Local = 1, n  (%)` = fmt_n_pct(sum(x == 1, na.rm = TRUE), total = n),
        `Local = NA, n  (%)` = fmt_n_pct(sum(is.na(x)), total = n),
        Total = as.character(n)
        )
}

table2_rows <- lapply(seq_along(class_levels), function(i) {
        x <- labels_df$Local[labels_df$Class == class_levels[i]]
        data.frame(
                Class = class_labels[i],
                t(cell(x)),
                check.names = FALSE,
                row.names = NULL
               )
        }
)

table2_rows[[3]] <- data.frame(
        Class = "Total",
        t(cell(labels_df$Local)),
        check.names = FALSE,
        row.names = NULL
)

table2 <- do.call(rbind, table2_rows)
print(table2)

# TODO 3: Calculate textual feature support and create the bar chart.
cat("=== TODO_3 ===\n")
feature_matrix <- as.matrix(analysis_df[, feature_cols])
k_j <- colSums(feature_matrix == 1, na.rm = TRUE)

support_levels <- c("Zero", "Very low", "Low", "Moderate", "Higher")
support_ranges <- c("k = 0", "1-5", "6-20", "21-100", "> 100")

support_group <- cut(
  k_j,
  breaks = c(-Inf, 0, 5, 20, 100, Inf),
  labels = support_levels,
  right  = TRUE
)
support_counts <- table(support_group)

support_table <- data.frame(
  Support = support_levels,
  Range = support_ranges,
  n_features = as.vector(support_counts),
  percent = round(100 * as.vector(support_counts) / length(feature_cols), 2)
)

print(support_table)

bar_mids <- barplot(
  support_table$n_features,
  names.arg = paste0(support_table$Support, "\n(", support_table$Range, ")"),
  col = "steelblue",
  border = NA,
  ylim = c(0, max(support_table$n_features) * 1.15),
  xlab = "Support group (k_j = number of observations with feature = 1)",
  ylab = "Number of textual features",
  main = "Figure 1. Textual Feature Support Distribution"
)

text(bar_mids, support_table$n_features,
     labels = support_table$n_features, pos = 3, cex = 0.9)

# TODO 4: Calculate occurrence rates by Class and create the scatter plot.
cat("=== TODO_4 ===\n")
occurrence_rate <- function(class_value) {
  m <- feature_matrix[which(analysis_df$Class == class_value), , drop = FALSE]
  n_observed <- colSums(!is.na(m))
  n_present  <- colSums(m == 1, na.rm = TRUE)
  ifelse(n_observed == 0, NA_real_, n_present / n_observed)
}

rate_df <- data.frame(
  feature = feature_cols,
  ad_rate = occurrence_rate("ad."),
  nonad_rate = occurrence_rate("nonad."),
  row.names = NULL
)
rate_df$difference <- rate_df$ad_rate - rate_df$nonad_rate

cat("\nOccurrence rates computed for", nrow(rate_df), "features.\n")
cat("Features with NA rate in at least one Class:",
    sum(is.na(rate_df$ad_rate) | is.na(rate_df$nonad_rate)), "\n")

valid_rate <- rate_df[!is.na(rate_df$ad_rate) & !is.na(rate_df$nonad_rate), ]
cat("\nPosition relative to y = x (features with both rates available):\n")
cat("  Above the line (higher in Ad):    ", sum(valid_rate$difference > 0), "\n")
cat("  Below the line (higher in Non-ad):", sum(valid_rate$difference < 0), "\n")
cat("  On the line (equal rates):        ", sum(valid_rate$difference == 0), "\n")
cat("  Both rates < 5%:                  ",
    sum(valid_rate$ad_rate < 0.05 & valid_rate$nonad_rate < 0.05), "\n")
cat("  |difference| >= 0.20:             ",
    sum(abs(valid_rate$difference) >= 0.20), "\n")

cat("\nTop 5 features with higher rate in Ad:\n")
print(head(valid_rate[order(-valid_rate$difference), ], 5), row.names = FALSE)
cat("\nTop 5 features with higher rate in Non-ad:\n")
print(head(valid_rate[order(valid_rate$difference), ], 5), row.names = FALSE)

plot(
  valid_rate$nonad_rate, valid_rate$ad_rate,
  xlim = c(0, 1), ylim = c(0, 1),
  pch = 16, cex = 0.6, col = rgb(0.12, 0.35, 0.65, 0.45),
  xlab = "Non-ad occurrence rate",
  ylab = "Ad occurrence rate",
  main = "Figure 2. Textual Feature Occurrence Rates by Class"
)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = "y = x", col = "red", lty = 2, lwd = 2, bty = "n")

# Outputs: Class summary, 2 tables and 2 figures.
