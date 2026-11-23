# ============================================================
# T06 - Descriptive Statistics and Visualization
# Owner: Quoc Bao
# Detailed instructions: T06 handoff document.
# ============================================================

# Load validated data from T05
source("MT2013/scripts/T05_preprocessing.R")
y <- analysis_df$Class

figure_dir <- "MT2013/output/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# TODO 1 - Class counts and geometry summary (the only table for the PDF).
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

# TODO 2 - Local: short completeness check, not a report table.
cat("\nLocal by Class (0 / 1 / NA):\n")
print(table(y, analysis_df$Local, useNA = "ifany"))

# TODO 3 - Support of each textual indicator: number of observed 1s.
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

# TODO 4 - Rates within each Class; difference is descriptive, not a p-value.
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

# TODO 5 - Width by Class: descriptive only, omitting missing Width.
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
