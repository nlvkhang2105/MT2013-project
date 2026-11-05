
# ============================================================
# T07 - Statistical Inference
# Owner: Huy
# Goal: Test hypotheses for RQ1 and RQ2
# ============================================================

source("MT2013/scripts/T05_preprocessing.R")

# Inputs: rq1_data, rq2_data
# Significance level: alpha = 0.05
# All tests are two-sided.


# ------------------------------------------------------------
# TODO 1: Compare mean Width between Ad and Non-ad.
# ------------------------------------------------------------

# Perform Welch's two-sample t-test (RQ1).
# Report mean difference (Ad - Non-ad), t-statistic,
# df, p-value, and 95% CI for the mean difference.


# ensure factor levels are ordered as "ad." then "nonad." for correct mean difference calculation (Ad - Non-ad)
rq1_data$Class <- factor(rq1_data$Class, levels = c("ad.", "nonad."))

# Welch's t-test (var.equal = FALSE)
welch_result <- t.test(
  Width ~ Class,
  data = rq1_data,
  var.equal = FALSE,
  alternative = "two.sided",
  conf.level = 0.95
)

mean_ad     <- welch_result$estimate[1]
mean_nonad  <- welch_result$estimate[2]
mean_diff   <- mean_ad - mean_nonad
t_stat      <- welch_result$statistic
df_val      <- welch_result$parameter
p_val_rq1   <- welch_result$p.value
ci_lower    <- welch_result$conf.int[1]
ci_upper    <- welch_result$conf.int[2]

cat("\n========================================================\n")
cat("            TABLE 1: RQ1 WELCH'S T-TEST RESULTS         \n")
cat("========================================================\n")
cat(sprintf("Mean (Ad)         : %.4f\n", mean_ad))
cat(sprintf("Mean (Non-ad)     : %.4f\n", mean_nonad))
cat(sprintf("Mean Difference   : %.4f\n", mean_diff))
cat(sprintf("t-statistic       : %.4f\n", t_stat))
cat(sprintf("Degrees of Freedom: %.4f\n", df_val))
cat(sprintf("p-value           : %.4e\n", p_val_rq1))
cat(sprintf("95%% CI            : [%.4f, %.4f]\n", ci_lower, ci_upper))
cat("========================================================\n\n")

if (p_val_rq1 < 0.05) {
  cat("RQ1 Conclusion: Reject H0. There is a statistically significant difference\n")
  cat("in average Width between advertisement and non-advertisement images (p < 0.05).\n")
} else {
  cat("RQ1 Conclusion: Fail to reject H0. There is no statistically significant difference\n")
  cat("in average Width between advertisement and non-advertisement images (p >= 0.05).\n")
}


# ------------------------------------------------------------
# TODO 2: Compare F1 and F2 proportions by Class.
# ------------------------------------------------------------

# Build 2x2 tables and check expected counts.
# Perform two-proportion tests using prop.test(correct = FALSE).
# Report proportion differences (Ad - Non-ad), p-values,
# and 95% CIs for the proportion differences (RQ2).


# ------------------------------------------------------------
# TODO 3: Adjust p-values and summarize conclusions.
# ------------------------------------------------------------

# Apply Holm correction to the two RQ2 p-values.
# Report raw and adjusted p-values.
# State conclusions at alpha = 0.05.


# Outputs: RQ1 Welch test, RQ2 proportion tests,
#          Holm-adjusted p-values, and conclusions.
