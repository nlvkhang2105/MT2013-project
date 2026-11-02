
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
