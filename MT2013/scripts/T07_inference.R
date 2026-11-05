
# ============================================================
# T07 - Statistical Inference
# Owner: Huy
# Goal: Test hypotheses for RQ1 and RQ2
# ============================================================

source("MT2013/scripts/T05_preprocessing.R")

# Significance level
alpha <- 0.05

# Inputs: rq1_data, rq2_model_data
# Significance level: alpha = 0.05
# RQ1: Two-sided Welch's t-test
# RQ2: Joint likelihood-ratio test

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
# TODO 2: Joint logistic regression LRT (RQ2)
# H0: beta_F1 = beta_F2 = 0 after accounting for Width
# M0: W + W^2; M1: W + W^2 + F1 + F2
# ------------------------------------------------------------

# Use one complete-case dataset for both nested models.
# Retain the old rq2_data list in T05 for T06 descriptive summaries.

if (nrow(rq2_model_data) == 0L ||
    any(!is.finite(rq2_model_data$Width)) ||
    any(rq2_model_data$Width <= 0)) {
  stop("RQ2 requires valid, positive, finite Width observations.")
}

rq2_model_data$W <- log2(rq2_model_data$Width)
rq2_model_data$Y <- as.integer(rq2_model_data$Class == "ad.")

# Verify the current dataset's expected complete-case sample.
stopifnot(nrow(rq2_model_data) == 2378L)

m0 <- glm(Y ~ W + I(W^2), data = rq2_model_data,
          family = binomial(link = "logit"))
m1 <- glm(Y ~ W + I(W^2) + F1 + F2, data = rq2_model_data,
          family = binomial(link = "logit"))

# A valid LRT requires the same rows and identifiable, converged fits.
stopifnot(
  nobs(m0) == nrow(rq2_model_data),
  nobs(m1) == nrow(rq2_model_data),
  isTRUE(m0$converged), isTRUE(m1$converged),
  m0$rank == ncol(model.matrix(m0)),
  m1$rank == ncol(model.matrix(m1))
)

ll0 <- as.numeric(logLik(m0))
ll1 <- as.numeric(logLik(m1))
G2  <- 2 * (ll1 - ll0)
df_lrt <- attr(logLik(m1), "df") - attr(logLik(m0), "df")
p_val_rq2 <- pchisq(G2, df = df_lrt, lower.tail = FALSE)
stopifnot(df_lrt == 2L, G2 >= -1e-8)

cat("========================================================\n")
cat("         RQ2: JOINT LIKELIHOOD-RATIO TEST              \n")
cat("========================================================\n")
cat(sprintf("Complete cases     : %d\n", nrow(rq2_model_data)))
cat(sprintf("Ad / Non-ad        : %d / %d\n",
            sum(rq2_model_data$Y == 1L), sum(rq2_model_data$Y == 0L)))
cat(sprintf("M0 log-likelihood  : %.4f\n", ll0))
cat(sprintf("M1 log-likelihood  : %.4f\n", ll1))
cat(sprintf("LRT G^2            : %.4f\n", G2))
cat(sprintf("Degrees of freedom : %d\n", df_lrt))
cat(sprintf("p-value            : %.4e\n", p_val_rq2))
cat("========================================================\n")

if (p_val_rq2 <= alpha) {
  cat("RQ2 conclusion: Reject H0. At least one of F1 and F2 provides\n")
  cat("additional association with the Ad label after adjusting for Width,\n")
  cat("subject to the model and LRT assumptions.\n\n")
} else {
  cat("RQ2 conclusion: Fail to reject H0. Insufficient evidence of a\n")
  cat("joint contribution from F1 and F2 after adjusting for Width.\n\n")
}

# ------------------------------------------------------------
# TODO 3: Check sparse feature combinations
# ------------------------------------------------------------

pattern_counts <- table(
  rq2_model_data$F1,
  rq2_model_data$F2,
  rq2_model_data$Class
)

if (any(pattern_counts < 5)) {
  warning("Sparse feature/class combinations; interpret LRT cautiously.")
}


# Outputs: welch_result, mean_diff, ci_lower, ci_upper,
#          rq2_model_data, m0, m1, G2, df_lrt, p_val_rq2.
