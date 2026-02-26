# utils_stats.R
# Statistical calculation utilities for ESCAPE
# Extracted for testability and reuse

#' Calculate Pearson correlation and R-squared
#' @param x Predictor variable
#' @param y Criterion variable
#' @return List with r and r2
calc_correlation <- function(x, y) {
  r_val <- cor(x, y, use = "complete.obs")
  list(
    r = r_val,
    r2 = r_val^2,
    r_formatted = round(r_val, 4),
    r2_pct = round(r_val^2 * 100, 2)
  )
}

#' Calculate Cohen's d effect size
#' @param group1 Values from first group
#' @param group2 Values from second group
#' @return Cohen's d value
calc_cohens_d <- function(group1, group2) {
  m1 <- mean(group1, na.rm = TRUE)
  m2 <- mean(group2, na.rm = TRUE)
  s1 <- sd(group1, na.rm = TRUE)
  s2 <- sd(group2, na.rm = TRUE)

  # Pooled SD (simple average of variances, then sqrt)
  sd_pooled <- sqrt((s1^2 + s2^2) / 2)

  abs(m1 - m2) / sd_pooled
}

#' Calculate Hedges' g effect size (bias-corrected Cohen's d)
#' @param group1 Values from first group
#' @param group2 Values from second group
#' @return Hedges' g value
calc_hedges_g <- function(group1, group2) {
  m1 <- mean(group1, na.rm = TRUE)
  m2 <- mean(group2, na.rm = TRUE)
  s1 <- sd(group1, na.rm = TRUE)
  s2 <- sd(group2, na.rm = TRUE)
  n1 <- sum(!is.na(group1))
  n2 <- sum(!is.na(group2))

  # Pooled SD (weighted by sample size)
  sd_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))

  abs(m1 - m2) / sd_pooled
}

#' Calculate Common Language Effect Size (CLES)
#' Probability that a randomly chosen member of group 1 will have a higher
#' score than a randomly chosen member of group 2
#' @param group1 Values from first group
#' @param group2 Values from second group
#' @return CLES probability (0-1)
calc_cles <- function(group1, group2) {
  m1 <- mean(group1, na.rm = TRUE)
  m2 <- mean(group2, na.rm = TRUE)
  s1 <- sd(group1, na.rm = TRUE)
  s2 <- sd(group2, na.rm = TRUE)

  z_diff <- abs(m1 - m2) / sqrt(s1^2 + s2^2)
  pnorm(z_diff)
}

#' Calculate confidence interval for Cohen's d
#' @param d Cohen's d value
#' @param n1 Sample size of group 1
#' @param n2 Sample size of group 2
#' @param ci Confidence level (default 0.95)
#' @return List with lower and upper bounds
calc_d_ci <- function(d, n1, n2, ci = 0.95) {
  z <- qnorm(1 - (1 - ci) / 2)
  se_d <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))

  list(
    lower = d - z * se_d,
    upper = d + z * se_d
  )
}

#' Calculate all effect size metrics at once
#' @param above_group Values for above-cutoff group
#' @param below_group Values for below-cutoff group
#' @param r_value Correlation coefficient
#' @return Data frame with all metrics
calc_all_effect_sizes <- function(above_group, below_group, r_value) {
  n1 <- sum(!is.na(above_group))
  n2 <- sum(!is.na(below_group))

  if (n1 < 2 || n2 < 2) {
    return(data.frame(
      Metric = "Error",
      Value = "Insufficient data in one or both groups"
    ))
  }

  hedges_g <- calc_hedges_g(above_group, below_group)
  cohens_d <- calc_cohens_d(above_group, below_group)
  cles <- calc_cles(above_group, below_group)
  d_ci <- calc_d_ci(cohens_d, n1, n2)

  data.frame(
    Metric = c("Pearson's r", "Hedges' g", "Cohen's d", "Cohen's d [95% CI]", "CLES (Probability)"),
    Value = c(
      round(r_value, 3),
      round(hedges_g, 3),
      round(cohens_d, 3),
      paste0("[", round(d_ci$lower, 3), ", ", round(d_ci$upper, 3), "]"),
      round(cles, 3)
    )
  )
}

#' Generate expectancy table data
#' @param df Data frame with Predictor and Criterion columns
#' @param bins Number of bins
#' @param cutoff_y Criterion cutoff value
#' @return Data frame with expectancy data
calc_expectancy <- function(df, bins, cutoff_y) {
  df |>
    dplyr::mutate(
      ntile_X = dplyr::ntile(Predictor, bins),
      dicho_Y = as.numeric(Criterion > cutoff_y)
    ) |>
    dplyr::group_by(ntile_X) |>
    dplyr::summarise(
      lowerBound = min(Predictor),
      upperBound = max(Predictor),
      proportion = mean(dicho_Y),
      frequency = sum(dicho_Y),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      xlabels = paste(round(lowerBound, 1), "to", round(upperBound, 1))
    )
}

#' Calculate BESD (Binomial Effect Size Display) table
#' @param df Data frame with Predictor and Criterion columns
#' @param cutoff_x Predictor cutoff value
#' @param cutoff_y Criterion cutoff value
#' @param predictor_name Name of predictor variable
#' @param criterion_name Name of criterion variable
#' @return Matrix with BESD values
calc_besd <- function(df, cutoff_x, cutoff_y, predictor_name = "Predictor", criterion_name = "Criterion") {
  df <- df |> dplyr::mutate(dicho_Y = as.numeric(Criterion > cutoff_y))

  prob_Y_above <- mean(df$dicho_Y[df$Predictor > cutoff_x], na.rm = TRUE)
  prob_Y_below <- mean(df$dicho_Y[df$Predictor < cutoff_x], na.rm = TRUE)

  matrix(
    c(prob_Y_above, prob_Y_below, 1 - prob_Y_above, 1 - prob_Y_below),
    nrow = 2, ncol = 2,
    dimnames = list(
      c(
        paste(predictor_name, ">", round(cutoff_x, 2)),
        paste(predictor_name, "<", round(cutoff_x, 2))
      ),
      c(
        paste("p(", criterion_name, ") >", round(cutoff_y, 2)),
        paste("p(", criterion_name, ") <", round(cutoff_y, 2))
      )
    )
  )
}

#' Generate CLES verbal description
#' @param predictor_name Name of predictor variable
#' @param criterion_name Name of criterion variable
#' @param cutoff_x Predictor cutoff value
#' @param cles_pct CLES as percentage
#' @return Character string with verbal description
cles_verbal <- function(predictor_name, criterion_name, cutoff_x, cles_pct) {
  paste0(
    "A randomly chosen person with ", predictor_name, " > ", round(cutoff_x, 2),
    " has a ", cles_pct, "% chance of obtaining a higher ", criterion_name,
    " than a randomly chosen person with ", predictor_name, " < ", round(cutoff_x, 2), "."
  )
}

#' Interpret correlation strength
#' @param r Correlation coefficient
#' @return Character string describing strength
interpret_correlation <- function(r) {
  r_abs <- abs(r)
  direction <- if (r >= 0) "positive" else "negative"

  strength <- if (r_abs < 0.15) {
    "small"
  } else if (r_abs < 0.25) {
    "moderate"
  } else if (r_abs < 0.4) {
    "large"
  } else {
    "very large"
  }

  paste0("A ", strength, " ", direction, " correlation")
}

#' Interpret effect size (Cohen's d)
#' @param d Cohen's d value
#' @return Character string describing magnitude
interpret_cohens_d <- function(d) {
  d_abs <- abs(d)

  if (d_abs < 0.2) {
    "negligible"
  } else if (d_abs < 0.5) {
    "small"
  } else if (d_abs < 0.8) {
    "medium"
  } else {
    "large"
  }
}
