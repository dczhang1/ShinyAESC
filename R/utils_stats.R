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

#' Validate groups for two-group effect size calculations
#' @param group1 Values from first group
#' @param group2 Values from second group
#' @return Named list with cleaned vectors and diagnostics
validate_two_groups <- function(group1, group2) {
  g1 <- group1[!is.na(group1)]
  g2 <- group2[!is.na(group2)]
  n1 <- length(g1)
  n2 <- length(g2)
  list(
    group1 = g1,
    group2 = g2,
    n1 = n1,
    n2 = n2,
    ok = (n1 >= 2 && n2 >= 2),
    message = if (n1 < 2 || n2 < 2) "Insufficient non-missing values in one or both groups" else NULL
  )
}

#' Convert correlation to Cohen's d (theoretical)
#' @param r Correlation coefficient
#' @return Cohen's d
r_to_d <- function(r) {
  if (is.na(r) || abs(r) >= 1) return(NA_real_)
  2 * r / sqrt(1 - r^2)
}

#' Convert Cohen's d to correlation (theoretical)
#' @param d Cohen's d
#' @return Correlation coefficient
d_to_r <- function(d) {
  if (is.na(d)) return(NA_real_)
  d / sqrt(d^2 + 4)
}

#' Convert Cohen's d to CLES (parametric)
#' @param d Cohen's d
#' @return CLES probability
d_to_cles <- function(d) {
  if (is.na(d)) return(NA_real_)
  pnorm(d / sqrt(2))
}

#' Calculate confidence interval for r using Fisher z
#' @param r Correlation coefficient
#' @param n Sample size
#' @param ci Confidence level
#' @return List with lower and upper bounds
calc_r_ci <- function(r, n, ci = 0.95) {
  if (is.na(r) || n < 4 || abs(r) >= 1) return(list(lower = NA_real_, upper = NA_real_))
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  z_crit <- qnorm(1 - (1 - ci) / 2)
  z_low <- z - z_crit * se
  z_high <- z + z_crit * se
  list(lower = tanh(z_low), upper = tanh(z_high))
}

#' Calculate Cohen's d effect size
#' @param group1 Values from first group
#' @param group2 Values from second group
#' @return Cohen's d value
calc_cohens_d <- function(group1, group2) {
  v <- validate_two_groups(group1, group2)
  if (!v$ok) return(NA_real_)
  m1 <- mean(v$group1)
  m2 <- mean(v$group2)
  s1 <- sd(v$group1)
  s2 <- sd(v$group2)
  denom_df <- v$n1 + v$n2 - 2
  if (is.na(s1) || is.na(s2) || denom_df <= 0) return(NA_real_)
  # Weighted pooled SD (recommended default for two independent groups)
  sd_pooled <- sqrt(((v$n1 - 1) * s1^2 + (v$n2 - 1) * s2^2) / denom_df)
  if (!is.finite(sd_pooled) || sd_pooled <= 0) return(NA_real_)
  (m1 - m2) / sd_pooled
}

#' Calculate Hedges' g effect size (bias-corrected Cohen's d)
#' @param group1 Values from first group
#' @param group2 Values from second group
#' @return Hedges' g value
calc_hedges_g <- function(group1, group2) {
  d <- calc_cohens_d(group1, group2)
  v <- validate_two_groups(group1, group2)
  if (is.na(d) || !v$ok) return(NA_real_)
  df <- v$n1 + v$n2 - 2
  if (df <= 1) return(NA_real_)
  # Small sample correction
  J <- 1 - (3 / (4 * df - 1))
  J * d
}

#' Calculate Common Language Effect Size (CLES)
#' Probability that a randomly chosen member of group 1 will have a higher
#' score than a randomly chosen member of group 2
#' @param group1 Values from first group
#' @param group2 Values from second group
#' @return CLES probability (0-1)
calc_cles <- function(group1, group2) {
  d <- calc_cohens_d(group1, group2)
  d_to_cles(d)
}

#' Calculate confidence interval for Cohen's d
#' @param d Cohen's d value
#' @param n1 Sample size of group 1
#' @param n2 Sample size of group 2
#' @param ci Confidence level (default 0.95)
#' @return List with lower and upper bounds
calc_d_ci <- function(d, n1, n2, ci = 0.95) {
  if (is.na(d) || n1 < 2 || n2 < 2) return(list(lower = NA_real_, upper = NA_real_))
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
  v <- validate_two_groups(above_group, below_group)
  if (!v$ok) {
    return(data.frame(
      Metric = "Error",
      Value = "Insufficient data in one or both groups"
    ))
  }
  n1 <- v$n1
  n2 <- v$n2

  hedges_g <- calc_hedges_g(above_group, below_group)
  cohens_d <- calc_cohens_d(above_group, below_group)
  cles <- calc_cles(above_group, below_group)
  d_ci <- calc_d_ci(cohens_d, n1, n2)
  r_ci <- calc_r_ci(r_value, n1 + n2)

  data.frame(
    Metric = c(
      "Pearson's r",
      "Pearson's r [95% CI]",
      "Hedges' g",
      "Cohen's d",
      "Cohen's d [95% CI]",
      "CLES (Probability)"
    ),
    Value = c(
      round(r_value, 3),
      paste0("[", round(r_ci$lower, 3), ", ", round(r_ci$upper, 3), "]"),
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
    dplyr::arrange(ntile_X) |>
    dplyr::mutate(
      xlabels = paste(round(lowerBound, 1), "to", round(upperBound, 1)),
      xlabels = factor(xlabels, levels = unique(xlabels))
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
  above_idx <- df$Predictor >= cutoff_x
  below_idx <- df$Predictor < cutoff_x
  n_above <- sum(above_idx, na.rm = TRUE)
  n_below <- sum(below_idx, na.rm = TRUE)
  if (n_above < 1 || n_below < 1) {
    return(matrix(
      c(NA_real_, NA_real_, NA_real_, NA_real_),
      nrow = 2, ncol = 2,
      dimnames = list(
        c(
          paste(predictor_name, ">=", round(cutoff_x, 2)),
          paste(predictor_name, "<", round(cutoff_x, 2))
        ),
        c(
          paste("p(", criterion_name, ") >", round(cutoff_y, 2)),
          paste("p(", criterion_name, ") <=", round(cutoff_y, 2))
        )
      )
    ))
  }
  prob_Y_above <- mean(df$dicho_Y[above_idx], na.rm = TRUE)
  prob_Y_below <- mean(df$dicho_Y[below_idx], na.rm = TRUE)

  matrix(
    c(prob_Y_above, prob_Y_below, 1 - prob_Y_above, 1 - prob_Y_below),
    nrow = 2, ncol = 2,
    dimnames = list(
      c(
        paste(predictor_name, ">=", round(cutoff_x, 2)),
        paste(predictor_name, "<", round(cutoff_x, 2))
      ),
      c(
        paste("p(", criterion_name, ") >", round(cutoff_y, 2)),
        paste("p(", criterion_name, ") <=", round(cutoff_y, 2))
      )
    )
  )
}

#' Calculate canonical BESD from correlation
#' @param r Correlation coefficient
#' @param predictor_name Name of predictor variable
#' @param criterion_name Name of criterion variable
#' @return Matrix with BESD values
calc_besd_theoretical <- function(r, predictor_name = "Predictor", criterion_name = "Criterion") {
  if (is.na(r)) {
    return(matrix(
      c(NA_real_, NA_real_, NA_real_, NA_real_),
      nrow = 2, ncol = 2,
      dimnames = list(
        c(paste(predictor_name, "High"), paste(predictor_name, "Low")),
        c(paste(criterion_name, "Success"), paste(criterion_name, "Failure"))
      )
    ))
  }
  hit_high <- 0.5 + r / 2
  hit_low <- 0.5 - r / 2
  matrix(
    c(hit_high, hit_low, 1 - hit_high, 1 - hit_low),
    nrow = 2, ncol = 2,
    dimnames = list(
      c(paste(predictor_name, "High"), paste(predictor_name, "Low")),
      c(paste(criterion_name, "Success"), paste(criterion_name, "Failure"))
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
