# utils_plots.R
# Plot theming and helper functions for ESCAPE
# Linear-inspired, minimal aesthetic (static ggplot2 only)

library(ggplot2)

# Color palette for plots
plot_colors <- list(
  primary = "#5E6AD2",
  secondary = "#A1A1AA",
  success = "#3CCB7F",
  warning = "#FFBB38",
  danger = "#F87171",
  text = "#18181B",
  text_muted = "#71717A",
  grid = "#E4E4E7",
  background = "transparent",
  surface = "#FFFFFF"
)

#' Create a minimal ggplot2 theme
#' @return ggplot2 theme object
theme_minimal_linear <- function() {
  theme_minimal(base_family = "Inter") +
    theme(
      # Text
      text = element_text(color = plot_colors$text),
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, color = plot_colors$text_muted),

      # Axes
      axis.title = element_text(size = 11, color = plot_colors$text_muted),
      axis.text = element_text(size = 10, color = plot_colors$text_muted),
      axis.line = element_line(color = plot_colors$grid, linewidth = 0.5),
      axis.ticks = element_line(color = plot_colors$grid, linewidth = 0.5),

      # Grid
      panel.grid.major = element_line(color = plot_colors$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),

      # Legend
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill = "transparent"),

      # Panel
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),

      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

#' Create expectancy bar chart
#' @param df Expectancy data frame from calc_expectancy()
#' @param predictor_name Name of predictor variable
#' @param cutoff_y Criterion cutoff value
#' @return ggplot object
plot_expectancy <- function(df, predictor_name, cutoff_y) {
  ggplot(df, aes(x = xlabels, y = proportion)) +
    geom_col(fill = plot_colors$primary, width = 0.7) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = predictor_name,
      y = paste("Proportion above", cutoff_y)
    ) +
    theme_minimal_linear() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
    )
}

#' Create expectancy bar chart for landing page (striking, card-style aesthetic)
#' @param df Expectancy data frame from calc_expectancy()
#' @param predictor_name Name of predictor variable
#' @param cutoff_y Criterion cutoff value
#' @return ggplot object
plot_expectancy_landing <- function(df, predictor_name, cutoff_y) {
  primary_light <- paste0(plot_colors$primary, "99")
  df$pct_label <- scales::percent(df$proportion, accuracy = 1)
  df$tier <- ifelse(df$proportion >= 0.5, "above", "below")
  ggplot(df, aes(x = xlabels, y = proportion, fill = tier)) +
    geom_hline(
      yintercept = 0.5,
      color = plot_colors$grid,
      linewidth = 0.35,
      linetype = "dashed"
    ) +
    geom_col(width = 0.72, color = NA) +
    geom_text(
      aes(y = proportion / 2, label = pct_label),
      color = "white",
      fontface = "bold",
      size = 3.2,
      vjust = 0.5
    ) +
    scale_fill_manual(
      values = c("above" = plot_colors$primary, "below" = primary_light),
      guide = "none"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(),
      expand = expansion(mult = c(0, 0.06))
    ) +
    labs(x = predictor_name, y = NULL) +
    theme_minimal_linear() +
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 38, hjust = 1, size = 10, color = plot_colors$text_muted),
      axis.text.y = element_text(size = 9, color = plot_colors$text_muted),
      axis.line = element_line(color = paste0(plot_colors$grid, "CC"), linewidth = 0.5),
      panel.grid.major.y = element_line(color = paste0(plot_colors$grid, "99"), linewidth = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.margin = margin(10, 12, 10, 12),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}

#' Create histogram
#' @param data Numeric vector
#' @param title Plot title
#' @param fill_color Fill color for bars
#' @return ggplot object
plot_histogram <- function(data, title = "", fill_color = plot_colors$primary) {
  df <- data.frame(x = data)

  ggplot(df, aes(x = x)) +
    geom_histogram(
      bins = 20,
      fill = fill_color,
      color = "white",
      linewidth = 0.5
    ) +
    labs(title = title, x = NULL, y = "Count") +
    theme_minimal_linear()
}

#' Create scatter plot with regression line
#' @param df Data frame with Predictor and Criterion columns
#' @param predictor_name Name of predictor variable
#' @param criterion_name Name of criterion variable
#' @return ggplot object
plot_scatter <- function(df, predictor_name = "Predictor", criterion_name = "Criterion") {
  ggplot(df, aes(x = Predictor, y = Criterion)) +
    geom_point(
      alpha = 0.6,
      color = plot_colors$text_muted,
      size = 2
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = plot_colors$primary,
      fill = paste0(plot_colors$primary, "20"),
      linewidth = 1
    ) +
    labs(
      x = predictor_name,
      y = criterion_name
    ) +
    theme_minimal_linear()
}

#' Create density overlap plot for CLES visualization
#' @param df Data frame with Criterion and Group columns
#' @param criterion_name Name of criterion variable
#' @param predictor_name Name of predictor variable
#' @param cutoff_x Predictor cutoff value
#' @return ggplot object
plot_density_overlap <- function(df, criterion_name, predictor_name, cutoff_x) {
  ggplot(df, aes(x = Criterion, fill = Group)) +
    geom_density(alpha = 0.5, color = NA) +
    scale_fill_manual(
      values = c("Above" = plot_colors$primary, "Below" = plot_colors$secondary),
      name = paste(predictor_name, "cutoff:", round(cutoff_x, 2))
    ) +
    labs(
      title = paste("Distribution of", criterion_name, "by Group"),
      x = criterion_name,
      y = "Density"
    ) +
    theme_minimal_linear() +
    theme(
      legend.position = "bottom"
    )
}

#' Create icon array visualization for CLES
#' @param cles_prob CLES probability (0-1)
#' @param total_icons Total number of icons to display
#' @param icon_type Type of icon: "person", "circle", or "square"
#' @param layout Layout: "auto", "10x10", "20x20", "25x25"
#' @param predictor_name Name of predictor variable
#' @param criterion_name Name of criterion variable
#' @return ggplot object
plot_icon_array <- function(cles_prob, total_icons = 100, icon_type = "person", layout = "auto",
                             predictor_name = "Predictor", criterion_name = "Criterion") {
  
  # Calculate number of success and failure icons
  num_success <- round(cles_prob * total_icons)
  num_failure <- total_icons - num_success
  
  # Create data frame for icons
  icon_data <- data.frame(
    icon_id = 1:total_icons,
    status = c(rep("Success", num_success), rep("Failure", num_failure))
  )
  
  # Determine grid dimensions based on layout
  if (layout == "10x10") {
    n_cols <- 10
    n_rows <- ceiling(total_icons / n_cols)
  } else if (layout == "20x20") {
    n_cols <- 20
    n_rows <- ceiling(total_icons / n_cols)
  } else if (layout == "25x25") {
    n_cols <- 25
    n_rows <- ceiling(total_icons / n_cols)
  } else {
    # Auto layout - try to make it as square as possible
    n_cols <- ceiling(sqrt(total_icons))
    n_rows <- ceiling(total_icons / n_cols)
  }
  
  # Add grid positions
  icon_data <- icon_data %>%
    mutate(
      row = ((icon_id - 1) %/% n_cols),
      col = ((icon_id - 1) %% n_cols)
    )
  
  # Reverse row numbering so it goes from top to bottom
  icon_data$row <- n_rows - 1 - icon_data$row
  
  # Create plot based on icon type
  if (icon_type == "person") {
    # Use geom_point with person-like shape
    p <- ggplot(icon_data, aes(x = col, y = row, color = status)) +
      geom_point(size = 3, shape = 16) +
      scale_color_manual(
        values = c("Success" = plot_colors$primary, "Failure" = plot_colors$secondary),
        name = "Status"
      )
  } else if (icon_type == "circle") {
    p <- ggplot(icon_data, aes(x = col, y = row, color = status)) +
      geom_point(size = 4, shape = 21, stroke = 1) +
      scale_color_manual(
        values = c("Success" = plot_colors$primary, "Failure" = plot_colors$secondary),
        name = "Status"
      )
  } else if (icon_type == "square") {
    p <- ggplot(icon_data, aes(x = col, y = row, color = status)) +
      geom_point(size = 4, shape = 22, stroke = 1) +
      scale_color_manual(
        values = c("Success" = plot_colors$primary, "Failure" = plot_colors$secondary),
        name = "Status"
      )
  }
  
  # Add labels and theme
  p <- p +
    labs(
      title = paste0("Icon Array: ", num_success, " out of ", total_icons, " (", round(cles_prob * 100, 1), "%)"),
      subtitle = paste("Probability that a person with high", predictor_name, "scores higher on", criterion_name),
      x = NULL,
      y = NULL
    ) +
    scale_x_continuous(
      limits = c(-0.5, n_cols - 0.5),
      expand = c(0, 0),
      breaks = NULL
    ) +
    scale_y_continuous(
      limits = c(-0.5, n_rows - 0.5),
      expand = c(0, 0),
      breaks = NULL
    ) +
    coord_fixed(ratio = 1) +
    theme_minimal_linear() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = plot_colors$text_muted, hjust = 0.5)
    )
  
  p
}
