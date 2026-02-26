# utils_theme.R
# Linear-inspired theme configuration for ShinyAESC
# Using bslib for Bootstrap 5 theming

library(bslib)

# Color palette - Linear-inspired, near-monochrome
colors <- list(

  primary = "#5E6AD2",
  primary_hover = "#4850A8",
  success = "#3CCB7F",
  warning = "#FFBB38",
  danger = "#F87171",


  gray_50 = "#FAFAFA",
  gray_100 = "#F4F4F5",
  gray_200 = "#E4E4E7",
  gray_300 = "#D4D4D8",
  gray_400 = "#A1A1AA",
  gray_500 = "#71717A",
  gray_600 = "#52525B",
  gray_700 = "#3F3F46",
  gray_800 = "#27272A",
  gray_900 = "#18181B"
)

# Create the main theme
app_theme <- bs_theme(
  version = 5,

  # Core colors
  primary = colors$primary,
  secondary = colors$gray_500,
  success = colors$success,
  warning = colors$warning,
  danger = colors$danger,
  light = colors$gray_100,
  dark = colors$gray_900,

  # Typography - Inter for UI, monospace for stats
  base_font = font_google("Inter", wght = "400;500;600;700"),
  heading_font = font_google("Inter", wght = "600;700"),
  code_font = font_google("JetBrains Mono", wght = "400;500"),

  # Font sizes - Linear uses compact sizing
  font_size_base = "0.875rem",

  # Customize Bootstrap variables
  "body-bg" = colors$gray_50,
  "body-color" = colors$gray_900,

  # Cards
  "card-bg" = "#FFFFFF",
  "card-border-color" = colors$gray_300,
  "card-border-radius" = "0.5rem",
  "card-cap-bg" = "#FFFFFF",
  "card-cap-padding-y" = "0.75rem",

  # Inputs
  "input-bg" = "#FFFFFF",
  "input-border-color" = colors$gray_300,
  "input-border-radius" = "0.375rem",
  "input-focus-border-color" = colors$primary,
  "input-focus-box-shadow" = paste0("0 0 0 2px ", colors$primary, "20"),

  # Buttons
  "btn-border-radius" = "0.375rem",
  "btn-padding-x" = "1rem",
  "btn-padding-y" = "0.5rem",
  "btn-font-weight" = "500",

  # Sidebar
  "sidebar-bg" = "#FFFFFF",
  "sidebar-border-color" = colors$gray_300,

  # Nav tabs
  "nav-link-color" = colors$gray_500,
  "nav-tabs-link-active-color" = colors$gray_900,
  "nav-tabs-border-color" = colors$gray_300,

  # Tables
  "table-border-color" = colors$gray_300,
  "table-striped-bg" = colors$gray_50
)
