library(bslib)

colors <- list(
  primary = "#8B6EF0",
  primary_hover = "#744EDB",
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

app_theme <- bs_theme(
  version = 5,

  primary = colors$primary,
  secondary = colors$gray_500,
  success = colors$success,
  warning = colors$warning,
  danger = colors$danger,
  light = colors$gray_100,
  dark = colors$gray_900,

  base_font = font_google("Sora", wght = "400;500;600;700"),
  heading_font = font_google("Fraunces", wght = "500;600;700"),
  code_font = font_google("JetBrains Mono", wght = "400;500"),

  font_size_base = "0.875rem",

  "body-bg" = "#F5F2FF",
  "body-color" = colors$gray_900,

  "card-bg" = "#FFFFFF",
  "card-border-color" = colors$gray_300,
  "card-border-radius" = "0.5rem",
  "card-cap-bg" = "#FFFFFF",
  "card-cap-padding-y" = "0.75rem",

  "input-bg" = "#FFFFFF",
  "input-border-color" = colors$gray_300,
  "input-border-radius" = "0.375rem",
  "input-focus-border-color" = colors$primary,
  "input-focus-box-shadow" = paste0("0 0 0 2px ", colors$primary, "20"),

  "btn-border-radius" = "0.375rem",
  "btn-padding-x" = "1rem",
  "btn-padding-y" = "0.5rem",
  "btn-font-weight" = "500",

  "sidebar-bg" = "#FFFFFF",
  "sidebar-border-color" = colors$gray_300,

  "nav-link-color" = colors$gray_500,
  "nav-tabs-link-active-color" = colors$gray_900,
  "nav-tabs-border-color" = colors$gray_300,

  "table-border-color" = colors$gray_300,
  "table-striped-bg" = colors$gray_50
)
