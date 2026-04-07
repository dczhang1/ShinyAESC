library(bslib)

colors <- list(
  primary = "#2d5a3d",
  primary_hover = "#3d7a52",
  success = "#3d7a52",
  warning = "#d4af37",
  danger = "#c97b63",

  gray_50 = "#fafaf7",
  gray_100 = "#f5f7f4",
  gray_200 = "#e8ebe6",
  gray_300 = "#c9cfc4",
  gray_400 = "#4a4844",
  gray_500 = "#3a3835",
  gray_600 = "#3a3835",
  gray_700 = "#2c2a27",
  gray_800 = "#292524",
  gray_900 = "#141414"
)

app_theme <- bs_theme(
  version = 5,

  primary = colors$primary,
  secondary = colors$gray_500,
  success = colors$success,
  warning = colors$warning,
  danger = colors$danger,
  light = colors$gray_100,
  dark = "#0f2416",

  base_font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif',
  heading_font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif',
  code_font = 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',

  font_size_base = "1.0625rem",

  "body-bg" = "#f5f7f4",
  "body-color" = "#141414",

  "card-bg" = "#FFFFFF",
  "card-border-color" = "#dfe8e0",
  "card-border-radius" = "0.5rem",
  "card-cap-bg" = "#FFFFFF",
  "card-cap-padding-y" = "0.75rem",

  "input-bg" = "#FFFFFF",
  "input-border-color" = colors$gray_300,
  "input-border-radius" = "0.375rem",
  "input-focus-border-color" = colors$primary,
  "input-focus-box-shadow" = paste0("0 0 0 2px ", colors$primary, "33"),

  "btn-border-radius" = "0.375rem",
  "btn-padding-x" = "1rem",
  "btn-padding-y" = "0.5rem",
  "btn-font-weight" = "500",

  "sidebar-bg" = "#fafaf7",
  "sidebar-border-color" = "#e8ebe6",

  "nav-link-color" = colors$gray_700,
  "nav-tabs-link-active-color" = "#141414",
  "nav-tabs-border-color" = colors$gray_300,

  "table-border-color" = colors$gray_300,
  "table-striped-bg" = colors$gray_50
)
