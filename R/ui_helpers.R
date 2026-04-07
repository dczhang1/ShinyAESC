#' UI helper for landing feature cards
#' @param icon Lucide icon name
#' @param title Card title
#' @param description Card description
#' @param micro Short code-style micro-copy
#' @param color_class CSS color modifier class
#' @return Shiny tag object
feature_card_micro <- function(icon, title, description, micro, color_class = "feature-icon--purple") {
  div(
    class = "feature-card",
    div(
      class = paste("feature-icon", color_class),
      tags$i(`data-lucide` = icon)
    ),
    h3(class = "feature-title", title),
    p(class = "feature-description", description),
    code(class = "feature-micro", micro)
  )
}
