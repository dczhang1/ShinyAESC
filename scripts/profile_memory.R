args <- commandArgs(trailingOnly = TRUE)
app_dir <- if (length(args) >= 1) args[[1]] else "."
output_path <- if (length(args) >= 2) args[[2]] else file.path(tempdir(), "shinyescape-profvis.html")
port <- if (length(args) >= 3) as.integer(args[[3]]) else 5521L

if (!requireNamespace("profvis", quietly = TRUE)) {
  stop("The profvis package is required.")
}

if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  stop("The htmlwidgets package is required.")
}

setwd(app_dir)

profile <- profvis::profvis(
  {
    source("app.R", local = new.env(parent = globalenv()))
    app <- shiny::shinyApp(ui = ui, server = server)
    shiny::runApp(app, host = "127.0.0.1", port = port, launch.browser = FALSE)
  },
  interval = 0.01
)

htmlwidgets::saveWidget(profile, output_path, selfcontained = TRUE)
cat(normalizePath(output_path), "\n")
