args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) > 0) args[[1]] else "."

bytes_to_mb <- function(x) round(x / (1024 * 1024), 2)

size_if_exists <- function(path) {
  if (!file.exists(path)) return(0)
  file.info(path)$size
}

cat("== ShinyAESC Performance Snapshot ==\n")
cat("timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

www_path <- file.path(root, "www")
head_png <- file.path(root, "www", "images", "head.png")
head_jpg <- file.path(root, "www", "images", "head.jpg")
assets_path <- file.path(root, "www", "assets")

www_files <- if (dir.exists(www_path)) list.files(www_path, recursive = TRUE, full.names = TRUE) else character(0)
www_total <- if (length(www_files) == 0) 0 else sum(file.info(www_files)$size, na.rm = TRUE)

cat("www_total_mb:", bytes_to_mb(www_total), "\n")
cat("head_png_kb:", round(size_if_exists(head_png) / 1024, 2), "\n")
cat("head_jpg_kb:", round(size_if_exists(head_jpg) / 1024, 2), "\n")
cat("assets_dir_exists:", dir.exists(assets_path), "\n\n")

app_path <- file.path(root, "app.R")
if (file.exists(app_path)) {
  app_lines <- length(readLines(app_path, warn = FALSE))
  app_text <- readLines(app_path, warn = FALSE)
  reactive_like <- sum(grepl("reactive\\(|renderPlot\\(|renderTable\\(|renderUI\\(|renderText\\(|observeEvent\\(|observe\\(", app_text))
  bind_cache_count <- sum(grepl("bindCache(", app_text, fixed = TRUE))
  debounce_count <- sum(grepl("debounce(", app_text, fixed = TRUE))
  cat("app_lines:", app_lines, "\n")
  cat("reactive_like_count:", reactive_like, "\n")
  cat("bindCache_count:", bind_cache_count, "\n")
  cat("debounce_count:", debounce_count, "\n")
}
