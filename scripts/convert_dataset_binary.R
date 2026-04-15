args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript scripts/convert_dataset_binary.R <input_path> [output_stem]")
}

input_path <- normalizePath(args[[1]], mustWork = TRUE)
output_stem <- if (length(args) >= 2) {
  args[[2]]
} else {
  tools::file_path_sans_ext(input_path)
}

ext <- tolower(tools::file_ext(input_path))

df <- switch(ext,
  csv = data.table::fread(input_path, data.table = FALSE, showProgress = FALSE),
  xlsx = readxl::read_excel(input_path),
  sav = haven::read_sav(input_path),
  sas7bdat = haven::read_sas(input_path),
  stop("Unsupported input format.")
)

outputs <- character(0)

if (requireNamespace("qs", quietly = TRUE)) {
  qs_path <- paste0(output_stem, ".qs")
  qs::qsave(df, qs_path)
  outputs <- c(outputs, normalizePath(qs_path, mustWork = TRUE))
}

if (requireNamespace("arrow", quietly = TRUE)) {
  parquet_path <- paste0(output_stem, ".parquet")
  arrow::write_parquet(df, parquet_path)
  outputs <- c(outputs, normalizePath(parquet_path, mustWork = TRUE))
}

if (!length(outputs)) {
  stop("No binary output written. Install qs or arrow first.")
}

cat(paste(outputs, collapse = "\n"), "\n")
