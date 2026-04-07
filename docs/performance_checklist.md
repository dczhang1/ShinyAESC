# Performance Checklist

Use this checklist before and after optimization work to keep changes measurable.

## 1) Static Payload

- Run: `Rscript scripts/perf_check.R`
- Record:
  - `www_total_mb`
  - `head_png_kb` and `head_jpg_kb`
  - whether `www/assets` is present

## 2) Runtime Syntax Safety

- Run:
  - `Rscript -e "tryCatch(parse('app.R'), error=function(e) cat('ERROR:', e$message, '\n')); cat('OK\n')"`
  - `node -c www/js/main.js`

## 3) Reactive/Render Efficiency

- Run app locally: `Rscript -e "shiny::runApp('.', host='127.0.0.1', port=5521, launch.browser=FALSE)"`
- Interact with:
  - predictor/criterion selectors
  - cutoff sliders
  - bins input
- Confirm:
  - no repeated toast spam
  - smooth plot refresh
  - no icon flicker on view switches

## 4) Visual/UX Regression Pass

- Landing page loads with expected styles.
- Analysis view loads with expected styles.
- Creator image renders correctly.
- Downloads and report generation still succeed.

## 5) Optional Profiling

- Reactive graph: `options(shiny.reactlog = TRUE)` and inspect with `shiny::reactlogShow()`
- CPU profiling: `profvis::profvis({ shiny::runApp('.') })`
