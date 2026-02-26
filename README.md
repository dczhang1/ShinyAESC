# ESCAPE: Effect Size Calculator for Practical Effects

## Overview

ESCAPE is a web-based SaaS tool that helps researchers and practitioners calculate and visualize alternative effect size statistics. It makes statistical results more interpretable by providing indices such as the **Common Language Effect Size (CLES)** and the **Binomial Effect Size Display (BESD)**, alongside traditional statistics like Pearson's correlation and Cohen's d (implied via CLES conversions).

This tool was developed to accompany the paper:

> **Zhang, D. C. (2018).** Utility of alternative effect size statistics and the development of a web-based calculator: Shiny-AESC. *Frontiers in Psychology, 9*, 1221. [doi:10.3389/fpsyg.2018.01221](https://doi.org/10.3389/fpsyg.2018.01221)

The app has been modernized with a Linear-inspired UI, landing page, and modular architecture suitable for deployment as a Shiny SaaS application.

## Features

- **Landing page**: Hero section with sample data or upload options (CSV, Excel, SPSS, SAS).
- **Data upload**: Support for CSV, XLSX, .sav, and .sas7bdat (up to 30MB).
- **Variable selection**: Dynamically select predictor and criterion variables.
- **Expectancy charts**: Visualize the probability of success/failure across predictor bins.
- **Alternative effect sizes**:
  - **Common Language Effect Size (CLES)**: Probability that a randomly selected score from one group is higher than from another.
  - **Binomial Effect Size Display (BESD)**: Change in success rate as a function of the predictor.
- **Visualizations**: Histograms, scatterplots with regression lines, overlapping density plots (including Plotly interactivity where applicable).
- **Interactive cutoffs**: Adjust cutoffs for predictor and criterion to see impact on effect size estimates.
- **Modern UI**: bslib theme, custom CSS (landing and main), and Lucide icons.

## Installation & usage

Run the app locally with R and RStudio.

### Prerequisites

Install required R packages:

```r
install.packages(c(
  "shiny", "bslib", "tidyverse", "readxl", "haven",
  "psych", "DT", "rmarkdown", "knitr"
))
```

Report export requires `rmarkdown` and `knitr`.

### Running the app

1. Clone this repository.
2. Open `app.R` in RStudio.
3. Click **Run App** or run:

```r
shiny::runApp("app.R")
```

For deployment (e.g. Shiny Server, ShinyApps.io, RStudio Connect), deploy the project root with `app.R` as the application file.

## Project structure

| Path | Description |
|------|-------------|
| `app.R` | Main Shiny application (UI and server); entry point for the SaaS app. |
| `R/utils_theme.R` | Theme and UI styling utilities. |
| `R/utils_stats.R` | Effect size and statistical computation utilities. |
| `R/utils_plots.R` | Plotting and visualization utilities. |
| `www/css/` | Stylesheets (e.g. `landing.css`, `main.css`). |
| `www/js/` | Client-side JavaScript (e.g. `main.js`). |
| `data/sampleData.csv` | Sample dataset for demonstration. |
| `shinyInstructions.html` | In-app help documentation. |
| `archive/` | Deprecated single-file app versions (`appv2.r`, `app_v2.R`, `app_v3.R`) kept for reference. |

## Citation

If you use this tool in your research, please cite:

Zhang, D. C. (2018). Utility of alternative effect size statistics and the development of a web-based calculator: Shiny-AESC. *Frontiers in Psychology, 9*, 1221.
