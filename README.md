# ShinyAESC: Shiny Alternative Effect Size Calculator

## Overview
ShinyAESC is a web-based tool designed to help researchers and practitioners calculate and visualize alternative effect size statistics. It focuses on making statistical results more interpretable by providing indices such as the **Common Language Effect Size (CLES)** and the **Binomial Effect Size Display (BESD)**, alongside traditional statistics like Pearson's correlation and Cohen's d (implied via CLES conversions).

This tool was developed to accompany the paper:
> **Zhang, D. C. (2018).** Utility of alternative effect size statistics and the development of a web-based calculator: Shiny-AESC. *Frontiers in Psychology, 9*, 1221. [doi:10.3389/fpsyg.2018.01221](https://doi.org/10.3389/fpsyg.2018.01221)

## Features
- **Data Upload**: Upload your own CSV datasets (up to 30MB).
- **Variable Selection**: Dynamically select predictor and criterion variables.
- **Expectancy Charts**: Visualize the probability of success/failure across different bins of the predictor.
- **Alternative Effect Sizes**:
  - **Common Language Effect Size (CLES)**: Probability that a randomly selected score from one group is higher than a randomly selected score from another.
  - **Binomial Effect Size Display (BESD)**: Shows the change in success rate as a function of the predictor.
- **Visualizations**: Histograms, scatterplots with regression lines, and overlapping density plots.
- **Interactive Cutoffs**: Adjust cutoffs for both predictor and criterion variables to see how they impact effect size estimates.

## Installation & Usage
You can run this app locally using R and RStudio.

### Prerequisites
Ensure you have the following R packages installed:
```r
install.packages(c("tidyverse", "readr", "psych", "shiny", "mosaic"))
```

### Running the App
1. Clone this repository.
2. Open `appv2.r` in RStudio.
3. Click the "Run App" button or run:
   ```r
   shiny::runApp("appv2.r")
   ```

## Structure
- `appv2.r`: The main Shiny application source code (UI and Server).
- `shinyInstructions.html`: The help documentation displayed within the app.
- `sampleData.csv`: A sample dataset for demonstration purposes.

## Citation
If you use this tool in your research, please cite:
Zhang, D. C. (2018). Utility of alternative effect size statistics and the development of a web-based calculator: Shiny-AESC. *Frontiers in Psychology, 9*, 1221.
