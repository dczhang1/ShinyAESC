# ESCAPE — Effect Size Calculator for Practical Effects

**Turn statistical effect sizes into plain-language outcomes.**

[![Live App](https://img.shields.io/badge/Live_App-shinyescape-brightgreen?style=flat-square)](https://dczhang.shinyapps.io/shinyescape/)
[![Paper](https://img.shields.io/badge/Paper-Frontiers_in_Psychology-blue?style=flat-square)](https://doi.org/10.3389/fpsyg.2018.01221)

> **Try it now:** [dczhang.shinyapps.io/shinyescape](https://dczhang.shinyapps.io/shinyescape/)

---

## The Problem

A correlation of *r* = .03 sounds negligible—until you learn it means 17 fewer heart attacks per 1,000 patients, which is tens of thousands of lives across millions of prescriptions. That was the actual finding from the Physician's Health Study on aspirin (Rosenthal, 1990).

Traditional effect sizes like *r* and Cohen's *d* report magnitude, but most people—even academics—struggle to gauge what the numbers mean in practice. Cohen's benchmarks (.10 = small, .30 = medium, .50 = large) were never empirically grounded, and real-world effect sizes in psychology and organizational science cluster far below them. The median correlation in I-O psychology is |*r*| = .16 (Bosco et al., 2015). What Cohen called "medium" is actually larger than 70% of observed effects.

**ESCAPE bridges the gap between statistical reporting and practical understanding.**

---

## What ESCAPE Does

Upload your data (or use the built-in sample), select a predictor and criterion, and ESCAPE translates your effect size into four common-language formats:

| Output | What it tells you |
|--------|-------------------|
| **Binomial Effect Size Display (BESD)** | Success rates for two groups in a 2 × 2 table—e.g., "65% of high-scorers succeed vs. 35% of low-scorers" |
| **Common Language Effect Size (CLES)** | Probability that a random person from Group A outperforms a random person from Group B—e.g., "a 64% chance the trained employee performs better" |
| **Expectancy Chart** | Success rates across percentile bands—e.g., "45% of top-quartile applicants are top performers, vs. 15% of bottom-quartile" |
| **Icon Array** | Visual grid of filled/unfilled icons showing outcome proportions at a glance |

Alongside these practical translations, ESCAPE reports the traditional statistics: Pearson *r*, Cohen's *d*, group descriptive tables, and a converter for translating between indices without data.

### Key features

- **Upload or demo:** CSV, Excel (.xlsx), SPSS (.sav), or SAS (.sas7bdat); bundled NHIS 2024 sample dataset included
- **Guided workflow:** Step-by-step wizard for variable selection, binning, and cutoffs
- **Export:** Downloadable HTML report; optional PDF with a LaTeX engine
- **No install needed:** Use the live app in any modern browser
- **Learning guide:** Built-in ["Making Numbers Meaningful"](https://dczhang.shinyapps.io/shinyescape/learning-tool.html) primer on effect size communication

---

## Citation

If you use ESCAPE in published work, please cite:

> Zhang, D. C. (2018). Utility of alternative effect size statistics and the development of a web-based calculator: Shiny-AESC. *Frontiers in Psychology*, *9*, 1221. [https://doi.org/10.3389/fpsyg.2018.01221](https://doi.org/10.3389/fpsyg.2018.01221)

---

## Run Locally

**Requirements:** R ≥ 4.0.

```r
install.packages(c(
  "shiny", "bslib", "tidyverse", "readxl", "haven",
  "psych", "DT", "rmarkdown", "knitr"
))
```

Clone or download this repository, then:

```r
shiny::runApp(".")
```

Or from the command line:

```bash
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

---

## Project Structure

```
app.R                  Main application (UI + server)
R/
  utils_stats.R        Statistical computation functions
  utils_plots.R        ggplot2 chart rendering
  utils_theme.R        bslib theme and color palette
  ui_helpers.R         UI component helpers
www/
  css/                 Landing page and analysis styles
  js/main.js           Toast notifications, carousel, interactions
  learning-tool.html   Standalone guide to effect size communication
  assets/              Images and static resources
data/
  sampleData.csv       Demo dataset (NHIS 2024, n = 1,000)
docs/
  shinyInstructions.html   In-app Help tab content
```

The sample data uses variables from the NHIS 2024 Sample Adult public-use file (NCHS, CDC): vigorous leisure-time physical activity frequency and self-rated health. It is included for demonstration purposes and does not reflect NCHS or CDC endorsement.

---

## References

- Bosco, F. A., Aguinis, H., Singh, K., Field, J. G., & Pierce, C. A. (2015). Correlational effect size benchmarks. *Journal of Applied Psychology*, *100*(2), 431–449.
- Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for individual differences researchers. *Personality and Individual Differences*, *102*, 74–78.
- McGraw, K. O., & Wong, S. P. (1992). A common language effect size statistic. *Psychological Bulletin*, *111*(2), 361–365.
- Rosenthal, R. (1990). How are we doing in soft psychology? *American Psychologist*, *45*(6), 775–777.
- Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. *Journal of Educational Psychology*, *74*(2), 166–169.

---

## Author

**Don C. Zhang, Ph.D.**  

Department of Psychology, Louisiana State University

www.randmlab.com

---
