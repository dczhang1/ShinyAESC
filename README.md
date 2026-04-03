# ESCAPE

**Effect Size Calculator for Practical Effects**

ESCAPE is an [R Shiny](https://shiny.posit.co/) application that turns traditional effect sizes (correlations, *d*, etc.) into **probabilities, success rates, and visual summaries** so results are easier to explain to stakeholders who do not read statistics journals.

**Live app:** [https://dczhang.shinyapps.io/shinyaesc/](https://dczhang.shinyapps.io/shinyaesc/)

---

## Why ESCAPE

Reporting *r* or Cohen’s *d* satisfies methods sections; it rarely answers “what does this mean in practice?” ESCAPE emphasizes **common-language** summaries—especially the **Common Language Effect Size (CLES)** and **Binomial Effect Size Display (BESD)**—alongside expectancy curves, icon arrays, and classical indices, so you can communicate tradeoffs and magnitudes in plain language.

---

## Features (high level)

- **Landing + analysis flow:** Sample data or upload (CSV, Excel, SPSS, SAS)
- **Variable controls:** Predictor (X), criterion (Y), bins, cutoffs, predictor percentile splits
- **Practical outputs:** CLES, BESD, expectancy chart/table, icon array
- **Traditional outputs:** Correlation, *d* / related summaries, group tables, theoretical converter
- **Exports:** HTML report; optional PDF (requires a LaTeX engine such as [TinyTeX](https://yihui.org/tinytex/) for PDF)

---

## Attribution & citation

**Development and maintenance:** Don Zhang  

**Implementation:** R, Shiny, bslib (Bootstrap 5), ggplot2, and related packages (see `app.R` and `R/`).

The tool accompanies and extends the peer-reviewed description:

> Zhang, D. C. (2018). Utility of alternative effect size statistics and the development of a web-based calculator: Shiny-AESC. *Frontiers in Psychology*, *9*, 1221.  
> [https://doi.org/10.3389/fpsyg.2018.01221](https://doi.org/10.3389/fpsyg.2018.01221)

If you use ESCAPE in research, please cite that paper (and the app URL if helpful for reproducibility).

---

## Run locally

**Requirements:** R (4.x recommended) and packages used by the app, including `shiny`, `bslib`, `tidyverse`, `readxl`, `haven`, `psych`, `DT`, and for reports `rmarkdown`, `knitr`.

```r
install.packages(c(
  "shiny", "bslib", "tidyverse", "readxl", "haven",
  "psych", "DT", "rmarkdown", "knitr"
))
```

From the repository root:

```r
shiny::runApp("app.R")
```

Or in a shell:

```bash
Rscript app.R
```

---

## Repository layout

| Path | Role |
|------|------|
| `app.R` | Main Shiny app (UI + server) |
| `R/utils_*.R` | Theme, statistics, plots |
| `www/` | CSS, JS, assets |
| `data/sampleData.csv` | Bundled demo data |
| `docs/shinyInstructions.html` | In-app Help tab content |
| `docs/README.md` | Additional project notes and structure |

---

## More documentation

Extended overview, feature list, and roadmap-style notes: **[docs/README.md](docs/README.md)**.

For coding agents and contributors, see **[AGENTS.md](AGENTS.md)** (build commands, conventions, file map).
