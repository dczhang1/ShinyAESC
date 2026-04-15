# ESCAPE - Effect Size Calculator for Practical Effects
# Modernized version with Linear-inspired design
# Features: Landing page, modern UI, comprehensive effect size analysis

# Load packages
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readxl)
library(haven)
library(psych)
library(DT)
library(tibble)

max_upload_mb <- 1
max_dataset_rows <- 10000L

options(shiny.maxRequestSize = max_upload_mb * 1024^2)

# Source utility modules
source("R/utils_theme.R")
source("R/utils_stats.R")
source("R/utils_plots.R")
source("R/ui_helpers.R")

# Resolve sample data path (works from project root or app dir)
get_sample_data_path <- function() {
  app_dir <- tryCatch(
    getShinyOption("appDir", getwd()),
    error = function(e) getwd()
  )
  candidates <- c(
    "data/sampleData.csv",
    "sampleData.csv",
    file.path(app_dir, "data/sampleData.csv"),
    file.path(app_dir, "sampleData.csv")
  )
  for (path in candidates) {
    if (length(path) && !is.na(path) && file.exists(path)) {
      return(path)
    }
  }
  NULL
}

read_static_html_body <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  raw_html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  body_html <- sub("(?is).*<body[^>]*>", "", raw_html, perl = TRUE)
  body_html <- sub("(?is)</body>.*$", "", body_html, perl = TRUE)
  body_html <- gsub("(?is)<style[^>]*>.*?</style>", "", body_html, perl = TRUE)
  body_html <- gsub("(?is)<script[^>]*>.*?</script>", "", body_html, perl = TRUE)
  body_html <- gsub("<table(\\s|>)", "<table class=\"table escape-article-table\"\\1", body_html, perl = TRUE)
  body_html
}

sanitize_html <- function(x) {
  if (is.null(x) || length(x) == 0) return(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

compact_upload_signature <- function(path, name = NULL) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  info <- file.info(path)
  file_name <- if (!is.null(name) && nzchar(name)) name else basename(path)
  paste(file_name, info$size, as.numeric(info$mtime), sep = "::")
}

read_csv_frame <- function(path, select = NULL, n_max = NULL) {
  args <- list(
    input = path,
    data.table = FALSE,
    showProgress = FALSE
  )
  if (!is.null(select)) {
    args$select <- select
  }
  if (!is.null(n_max)) {
    args$nrows <- n_max
  }
  out <- do.call(data.table::fread, args)
  as.data.frame(out, stringsAsFactors = FALSE)
}

read_excel_frame <- function(path, selected_cols = NULL, n_max = NULL) {
  args <- list(path)
  if (!is.null(n_max)) {
    args$n_max <- n_max
  }
  out <- do.call(readxl::read_excel, args)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (!is.null(selected_cols)) {
    keep <- intersect(selected_cols, names(out))
    out <- out[, keep, drop = FALSE]
  }
  out
}

read_haven_frame <- function(path, reader, selected_cols = NULL, n_max = NULL) {
  args <- list(path)
  if (!is.null(selected_cols)) {
    args$col_select <- dplyr::all_of(selected_cols)
  }
  if (!is.null(n_max)) {
    args$n_max <- n_max
  }

  out <- tryCatch(
    do.call(reader, args),
    error = function(e) NULL
  )

  if (is.null(out)) {
    fallback_args <- list(path)
    if (!is.null(n_max)) {
      fallback_args$n_max <- n_max
    }
    out <- do.call(reader, fallback_args)
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    if (!is.null(selected_cols)) {
      keep <- intersect(selected_cols, names(out))
      out <- out[, keep, drop = FALSE]
    }
    return(out)
  }

  as.data.frame(out, stringsAsFactors = FALSE)
}

read_tabular_data <- function(path, ext, selected_cols = NULL, n_max = NULL) {
  switch(ext,
    csv = read_csv_frame(path, select = selected_cols, n_max = n_max),
    xlsx = read_excel_frame(path, selected_cols = selected_cols, n_max = n_max),
    sav = read_haven_frame(path, haven::read_sav, selected_cols = selected_cols, n_max = n_max),
    sas7bdat = read_haven_frame(path, haven::read_sas, selected_cols = selected_cols, n_max = n_max),
    NULL
  )
}

normalize_analysis_frame <- function(df, predictor, criterion) {
  if (is.null(df) || !all(c(predictor, criterion) %in% names(df))) {
    return(NULL)
  }

  predictor_vals <- suppressWarnings(as.numeric(df[[predictor]]))
  criterion_vals <- suppressWarnings(as.numeric(df[[criterion]]))
  out <- data.frame(
    Predictor = predictor_vals,
    Criterion = criterion_vals
  )
  out[stats::complete.cases(out), , drop = FALSE]
}

# ============================================
# UI COMPONENTS
# ============================================

landing_cite_format_labels <- c(
  bibtex = "BibTeX",
  apa = "APA 7",
  mla = "MLA 9",
  chicago = "Chicago",
  ris = "RIS",
  vancouver = "Vancouver"
)

landing_cite_fpsyg <- list(
  bibtex = paste0(
    "@article{Zhang2018ShinyAESC,\n",
    "  author  = {Zhang, Don C.},\n",
    "  title   = {Utility of Alternative Effect Size Statistics and the Development of a Web-Based Calculator: {Shiny-AESC}},\n",
    "  journal = {Frontiers in Psychology},\n",
    "  volume  = {9},\n",
    "  pages   = {1221},\n",
    "  year    = {2018},\n",
    "  doi     = {10.3389/fpsyg.2018.01221}\n",
    "}"
  ),
  apa = paste0(
    "Zhang, D. C. (2018). Utility of alternative effect size statistics and the development of ",
    "a web-based calculator: Shiny-AESC. Frontiers in Psychology, 9, Article 1221. ",
    "https://doi.org/10.3389/fpsyg.2018.01221"
  ),
  mla = paste0(
    "Zhang, Don C. \"Utility of Alternative Effect Size Statistics and the Development of a ",
    "Web-Based Calculator: Shiny-AESC.\" Frontiers in Psychology, vol. 9, 2018, ",
    "doi:10.3389/fpsyg.2018.01221."
  ),
  chicago = paste0(
    "Zhang, Don C. 2018. \"Utility of Alternative Effect Size Statistics and the Development ",
    "of a Web-Based Calculator: Shiny-AESC.\" Frontiers in Psychology 9:1221. ",
    "https://doi.org/10.3389/fpsyg.2018.01221."
  ),
  ris = paste0(
    "TY  - JOUR\n",
    "AU  - Zhang, Don C.\n",
    "TI  - Utility of alternative effect size statistics and the development of a web-based ",
    "calculator: Shiny-AESC\n",
    "JO  - Frontiers in Psychology\n",
    "VL  - 9\n",
    "SP  - 1221\n",
    "PY  - 2018\n",
    "DO  - 10.3389/fpsyg.2018.01221\n",
    "ER  - "
  ),
  vancouver = paste0(
    "Zhang DC. Utility of alternative effect size statistics and the development of a web-based ",
    "calculator: Shiny-AESC. Front Psychol. 2018;9:1221. doi:10.3389/fpsyg.2018.01221"
  )
)

landing_cite_ijsa <- list(
  bibtex = paste0(
    "@article{Zhang2018GraphicalInterview,\n",
    "  author  = {Zhang, Don C. and Highhouse, Scott and Brooks, Margaret E. and Zhang, Yu},\n",
    "  title   = {Communicating the Validity of Structured Job Interviews with Graphical Visual Aids},\n",
    "  journal = {International Journal of Selection and Assessment},\n",
    "  volume  = {26},\n",
    "  number  = {2-4},\n",
    "  pages   = {93--108},\n",
    "  year    = {2018},\n",
    "  doi     = {10.1111/ijsa.12220}\n",
    "}"
  ),
  apa = paste0(
    "Zhang, D. C., Highhouse, S., Brooks, M. E., & Zhang, Y. (2018). Communicating the validity ",
    "of structured job interviews with graphical visual aids. International Journal of Selection ",
    "and Assessment, 26(2-4), 93-108. https://doi.org/10.1111/ijsa.12220"
  ),
  mla = paste0(
    "Zhang, Don C., et al. \"Communicating the Validity of Structured Job Interviews with ",
    "Graphical Visual Aids.\" International Journal of Selection and Assessment, vol. 26, ",
    "no. 2-4, 2018, pp. 93-108. doi:10.1111/ijsa.12220."
  ),
  chicago = paste0(
    "Zhang, Don C., Scott Highhouse, Margaret E. Brooks, and Yu Zhang. 2018. \"Communicating ",
    "the Validity of Structured Job Interviews with Graphical Visual Aids.\" International ",
    "Journal of Selection and Assessment 26 (2-4): 93-108. ",
    "https://doi.org/10.1111/ijsa.12220."
  ),
  ris = paste0(
    "TY  - JOUR\n",
    "AU  - Zhang, Don C.\n",
    "AU  - Highhouse, Scott\n",
    "AU  - Brooks, Margaret E.\n",
    "AU  - Zhang, Yu\n",
    "TI  - Communicating the validity of structured job interviews with graphical visual aids\n",
    "JO  - International Journal of Selection and Assessment\n",
    "VL  - 26\n",
    "IS  - 2-4\n",
    "SP  - 93\n",
    "EP  - 108\n",
    "PY  - 2018\n",
    "DO  - 10.1111/ijsa.12220\n",
    "ER  - "
  ),
  vancouver = paste0(
    "Zhang DC, Highhouse S, Brooks ME, Zhang Y. Communicating the validity of structured job ",
    "interviews with graphical visual aids. Int J Sel Assess. 2018;26(2-4):93-108. ",
    "doi:10.1111/ijsa.12220"
  )
)

landing_cite_combined <- function() {
  fmt_names <- names(landing_cite_fpsyg)
  papers <- list(
    list(
      id = "fpsyg",
      label = "Frontiers in Psychology (2018)",
      doi = "https://doi.org/10.3389/fpsyg.2018.01221",
      doi_short = "Frontiers",
      texts = landing_cite_fpsyg
    ),
    list(
      id = "ijsa",
      label = "International Journal of Selection and Assessment (2018)",
      doi = "https://doi.org/10.1111/ijsa.12220",
      doi_short = "IJSA",
      texts = landing_cite_ijsa
    )
  )

  panel_elts <- list()
  for (pj in seq_along(papers)) {
    pd <- papers[[pj]]
    for (i in seq_along(fmt_names)) {
      nm <- fmt_names[[i]]
      is_active <- identical(pd$id, "fpsyg") && i == 1L
      panel_elts[[length(panel_elts) + 1L]] <- tags$pre(
        class = if (is_active) "landing-cite-panel is-active" else "landing-cite-panel",
        `data-paper` = pd$id,
        `data-format` = nm,
        pd$texts[[nm]]
      )
    }
  }

  panel_wrap <- tags$div(class = "landing-cite-panel-wrap")
  for (k in seq_along(panel_elts)) {
    panel_wrap <- htmltools::tagAppendChild(panel_wrap, panel_elts[[k]])
  }

  div(
    class = "landing-cite-card landing-cite-card--combined",
    div(
      class = "landing-cite-combined-top",
      div(
        class = "landing-cite-paper-bar",
        role = "tablist",
        `aria-label` = "Publication",
        lapply(seq_along(papers), function(j) {
          pd <- papers[[j]]
          tags$button(
            type = "button",
            class = if (j == 1L) "landing-cite-paper-btn is-active" else "landing-cite-paper-btn",
            `data-paper` = pd$id,
            role = "tab",
            `aria-selected` = if (j == 1L) "true" else "false",
            pd$label
          )
        })
      ),
      div(
        class = "landing-cite-doi-row",
        lapply(papers, function(pd) {
          tags$a(
            href = pd$doi,
            class = "landing-cite-doi-mini",
            target = "_blank",
            rel = "noopener noreferrer",
            pd$doi_short
          )
        })
      )
    ),
    div(
      class = "landing-cite-format-bar",
      role = "tablist",
      `aria-label` = "Citation format",
      lapply(seq_along(fmt_names), function(i) {
        nm <- fmt_names[[i]]
        lab <- unname(landing_cite_format_labels[nm])
        lab <- if (length(lab) == 1L && !is.na(lab) && nzchar(lab)) as.character(lab) else nm
        tags$button(
          type = "button",
          class = if (i == 1L) "landing-cite-format-btn is-active" else "landing-cite-format-btn",
          `data-format` = nm,
          role = "tab",
          `aria-selected` = if (i == 1L) "true" else "false",
          lab
        )
      })
    ),
    panel_wrap,
    tags$button(
      type = "button",
      class = "landing-cite-copy",
      tags$i(`data-lucide` = "copy", `aria-hidden` = "true"),
      tags$span("Copy")
    )
  )
}

landing_page_ui <- function() {
  div(
    class = "landing-page",
    tags$header(
      class = "landing-nav",
      div(
        class = "landing-nav-inner",
        tags$a(
          class = "landing-nav-brand",
          href = "#landing-get-started",
          `aria-label` = "ESCAPE, go to top",
          "ESCAPE"
        ),
        tags$nav(
          class = "landing-nav-links",
          role = "navigation",
          `aria-label` = "Landing sections",
          tags$a(class = "landing-nav-pill landing-nav-pill--ghost", href = "#landing-about", "About"),
          actionButton("nav_learn", "Learn", class = "landing-nav-pill landing-nav-pill--ghost"),
          actionButton("nav_translator", "Translator", class = "landing-nav-pill landing-nav-pill--ghost"),
          actionButton(
            "nav_get_started",
            "Get Started",
            class = "landing-nav-pill landing-nav-pill--cta"
          )
        )
      )
    ),
    div(
      id = "landing-get-started",
      class = "hero-section hero-section--observatory",
      tags$canvas(class = "hero-particles", id = "hero-particles-canvas", `aria-hidden` = "true"),
      div(
        class = "hero-content",
        p(class = "hero-tagline hero-reveal", "Effect Size Calculator for Practical Effects"),
        h1(class = "hero-headline hero-reveal", "Make your findings impossible to misunderstand."),
        span(class = "hero-brand-accent-line hero-reveal", role = "presentation", `aria-hidden` = "true"),
        p(
          class = "hero-subtitle hero-reveal",
          "ESCAPE converts r, d, and g into probabilities, success rates, and visual summaries ",
          "that HR professionals, managers, and policy makers can understand \u2014 and act on."
        ),
        div(
          class = "hero-actions hero-reveal",
          actionButton("open_guided_upload", "Start Communicating", class = "btn btn-primary btn-hero-cta"),
          actionButton("hero_learn", "Learn", class = "btn btn-outline-hero btn-hero-cta")
        ),
        p(
          class = "hero-trust-signal hero-reveal",
          "No account needed \u00B7 Your data stays in your browser \u00B7 CSV, Excel, SPSS, SAS"
        )
      ),
      div(
        class = "hero-visual hero-reveal hero-reveal--delay",
        div(
          class = "transformation-card transformation-card--tilt",
          div(
            class = "transformation-panel transformation-before",
            span(class = "transformation-label", "BEFORE"),
            div(
              class = "apa-excerpt",
              p(
                class = "apa-text",
                "A Pearson correlation analysis revealed a statistically significant relationship between structured interview scores and first-year job performance, ",
                em("r"),
                "(98) = .42, ",
                em("p"),
                " < .001. The effect size, using Cohen\u2019s ",
                em("d"),
                ", was 0.90."
              ),
              p(
                class = "apa-text apa-text--faded",
                "These results suggest..."
              )
            )
          ),
          div(
            class = "transformation-arrow",
            tags$i(`data-lucide` = "chevron-right")
          ),
          div(
            class = "transformation-panel transformation-after",
            span(class = "transformation-label", "AFTER"),
            div(class = "transformation-stat", "71 out of 100"),
            div(
              class = "transformation-icon-array",
              tagList(lapply(1:71, function(i) {
                div(class = paste0("mini-icon mini-success"), tags$i(`data-lucide` = "user-check"))
              })),
              tagList(lapply(1:29, function(i) {
                div(class = paste0("mini-icon mini-neutral"), tags$i(`data-lucide` = "user"))
              }))
            ),
            span(class = "transformation-caption", "successful hires")
          )
        )
      )
    ),
    div(
      class = "insight-section scroll-section",
      h2(class = "section-title section-title--landing", "The insight"),
      p(
        class = "insight-lead",
        "Reviewers ask for ",
        tags$strong("p-values and effect sizes."),
        " Decision-makers ask ",
        tags$strong("what it means in practice."),
        " The gap between those two sentences is where ESCAPE works."
      ),
      div(
        class = "insight-morph",
        `aria-live` = "polite",
        span(class = "insight-morph-line insight-morph-line--a", "r = 0.42"),
        span(class = "insight-morph-line insight-morph-line--b", "71% success rate")
      )
    ),
    div(
      class = "how-it-works-section",
      h2(class = "section-title section-title--landing", "How it works"),
      div(
        class = "steps-grid steps-grid--plain",
        div(
          class = "step-card",
          span(class = "step-number", "1"),
          div(class = "step-icon-wrap", tags$i(`data-lucide` = "upload")),
          h3(class = "step-title", "Upload"),
          p(class = "step-description", "Drop in your CSV, Excel, SPSS, or SAS file.")
        ),
        div(
          class = "step-card",
          span(class = "step-number", "2"),
          div(class = "step-icon-wrap", tags$i(`data-lucide` = "line-chart")),
          h3(class = "step-title", "Analyze"),
          p(class = "step-description", "Choose predictor, criterion, and metrics.")
        ),
        div(
          class = "step-card step-card--highlight",
          span(class = "step-number", "3"),
          div(class = "step-icon-wrap", tags$i(`data-lucide` = "messages-square")),
          h3(class = "step-title", "Communicate"),
          p(class = "step-description", "Export expectancy charts, icon arrays, and plain language.")
        )
      )
    ),
    div(
      class = "sample-section live-preview-section scroll-section",
      h2(class = "section-title section-title--landing", "Live preview"),
      p(class = "section-subtitle", "Our sample dataset shows vigorous leisure-time activity and self-rated health in U.S. adults (NHIS 2024 public-use data)"),
      div(
        class = "sample-preview",
        div(
          class = "sample-table-container",
          tags$caption(
            style = "text-align: left; padding: var(--space-4); caption-side: top; border-bottom: 1px solid rgba(45, 90, 61, 0.08);",
            div(
              style = "display: flex; align-items: center; gap: var(--space-2); margin-bottom: 4px;",
              tags$div(style = "font-family: var(--font-sans); font-size: 1rem; font-weight: 600; color: var(--forest-900);", "NHIS 2024 Data"),
              tags$span(class = "data-info-badge", style = "margin-bottom: 0; padding: 2px 6px; font-size: 0.75rem;", "n = 1,000")
            ),
            div(
              class = "text-muted", style = "font-size: 0.85em; font-weight: normal; line-height: 1.4;",
              "Vigorous activity (days/week) vs. Self-rated health"
            )
          ),
          tags$table(
            class = "sample-table",
            tags$thead(
              tags$tr(
                tags$th("Vigorous days/week"),
                tags$th("Self-rated health")
              )
            ),
            tags$tbody(
              tags$tr(tags$td("1"), tags$td("2")),
              tags$tr(tags$td("2"), tags$td("4")),
              tags$tr(tags$td("2"), tags$td("4")),
              tags$tr(tags$td("4"), tags$td("4")),
              tags$tr(tags$td("4"), tags$td("5")),
              tags$tr(class = "sample-more", tags$td(colspan = "2", "...and 995 more rows"))
            )
          )
        ),
        div(
          class = "sample-insight",
          tags$i(`data-lucide` = "lightbulb", class = "insight-icon"),
          div(
            tags$strong("Key Insight"),
            p(
              "Adults in the top fifth for vigorous activity days are ",
              tags$strong("about 1.2\u00D7 as likely"),
              " to score above a middling self-rated health threshold as those in the bottom fifth \u2014 see the expectancy chart."
            ),
            div(class = "sample-insight-chart live-preview-chart", plotOutput("landing_expectancy_plot", width = "100%", height = "300px"))
          )
        )
      ),
      actionButton(
        "try_sample_bottom",
        label = tagList(
          tags$i(`data-lucide` = "arrow-right", style = "width: 16px; height: 16px;"),
          "Try this dataset"
        ),
        class = "btn btn-primary btn-sample-cta"
      )
    ),
    div(
      class = "features-section scroll-section",
      h2(class = "section-title section-title--landing", "What you get"),
      div(
        class = "features-grid features-grid--four",
        feature_card_micro(
          "shield-check",
          "Privacy first",
          "All analysis runs in your browser. Your file never leaves your device.",
          "No upload to a server \u2014 full client-side workflow",
          "feature-icon--gold"
        ),
        feature_card_micro(
          "bar-chart-3",
          "Expectancy charts",
          "Turn coefficients into \"out of 100\" style probabilities stakeholders recognize.",
          "Quintile bars with proportions above a cutoff",
          "feature-icon--gold"
        ),
        feature_card_micro(
          "layers",
          "Many effect sizes",
          "Cohen's d, Hedges' g, CLES, BESD, and correlations in one place.",
          "Traditional and practical metrics side by side",
          "feature-icon--gold"
        ),
        feature_card_micro(
          "download",
          "Export reports",
          "Generate HTML you can share with teams who do not read APA prose.",
          "One-click report with charts and explanations",
          "feature-icon--gold"
        )
      )
    ),
    div(
      class = "teaching-section scroll-section",
      div(
        style = "max-width: 1000px; margin: 0 auto; padding: var(--space-12) var(--space-8); text-align: center;",
        h2(class = "section-title section-title--landing", "For Teaching"),
        p(
          style = "font-size: 1.125rem; color: var(--stone-700); line-height: 1.65; margin: 0 auto var(--space-8); max-width: 42rem;",
          "Use ESCAPE in statistics, research methods, and domain courses to help students interpret and communicate findings."
        ),
        div(
          class = "steps-grid--plain",
          div(
            class = "step-card",
            div(class = "step-icon-wrap", tags$i(`data-lucide` = "bar-chart-3")),
            h3(class = "step-title", "Statistics"),
            p(class = "step-description", "Compare traditional vs. practical effect sizes, demonstrate CLES and BESD.")
          ),
          div(
            class = "step-card",
            div(class = "step-icon-wrap", tags$i(`data-lucide` = "book-open")),
            h3(class = "step-title", "Research Methods"),
            p(class = "step-description", "Bridge statistical and practical significance. Write for non-technical audiences.")
          ),
          div(
            class = "step-card",
            div(class = "step-icon-wrap", tags$i(`data-lucide` = "users")),
            h3(class = "step-title", "Domain Courses"),
            p(class = "step-description", "I/O psychology, education, business analytics, and related fields.")
          )
        )
      )
    ),
    div(
      class = "workflow-section scroll-section",
      div(
        style = "max-width: 800px; margin: 0 auto; padding: var(--space-12) var(--space-8); text-align: center;",
        h2(class = "section-title section-title--landing", "In Your Research Workflow"),
        p(
          style = "font-size: 1.125rem; color: var(--stone-700); line-height: 1.65; margin: 0 auto var(--space-8); max-width: 42rem;",
          "ESCAPE fits naturally into your existing analysis pipeline. Use it alongside your standard statistical software."
        ),
        div(
          style = "max-width: 600px; margin: 0 auto; text-align: left;",
          div(
            style = "display: flex; gap: var(--space-4); margin-bottom: var(--space-6); align-items: flex-start;",
            div(
              style = "flex-shrink: 0; width: 32px; height: 32px; border-radius: 50%; background: var(--gold-100); color: var(--forest-600); display: flex; align-items: center; justify-content: center; font-weight: 700; font-family: var(--font-sans);",
              "1"
            ),
            div(
              style = "flex: 1;",
              h3(style = "font-family: var(--font-sans); font-size: var(--text-lg); font-weight: 600; color: var(--forest-900); margin: 0 0 var(--space-2) 0;", "Upload your data"),
              p(
                style = "font-size: var(--text-base); color: var(--stone-700); margin: 0; line-height: 1.6;",
                "CSV, Excel, SPSS, or SAS files. Your data never leaves your browser."
              )
            )
          ),
          div(
            style = "display: flex; gap: var(--space-4); margin-bottom: var(--space-6); align-items: flex-start;",
            div(
              style = "flex-shrink: 0; width: 32px; height: 32px; border-radius: 50%; background: var(--gold-100); color: var(--forest-600); display: flex; align-items: center; justify-content: center; font-weight: 700; font-family: var(--font-sans);",
              "2"
            ),
            div(
              style = "flex: 1;",
              h3(style = "font-family: var(--font-sans); font-size: var(--text-lg); font-weight: 600; color: var(--forest-900); margin: 0 0 var(--space-2) 0;", "Select variables"),
              p(
                style = "font-size: var(--text-base); color: var(--stone-700); margin: 0; line-height: 1.6;",
                "Choose your predictor (X) and criterion (Y) from dropdown menus."
              )
            )
          ),
          div(
            style = "display: flex; gap: var(--space-4); margin-bottom: var(--space-6); align-items: flex-start;",
            div(
              style = "flex-shrink: 0; width: 32px; height: 32px; border-radius: 50%; background: var(--gold-100); color: var(--forest-600); display: flex; align-items: center; justify-content: center; font-weight: 700; font-family: var(--font-sans);",
              "3"
            ),
            div(
              style = "flex: 1;",
              h3(style = "font-family: var(--font-sans); font-size: var(--text-lg); font-weight: 600; color: var(--forest-900); margin: 0 0 var(--space-2) 0;", "Export practical metrics"),
              p(
                style = "font-size: var(--text-base); color: var(--stone-700); margin: 0; line-height: 1.6;",
                "Generate expectancy charts, icon arrays, CLES, BESD, and traditional effect sizes."
              )
            )
          ),
          div(
            style = "display: flex; gap: var(--space-4); align-items: flex-start;",
            div(
              style = "flex-shrink: 0; width: 32px; height: 32px; border-radius: 50%; background: var(--gold-100); color: var(--forest-600); display: flex; align-items: center; justify-content: center; font-weight: 700; font-family: var(--font-sans);",
              "4"
            ),
            div(
              style = "flex: 1;",
              h3(style = "font-family: var(--font-sans); font-size: var(--text-lg); font-weight: 600; color: var(--forest-900); margin: 0 0 var(--space-2) 0;", "Cite in your methods section"),
              p(
                style = "font-size: var(--text-base); color: var(--stone-700); margin: 0; line-height: 1.6;",
                "Practical effect sizes calculated using ESCAPE (Zhang, 2018). See citation formats below."
              )
            )
          )
        )
      )
    ),
    div(
      id = "landing-about",
      class = "creator-bio-section scroll-section",
      h2(class = "section-title section-title--landing", "About the creator"),
      div(
        class = "creator-bio-card",
        div(
          class = "creator-bio-avatar",
          tags$img(
            src = "images/head.jpg",
            alt = "Portrait of Don Zhang",
            class = "creator-bio-avatar-img",
            width = "96",
            height = "96",
            loading = "lazy",
            decoding = "async"
          )
        ),
        div(
          class = "creator-bio-body",
          h3(class = "creator-bio-name", "Don Zhang"),
          p(
            class = "creator-bio-text",
            "Don Zhang, Ph.D. is associate professor of industrial and organizational psychology at Louisiana State University. He is the Director of the I/O psychology PhD program and leads the Risk and Decision Making Lab.",
            "His research focuses on employee selection, decision making, risk taking, and science communication.",
          ),
          div(
            class = "creator-bio-actions",
            tags$a(
              class = "creator-bio-icon-link",
              href = "https://scholar.google.com/citations?user=GTukwAEAAAAJ&hl=en",
              target = "_blank",
              rel = "noopener noreferrer",
              `aria-label` = "Google Scholar profile",
              title = "Google Scholar",
              tags$i(`data-lucide` = "library", `aria-hidden` = "true")
            ),
            tags$a(
              class = "creator-bio-icon-link",
              href = "mailto:zhang1@lsu.edu",
              `aria-label` = "Email zhang1@lsu.edu",
              title = "Email",
              tags$i(`data-lucide` = "mail", `aria-hidden` = "true")
            ),
            tags$a(
              class = "creator-bio-icon-link",
              href = "https://www.lsu.edu/hss/psychology/faculty/industrial/zhang.php",
              target = "_blank",
              rel = "noopener noreferrer",
              `aria-label` = "LSU faculty profile",
              title = "LSU faculty page",
              tags$i(`data-lucide` = "building-2", `aria-hidden` = "true")
            ),
            tags$a(
              class = "creator-bio-icon-link",
              href = "https://randmlab.com",
              target = "_blank",
              rel = "noopener noreferrer",
              `aria-label` = "Risk and Decision Making Lab website",
              title = "R&DM Lab website",
              tags$i(`data-lucide` = "flask-conical", `aria-hidden` = "true")
            ),
            tags$a(
              class = "creator-bio-icon-link",
              href = "https://github.com/dczhang1/ShinyAESC",
              target = "_blank",
              rel = "noopener noreferrer",
              `aria-label` = "GitHub repository",
              title = "GitHub",
              tags$svg(
                xmlns = "http://www.w3.org/2000/svg",
                width = "20", height = "20",
                viewBox = "0 0 24 24",
                fill = "none",
                stroke = "currentColor",
                `stroke-width` = "2",
                `stroke-linecap` = "round",
                `stroke-linejoin` = "round",
                tags$path(d = "M15 22v-4a4.8 4.8 0 0 0-1-3.5c3 0 6-2 6-5.5.08-1.25-.27-2.48-1-3.5.28-1.15.28-2.35 0-3.5 0 0-1 0-3 1.5-2.64-.5-5.36-.5-8 0C6 2 5 2 5 2c-.3 1.15-.3 2.35 0 3.5A5.403 5.403 0 0 0 4 9c0 3.5 3 5.5 6 5.5-.39.49-.68 1.05-.85 1.65-.17.6-.22 1.23-.15 1.85v4"),
                tags$path(d = "M9 18c-4.51 2-5-2-7-2")
              )
            )
          )
        )
      )
    ),
    div(
      class = "landing-citations scroll-section",
      h2(class = "section-title section-title--landing", "How to cite"),
      p(
        class = "landing-citations-intro",
        "If ESCAPE informs your research or practice, pick a format and copy the citation."
      ),
      landing_cite_combined()
    )
  )
}

learn_page_ui <- function() {
  div(
    class = "landing-page utility-page utility-page--learn",
    tags$header(
      class = "landing-nav",
      div(
        class = "landing-nav-inner",
        tags$a(
          class = "landing-nav-brand",
          href = "#",
          `aria-label` = "ESCAPE, go to home",
          onclick = "Shiny.setInputValue('learn_go_home', Date.now(), {priority: 'event'}); return false;",
          "ESCAPE"
        ),
        tags$nav(
          class = "landing-nav-links",
          role = "navigation",
          `aria-label` = "Learn page navigation",
          actionButton("learn_go_home", "Home", class = "landing-nav-pill landing-nav-pill--ghost"),
          actionButton("learn_go_translator", "Translator", class = "landing-nav-pill landing-nav-pill--ghost"),
          actionButton("nav_get_started_learn", "Get Started", class = "landing-nav-pill landing-nav-pill--cta")
        )
      )
    ),
    div(
      class = "utility-content-shell",
      div(
        class = "utility-page-layout utility-page-layout--single",
        div(
          class = "utility-article-card",
          div(
            class = "utility-context-note",
            tags$strong("Scope: "),
            "This page explains why and how practical effect-size translations work. Use the Translator page when you want the quick interactive version."
          ),
          div(class = "escape-article", htmlOutput("learning_tool_content"))
        )
      ),
      div(
        class = "utility-back",
        actionButton("learn_go_home_bottom", "Back to Home", class = "btn btn-outline-hero")
      )
    )
  )
}

translator_page_ui <- function() {
  div(
    class = "landing-page utility-page utility-page--translator",
    tags$header(
      class = "landing-nav",
      div(
        class = "landing-nav-inner",
        tags$a(
          class = "landing-nav-brand",
          href = "#",
          `aria-label` = "ESCAPE, go to home",
          onclick = "Shiny.setInputValue('translator_go_home', Date.now(), {priority: 'event'}); return false;",
          "ESCAPE"
        ),
        tags$nav(
          class = "landing-nav-links",
          role = "navigation",
          `aria-label` = "Translator page navigation",
          actionButton("translator_go_home_nav", "Home", class = "landing-nav-pill landing-nav-pill--ghost"),
          actionButton("translator_go_learn", "Learn", class = "landing-nav-pill landing-nav-pill--ghost"),
          actionButton("nav_get_started_translator", "Get Started", class = "landing-nav-pill landing-nav-pill--cta")
        )
      )
    ),
    div(
      class = "utility-content-shell",
      div(
        class = "translator-layout",
        div(
          class = "translator-frame-card",
          tags$iframe(
            src = "effect-size-translator.html",
            class = "translator-frame",
            title = "Effect Size Translator",
            loading = "eager"
          )
        )
      )
    )
  )
}

# Analysis Interface Component - Redesigned layout: content-driven height, no fill collapse
analysis_ui <- function() {
  tagList(
    div(
      class = "navbar navbar-static-top",
      div(
        class = "container-fluid",
        tags$span(
          class = "d-flex align-items-center gap-2",
          tags$i(`data-lucide` = "bar-chart-2", style = "width: 20px; height: 20px;"),
          "ESCAPE"
        )
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        open = list(desktop = "open", mobile = "always"),
        div(
          class = "sidebar-panel",

          # Panel header
          div(
            class = "sidebar-panel-header",
            div(
              class = "sidebar-panel-title",
              span(class = "sidebar-panel-accent"),
              span("Analysis Controls")
            ),
            span(class = "sidebar-panel-tag", "Setup")
          ),

          # Back to Landing
          actionButton(
            "back_to_landing",
            label = tagList(
              tags$i(`data-lucide` = "arrow-left", style = "width: 14px; height: 14px;"),
              "New Analysis"
            ),
            class = "btn-ghost btn-sm w-100 mb-3"
          ),

          # Data Info
          uiOutput("data_info"),
          div(class = "sidebar-panel-divider"),

          # File Upload Section
          div(
            class = "sidebar-section sidebar-section--data",
            h4(
              class = "sidebar-section-title",
              tags$i(`data-lucide` = "upload", style = "width: 14px; height: 14px;"),
              "Data"
            ),
            fileInput(
              "file1",
              label = NULL,
              accept = c(".csv", ".xlsx", ".sav", ".sas7bdat"),
              placeholder = "Drop file or click",
              buttonLabel = tags$span(
                tags$i(`data-lucide` = "folder-open", style = "width: 14px; height: 14px;")
              )
            ),
            p(
              class = "text-muted small mt-2 mb-0",
              paste0(
                "Max upload: ", max_upload_mb, " MB and ",
                format(max_dataset_rows, big.mark = ","), " rows. CSV is fastest."
              )
            )
          ),

          # Variables Section
          div(
            class = "sidebar-section sidebar-section--vars",
            h4(
              class = "sidebar-section-title",
              tags$i(`data-lucide` = "sliders", style = "width: 14px; height: 14px;"),
              "Variables"
            ),
            selectInput("predictorVar", "Predictor (X)", choices = NULL),
            selectInput("criterionVar", "Criterion (Y)", choices = NULL),
            textInput(
              "predictor_display_name",
              "Predictor display name",
              value = "",
              placeholder = "Uses column name if empty"
            ),
            textInput(
              "criterion_display_name",
              "Criterion display name",
              value = "",
              placeholder = "Uses column name if empty"
            )
          ),

          # Parameters Section
          div(
            class = "sidebar-section sidebar-section--params",
            h4(
              class = "sidebar-section-title",
              tags$i(`data-lucide` = "settings", style = "width: 14px; height: 14px;"),
              "Parameters"
            ),
            numericInput("bins", "Number of bins", value = 5, min = 3, max = 12, step = 1),
            numericInput("cutoffInput", "Criterion cutoff (Y)", value = 3.5, step = 0.1),
            actionButton(
              "setmean",
              label = tags$span(
                tags$i(`data-lucide` = "target", style = "width: 12px; height: 12px; margin-right: 4px;"),
                "Set to mean"
              ),
              class = "btn-secondary btn-sm w-100 mb-3"
            ),
            sliderInput(
              "cutoff.X",
              "Predictor percentile (X)",
              min = 0.01, max = 0.99, value = 0.5, step = 0.01
            )
          ),

          # Export Section
          div(
            class = "sidebar-section sidebar-section--export",
            downloadButton(
              "report",
              label = tags$span(
                tags$i(`data-lucide` = "download", style = "width: 14px; height: 14px;"),
                "Export HTML Report"
              ),
              class = "btn-primary w-100"
            ),
            downloadButton(
              "report_pdf",
              label = tags$span(
                tags$i(`data-lucide` = "file-text", style = "width: 14px; height: 14px;"),
                "Export PDF Report"
              ),
              class = "btn-outline-primary w-100 mt-2"
            )
          )
        )
      ),
      fillable = FALSE,
      border = FALSE,
      border_radius = FALSE,
      tags$main(
        class = "bslib-page-main bslib-gap-spacing html-fill-item html-fill-container",

        # Main Content - Tabbed Navigation or Empty State
        div(
          style = "display: none;",
          textOutput("data_ready", inline = TRUE)
        ),
        conditionalPanel(
          condition = "output.data_ready !== 'yes'",
          card(
            card_body(
              class = "d-flex flex-column align-items-center justify-content-center py-5",
              tags$i(`data-lucide` = "database", style = "width: 48px; height: 48px; color: var(--bs-secondary); margin-bottom: 1rem;"),
              p(class = "mb-0 text-muted", "Load a dataset or use sample data to get started.")
            )
          )
        ),
        conditionalPanel(
          condition = "output.data_ready === 'yes'",
          navset_card_underline(
            # Bookmark values: summary, traditional, practical, help
            id = "main_tabs",

            # ---- Summary (getting started, overview, descriptives, data) ----
            nav_panel(
              title = tags$span(
                tags$i(`data-lucide` = "layout-dashboard", style = "width: 14px; height: 14px; margin-right: 6px;"),
                "Summary"
              ),
              value = "summary",
              div(
                class = "analysis-page analysis-page--nested",
                navset_pill(
                  id = "summary_sections",
                  nav_panel(
                    title = "Getting started",
                    value = "summary_start",
                    div(
                      class = "analysis-subsection",
                      card(
                        card_header("Guided Workflow"),
                        card_body(
                          class = "start-guide-card",
                          div(
                            class = "start-guide",
                            tags$ol(
                              tags$li("Choose Predictor (X) and Criterion (Y) in the sidebar."),
                              tags$li("Optionally set display names so plots and reports use clearer labels than raw column names."),
                              tags$li("Set Criterion cutoff and Predictor percentile."),
                              tags$li("Use Summary (Overview and Descriptives) for a quick read of your data."),
                              tags$li("Use Practical effect sizes for alternative (CLES, BESD) and graphical (expectancy, icon array) presentations."),
                              tags$li("Use Traditional effect sizes for classical indices and group summaries."),
                              tags$li("Use the Translator page from the main menu when you only have a published r or d and need a practical translation."),
                              tags$li("Export report from the sidebar when ready."),
                              tags$li("Open the Help tab for the app guide.")
                            )
                          )
                        )
                      ),
                      card(
                        card_header("Your current analysis question"),
                        card_body(class = "start-brief-card", uiOutput("start_here_blurb"))
                      )
                    )
                  ),
                  nav_panel(
                    title = "Overview",
                    value = "summary_overview",
                    div(
                      class = "analysis-subsection",
                      layout_columns(
                        col_widths = c(4, 4, 4),
                        value_box(
                          title = "Correlation",
                          value = textOutput("overview_r", inline = TRUE),
                          showcase = tags$i(`data-lucide` = "trending-up", style = "width: 32px; height: 32px; color: rgba(255,255,255,0.95);"),
                          theme = "light"
                        ),
                        value_box(
                          title = "Cohen's d",
                          value = textOutput("overview_d", inline = TRUE),
                          showcase = tags$i(`data-lucide` = "layers", style = "width: 32px; height: 32px; color: rgba(255,255,255,0.95);"),
                          theme = "light"
                        ),
                        value_box(
                          title = "CLES",
                          value = textOutput("overview_cles", inline = TRUE),
                          showcase = tags$i(`data-lucide` = "percent", style = "width: 32px; height: 32px; color: rgba(228,199,103,0.95);"),
                          theme = "light"
                        )
                      ),
                      layout_columns(
                        col_widths = c(7, 5),
                        card(
                          card_header(
                            div(
                              class = "d-flex justify-content-between align-items-center w-100",
                              span("Scatterplot"),
                              downloadButton(
                                "download_overview_scatter",
                                label = "Download",
                                class = "btn-ghost btn-sm"
                              )
                            )
                          ),
                          card_body(
                            class = "analysis-plot-wrap",
                            style = "height: 340px;",
                            plotOutput("overview_scatter", width = "100%", height = "340px")
                          )
                        ),
                        card(
                          card_header("Key Insight"),
                          card_body(class = "analysis-insight-wrap", uiOutput("overview_insight"))
                        )
                      )
                    )
                  ),
                  nav_panel(
                    title = "Descriptives",
                    value = "summary_descriptives",
                    div(
                      class = "analysis-subsection",
                      card(
                        card_header("Descriptive Statistics & Correlation"),
                        card_body(
                          class = "card-body-scroll",
                          tableOutput("descriptable"),
                          div(
                            class = "correlation-line mt-3 pt-3",
                            tags$span(class = "correlation-r", textOutput("validity_r", inline = TRUE)),
                            tags$span(class = "text-muted ms-2", textOutput("validity_r2", inline = TRUE))
                          )
                        )
                      ),
                      layout_columns(
                        col_widths = c(6, 6),
                        card(
                          card_header(
                            div(
                              class = "d-flex justify-content-between align-items-center w-100",
                              span(textOutput("hist_x_title", inline = TRUE)),
                              downloadButton(
                                "download_histogram_x",
                                label = "Download",
                                class = "btn-ghost btn-sm"
                              )
                            )
                          ),
                          card_body(
                            class = "analysis-plot-wrap",
                            style = "height: 340px;",
                            plotOutput("histogram.X", width = "100%", height = "340px")
                          )
                        ),
                        card(
                          card_header(
                            div(
                              class = "d-flex justify-content-between align-items-center w-100",
                              span(textOutput("hist_y_title", inline = TRUE)),
                              downloadButton(
                                "download_histogram_y",
                                label = "Download",
                                class = "btn-ghost btn-sm"
                              )
                            )
                          ),
                          card_body(
                            class = "analysis-plot-wrap",
                            style = "height: 340px;",
                            plotOutput("histogram.Y", width = "100%", height = "340px")
                          )
                        )
                      )
                    )
                  ),
                  nav_panel(
                    title = "Data",
                    value = "summary_data",
                    div(
                      class = "analysis-subsection",
                      card(
                        card_header("Raw Data"),
                        card_body(style = "overflow-x: auto;", DTOutput("contents"))
                      )
                    )
                  )
                )
              )
            ),

            # ---- Traditional effect sizes ----
            nav_panel(
              title = tags$span(
                tags$i(`data-lucide` = "layers", style = "width: 14px; height: 14px; margin-right: 6px;"),
                "Traditional effect sizes"
              ),
              value = "traditional",
              div(
                class = "analysis-page",
                div(
                  class = "analysis-subsection",
                  layout_columns(
                    col_widths = c(6, 6),
                    card(
                      card_header("Effect Size Indices"),
                      card_body(tableOutput("cles"))
                    ),
                    card(
                      card_header("Group Statistics"),
                      card_body(tableOutput("clestable"))
                    )
                  )
                )
              )
            ),

            # ---- Practical effect sizes ----
            nav_panel(
              title = tags$span(
                tags$i(`data-lucide` = "bar-chart-3", style = "width: 14px; height: 14px; margin-right: 6px;"),
                "Practical effect sizes"
              ),
              value = "practical",
              div(
                class = "analysis-page analysis-page--nested",
                navset_pill(
                  id = "practical_sections",
                  nav_panel(
                    title = "Alternative effect sizes",
                    value = "practical_alternative",
                    div(
                      class = "analysis-subsection",
                      tags$p(
                        class = "text-muted small mb-3",
                        "CLES and BESD translate the effect into probabilities and success rates rather than only standardized mean differences."
                      ),
                      card(
                        card_header(
                          div(
                            class = "d-flex justify-content-between align-items-center w-100",
                            span("Common Language Effect Size (CLES)"),
                            downloadButton(
                              "download_cles_plot",
                              label = "Download",
                              class = "btn-ghost btn-sm"
                            )
                          )
                        ),
                        card_body(
                          class = "card-body-scroll",
                          div(
                            class = "cles-verbal-callout",
                            textOutput("cles.verbal")
                          ),
                          div(
                            class = "analysis-plot-wrap",
                            style = "height: 380px;",
                            plotOutput("histogram.overlap", width = "100%", height = "380px")
                          )
                        )
                      ),
                      card(
                        card_header("Binomial Effect Size Display (BESD)"),
                        card_body(
                          tags$h6("Empirical (cutoff-based)"),
                          tableOutput("besd"),
                          tags$hr(),
                          tags$h6("Theoretical (from r)"),
                          tableOutput("besd_theoretical")
                        )
                      )
                    )
                  ),
                  nav_panel(
                    title = "Graphical effect sizes",
                    value = "practical_graphical",
                    div(
                      class = "analysis-subsection",
                      tags$p(
                        class = "text-muted small mb-3",
                        "Expectancy charts and icon arrays show the same ideas in visual form for communication and intuition."
                      ),
                      card(
                        card_header(
                          div(
                            class = "d-flex justify-content-between align-items-center w-100",
                            span("Expectancy Chart"),
                            downloadButton(
                              "download_expectancy_plot",
                              label = "Download",
                              class = "btn-ghost btn-sm"
                            )
                          )
                        ),
                        card_body(
                          class = "analysis-plot-wrap",
                          style = "height: 420px;",
                          plotOutput("expectancyPlot", width = "100%", height = "420px")
                        )
                      ),
                      card(
                        card_header("Expectancy Table"),
                        card_body(class = "card-body-scroll", tableOutput("expectancyTable"))
                      ),
                      card(
                        card_header(
                          div(
                            class = "d-flex justify-content-between align-items-center w-100",
                            span("Icon Array Visualization"),
                            downloadButton(
                              "download_icon_array",
                              label = "Download",
                              class = "btn-ghost btn-sm"
                            )
                          )
                        ),
                        card_body(
                          class = "card-body-scroll",
                          div(
                            class = "d-flex gap-3 mb-3",
                            style = "flex-wrap: wrap;",
                            div(
                              class = "flex-grow-1",
                              style = "min-width: 150px;",
                              selectInput(
                                "iconarray_type",
                                "Icon type",
                                choices = c("Person" = "person", "Circle" = "circle", "Square" = "square"),
                                selected = "person"
                              )
                            ),
                            div(
                              class = "flex-grow-1",
                              style = "min-width: 120px;",
                              numericInput(
                                "iconarray_total",
                                "Total icons",
                                value = 100,
                                min = 10,
                                max = 500,
                                step = 10
                              )
                            ),
                            div(
                              class = "flex-grow-1",
                              style = "min-width: 120px;",
                              selectInput(
                                "iconarray_layout",
                                "Layout",
                                choices = c("Auto" = "auto", "10x10" = "10x10", "20x20" = "20x20", "25x25" = "25x25"),
                                selected = "auto"
                              )
                            )
                          ),
                          tags$p(class = "mb-3", textOutput("iconarray_description")),
                          div(
                            class = "analysis-plot-wrap",
                            style = "height: 420px;",
                            plotOutput("iconarray_plot", width = "100%", height = "420px")
                          )
                        )
                      ),
                      card(
                        card_header("Icon Array Settings"),
                        card_body(
                          div(
                            class = "iconarray-legend",
                            tags$div(
                              class = "legend-item",
                              tags$span(class = "legend-color", style = paste0("background-color: ", plot_colors$primary)),
                              tags$span("Success (CLES)")
                            ),
                            tags$div(
                              class = "legend-item",
                              tags$span(class = "legend-color", style = paste0("background-color: ", plot_colors$secondary)),
                              tags$span("Failure (1 - CLES)")
                            )
                          ),
                          div(
                            class = "iconarray-stats mt-3",
                            tags$p(class = "mb-1", textOutput("iconarray_stats"))
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),

            # ---- Help (app guide + future interpretation / educational content) ----
            nav_panel(
              title = tags$span(
                tags$i(`data-lucide` = "help-circle", style = "width: 14px; height: 14px; margin-right: 6px;"),
                "Help"
              ),
              value = "help",
              div(
                class = "analysis-page analysis-page--nested",
                div(
                  class = "analysis-subsection analysis-page--help",
                  card(
                    card_body(class = "help-body", htmlOutput("instructions"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# ============================================
# MAIN UI
# ============================================

ui <- page_fillable(
  theme = app_theme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/landing.css"),
    tags$script(src = "https://unpkg.com/lucide@1.8.0/dist/umd/lucide.js", integrity = "sha384-bgSnYl8X5v7B1Jo4mdsVIh2rFFjtrqNJnu7TILKXhwh/dr1HvKK2D1ApUikQ6NCb", crossorigin = "anonymous"),
    tags$script(src = "js/main.js")
  ),
  tags$a(
    class = "skip-link",
    href = "#main-content",
    "Skip to main content"
  ),
  tags$main(
    id = "main-content",
    tabindex = "-1",
    uiOutput("main_ui")
  )
)

# ============================================
# SERVER LOGIC
# ============================================

server <- function(input, output, session) {
  # --- App State ---
  app_state <- reactiveValues(
    view = "landing",
    data_source = NULL, # "sample" or "uploaded"
    landing_file = NULL, # preserved file from landing upload (lost when DOM switches)
    guided_done = FALSE,
    guided_step = 1,
    guided_entry_from_landing = FALSE,
    guided_wizard_completed_once = FALSE
  )
  guided_file_meta <- reactiveVal(NULL)

  # --- Navigation ---

  observeEvent(input$try_sample_bottom, {
    app_state$view <- "analysis"
    app_state$data_source <- "sample"
    app_state$guided_entry_from_landing <- FALSE
  })

  open_guided_from_landing <- function() {
    app_state$landing_file <- NULL
    guided_file_meta(NULL)
    app_state$view <- "analysis"
    app_state$data_source <- "uploaded"
    app_state$guided_step <- 1
    if (!isTRUE(app_state$guided_wizard_completed_once)) {
      app_state$guided_done <- FALSE
      app_state$guided_entry_from_landing <- TRUE
    } else {
      app_state$guided_done <- TRUE
      app_state$guided_entry_from_landing <- FALSE
    }
  }

  observeEvent(input$open_guided_upload, {
    open_guided_from_landing()
  })

  observeEvent(input$nav_get_started, {
    open_guided_from_landing()
  })

  observeEvent(input$nav_learn, {
    app_state$view <- "learn"
  })

  observeEvent(input$hero_learn, {
    app_state$view <- "learn"
  })

  observeEvent(input$nav_translator, {
    app_state$view <- "translator"
  })

  observeEvent(input$learn_go_home, {
    app_state$view <- "landing"
  })

  observeEvent(input$learn_go_home_bottom, {
    app_state$view <- "landing"
  })

  observeEvent(input$nav_get_started_learn, {
    open_guided_from_landing()
  })

  observeEvent(input$learn_go_translator, {
    app_state$view <- "translator"
  })

  observeEvent(input$translator_go_home, {
    app_state$view <- "landing"
  })

  observeEvent(input$translator_go_home_nav, {
    app_state$view <- "landing"
  })

  observeEvent(input$translator_go_learn, {
    app_state$view <- "learn"
  })

  observeEvent(input$nav_get_started_translator, {
    open_guided_from_landing()
  })

  # Upload file from guided modal step 1
  observeEvent(input$guided_file, {
    req(input$guided_file)
    guided_file_meta(input$guided_file)
    app_state$landing_file <- input$guided_file
    app_state$data_source <- "uploaded"
    session$sendCustomMessage("initIcons", list())
  })

  # Sidebar upload should override preserved landing upload
  observeEvent(input$file1, {
    req(input$file1)
    app_state$landing_file <- NULL
    guided_file_meta(NULL)
    app_state$data_source <- "uploaded"
    app_state$guided_entry_from_landing <- FALSE
    if (!isTRUE(app_state$guided_done)) {
      app_state$guided_step <- 1
    }
  })

  # Back to landing
  observeEvent(input$back_to_landing, {
    app_state$view <- "landing"
    app_state$data_source <- NULL
    app_state$landing_file <- NULL
    guided_file_meta(NULL)
  })

  # Render main UI based on state
  output$main_ui <- renderUI({
    if (app_state$view == "landing") {
      landing_page_ui()
    } else if (app_state$view == "learn") {
      learn_page_ui()
    } else if (app_state$view == "translator") {
      translator_page_ui()
    } else {
      analysis_ui()
    }
  })

  # Re-initialize Lucide icons once after UI switch
  observe({
    session$sendCustomMessage("initIcons", list())
  }) |> bindEvent(app_state$view, ignoreInit = TRUE)

  # --- Data Loading ---
  last_load_signature <- reactiveVal(NULL)
  last_load_error <- reactiveVal(NULL)
  last_load_warning <- reactiveVal(NULL)

  current_input_file <- reactive({
    if (!is.null(app_state$data_source) && app_state$data_source == "uploaded" && !is.null(app_state$landing_file)) {
      return(app_state$landing_file)
    }
    if (!is.null(input$file1)) {
      return(input$file1)
    }
    NULL
  })

  data_profile <- reactive({
    if (app_state$view == "analysis" && identical(app_state$data_source, "sample")) {
      sample_path <- get_sample_data_path()
      if (is.null(sample_path)) {
        return(NULL)
      }
      raw <- tryCatch(read_csv_frame(sample_path), error = function(e) NULL)
      if (is.null(raw)) {
        return(NULL)
      }
      last_load_error(NULL)
      last_load_warning(NULL)
      last_load_signature(compact_upload_signature(sample_path, "sampleData.csv"))
      return(list(
        mode = "sample",
        path = sample_path,
        ext = "csv",
        name = "sampleData.csv",
        nrow = nrow(raw),
        ncol = ncol(raw),
        column_names = names(raw)
      ))
    }

    inFile <- current_input_file()
    if (is.null(inFile)) {
      return(NULL)
    }

    ext <- tolower(tools::file_ext(inFile$name))
    if (!ext %in% c("csv", "xlsx", "sav", "sas7bdat")) {
      last_load_error("Unsupported file type. Please upload CSV, Excel, SPSS, or SAS files.")
      last_load_warning(NULL)
      return(NULL)
    }

    tryCatch(
      {
        preview <- read_tabular_data(inFile$datapath, ext, n_max = 200)
        if (is.null(preview)) {
          last_load_error("Could not parse the file. Verify the file contents match the extension.")
          return(NULL)
        }

        row_count <- if (ext == "csv" && ncol(preview) > 0) {
          nrow(read_csv_frame(inFile$datapath, select = names(preview)[1]))
        } else {
          nrow(read_tabular_data(inFile$datapath, ext))
        }

        if (!is.na(row_count) && row_count > max_dataset_rows) {
          last_load_error(
            paste0(
              "This dataset has ", format(row_count, big.mark = ","),
              " rows. ESCAPE currently accepts up to ",
              format(max_dataset_rows, big.mark = ","), " rows."
            )
          )
          last_load_warning(NULL)
          return(NULL)
        }

        last_load_error(NULL)
        if (ext %in% c("xlsx", "sav", "sas7bdat")) {
          last_load_warning("Excel, SPSS, and SAS files are slower to parse than CSV. CSV is recommended for the fastest analysis.")
        } else {
          last_load_warning(NULL)
        }
        last_load_signature(compact_upload_signature(inFile$datapath, inFile$name))

        list(
          mode = "uploaded",
          path = inFile$datapath,
          ext = ext,
          name = inFile$name,
          nrow = row_count,
          ncol = ncol(preview),
          column_names = names(preview)
        )
      },
      error = function(e) {
        last_load_error("Error loading file. Please check that the file is valid and try again.")
        last_load_warning(NULL)
        NULL
      }
    )
  })

  data_set <- reactive({
    profile <- data_profile()
    if (is.null(profile)) {
      return(NULL)
    }

    tryCatch(
      {
        if (identical(profile$mode, "sample")) {
          return(read_csv_frame(profile$path))
        }
        read_tabular_data(profile$path, profile$ext, n_max = 1000)
      },
      error = function(e) NULL
    )
  })

  observeEvent(last_load_signature(), ignoreNULL = TRUE, {
    profile <- data_profile()
    req(profile)
    msg <- if (!is.null(profile$nrow) && !is.na(profile$nrow)) {
      paste("Loaded", profile$nrow, "rows from your file")
    } else {
      paste("Loaded", profile$name)
    }
    session$sendCustomMessage("showToast", list(
      message = msg,
      type = "success",
      duration = 3000
    ))
  })

  observeEvent(last_load_error(), ignoreNULL = TRUE, {
    session$sendCustomMessage("showToast", list(
      message = last_load_error(),
      type = "error",
      duration = 5000
    ))
  })

  observeEvent(last_load_warning(), ignoreNULL = TRUE, {
    session$sendCustomMessage("showToast", list(
      message = last_load_warning(),
      type = "warning",
      duration = 4500
    ))
  })

  guided_step2_state <- reactive({
    is_numeric_like <- function(x) {
      if (is.numeric(x) || is.integer(x)) {
        return(TRUE)
      }
      # Accept columns that are safely coercible to numeric (e.g., labelled types)
      suppressWarnings({
        x_num <- as.numeric(x)
      })
      !all(is.na(x_num)) && all(is.na(x) == is.na(x_num))
    }
    column_type_label <- function(x) {
      paste(class(x), collapse = "/")
    }

    df <- data_set()
    profile <- data_profile()
    if (is.null(profile) || profile$ncol < 2) {
      return(list(
        ready = FALSE,
        message = "Your dataset needs at least 2 columns.",
        predictor = NULL,
        criterion = NULL
      ))
    }
    dsnames <- profile$column_names
    pred <- input$guided_predictorVar
    crit <- input$guided_criterionVar
    if (is.null(pred) || !pred %in% dsnames) pred <- dsnames[1]
    if (is.null(crit) || !crit %in% dsnames) crit <- dsnames[min(2, length(dsnames))]
    if (identical(pred, crit) && length(dsnames) >= 2) {
      crit <- dsnames[ifelse(match(pred, dsnames) == 1, 2, 1)]
    }

    if (identical(pred, crit)) {
      return(list(
        ready = FALSE,
        message = "Please choose different variables for X and Y.",
        predictor = pred,
        criterion = crit
      ))
    }

    analysis_preview <- tryCatch(
      {
        if (identical(profile$mode, "sample")) {
          df[, c(pred, crit), drop = FALSE]
        } else {
          read_tabular_data(profile$path, profile$ext, selected_cols = c(pred, crit), n_max = 500)
        }
      },
      error = function(e) NULL
    )
    if (is.null(analysis_preview)) {
      return(list(
        ready = FALSE,
        message = "Unable to inspect the selected columns. Please choose different variables.",
        predictor = pred,
        criterion = crit
      ))
    }

    pred_is_numeric <- is_numeric_like(analysis_preview[[pred]])
    crit_is_numeric <- is_numeric_like(analysis_preview[[crit]])
    if (!pred_is_numeric || !crit_is_numeric) {
      bad_vars <- character(0)
      if (!pred_is_numeric) {
        bad_vars <- c(
          bad_vars,
          paste0("Predictor (X) `", pred, "` is type ", column_type_label(analysis_preview[[pred]]), ".")
        )
      }
      if (!crit_is_numeric) {
        bad_vars <- c(
          bad_vars,
          paste0("Criterion (Y) `", crit, "` is type ", column_type_label(analysis_preview[[crit]]), ".")
        )
      }
      return(list(
        ready = FALSE,
        message = paste(
          c(
            "X and Y must both be numeric columns.",
            bad_vars,
            "Please choose numeric variables."
          ),
          collapse = " "
        ),
        predictor = pred,
        criterion = crit
      ))
    }

    list(
      ready = TRUE,
      message = NULL,
      predictor = pred,
      criterion = crit
    )
  })

  output$guided_step2_message <- renderUI({
    step2_state <- guided_step2_state()
    if (is.null(step2_state$message)) {
      return(NULL)
    }
    tags$p(class = "text-danger mb-0", step2_state$message)
  })

  output$guided_step1_info <- renderUI({
    req(isTRUE(app_state$guided_step == 1), !isTRUE(app_state$guided_done))
    file_meta <- guided_file_meta()
    has_file <- !is.null(file_meta)
    profile <- data_profile()
    has_data <- !is.null(profile)
    current_file <- if (!is.null(file_meta)) {
      file_meta$name
    } else if (!is.null(app_state$landing_file)) {
      app_state$landing_file$name
    } else if (!is.null(input$file1)) {
      input$file1$name
    } else {
      "No file selected"
    }
    data_stats_detail <- if (has_data) {
      tagList(
        tags$span(class = "data-info-dot", "\u2022"),
        tags$span(paste(profile$nrow, "rows")),
        tags$span(class = "data-info-dot", "\u2022"),
        tags$span(paste(profile$ncol, "columns"))
      )
    } else if (has_file) {
      tagList(
        tags$span(class = "data-info-dot", "\u2022"),
        tags$span("File selected")
      )
    } else {
      NULL
    }
    tagList(
      tags$div(
        class = "data-info-card mb-3",
        div(class = "data-info-badge", "Your Data"),
        div(
          class = "data-info-stats",
          tags$span(current_file),
          data_stats_detail
        )
      ),
      if (!has_file) {
        tags$p(class = "text-danger mb-0", "Waiting for file to load. Please upload a valid file to continue.")
      } else if (!has_data) {
        tags$p(class = "text-muted mb-0", "File selected. Continue to choose variables in Step 2.")
      } else {
        tags$p(class = "text-muted mb-0", "Dataset loaded. Continue to choose Predictor (X) and Criterion (Y).")
      }
    )
  })

  output$guided_wizard_footer_step1 <- renderUI({
    req(isTRUE(app_state$guided_step == 1), !isTRUE(app_state$guided_done))
    file_meta <- guided_file_meta()
    step1_ready <- !is.null(file_meta) || !is.null(app_state$landing_file)
    tagList(
      actionButton("guided_skip", "Skip guide", class = "btn-ghost"),
      if (step1_ready) {
        actionButton("guided_next", "Continue", class = "btn-primary")
      } else {
        tags$button(type = "button", class = "btn btn-primary", disabled = "disabled", "Continue")
      }
    )
  })

  show_guided_wizard <- function() {
    profile <- data_profile()
    has_data <- !is.null(profile)
    has_two_cols <- has_data && profile$ncol >= 2
    dsnames <- if (has_data) profile$column_names else character(0)

    step <- app_state$guided_step

    step_body <- if (step == 1) {
      div(
        class = "guided-wizard-body",
        div(
          class = "guided-wizard-progress",
          tags$span(class = "guided-wizard-progress__step guided-wizard-progress__step--active", "1"),
          tags$span(class = "guided-wizard-progress__divider"),
          tags$span(class = "guided-wizard-progress__step", "2")
        ),
        tags$p(class = "mb-3 guided-wizard-title", "Step 1 of 2: Confirm your uploaded dataset."),
        fileInput(
          "guided_file",
          label = "Upload dataset",
          accept = c(".csv", ".xlsx", ".sav", ".sas7bdat"),
          placeholder = "Drop file here or click to browse",
          buttonLabel = tags$span(
            tags$i(`data-lucide` = "folder-open", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Choose file"
          )
        ),
        tags$p(
          class = "text-muted small mb-3",
          paste0(
            "Max upload: ", max_upload_mb, " MB and ",
            format(max_dataset_rows, big.mark = ","), " rows. CSV is recommended for the fastest analysis."
          )
        ),
        uiOutput("guided_step1_info")
      )
    } else {
      step2_state <- guided_step2_state()
      pred_selected <- step2_state$predictor
      crit_selected <- step2_state$criterion

      div(
        class = "guided-wizard-body",
        div(
          class = "guided-wizard-progress",
          tags$span(class = "guided-wizard-progress__step", "1"),
          tags$span(class = "guided-wizard-progress__divider"),
          tags$span(class = "guided-wizard-progress__step guided-wizard-progress__step--active", "2")
        ),
        tags$p(class = "mb-3 guided-wizard-title", "Step 2 of 2: Choose your analysis variables."),
        if (!has_two_cols) {
          tags$p(class = "text-danger mb-0", "Your dataset needs at least 2 columns to select X and Y variables.")
        } else {
          tagList(
            selectInput("guided_predictorVar", "Predictor (X)", choices = dsnames, selected = pred_selected),
            selectInput("guided_criterionVar", "Criterion (Y)", choices = dsnames, selected = crit_selected),
            uiOutput("guided_step2_message"),
            tags$p(class = "text-muted mb-0", "These choices will be applied to the full dashboard.")
          )
        }
      )
    }

    footer <- if (step == 1) {
      uiOutput("guided_wizard_footer_step1")
    } else {
      tagList(
        actionButton("guided_back", "Back", class = "btn-ghost"),
        actionButton("guided_finish", "Open Dashboard", class = "btn-primary")
      )
    }

    showModal(
      modalDialog(
        class = "guided-startup-modal",
        title = "Guided startup",
        step_body,
        footer = footer,
        easyClose = FALSE
      )
    )
    session$sendCustomMessage("focusGuidedWizard", list(step = step))
  }

  observeEvent(list(app_state$view, app_state$data_source),
    {
      if (identical(app_state$view, "analysis") &&
        identical(app_state$data_source, "uploaded") &&
        !isTRUE(app_state$guided_done) &&
        isTRUE(app_state$guided_entry_from_landing) &&
        !isTRUE(app_state$guided_wizard_completed_once)) {
        show_guided_wizard()
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(input$guided_skip, {
    app_state$guided_wizard_completed_once <- TRUE
    app_state$guided_done <- TRUE
    app_state$guided_entry_from_landing <- FALSE
    removeModal()
  })

  observeEvent(input$guided_next, {
    app_state$guided_step <- 2
    show_guided_wizard()
  })

  observeEvent(input$guided_back, {
    app_state$guided_step <- 1
    show_guided_wizard()
  })

  observeEvent(input$guided_finish, {
    step2_state <- guided_step2_state()
    if (!isTRUE(step2_state$ready)) {
      msg <- if (!is.null(step2_state$message)) step2_state$message else "Please choose valid X and Y variables."
      showNotification(msg, type = "warning")
      show_guided_wizard()
      return(NULL)
    }
    updateSelectInput(session, "predictorVar", selected = step2_state$predictor)
    updateSelectInput(session, "criterionVar", selected = step2_state$criterion)
    app_state$guided_wizard_completed_once <- TRUE
    app_state$guided_done <- TRUE
    app_state$guided_entry_from_landing <- FALSE
    removeModal()
  })

  # Hidden output for empty-state conditionalPanel (only in analysis view)
  output$data_ready <- renderText({
    if (app_state$view != "analysis") {
      return("no")
    }
    if (is.null(data_profile())) {
      return("no")
    }
    "yes"
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
  outputOptions(output, "guided_step1_info", suspendWhenHidden = FALSE)
  outputOptions(output, "guided_wizard_footer_step1", suspendWhenHidden = FALSE)

  # Sample data for landing page "See it in action" expectancy preview
  landing_preview_data <- reactive({
    if (app_state$view != "landing") {
      return(NULL)
    }
    path <- get_sample_data_path()
    if (is.null(path)) {
      return(NULL)
    }
    raw <- tryCatch(read_csv_frame(path), error = function(e) NULL)
    if (is.null(raw) || ncol(raw) < 2) {
      return(NULL)
    }
    df <- raw %>%
      dplyr::select(Predictor = 1, Criterion = 2) %>%
      na.omit()
    list(df = df, predictor_name = "Vigorous activity (days/week)", criterion_name = "Self-rated health")
  })

  # Expectancy chart in landing Key Insight (sample data, fixed bins=5, cutoff=3.0)
  output$landing_expectancy_plot <- renderPlot({
    preview <- landing_preview_data()
    if (is.null(preview) || nrow(preview$df) < 2) {
      return(invisible(NULL))
    }
    exp_df <- calc_expectancy(preview$df, bins = 5, cutoff_y = 3.0)
    plot_expectancy_landing(exp_df, preview$predictor_name, 3.0)
  })

  # Data info display
  output$data_info <- renderUI({
    profile <- data_profile()
    if (is.null(profile)) {
      return(NULL)
    }

    source_text <- if (!is.null(app_state$data_source) && app_state$data_source == "sample") {
      "Sample Data"
    } else {
      "Your Data"
    }

    div(
      class = "data-info-card",
      div(class = "data-info-badge", source_text),
      div(
        class = "data-info-stats",
        tags$span(paste(profile$nrow, "rows")),
        tags$span(class = "data-info-dot", "\u2022"),
        tags$span(paste(profile$ncol, "columns"))
      )
    )
  })

  effective_vars <- reactive({
    profile <- data_profile()
    if (is.null(profile) || profile$ncol < 2) {
      return(list(predictor = NULL, criterion = NULL))
    }
    dsnames <- profile$column_names
    pred <- input$predictorVar
    crit <- input$criterionVar
    if (is.null(pred) || !pred %in% dsnames) pred <- dsnames[1]
    if (is.null(crit) || !crit %in% dsnames) crit <- dsnames[min(2, length(dsnames))]
    list(predictor = pred, criterion = crit)
  })

  # Keep sidebar selectors in sync with effective vars
  observe({
    ev <- effective_vars()
    profile <- data_profile()
    if (is.null(profile) || is.null(ev$predictor)) {
      return(NULL)
    }
    dsnames <- profile$column_names
    if (!identical(input$predictorVar, ev$predictor)) {
      updateSelectInput(session, "predictorVar", choices = dsnames, selected = ev$predictor)
    }
    if (!identical(input$criterionVar, ev$criterion)) {
      updateSelectInput(session, "criterionVar", choices = dsnames, selected = ev$criterion)
    }
  })

  observeEvent(input$predictorVar,
    {
      updateTextInput(session, "predictor_display_name", value = "")
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(input$criterionVar,
    {
      updateTextInput(session, "criterion_display_name", value = "")
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  predictor_display_name <- reactive({
    ev <- effective_vars()
    req(ev$predictor)
    v <- input$predictor_display_name
    if (is.null(v) || !nzchar(trimws(v))) ev$predictor else trimws(v)
  })

  criterion_display_name <- reactive({
    ev <- effective_vars()
    req(ev$criterion)
    v <- input$criterion_display_name
    if (is.null(v) || !nzchar(trimws(v))) ev$criterion else trimws(v)
  })

  analysis_data <- reactive({
    profile <- data_profile()
    req(profile)
    ev <- effective_vars()
    req(ev$predictor, ev$criterion)
    if (identical(profile$mode, "sample")) {
      return(normalize_analysis_frame(read_csv_frame(profile$path), ev$predictor, ev$criterion))
    }

    if (identical(profile$ext, "csv")) {
      return(normalize_analysis_frame(
        read_csv_frame(profile$path, select = unique(c(ev$predictor, ev$criterion))),
        ev$predictor,
        ev$criterion
      ))
    }

    normalize_analysis_frame(
      read_tabular_data(profile$path, profile$ext),
      ev$predictor,
      ev$criterion
    )
  })

  analysis_signature <- reactive({
    ev <- effective_vars()
    paste(
      last_load_signature(),
      ev$predictor,
      ev$criterion,
      sep = "::"
    )
  })

  cutoff_x_input <- reactive(input$cutoff.X) |> debounce(250)
  cutoff_y_input <- reactive(input$cutoffInput) |> debounce(250)
  bins_input <- reactive(input$bins) |> debounce(250)

  validity_stats <- reactive({
    df <- analysis_data()
    calc_correlation(df$Predictor, df$Criterion)
  })

  cutoff_X_val <- reactive({
    df <- analysis_data()
    quantile(df$Predictor, probs = cutoff_x_input(), na.rm = TRUE)
  })

  df_exp <- reactive({
    df <- analysis_data()
    calc_expectancy(df, bins_input(), cutoff_y_input())
  })

  group_slices <- reactive({
    df <- analysis_data()
    cutoff_x <- cutoff_X_val()
    above_idx <- df$Predictor >= cutoff_x
    below_idx <- df$Predictor < cutoff_x
    above_vals <- df$Criterion[above_idx]
    below_vals <- df$Criterion[below_idx]
    list(
      above_vals = above_vals,
      below_vals = below_vals,
      n_above = length(above_vals),
      n_below = length(below_vals)
    )
  })

  group_summary <- reactive({
    slices <- group_slices()
    data.frame(
      Group = c("Above", "Below"),
      Mean = c(mean(slices$above_vals), mean(slices$below_vals)),
      SD = c(sd(slices$above_vals), sd(slices$below_vals)),
      n = c(slices$n_above, slices$n_below)
    )
  })

  density_data <- reactive({
    slices <- group_slices()
    data.frame(
      Criterion = c(slices$above_vals, slices$below_vals),
      Group = c(
        rep("Above", length(slices$above_vals)),
        rep("Below", length(slices$below_vals))
      )
    )
  })

  effect_sizes <- reactive({
    slices <- group_slices()
    above_vals <- slices$above_vals
    below_vals <- slices$below_vals

    if (length(above_vals) < 2 || length(below_vals) < 2) {
      return(list(d = NA, cles = NA))
    }

    list(
      d = calc_cohens_d(above_vals, below_vals),
      cles = calc_cles(above_vals, below_vals)
    )
  })

  # Set mean button
  observeEvent(input$setmean, {
    df <- analysis_data()
    mean_val <- round(mean(df$Criterion, na.rm = TRUE), 2)
    updateNumericInput(session, "cutoffInput", value = mean_val)
  })

  # Default criterion cutoff to mean when criterion variable changes
  last_criterion_var <- reactiveVal(NULL)
  observeEvent(effective_vars(), {
    ev <- effective_vars()
    if (is.null(ev$criterion)) {
      return(NULL)
    }
    if (identical(ev$criterion, last_criterion_var())) {
      return(NULL)
    }
    last_criterion_var(ev$criterion)
    sd <- analysis_data()
    if (!is.null(sd) && nrow(sd) > 0) {
      updateNumericInput(session, "cutoffInput", value = round(mean(sd$Criterion, na.rm = TRUE), 2))
    }
  })

  # --- Overview Tab Outputs ---

  output$overview_r <- renderText({
    r <- validity_stats()$r
    paste0("r = ", round(r, 3))
  })

  output$overview_d <- renderText({
    d <- effect_sizes()$d
    if (is.na(d)) {
      return("--")
    }
    direction <- if (d >= 0) "positive" else "negative"
    paste0("|d| = ", round(abs(d), 2), " (", direction, ")")
  })

  output$overview_cles <- renderText({
    cles <- effect_sizes()$cles
    if (is.na(cles)) {
      return("--")
    }
    paste0(round(cles * 100, 1), "%")
  })

  output$overview_scatter <- renderPlot(
    {
      p <- plot_scatter(analysis_data(), predictor_display_name(), criterion_display_name())
      print(p)
    },
    width = 560,
    height = 340,
    res = 96
  ) |> bindCache(
    analysis_signature(),
    predictor_display_name(),
    criterion_display_name()
  )

  output$overview_insight <- renderUI({
    pd <- predictor_display_name()
    cd <- criterion_display_name()
    r <- validity_stats()$r
    cles <- effect_sizes()$cles
    d <- effect_sizes()$d
    slices <- group_slices()
    n_above <- slices$n_above
    n_below <- slices$n_below
    d_ci <- calc_d_ci(d, n_above, n_below)
    r_ci <- calc_r_ci(r, nrow(analysis_data()))

    r_strength <- interpret_correlation(r)
    d_magnitude <- if (!is.na(d)) interpret_cohens_d(d) else "unknown"
    d_direction <- if (!is.na(d) && d >= 0) "higher" else "lower"

    cles_content <- if (!is.na(cles)) {
      list(
        "A randomly selected person from the top ",
        round(cutoff_x_input() * 100), "% on ",
        pd, " has a ",
        tags$strong(paste0(round(cles * 100, 1), "%")),
        " probability of scoring higher on ",
        cd, " than someone from the bottom ",
        round((1 - cutoff_x_input()) * 100), "%."
      )
    } else {
      list("Adjust the cutoff percentile to see CLES insights.")
    }

    div(
      class = "insight-content",
      div(
        class = "insight-row",
        tags$i(`data-lucide` = "info", class = "insight-icon-small"),
        p(paste0(r_strength, " (r = ", round(r, 3), ") with ", d_magnitude, " effect size."))
      ),
      div(
        class = "insight-row",
        tags$i(`data-lucide` = "activity", class = "insight-icon-small"),
        p(
          paste0(
            "Direction: higher predictor group tends to score ", d_direction, " on ", cd,
            ". Uncertainty ranges: r 95% CI [", round(r_ci$lower, 2), ", ", round(r_ci$upper, 2),
            "], d 95% CI [", round(d_ci$lower, 2), ", ", round(d_ci$upper, 2), "]."
          )
        )
      ),
      div(
        class = "insight-row highlight",
        tags$i(`data-lucide` = "lightbulb", class = "insight-icon-small"),
        p(cles_content)
      )
    )
  })

  # --- Data Tab Outputs ---

  output$contents <- renderDT({
    df <- data_set()
    req(df)
    datatable(
      utils::head(df, 1000),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = "frtip",
        language = list(search = "", searchPlaceholder = "Search...")
      ),
      class = "display compact"
    )
  }, server = TRUE)

  # --- Expectancy Tab Outputs ---

  output$expectancyPlot <- renderPlot(
    {
      p <- plot_expectancy(
        df_exp(),
        predictor_display_name(),
        cutoff_y_input(),
        criterion_display_name()
      )
      print(p)
    },
    width = 720,
    height = 420,
    res = 96
  ) |> bindCache(
    analysis_signature(),
    bins_input(),
    cutoff_y_input(),
    predictor_display_name(),
    criterion_display_name()
  )

  output$expectancyTable <- renderTable(
    {
      criterion_col <- paste0("Proportion (", criterion_display_name(), " > ", round(cutoff_y_input(), 3), ")")
      count_col <- paste0("Count (", criterion_display_name(), " > ", round(cutoff_y_input(), 3), ")")

      df_exp() %>%
        transmute(
          Bin = ntile_X,
          Range = xlabels,
          !!criterion_col := proportion,
          !!count_col := as.integer(round(frequency)),
          Total = as.integer(round(n))
        )
    },
    digits = 3
  )

  # --- Statistics Tab Outputs ---

  output$descriptable <- renderTable(
    {
      analysis_data() %>%
        psych::describe() %>%
        as.data.frame() %>%
        select(n, mean, sd, median, min, max, skew, kurtosis)
    },
    rownames = TRUE,
    digits = 3
  )

  output$validity_r <- renderText({
    paste0("r = ", validity_stats()$r_formatted)
  })

  output$validity_r2 <- renderText({
    paste0("(", validity_stats()$r2_pct, "% variance explained)")
  })

  output$hist_x_title <- renderText({
    paste("Distribution of", predictor_display_name())
  })

  output$hist_y_title <- renderText({
    paste("Distribution of", criterion_display_name())
  })

  output$histogram.X <- renderPlot(
    {
      p <- plot_histogram(analysis_data()$Predictor, fill_color = plot_colors$primary)
      print(p)
    },
    width = 560,
    height = 340,
    res = 96
  ) |> bindCache(analysis_signature(), "predictor_hist")

  output$histogram.Y <- renderPlot(
    {
      p <- plot_histogram(analysis_data()$Criterion, fill_color = plot_colors$success)
      print(p)
    },
    width = 560,
    height = 340,
    res = 96
  ) |> bindCache(analysis_signature(), "criterion_hist")

  output$corplot <- renderPlot(
    {
      p <- plot_scatter(analysis_data(), predictor_display_name(), criterion_display_name())
      print(p)
    },
    width = 720,
    height = 480,
    res = 96
  ) |> bindCache(
    analysis_signature(),
    predictor_display_name(),
    criterion_display_name()
  )

  # --- Effect Sizes Tab Outputs ---

  output$clestable <- renderTable(
    {
      group_summary()
    },
    digits = 3
  )

  output$cles <- renderTable({
    stats <- group_summary()

    if (nrow(stats) < 2) {
      return(NULL)
    }

    slices <- group_slices()
    above_vals <- slices$above_vals
    below_vals <- slices$below_vals

    if (length(above_vals) < 2 || length(below_vals) < 2) {
      return(NULL)
    }

    calc_all_effect_sizes(above_vals, below_vals, validity_stats()$r)
  })

  output$cles.verbal <- renderText({
    slices <- group_slices()
    above_vals <- slices$above_vals
    below_vals <- slices$below_vals

    if (length(above_vals) < 2 || length(below_vals) < 2) {
      return("Insufficient data for CLES calculation.")
    }

    cles_val <- calc_cles(above_vals, below_vals)
    cles_verbal(predictor_display_name(), criterion_display_name(), cutoff_X_val(), round(cles_val * 100, 1))
  })

  output$histogram.overlap <- renderPlot(
    {
      p <- plot_density_overlap(density_data(), criterion_display_name(), predictor_display_name(), cutoff_X_val())
      print(p)
    },
    width = 680,
    height = 380,
    res = 96
  ) |> bindCache(
    analysis_signature(),
    criterion_display_name(),
    predictor_display_name(),
    cutoff_X_val()
  )

  output$besd <- renderTable(
    {
      df <- analysis_data()
      calc_besd(df, cutoff_X_val(), cutoff_y_input(), predictor_display_name(), criterion_display_name())
    },
    rownames = TRUE,
    digits = 3
  )

  output$besd_theoretical <- renderTable(
    {
      calc_besd_theoretical(validity_stats()$r, predictor_display_name(), criterion_display_name())
    },
    rownames = TRUE,
    digits = 3
  )

  output$start_here_blurb <- renderUI({
    ev <- effective_vars()
    if (is.null(ev$predictor) || is.null(ev$criterion)) {
      return(tags$p("Select variables to begin."))
    }
    tags$div(
      tags$p(tags$strong("Primary question: "), paste0("How much does ", predictor_display_name(), " improve practical outcomes on ", criterion_display_name(), "?")),
      tags$p(
        tags$strong("Recommended path: "),
        "Summary (Overview and Descriptives) → Traditional effect sizes (indices and group stats) → Practical effect sizes (alternative CLES/BESD; graphical expectancy and icon array). Use the Translator page from the main menu when you only have published r or d. Use Help for the app guide and interpretation resources."
      )
    )
  })

  # --- Figure Downloads ---

  sanitize_filename <- function(x) {
    x <- gsub("[^A-Za-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    ifelse(nzchar(x), x, "figure")
  }

  output$download_overview_scatter <- downloadHandler(
    filename = function() {
      paste0(
        "ESCAPE_Scatter_",
        sanitize_filename(predictor_display_name()),
        "_vs_",
        sanitize_filename(criterion_display_name()),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      p <- plot_scatter(analysis_data(), predictor_display_name(), criterion_display_name())
      ggplot2::ggsave(
        filename = file,
        plot = p,
        width = 7,
        height = 5,
        dpi = 300,
        bg = "white"
      )
    }
  )

  output$download_histogram_x <- downloadHandler(
    filename = function() {
      paste0(
        "ESCAPE_Histogram_",
        sanitize_filename(predictor_display_name()),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      p <- plot_histogram(
        analysis_data()$Predictor,
        title = paste("Distribution of", predictor_display_name()),
        fill_color = plot_colors$primary
      )
      ggplot2::ggsave(
        filename = file,
        plot = p,
        width = 7,
        height = 5,
        dpi = 300,
        bg = "white"
      )
    }
  )

  output$download_histogram_y <- downloadHandler(
    filename = function() {
      paste0(
        "ESCAPE_Histogram_",
        sanitize_filename(criterion_display_name()),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      p <- plot_histogram(
        analysis_data()$Criterion,
        title = paste("Distribution of", criterion_display_name()),
        fill_color = plot_colors$success
      )
      ggplot2::ggsave(
        filename = file,
        plot = p,
        width = 7,
        height = 5,
        dpi = 300,
        bg = "white"
      )
    }
  )

  output$download_expectancy_plot <- downloadHandler(
    filename = function() {
      paste0(
        "ESCAPE_Expectancy_",
        sanitize_filename(predictor_display_name()),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      p <- plot_expectancy(
        df_exp(),
        predictor_display_name(),
        input$cutoffInput,
        criterion_display_name()
      )
      ggplot2::ggsave(
        filename = file,
        plot = p,
        width = 7,
        height = 5,
        dpi = 300,
        bg = "white"
      )
    }
  )

  output$download_cles_plot <- downloadHandler(
    filename = function() {
      paste0(
        "ESCAPE_CLES_Overlap_",
        sanitize_filename(predictor_display_name()),
        "_",
        sanitize_filename(criterion_display_name()),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      p <- plot_density_overlap(density_data(), criterion_display_name(), predictor_display_name(), cutoff_X_val())
      ggplot2::ggsave(
        filename = file,
        plot = p,
        width = 7,
        height = 5,
        dpi = 300,
        bg = "white"
      )
    }
  )

  # --- Icon Array Tab Outputs ---

  output$iconarray_description <- renderText({
    ev <- effective_vars()
    cles_val <- effect_sizes()$cles

    if (is.na(cles_val)) {
      return("Adjust the cutoff percentile to see CLES icon array.")
    }

    paste0(
      "Out of ", input$iconarray_total, " random pairs, approximately ",
      round(cles_val * input$iconarray_total), " times the high-scoring person will ",
      "outperform the low-scoring person."
    )
  })

  output$iconarray_plot <- renderPlot(
    {
      cles_val <- effect_sizes()$cles

      if (is.na(cles_val)) {
        # Return empty plot if no data
        ggplot() +
          labs(title = "No data available") +
          theme_minimal_linear()
      } else {
        plot_icon_array(
          cles_prob = cles_val,
          total_icons = input$iconarray_total,
          icon_type = input$iconarray_type,
          layout = input$iconarray_layout,
          predictor_name = predictor_display_name(),
          criterion_name = criterion_display_name()
        )
      }
    },
    width = 720,
    height = 420,
    res = 96
  )

  output$iconarray_stats <- renderText({
    cles_val <- effect_sizes()$cles

    if (is.na(cles_val)) {
      return("CLES: Not available")
    }

    num_success <- round(cles_val * input$iconarray_total)
    num_failure <- input$iconarray_total - num_success

    paste0(
      "Success: ", num_success, " (", round(cles_val * 100, 1), "%) | ",
      "Failure: ", num_failure, " (", round((1 - cles_val) * 100, 1), "%)"
    )
  })

  output$download_icon_array <- downloadHandler(
    filename = function() {
      paste0(
        "ESCAPE_IconArray_",
        sanitize_filename(predictor_display_name()),
        "_",
        sanitize_filename(criterion_display_name()),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      cles_val <- effect_sizes()$cles

      if (!is.na(cles_val)) {
        p <- plot_icon_array(
          cles_prob = cles_val,
          total_icons = input$iconarray_total,
          icon_type = input$iconarray_type,
          layout = input$iconarray_layout,
          predictor_name = predictor_display_name(),
          criterion_name = criterion_display_name()
        )
        ggplot2::ggsave(
          filename = file,
          plot = p,
          width = 7,
          height = 5,
          dpi = 300,
          bg = "white"
        )
      }
    }
  )

  # --- Help Content ---

  output$instructions <- renderUI({
    if (file.exists("docs/shinyInstructions.html")) {
      raw_html <- paste(readLines("docs/shinyInstructions.html", warn = FALSE), collapse = "\n")
      body_html <- sub("(?is).*<body[^>]*>", "", raw_html, perl = TRUE)
      body_html <- sub("(?is)</body>.*$", "", body_html, perl = TRUE)
      body_html <- gsub("(?is)<style[^>]*>.*?</style>", "", body_html, perl = TRUE)
  body_html <- gsub("(?is)<script[^>]*>.*?</script>", "", body_html, perl = TRUE)
  body_html <- gsub("\\s+on\\w+\\s*=\\s*\"[^\"]*\"", "", body_html, perl = TRUE)
  body_html <- gsub("\\s+on\\w+\\s*=\\s*'[^']*'", "", body_html, perl = TRUE)
      HTML(sprintf('<div class="help-content">%s</div>', body_html))
    } else {
      div(
        class = "help-content",
        h3("Getting Started"),
        tags$ol(
          tags$li("Upload your data file (CSV, Excel, SPSS, or SAS format)"),
          tags$li("Select your predictor (X) and criterion (Y) variables"),
          tags$li("Adjust the cutoff percentile to define your comparison groups"),
          tags$li("Explore the tabs for different analyses")
        ),
        h3("Understanding Effect Sizes"),
        tags$dl(
          tags$dt("Cohen's d"),
          tags$dd("Standardized difference between two group means. Small: 0.2, Medium: 0.5, Large: 0.8"),
          tags$dt("CLES"),
          tags$dd("The probability that a randomly selected person from one group will score higher than someone from the other group."),
          tags$dt("BESD"),
          tags$dd("Binomial Effect Size Display - shows success rates for each group in a 2x2 table.")
        ),
        h3("Attribution"),
        tags$p(
          tags$strong("ESCAPE"),
          " (Effect Size Calculator for Practical Effects) is developed and maintained by ",
          tags$strong("Don Zhang"),
          ". The application is built with ",
          tags$a(href = "https://shiny.posit.co/", target = "_blank", rel = "noopener noreferrer", "R Shiny"),
          ". Public deployment: ",
          tags$a(
            href = "https://dczhang.shinyapps.io/shinyescape/",
            target = "_blank",
            rel = "noopener noreferrer",
            "https://dczhang.shinyapps.io/shinyescape/"
          ),
          "."
        )
      )
    }
  })

  output$learning_tool_content <- renderUI({
    body_html <- read_static_html_body("www/learning-tool.html")

    if (is.null(body_html)) {
      return(
        div(
          class = "help-content",
          h3("Learning tool unavailable"),
          p("The source file `www/learning-tool.html` could not be loaded.")
        )
      )
    }

    HTML(body_html)
  })

  # --- Report Generation ---

  assemble_report_params <- function() {
    req(analysis_data())
    pd <- predictor_display_name()
    cd <- criterion_display_name()
    pd_safe <- sanitize_html(pd)
    cd_safe <- sanitize_html(cd)
    df <- analysis_data()
    slices <- group_slices()
    above_vals <- slices$above_vals
    below_vals <- slices$below_vals
    corr <- validity_stats()$r
    corr_abs <- abs(corr)
    effect_band <- if (corr_abs < 0.10) "very small" else if (corr_abs < 0.30) "small" else if (corr_abs < 0.50) "moderate" else "large"
    direction <- if (corr >= 0) "higher" else "lower"
    practical_message <- paste0(
      "The predictor has a ", effect_band, " relationship with the criterion (r = ",
      sprintf("%.3f", corr),
      "). In practical terms, higher predictor values are associated with ",
      direction,
      " criterion outcomes."
    )
    cles_val <- calc_cles(above_vals, below_vals)
    cles_narrative <- cles_verbal(pd_safe, cd_safe, cutoff_X_val(), round(cles_val * 100, 1))
    desc_tbl <- psych::describe(df) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Variable") %>%
      dplyr::mutate(Variable = dplyr::case_when(
        Variable == "Predictor" ~ pd,
        Variable == "Criterion" ~ cd,
        TRUE ~ Variable
      )) %>%
      dplyr::select(Variable, n, mean, sd, median, min, max)
    exp_tbl <- df_exp() %>%
      dplyr::transmute(
        ntile_X,
        xlabels,
        proportion,
        frequency,
        n
      )
    besd_emp <- calc_besd(df, cutoff_X_val(), cutoff_y_input(), pd, cd) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Group")
    besd_theory <- calc_besd_theoretical(corr, pd, cd) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Group")
    icon_plot_obj <- plot_icon_array(
      cles_prob = cles_val,
      total_icons = 100,
      icon_type = "person",
      layout = "auto",
      predictor_name = pd,
      criterion_name = cd
    )
    list(
      predictor = pd_safe,
      criterion = cd_safe,
      n_obs = nrow(df),
      bins = input$bins,
      cutoff_y = cutoff_y_input(),
      cutoff_x_pct = round(cutoff_x_input() * 100),
      r = corr,
      r2 = corr^2,
      practical_message = practical_message,
      cles_narrative = cles_narrative,
      desc_stats = desc_tbl,
      effect_sizes = calc_all_effect_sizes(above_vals, below_vals, corr),
      exp_table = exp_tbl,
      besd_empirical = besd_emp,
      besd_theoretical = besd_theory,
      scatter_plot = plot_scatter(df, pd, cd),
      exp_plot = plot_expectancy(df_exp(), pd, cutoff_y_input(), cd),
      icon_plot = icon_plot_obj
    )
  }

  output$report <- downloadHandler(
    filename = function() {
      paste0("ESCAPE_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(analysis_data())
      tempReport <- file.path(tempdir(), "report.Rmd")

      rmd_content <- '
---
title: "ESCAPE Professional Analysis Report"
date: "`r Sys.Date()`"
params:
  predictor: ""
  criterion: ""
  n_obs: 0
  bins: 10
  cutoff_y: 0
  cutoff_x_pct: 0
  r: 0
  r2: 0
  practical_message: ""
  cles_narrative: ""
  desc_stats: NULL
  effect_sizes: NULL
  exp_table: NULL
  besd_empirical: NULL
  besd_theoretical: NULL
  scatter_plot: NULL
  exp_plot: NULL
  icon_plot: NULL
output:
  html_document:
    theme: cosmo
    self_contained: true
    toc: true
    toc_depth: 2
    number_sections: false
    df_print: paged
    code_folding: hide
    fig_caption: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

<style>
@import url("https://fonts.googleapis.com/css2?family=Fira+Code:wght@500;600&family=Fira+Sans:wght@400;500;600;700&display=swap");

:root {
  --report-bg: #f8fafc;
  --report-surface: #ffffff;
  --report-primary: #1e40af;
  --report-secondary: #3b82f6;
  --report-accent: #f59e0b;
  --report-border: #dbe5f3;
  --report-text: #102a43;
  --report-muted: #486581;
}

body {
  font-family: "Fira Sans", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  font-size: 16px;
  line-height: 1.6;
  color: var(--report-text);
  background: var(--report-bg);
}

h1, h2, h3 {
  color: var(--report-primary);
  letter-spacing: -0.01em;
}

#header,
.fluid-row#header {
  margin-bottom: 1.25rem;
}

#header h1.title,
h1.title,
h1.title.toc-ignore,
.title-block h1,
.fluid-row#header h1 {
  font-size: 2.75rem !important;
  line-height: 1.12 !important;
  font-weight: 700 !important;
  letter-spacing: -0.03em !important;
  margin-bottom: 0.35rem !important;
  color: var(--report-primary) !important;
}

#header .date,
h4.date {
  font-size: 0.95rem !important;
  color: var(--report-muted) !important;
  font-weight: 500 !important;
}

h2 {
  font-size: 1.22rem;
  border-bottom: 2px solid var(--report-border);
  padding-bottom: 0.35rem;
  margin-top: 1.6rem;
}

h3 {
  font-size: 1.05rem;
  margin-top: 1.1rem;
}

p, li {
  color: var(--report-text);
}

.summary-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(160px, 1fr));
  gap: 0.75rem;
  margin: 0.75rem 0 1rem 0;
}

.summary-card {
  background: var(--report-surface);
  border: 1px solid var(--report-border);
  border-radius: 12px;
  padding: 0.75rem 0.85rem;
  box-shadow: 0 8px 20px rgba(30, 64, 175, 0.07);
}

.summary-label {
  color: var(--report-muted);
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  margin-bottom: 0.2rem;
}

.summary-value {
  color: var(--report-primary);
  font-family: "Fira Code", ui-monospace, monospace;
  font-weight: 600;
  font-size: 1rem;
}

.report-callout {
  background: linear-gradient(120deg, rgba(30, 64, 175, 0.08), rgba(59, 130, 246, 0.06));
  border-left: 4px solid var(--report-secondary);
  border-radius: 10px;
  padding: 0.8rem 0.9rem;
  margin: 0.9rem 0 1rem 0;
}

table {
  background: var(--report-surface);
  border: 1px solid var(--report-border);
  border-radius: 10px;
  overflow: hidden;
}

thead th {
  background: #eef4ff;
  color: var(--report-primary);
  font-weight: 700;
}

tbody tr:nth-child(even) {
  background: #f9fbff;
}

.tocify {
  border-radius: 10px;
  border: 1px solid var(--report-border);
}
</style>

## Executive Summary

```{r results="asis"}
cards <- c(
  sprintf("<div class=\\\"summary-card\\\"><div class=\\\"summary-label\\\">Predictor</div><div class=\\\"summary-value\\\">%s</div></div>", params$predictor),
  sprintf("<div class=\\\"summary-card\\\"><div class=\\\"summary-label\\\">Criterion</div><div class=\\\"summary-value\\\">%s</div></div>", params$criterion),
  sprintf("<div class=\\\"summary-card\\\"><div class=\\\"summary-label\\\">Sample Size</div><div class=\\\"summary-value\\\">%s</div></div>", params$n_obs),
  sprintf("<div class=\\\"summary-card\\\"><div class=\\\"summary-label\\\">Top-group cutoff</div><div class=\\\"summary-value\\\">%s%%</div></div>", params$cutoff_x_pct),
  sprintf("<div class=\\\"summary-card\\\"><div class=\\\"summary-label\\\">Criterion threshold</div><div class=\\\"summary-value\\\">%s</div></div>", params$cutoff_y),
  sprintf("<div class=\\\"summary-card\\\"><div class=\\\"summary-label\\\">Variance explained</div><div class=\\\"summary-value\\\">%.1f%%</div></div>", params$r2 * 100)
)
cat(paste0("<div class=\\\"summary-grid\\\">", paste(cards, collapse = ""), "</div>"))
```

```{r results="asis"}
cat(sprintf("<div class=\\\"report-callout\\\"><strong>Practical interpretation.</strong> %s</div>", params$practical_message))
cat(sprintf("<div class=\\\"report-callout\\\"><strong>CLES interpretation.</strong> %s</div>", params$cles_narrative))
```

## Core Effect Size Findings

```{r}
core_tbl <- data.frame(
  Metric = c("Pearsons r", "Variance explained (R^2)", "Criterion cutoff", "Expectancy bins"),
  Value = c(
    sprintf("%.3f", params$r),
    sprintf("%.1f%%", params$r2 * 100),
    as.character(params$cutoff_y),
    as.character(params$bins)
  ),
  stringsAsFactors = FALSE
)
knitr::kable(core_tbl, align = c("l", "r"))
```

```{r}
knitr::kable(params$effect_sizes, align = c("l", "r"))
```

## Descriptive Statistics

```{r}
knitr::kable(params$desc_stats, align = c("l", "r", "r", "r", "r", "r", "r"), digits = 3)
```

### Scatterplot

```{r fig.width=9, fig.height=5}
params$scatter_plot
```

### Expectancy Chart

```{r fig.width=9, fig.height=4.5}
params$exp_plot
```

## Expectancy Table

```{r}
exp_tbl <- params$exp_table
if (!is.null(exp_tbl) && nrow(exp_tbl) > 0) {
  exp_tbl$proportion <- sprintf("%.1f%%", exp_tbl$proportion * 100)
  knitr::kable(exp_tbl, align = c("r", "l", "r", "r", "r"), col.names = c("Bin", "Predictor range", "Success rate", "Successes", "N"))
}
```

## Icon Array (Practical Probability)

```{r fig.width=9, fig.height=4.8}
params$icon_plot
```

## BESD (Observed Data)

```{r}
knitr::kable(params$besd_empirical, align = c("l", "r", "r"), digits = 3)
```

## BESD (Theoretical From r)

```{r}
knitr::kable(params$besd_theoretical, align = c("l", "r", "r"), digits = 3)
```

---

<small>
<strong>Generated by ESCAPE</strong> (Effect Size Calculator for Practical Effects).
<strong>Author:</strong> Don Zhang.
<strong>Implementation:</strong> R Shiny.
<strong>App URL:</strong> <a href="https://dczhang.shinyapps.io/shinyescape/">https://dczhang.shinyapps.io/shinyescape/</a>
</small>
'
      writeLines(rmd_content, tempReport)
      on.exit(unlink(tempReport), add = TRUE)

      p <- assemble_report_params()

      rmarkdown::render(
        tempReport,
        output_file = file,
        params = p,
        envir = new.env(parent = globalenv())
      )
    }
  )

  output$report_pdf <- downloadHandler(
    filename = function() {
      paste0("ESCAPE_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      p <- assemble_report_params()
      tempReport <- file.path(tempdir(), "report_pdf.Rmd")

      rmd_pdf <- '
---
title: "ESCAPE Professional Analysis Report"
date: "`r Sys.Date()`"
params:
  predictor: ""
  criterion: ""
  n_obs: 0
  bins: 10
  cutoff_y: 0
  cutoff_x_pct: 0
  r: 0
  r2: 0
  practical_message: ""
  cles_narrative: ""
  desc_stats: NULL
  effect_sizes: NULL
  exp_table: NULL
  besd_empirical: NULL
  besd_theoretical: NULL
  scatter_plot: NULL
  exp_plot: NULL
  icon_plot: NULL
output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: false
geometry: margin=1in
fontsize: 11pt
documentclass: article
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 4.25,
  dpi = 150,
  dev = "png",
  fig.ext = "png"
)
```

## Executive Summary

```{r}
kpi <- data.frame(
  Metric = c("Predictor", "Criterion", "Sample size", "Top-group cutoff (percent)", "Criterion threshold", "Variance explained (percent)"),
  Value = c(
    as.character(params$predictor),
    as.character(params$criterion),
    as.character(params$n_obs),
    as.character(params$cutoff_x_pct),
    as.character(params$cutoff_y),
    sprintf("%.1f", params$r2 * 100)
  ),
  stringsAsFactors = FALSE
)
knitr::kable(kpi, booktabs = TRUE)
```

```{r results="asis"}
cat("Practical interpretation. ", params$practical_message, "\n\n", "CLES interpretation. ", params$cles_narrative, sep = "")
```

## Core Effect Size Findings

```{r}
core_tbl <- data.frame(
  Metric = c("Pearsons r", "Variance explained (R^2)", "Criterion cutoff", "Expectancy bins"),
  Value = c(
    sprintf("%.3f", params$r),
    sprintf("%.1f%%", params$r2 * 100),
    as.character(params$cutoff_y),
    as.character(params$bins)
  ),
  stringsAsFactors = FALSE
)
knitr::kable(core_tbl, booktabs = TRUE, align = c("l", "r"))
```

```{r}
knitr::kable(params$effect_sizes, booktabs = TRUE, align = c("l", "r"))
```

## Descriptive Statistics

```{r}
knitr::kable(params$desc_stats, booktabs = TRUE, align = c("l", "r", "r", "r", "r", "r", "r"), digits = 3)
```

### Scatterplot

```{r fig.width=7, fig.height=4.5}
params$scatter_plot
```

### Expectancy Chart

```{r fig.width=7, fig.height=4}
params$exp_plot
```

## Expectancy Table

```{r}
exp_tbl <- params$exp_table
if (!is.null(exp_tbl) && nrow(exp_tbl) > 0) {
  exp_tbl$proportion <- sprintf("%.1f%%", exp_tbl$proportion * 100)
  knitr::kable(exp_tbl, booktabs = TRUE, align = c("r", "l", "r", "r", "r"), col.names = c("Bin", "Predictor range", "Success rate", "Successes", "N"))
}
```

## Icon Array (Practical Probability)

```{r fig.width=7, fig.height=4.5}
params$icon_plot
```

## BESD (Observed Data)

```{r}
knitr::kable(params$besd_empirical, booktabs = TRUE, align = c("l", "r", "r"), digits = 3)
```

## BESD (Theoretical From r)

```{r}
knitr::kable(params$besd_theoretical, booktabs = TRUE, align = c("l", "r", "r"), digits = 3)
```

*Generated by ESCAPE (Effect Size Calculator for Practical Effects). Author: Don Zhang. Implementation: R Shiny. App URL: https://dczhang.shinyapps.io/shinyescape/*
'
      writeLines(rmd_pdf, tempReport)
      on.exit(unlink(tempReport), add = TRUE)

      err <- tryCatch(
        {
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = p,
            output_format = rmarkdown::pdf_document(
              toc = TRUE,
              toc_depth = 2,
              number_sections = FALSE
            ),
            envir = new.env(parent = globalenv())
          )
          NULL
        },
        error = function(e) e
      )

      if (!is.null(err)) {
        session$sendCustomMessage("showToast", list(
          message = "PDF export failed. Try exporting as HTML instead, or ensure a LaTeX engine is installed.",
          type = "error"
        ))
        stop("PDF export failed")
      }
    }
  )
}

# ============================================
# RUN APP
# ============================================

shinyApp(ui = ui, server = server)
