# ESCAPE - Effect Size Calculator for Practical Effects
# Modernized version with Linear-inspired design
# Features: Landing page, modern UI, comprehensive effect size analysis

# Load packages
library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(haven)
library(psych)
library(DT)

# Source utility modules
source("R/utils_theme.R")
source("R/utils_stats.R")
source("R/utils_plots.R")

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
    if (length(path) && !is.na(path) && file.exists(path)) return(path)
  }
  NULL
}

# ============================================
# UI COMPONENTS
# ============================================

# Landing Page Component
landing_page_ui <- function() {
  div(
    class = "landing-page",

    # ---- Hero Section ----
    div(
      class = "hero-section",
      div(
        class = "hero-content",
        p(class = "hero-tagline", "Effect Size Calculator for Practical Effects"),
        div(
          class = "app-name-badge",
          tags$strong("ESCAPE")
        ),
        h1(class = "hero-title", "Stop reporting correlations. Start communicating results."),
        p(
          class = "hero-subtitle",
          "Effect sizes satisfy reviewers. They rarely move anyone else. ",
          "ESCAPE converts r, d, and g into probabilities, success rates, and visual summaries ",
          "that hiring managers, clinicians, and policy teams can actually understand \u2014 and act on."
        ),
        div(
          class = "hero-actions",
          actionButton("try_sample", "Try with Sample Data", class = "btn-primary"),
          actionButton("open_guided_upload", "Upload Your Data", class = "btn-outline-primary")
        ),
        p(class = "hero-trust-signal",
          "No account needed \u00B7 Your data never leaves your browser \u00B7 Works with CSV, Excel, SPSS, SAS"
        )
      ),

      # Hero Visual - Before / After transformation card
      div(
        class = "hero-visual",
        div(
          class = "transformation-card",
          div(class = "transformation-panel transformation-before",
            span(class = "transformation-label", "BEFORE"),
            div(class = "apa-excerpt",
              p(class = "apa-text",
                "A Pearson correlation analysis revealed a statistically significant relationship between structured interview scores and first-year job performance, ",
                em("r"),
                "(98) = .42, ",
                em("p"),
                " < .001. The effect size, using Cohen\u2019s ",
                em("d"),
                ", was 0.90."
              ),
              p(class = "apa-text apa-text--faded",
                "These results suggest..."
              )
            )
          ),
          div(class = "transformation-arrow",
            tags$i(`data-lucide` = "chevron-right")
          ),
          div(class = "transformation-panel transformation-after",
            span(class = "transformation-label", "AFTER"),
            div(class = "transformation-stat", "71 out of 100"),
            div(class = "transformation-icon-array",
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

    # ---- The Problem Section ----
    div(
      class = "problem-section",
      h2(class = "section-title", "The problem with reporting statistics"),
      div(
        class = "problem-callout",
        div(class = "problem-row",
          code("r = 0.35, p < .001"),
          span(class = "problem-arrow", "\u2192"),
          span("means nothing to a hiring manager")
        ),
        div(class = "problem-row",
          code("Cohen's d = 0.74"),
          span(class = "problem-arrow", "\u2192"),
          span("means nothing to a hospital board")
        ),
        div(class = "problem-row",
          code("R\u00B2 = 0.12"),
          span(class = "problem-arrow", "\u2192"),
          span("means nothing to a school parent")
        ),
        p(class = "problem-summary",
          "ESCAPE translates these numbers into language that drives decisions."
        )
      )
    ),

    # ---- How It Works Section ----
    div(
      class = "how-it-works-section",
      h2(class = "section-title", "How it works"),
      div(
        class = "steps-grid",
        div(class = "step-card",
          span(class = "step-number", "1"),
          h3(class = "step-title", "Upload"),
          p(class = "step-description", "Drop in your CSV, Excel, SPSS, or SAS file.")
        ),
        div(class = "step-connector", tags$i(`data-lucide` = "chevron-right")),
        div(class = "step-card",
          span(class = "step-number", "2"),
          h3(class = "step-title", "Analyze"),
          p(class = "step-description", "Select variables and effect size metrics.")
        ),
        div(class = "step-connector", tags$i(`data-lucide` = "chevron-right")),
        div(class = "step-card step-card--highlight",
          span(class = "step-number", "3"),
          h3(class = "step-title", "Communicate"),
          p(class = "step-description", "Get plain-language reports with icon arrays and charts.")
        )
      )
    ),

    # ---- Sample Data Preview (moved up) ----
    div(
      class = "sample-section",
      h2(class = "section-title", "See it in action"),
      p(class = "section-subtitle", "Our sample dataset shows the relationship between SAT scores and college GPA"),
      div(
        class = "sample-preview",
          div(
            class = "sample-table-container",
            tags$caption(class = "sample-caption",
              "Sample dataset: SAT scores (scaled, n = 1,000) vs. College GPA"
            ),
            tags$table(
              class = "sample-table",
              tags$thead(
                tags$tr(
                  tags$th("SAT Score"),
                  tags$th("College GPA")
                )
              ),
              tags$tbody(
                tags$tr(tags$td("127"), tags$td("3.18")),
                tags$tr(tags$td("122"), tags$td("3.33")),
                tags$tr(tags$td("116"), tags$td("3.25")),
                tags$tr(tags$td("95"), tags$td("2.42")),
                tags$tr(tags$td("107"), tags$td("2.63")),
                tags$tr(class = "sample-more", tags$td(colspan = "2", "...and 995 more rows"))
              )
            )
          ),
          div(
            class = "sample-insight",
            tags$i(`data-lucide` = "lightbulb", class = "insight-icon"),
            div(
              tags$strong("Key Insight"),
              p("Students in the top 20% of SAT scores are ",
                tags$strong("6.9\u00D7 more likely"),
                " to achieve a 3.0+ GPA \u2014 that\u2019s the kind of statement that gets a committee\u2019s attention."
              ),
              div(class = "sample-insight-chart", plotOutput("landing_expectancy_plot", width = "100%", height = "300px"))
            )
          )
      ),
      actionButton(
        "try_sample_bottom",
        label = tagList(
          tags$i(`data-lucide` = "arrow-right", style = "width: 16px; height: 16px;"),
          "Explore This Dataset"
        ),
        class = "btn-primary"
      )
    ),

    # ---- Use Cases Section (auto-advancing carousel) ----
    div(
      class = "use-cases-section",
      h2(class = "section-title", "From the lab to the boardroom"),
      p(class = "section-subtitle", "See how ESCAPE helps researchers in HR, healthcare, and education make their findings unignorable"),
      div(
        class = "use-cases-carousel",
        tags$button(
          type = "button", class = "carousel-nav carousel-prev",
          `aria-label` = "Previous",
          tags$i(`data-lucide` = "chevron-left")
        ),
        div(
          class = "carousel-viewport",
          div(
            class = "carousel-track",
            use_case_card("user-check", "Selection Tool Validity",
              "67% of high-scoring applicants outperform",
              "HR departments assess whether pre-employment tests predict job performance. Instead of reporting r = 0.35, ESCAPE shows that 67% of high-scoring applicants outperform low-scoring applicants\u2014helping hiring managers make informed decisions.",
              "assets/selection.jpg"
            ),
            use_case_card("heart-pulse", "Health Intervention Effectiveness",
              "71% chance of better outcomes",
              "Clinical trials evaluate treatment outcomes. A correlation of r = 0.42 becomes: \"Patients receiving the new treatment have a 71% chance of better outcomes compared to standard care.\" This makes results accessible to patients and policymakers.",
              "assets/health.jpg"
            ),
            use_case_card("graduation-cap", "Educational Assessment",
              "2.3\u00D7 more likely to graduate on time",
              "Schools analyze whether entrance exams predict academic success. Rather than abstract correlations, educators can say: \"Students scoring above the 75th percentile are 2.3 times more likely to graduate on time.\"",
              "assets/education.jpg"
            ),
            use_case_card("scale", "Psychological Scale Validation",
              "78% of individuals correctly identified",
              "Researchers validate new measurement instruments. Instead of technical metrics, they can demonstrate: \"The new anxiety scale correctly identifies 78% of individuals who need clinical intervention.\"",
              "assets/validation.jpg"
            ),
            use_case_card("target", "Program Evaluation",
              "64% more likely to achieve positive outcomes",
              "Nonprofits and government agencies evaluate program impact. A correlation of r = 0.28 translates to: \"Participants are 64% more likely to achieve positive outcomes than non-participants.\"",
              "assets/program.jpg"
            ),
            use_case_card("trending-up", "Predictive Analytics",
              "73% more likely to make repeat purchases",
              "Business analysts forecast customer behavior. Rather than r = 0.45, stakeholders understand: \"Customers with high engagement scores are 73% more likely to make repeat purchases.\"",
              "assets/predictive.jpg"
            )
          )
        ),
        tags$button(
          type = "button", class = "carousel-nav carousel-next",
          `aria-label` = "Next",
          tags$i(`data-lucide` = "chevron-right")
        ),
        div(
          class = "carousel-indicators",
          tags$button(class = "carousel-dot active", `data-slide` = "0", `aria-label` = "Slide 1"),
          tags$button(class = "carousel-dot", `data-slide` = "1", `aria-label` = "Slide 2"),
          tags$button(class = "carousel-dot", `data-slide` = "2", `aria-label` = "Slide 3"),
          tags$button(class = "carousel-dot", `data-slide` = "3", `aria-label` = "Slide 4"),
          tags$button(class = "carousel-dot", `data-slide` = "4", `aria-label` = "Slide 5"),
          tags$button(class = "carousel-dot", `data-slide` = "5", `aria-label` = "Slide 6")
        )
      )
    ),

    # ---- Features Section ----
    div(
      class = "features-section",
      h2(class = "section-title", "From raw numbers to boardroom-ready insight"),
      div(
        class = "features-grid",
        feature_card_micro(
          "shield-check",
          "Privacy First",
          "All analysis happens in your browser. Your data never leaves your computer.",
          "Runs entirely in your browser \u2014 no server, no uploads",
          "feature-icon--green"
        ),
        feature_card_micro(
          "bar-chart-3",
          "Expectancy Charts",
          "Show \"out of 100 people\" style probabilities instead of abstract coefficients.",
          "r = 0.35 \u2192 \"67 out of 100 top scorers succeed\"",
          "feature-icon--blue"
        ),
        feature_card_micro(
          "layers",
          "Multiple Effect Sizes",
          "Calculate Cohen's d, Hedges' g, CLES, and BESD, all in one view.",
          "Cohen's d, Hedges' g, CLES, BESD \u2014 all in one view",
          "feature-icon--purple"
        ),
        feature_card_micro(
          "trending-up",
          "Bridge Traditional Statistics",
          "Keep familiar descriptives and correlations, paired with consumer-friendly translations.",
          "Pair every r value with a plain-language equivalent",
          "feature-icon--teal"
        ),
        feature_card_micro(
          "download",
          "Export Reports",
          "Generate professional HTML reports with effect sizes explained in clear, shareable language.",
          "One-click HTML report, ready to email to stakeholders",
          "feature-icon--emerald"
        ),
        feature_card_micro(
          "file-spreadsheet",
          "Multiple Formats",
          "Import data from CSV, Excel (.xlsx), SPSS (.sav), and SAS (.sas7bdat) files.",
          "CSV, Excel, SPSS, SAS \u2014 drag and drop any format",
          "feature-icon--orange"
        )
      )
    ),

    # ---- Footer ----
    div(
      class = "landing-footer",
      div(class = "footer-content",
        p(class = "footer-brand",
          tags$strong("ESCAPE"), " \u2014 Effect Size Calculator for Practical Effects"
        ),
        p(class = "footer-built",
          "Built with ",
          tags$i(`data-lucide` = "heart", style = "width: 14px; height: 14px; color: #F87171;"),
          " using R Shiny"
        ),
        p(class = "footer-built", "Created by Don Zhang")
      )
    )
  )
}

# Feature Card Helper (with micro-example)
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

# Use Case Card Helper (carousel slide with stock image)
use_case_card <- function(icon, title, stat, description, image) {
  div(
    class = "use-case-card",
    div(class = "use-case-image",
      tags$img(src = image, alt = title, class = "use-case-img")
    ),
    div(
      class = "use-case-content",
      div(
        class = "use-case-icon-badge",
        tags$i(`data-lucide` = icon)
      ),
      span(class = "use-case-stat", stat),
      h3(class = "use-case-title", title),
      p(class = "use-case-description", description)
    )
  )
}

# Analysis Interface Component - Redesigned layout: content-driven height, no fill collapse
analysis_ui <- function() {
  page_sidebar(
    title = tags$span(
      class = "d-flex align-items-center gap-2",
      tags$i(`data-lucide` = "bar-chart-2", style = "width: 20px; height: 20px;"),
      "ESCAPE",
      tags$small(class = "text-muted", "by Don Zhang")
    ),
    theme = app_theme,
    fillable = FALSE,

    sidebar = sidebar(
      width = 400,

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
          selectInput("criterionVar", "Criterion (Y)", choices = NULL)
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
                          tags$li("Set Criterion cutoff and Predictor percentile."),
                          tags$li("Use Summary (Overview and Descriptives) for a quick read of your data."),
                          tags$li("Use Practical effect sizes for alternative (CLES, BESD) and graphical (expectancy, icon array) presentations."),
                          tags$li("Use Traditional effect sizes for classical indices, group summaries, and the theoretical converter."),
                          tags$li("Export report from the sidebar when ready."),
                          tags$li("Open the Help tab for the app guide and (soon) interpretation resources.")
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
                      showcase = tags$i(`data-lucide` = "trending-up", style = "width: 32px; height: 32px; color: var(--color-primary);"),
                      theme = "light"
                    ),
                    value_box(
                      title = "Cohen's d",
                      value = textOutput("overview_d", inline = TRUE),
                      showcase = tags$i(`data-lucide` = "layers", style = "width: 32px; height: 32px; color: var(--color-success);"),
                      theme = "light"
                    ),
                    value_box(
                      title = "CLES",
                      value = textOutput("overview_cles", inline = TRUE),
                      showcase = tags$i(`data-lucide` = "percent", style = "width: 32px; height: 32px; color: var(--color-warning);"),
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
            class = "analysis-page analysis-page--nested",
            navset_pill(
              id = "traditional_sections",
              nav_panel(
                title = "Indices & groups",
                value = "traditional_indices",
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
              ),
              nav_panel(
                title = "Converter",
                value = "traditional_converter",
                div(
                  class = "analysis-subsection",
                  card(
                    card_header("Effect Size Converter (Theoretical)"),
                    card_body(
                      class = "converter-controls-card",
                      layout_columns(
                        col_widths = c(4, 4, 4),
                        selectInput("converter_input_type", "Input type", choices = c("Correlation (r)" = "r", "Cohen's d" = "d")),
                        numericInput("converter_value", "Input value", value = 0.3, step = 0.01),
                        uiOutput("converter_validity")
                      ),
                      tags$p(class = "text-muted", "Use this mode when you only have published summary effect sizes.")
                    )
                  ),
                  card(
                    card_header("Converted Metrics"),
                    card_body(
                      class = "converter-output-card",
                      tableOutput("converter_table"),
                      tags$hr(),
                      uiOutput("converter_interpretation")
                    )
                  )
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
                      tags$p(class = "mb-3", textOutput("cles.verbal")),
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
            navset_pill(
              id = "help_sections",
              nav_panel(
                title = "App guide",
                value = "help_app",
                div(
                  class = "analysis-subsection analysis-page--help",
                  card(
                    card_body(class = "help-body", htmlOutput("instructions"))
                  )
                )
              ),
              nav_panel(
                title = "Interpretation resources",
                value = "help_interpretation",
                div(
                  class = "analysis-subsection",
                  card(
                    card_header("Effect sizes in context"),
                    card_body(
                      tags$p(
                        class = "mb-3 text-muted",
                        "Planned for a future update: short guides on reading effect sizes from scientific papers, common pitfalls, and how to judge whether an effect is practically meaningful in your setting—not only whether it is statistically nonzero."
                      ),
                      tags$p(
                        class = "mb-0",
                        tags$em("This section will host educational material and optional technical notes when they are ready.")
                      )
                    )
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
    tags$link(rel = "stylesheet", href = "css/main.css"),
    tags$link(rel = "stylesheet", href = "css/landing.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Sora:wght@300;400;500;600;700&family=Fraunces:opsz,wght@9..144,500;9..144,700&family=JetBrains+Mono:wght@400;500&display=swap"
    ),
    tags$script(src = "https://unpkg.com/lucide@latest/dist/umd/lucide.js"),
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
    view = "landing",  # "landing" or "analysis"
    data_source = NULL,  # "sample" or "uploaded"
    landing_file = NULL,  # preserved file from landing upload (lost when DOM switches)
    guided_done = FALSE,  # first-time own-data wizard is session-scoped
    guided_step = 1
  )
  guided_file_meta <- reactiveVal(NULL)

  # --- Navigation ---

  # Try sample data buttons
  observeEvent(input$try_sample, {
    app_state$view <- "analysis"
    app_state$data_source <- "sample"
  })

  observeEvent(input$try_sample_bottom, {
    app_state$view <- "analysis"
    app_state$data_source <- "sample"
  })

  # Open guided upload from landing page
  observeEvent(input$open_guided_upload, {
    app_state$landing_file <- NULL
    guided_file_meta(NULL)
    app_state$view <- "analysis"
    app_state$data_source <- "uploaded"
    app_state$guided_step <- 1
    app_state$guided_done <- FALSE
  })

  # Upload file from guided modal step 1
  observeEvent(input$guided_file, {
    req(input$guided_file)
    guided_file_meta(input$guided_file)
    app_state$landing_file <- input$guided_file
    app_state$data_source <- "uploaded"
    if (isTRUE(app_state$guided_step == 1)) {
      show_guided_wizard()
    }
  })

  # Sidebar upload should override preserved landing upload
  observeEvent(input$file1, {
    req(input$file1)
    app_state$landing_file <- NULL
    guided_file_meta(NULL)
    app_state$data_source <- "uploaded"
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
    } else {
      analysis_ui()
    }
  })

  # Re-initialize Lucide icons after UI switch
  observe({
    invalidateLater(100)
    session$sendCustomMessage("initIcons", list())
  }) |> bindEvent(app_state$view)

  # --- Data Loading ---

  data_set <- reactive({
    # Prefer preserved landing upload, then sidebar file
    if (!is.null(app_state$data_source) && app_state$data_source == "uploaded" && !is.null(app_state$landing_file)) {
      inFile <- app_state$landing_file
    } else if (!is.null(input$file1)) {
      inFile <- input$file1
    } else {
      inFile <- NULL
    }

    if (is.null(inFile)) {
      # Use sample data only when sample mode is active
      if (app_state$view == "analysis" && identical(app_state$data_source, "sample")) {
        sample_path <- get_sample_data_path()
        if (!is.null(sample_path)) {
          return(read.csv(sample_path, header = TRUE))
        }
      }
      return(NULL)
    }

    ext <- tools::file_ext(inFile$name)

    tryCatch({
      df <- switch(ext,
        csv = read.csv(inFile$datapath, header = TRUE),
        xlsx = readxl::read_excel(inFile$datapath),
        sav = haven::read_sav(inFile$datapath),
        sas7bdat = haven::read_sas(inFile$datapath),
        NULL
      )

      session$sendCustomMessage("showToast", list(
        message = paste("Loaded", nrow(df), "rows from", inFile$name),
        type = "success",
        duration = 3000
      ))

      return(df)
    }, error = function(e) {
      session$sendCustomMessage("showToast", list(
        message = paste("Error loading file:", e$message),
        type = "error",
        duration = 5000
      ))
      return(NULL)
    })
  })

  guided_step2_state <- reactive({
    is_numeric_like <- function(x) {
      if (is.numeric(x) || is.integer(x)) return(TRUE)
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
    if (is.null(df) || ncol(df) < 2) {
      return(list(
        ready = FALSE,
        message = "Your dataset needs at least 2 columns.",
        predictor = NULL,
        criterion = NULL
      ))
    }
    dsnames <- names(df)
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

    pred_is_numeric <- is_numeric_like(df[[pred]])
    crit_is_numeric <- is_numeric_like(df[[crit]])
    if (!pred_is_numeric || !crit_is_numeric) {
      bad_vars <- character(0)
      if (!pred_is_numeric) {
        bad_vars <- c(
          bad_vars,
          paste0("Predictor (X) `", pred, "` is type ", column_type_label(df[[pred]]), ".")
        )
      }
      if (!crit_is_numeric) {
        bad_vars <- c(
          bad_vars,
          paste0("Criterion (Y) `", crit, "` is type ", column_type_label(df[[crit]]), ".")
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
    if (is.null(step2_state$message)) return(NULL)
    tags$p(class = "text-danger mb-0", step2_state$message)
  })

  show_guided_wizard <- function() {
    df <- data_set()
    file_meta <- guided_file_meta()
    has_file <- !is.null(file_meta)
    has_data <- !is.null(df)
    has_two_cols <- has_data && ncol(df) >= 2
    dsnames <- if (has_data) names(df) else character(0)
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
        tags$span(paste(nrow(df), "rows")),
        tags$span(class = "data-info-dot", "\u2022"),
        tags$span(paste(ncol(df), "columns"))
      )
    } else if (has_file) {
      tagList(
        tags$span(class = "data-info-dot", "\u2022"),
        tags$span("File selected")
      )
    } else {
      NULL
    }

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

    step1_ready <- has_file

    footer <- if (step == 1) {
      tagList(
        actionButton("guided_skip", "Skip guide", class = "btn-ghost"),
        if (step1_ready) {
          actionButton("guided_next", "Continue", class = "btn-primary")
        } else {
          tags$button(type = "button", class = "btn btn-primary", disabled = "disabled", "Continue")
        }
      )
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

  observeEvent(list(app_state$view, app_state$data_source), {
    if (identical(app_state$view, "analysis") &&
        identical(app_state$data_source, "uploaded") &&
        !isTRUE(app_state$guided_done)) {
      show_guided_wizard()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$guided_skip, {
    app_state$guided_done <- TRUE
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
    app_state$guided_done <- TRUE
    removeModal()
  })

  # Hidden output for empty-state conditionalPanel (only in analysis view)
  output$data_ready <- renderText({
    if (app_state$view != "analysis") return("no")
    if (is.null(data_set())) return("no")
    "yes"
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  # Sample data for landing page "See it in action" expectancy preview
  landing_preview_data <- reactive({
    if (app_state$view != "landing") return(NULL)
    path <- get_sample_data_path()
    if (is.null(path)) return(NULL)
    raw <- tryCatch(read.csv(path, header = TRUE), error = function(e) NULL)
    if (is.null(raw) || ncol(raw) < 2) return(NULL)
    df <- raw %>%
      dplyr::select(Predictor = 1, Criterion = 2) %>%
      na.omit()
    list(df = df, predictor_name = "SAT Score (scaled)", criterion_name = "College GPA")
  })

  # Expectancy chart in landing Key Insight (sample data, fixed bins=5, cutoff=3.0)
  output$landing_expectancy_plot <- renderPlot({
    preview <- landing_preview_data()
    if (is.null(preview) || nrow(preview$df) < 2) return(invisible(NULL))
    exp_df <- calc_expectancy(preview$df, bins = 5, cutoff_y = 3.0)
    plot_expectancy_landing(exp_df, preview$predictor_name, 3.0)
  })

  # Data info display
  output$data_info <- renderUI({
    df <- data_set()
    if (is.null(df)) return(NULL)

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
        tags$span(paste(nrow(df), "rows")),
        tags$span(class = "data-info-dot", "\u2022"),
        tags$span(paste(ncol(df), "columns"))
      )
    )
  })

  # Effective predictor/criterion (default to first two columns when inputs not yet valid)
  effective_vars <- reactive({
    df <- data_set()
    if (is.null(df) || ncol(df) < 2) return(list(predictor = NULL, criterion = NULL))
    dsnames <- names(df)
    pred <- input$predictorVar
    crit <- input$criterionVar
    if (is.null(pred) || !pred %in% dsnames) pred <- dsnames[1]
    if (is.null(crit) || !crit %in% dsnames) crit <- dsnames[min(2, length(dsnames))]
    list(predictor = pred, criterion = crit)
  })

  # Keep sidebar selectors in sync with effective vars
  observe({
    ev <- effective_vars()
    df <- data_set()
    if (is.null(df) || is.null(ev$predictor)) return(NULL)
    dsnames <- names(df)
    updateSelectInput(session, "predictorVar", choices = dsnames, selected = ev$predictor)
    updateSelectInput(session, "criterionVar", choices = dsnames, selected = ev$criterion)
  })

  # Selected data (filtered); uses effective vars so chain never blocks on first paint
  selected_data <- reactive({
    req(data_set())
    ev <- effective_vars()
    req(ev$predictor, ev$criterion)
    df <- data_set()
    df %>%
      select(Predictor = !!sym(ev$predictor), Criterion = !!sym(ev$criterion)) %>%
      na.omit()
  })

  # --- Calculations ---

  validity_stats <- reactive({
    df <- selected_data()
    calc_correlation(df$Predictor, df$Criterion)
  })

  cutoff_X_val <- reactive({
    df <- selected_data()
    quantile(df$Predictor, probs = input$cutoff.X, na.rm = TRUE)
  })

  df_exp <- reactive({
    df <- selected_data()
    calc_expectancy(df, input$bins, input$cutoffInput)
  })

  df_cles <- reactive({
    df <- selected_data()
    co_X <- cutoff_X_val()
    df %>%
      mutate(
        Group = case_when(
          Predictor < co_X ~ "Below",
          Predictor >= co_X ~ "Above",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(Group))
  })

  effect_sizes <- reactive({
    df <- df_cles()
    above_vals <- df %>% filter(Group == "Above") %>% pull(Criterion)
    below_vals <- df %>% filter(Group == "Below") %>% pull(Criterion)

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
    df <- selected_data()
    mean_val <- round(mean(df$Criterion, na.rm = TRUE), 2)
    updateNumericInput(session, "cutoffInput", value = mean_val)
  })

  # Default criterion cutoff to mean when criterion variable changes
  last_criterion_var <- reactiveVal(NULL)
  observeEvent(effective_vars(), {
    ev <- effective_vars()
    if (is.null(ev$criterion)) return(NULL)
    if (identical(ev$criterion, last_criterion_var())) return(NULL)
    last_criterion_var(ev$criterion)
    sd <- selected_data()
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
    if (is.na(d)) return("--")
    direction <- if (d >= 0) "positive" else "negative"
    paste0("|d| = ", round(abs(d), 2), " (", direction, ")")
  })

  output$overview_cles <- renderText({
    cles <- effect_sizes()$cles
    if (is.na(cles)) return("--")
    paste0(round(cles * 100, 1), "%")
  })

  output$overview_scatter <- renderPlot({
    ev <- effective_vars()
    p <- plot_scatter(selected_data(), ev$predictor, ev$criterion)
    print(p)
  }, width = 560, height = 340, res = 96)

  output$overview_insight <- renderUI({
    ev <- effective_vars()
    r <- validity_stats()$r
    cles <- effect_sizes()$cles
    d <- effect_sizes()$d
    n_above <- sum(df_cles()$Group == "Above")
    n_below <- sum(df_cles()$Group == "Below")
    d_ci <- calc_d_ci(d, n_above, n_below)
    r_ci <- calc_r_ci(r, nrow(selected_data()))

    r_strength <- interpret_correlation(r)
    d_magnitude <- if (!is.na(d)) interpret_cohens_d(d) else "unknown"
    d_direction <- if (!is.na(d) && d >= 0) "higher" else "lower"

    cles_text <- if (!is.na(cles)) {
      paste0(
        "A randomly selected person from the top ",
        round(input$cutoff.X * 100), "% on ",
        ev$predictor, " has a ",
        tags$strong(paste0(round(cles * 100, 1), "%")),
        " probability of scoring higher on ",
        ev$criterion, " than someone from the bottom ",
        round((1 - input$cutoff.X) * 100), "%."
      )
    } else {
      "Adjust the cutoff percentile to see CLES insights."
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
            "Direction: higher predictor group tends to score ", d_direction, " on ", ev$criterion,
            ". Uncertainty ranges: r 95% CI [", round(r_ci$lower, 2), ", ", round(r_ci$upper, 2),
            "], d 95% CI [", round(d_ci$lower, 2), ", ", round(d_ci$upper, 2), "]."
          )
        )
      ),
      div(
        class = "insight-row highlight",
        tags$i(`data-lucide` = "lightbulb", class = "insight-icon-small"),
        p(HTML(cles_text))
      )
    )
  })

  # --- Data Tab Outputs ---

  output$contents <- renderDT({
    req(data_set())
    datatable(
      data_set(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'frtip',
        language = list(search = "", searchPlaceholder = "Search...")
      ),
      class = "display compact"
    )
  })

  # --- Expectancy Tab Outputs ---

  output$expectancyPlot <- renderPlot({
    ev <- effective_vars()
    p <- plot_expectancy(df_exp(), ev$predictor, input$cutoffInput)
    print(p)
  }, width = 720, height = 420, res = 96)

  output$expectancyTable <- renderTable({
    df_exp() %>%
      select(Bin = ntile_X, Range = xlabels, Proportion = proportion, Count = frequency, Total = n)
  }, digits = 3)

  # --- Statistics Tab Outputs ---

  output$descriptable <- renderTable({
    selected_data() %>%
      psych::describe() %>%
      as.data.frame() %>%
      select(n, mean, sd, median, min, max, skew, kurtosis)
  }, rownames = TRUE, digits = 3)

  output$validity_r <- renderText({
    paste0("r = ", validity_stats()$r_formatted)
  })

  output$validity_r2 <- renderText({
    paste0("(", validity_stats()$r2_pct, "% variance explained)")
  })

  output$hist_x_title <- renderText({
    ev <- effective_vars()
    paste("Distribution of", ev$predictor)
  })

  output$hist_y_title <- renderText({
    ev <- effective_vars()
    paste("Distribution of", ev$criterion)
  })

  output$histogram.X <- renderPlot({
    p <- plot_histogram(selected_data()$Predictor, fill_color = plot_colors$primary)
    print(p)
  }, width = 560, height = 340, res = 96)

  output$histogram.Y <- renderPlot({
    p <- plot_histogram(selected_data()$Criterion, fill_color = plot_colors$success)
    print(p)
  }, width = 560, height = 340, res = 96)

  output$corplot <- renderPlot({
    ev <- effective_vars()
    p <- plot_scatter(selected_data(), ev$predictor, ev$criterion)
    print(p)
  }, width = 720, height = 480, res = 96)

  # --- Effect Sizes Tab Outputs ---

  output$clestable <- renderTable({
    df_cles() %>%
      group_by(Group) %>%
      summarise(Mean = mean(Criterion), SD = sd(Criterion), n = n(), .groups = "drop")
  }, digits = 3)

  output$cles <- renderTable({
    stats <- df_cles() %>%
      group_by(Group) %>%
      summarise(m = mean(Criterion), s = sd(Criterion), n = n(), .groups = "drop")

    if (nrow(stats) < 2) return(NULL)

    above_vals <- df_cles() %>% filter(Group == "Above") %>% pull(Criterion)
    below_vals <- df_cles() %>% filter(Group == "Below") %>% pull(Criterion)

    if (length(above_vals) < 2 || length(below_vals) < 2) return(NULL)

    calc_all_effect_sizes(above_vals, below_vals, validity_stats()$r)
  })

  output$cles.verbal <- renderText({
    ev <- effective_vars()
    df <- df_cles()
    above_vals <- df %>% filter(Group == "Above") %>% pull(Criterion)
    below_vals <- df %>% filter(Group == "Below") %>% pull(Criterion)

    if (length(above_vals) < 2 || length(below_vals) < 2) {
      return("Insufficient data for CLES calculation.")
    }

    cles_val <- calc_cles(above_vals, below_vals)
    cles_verbal(ev$predictor, ev$criterion, cutoff_X_val(), round(cles_val * 100, 1))
  })

  output$histogram.overlap <- renderPlot({
    ev <- effective_vars()
    p <- plot_density_overlap(df_cles(), ev$criterion, ev$predictor, cutoff_X_val())
    print(p)
  }, width = 680, height = 380, res = 96)

  output$besd <- renderTable({
    ev <- effective_vars()
    df <- selected_data()
    calc_besd(df, cutoff_X_val(), input$cutoffInput, ev$predictor, ev$criterion)
  }, rownames = TRUE, digits = 3)

  output$besd_theoretical <- renderTable({
    ev <- effective_vars()
    calc_besd_theoretical(validity_stats()$r, ev$predictor, ev$criterion)
  }, rownames = TRUE, digits = 3)

  output$start_here_blurb <- renderUI({
    ev <- effective_vars()
    if (is.null(ev$predictor) || is.null(ev$criterion)) return(tags$p("Select variables to begin."))
    tags$div(
      tags$p(tags$strong("Primary question: "), paste0("How much does ", ev$predictor, " improve practical outcomes on ", ev$criterion, "?")),
      tags$p(
        tags$strong("Recommended path: "),
        "Summary (Overview and Descriptives) → Practical effect sizes (alternative CLES/BESD; graphical expectancy and icon array) → Traditional effect sizes (indices, group stats, Converter when you only have published effects). Use Help for the app guide and interpretation resources as they are added."
      )
    )
  })

  converter_metrics <- reactive({
    in_type <- input$converter_input_type
    in_val <- input$converter_value
    if (is.null(in_type) || is.null(in_val) || !is.finite(in_val)) return(NULL)

    if (in_type == "r") {
      if (in_val <= -1 || in_val >= 1) return(NULL)
      r <- in_val
      d <- r_to_d(r)
    } else {
      d <- in_val
      r <- d_to_r(d)
    }

    if (!is.finite(r) || !is.finite(d)) return(NULL)
    cles <- d_to_cles(d)
    data.frame(
      Metric = c("Correlation (r)", "Cohen's d", "CLES (Probability)", "BESD High Group Success", "BESD Low Group Success"),
      Value = c(round(r, 3), round(d, 3), round(cles, 3), round(0.5 + r / 2, 3), round(0.5 - r / 2, 3))
    )
  })

  output$converter_table <- renderTable({
    cm <- converter_metrics()
    validate(need(!is.null(cm), "Enter a valid effect size value. For r, use (-1, 1)."))
    cm
  }, digits = 3)

  output$converter_validity <- renderUI({
    in_type <- input$converter_input_type
    in_val <- input$converter_value
    if (is.null(in_val)) return(tags$span(class = "text-muted", ""))
    if (in_type == "r" && (in_val <= -1 || in_val >= 1)) {
      return(tags$span(style = "color: var(--color-danger);", "For r input, value must be between -1 and 1 (exclusive)."))
    }
    tags$span(style = "color: var(--color-success);", "Input is valid.")
  })

  output$converter_interpretation <- renderUI({
    cm <- converter_metrics()
    req(!is.null(cm))
    r_val <- as.numeric(cm$Value[cm$Metric == "Correlation (r)"])
    cles_val <- as.numeric(cm$Value[cm$Metric == "CLES (Probability)"])
    if (!is.finite(r_val) || !is.finite(cles_val)) return(NULL)
    tags$p(
      class = "mb-0 text-muted",
      paste0(
        "Interpretation: with r = ", round(r_val, 3), ", translated CLES is about ",
        round(cles_val * 100, 1), "%, meaning roughly ",
        round(cles_val * 100), " out of 100 random pairings favor the higher-scoring group."
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
      ev <- effective_vars()
      paste0(
        "ESCAPE_Scatter_",
        sanitize_filename(ev$predictor),
        "_vs_",
        sanitize_filename(ev$criterion),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      ev <- effective_vars()
      p <- plot_scatter(selected_data(), ev$predictor, ev$criterion)
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
      ev <- effective_vars()
      paste0(
        "ESCAPE_Histogram_",
        sanitize_filename(ev$predictor),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      p <- plot_histogram(selected_data()$Predictor, fill_color = plot_colors$primary)
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
      ev <- effective_vars()
      paste0(
        "ESCAPE_Histogram_",
        sanitize_filename(ev$criterion),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      p <- plot_histogram(selected_data()$Criterion, fill_color = plot_colors$success)
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
      ev <- effective_vars()
      paste0(
        "ESCAPE_Expectancy_",
        sanitize_filename(ev$predictor),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      ev <- effective_vars()
      p <- plot_expectancy(df_exp(), ev$predictor, input$cutoffInput)
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
      ev <- effective_vars()
      paste0(
        "ESCAPE_CLES_Overlap_",
        sanitize_filename(ev$predictor),
        "_",
        sanitize_filename(ev$criterion),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      ev <- effective_vars()
      p <- plot_density_overlap(df_cles(), ev$criterion, ev$predictor, cutoff_X_val())
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

  output$iconarray_plot <- renderPlot({
    ev <- effective_vars()
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
        predictor_name = ev$predictor,
        criterion_name = ev$criterion
      )
    }
  }, width = 720, height = 420, res = 96)

  output$iconarray_stats <- renderText({
    ev <- effective_vars()
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
      ev <- effective_vars()
      paste0(
        "ESCAPE_IconArray_",
        sanitize_filename(ev$predictor),
        "_",
        sanitize_filename(ev$criterion),
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      ev <- effective_vars()
      cles_val <- effect_sizes()$cles
      
      if (!is.na(cles_val)) {
        p <- plot_icon_array(
          cles_prob = cles_val,
          total_icons = input$iconarray_total,
          icon_type = input$iconarray_type,
          layout = input$iconarray_layout,
          predictor_name = ev$predictor,
          criterion_name = ev$criterion
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
        )
      )
    }
  })

  # --- Report Generation ---

  assemble_report_params <- function() {
    req(selected_data())
    ev <- effective_vars()
    df <- selected_data()
    df_grouped <- df_cles()
    above_vals <- df_grouped %>% filter(Group == "Above") %>% pull(Criterion)
    below_vals <- df_grouped %>% filter(Group == "Below") %>% pull(Criterion)
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
    cles_narrative <- cles_verbal(ev$predictor, ev$criterion, cutoff_X_val(), round(cles_val * 100, 1))
    desc_tbl <- psych::describe(df) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Variable") %>%
      dplyr::select(Variable, n, mean, sd, median, min, max)
    exp_tbl <- df_exp() %>%
      dplyr::transmute(
        ntile_X,
        xlabels,
        proportion,
        frequency,
        n
      )
    besd_emp <- calc_besd(df, cutoff_X_val(), input$cutoffInput, ev$predictor, ev$criterion) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Group")
    besd_theory <- calc_besd_theoretical(corr, ev$predictor, ev$criterion) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Group")
    icon_plot_obj <- plot_icon_array(
      cles_prob = cles_val,
      total_icons = 100,
      icon_type = "person",
      layout = "auto",
      predictor_name = ev$predictor,
      criterion_name = ev$criterion
    )
    list(
      predictor = ev$predictor,
      criterion = ev$criterion,
      n_obs = nrow(df),
      bins = input$bins,
      cutoff_y = input$cutoffInput,
      cutoff_x_pct = round(input$cutoff.X * 100),
      r = corr,
      r2 = corr^2,
      practical_message = practical_message,
      cles_narrative = cles_narrative,
      desc_stats = desc_tbl,
      effect_sizes = calc_all_effect_sizes(above_vals, below_vals, corr),
      exp_table = exp_tbl,
      besd_empirical = besd_emp,
      besd_theoretical = besd_theory,
      scatter_plot = plot_scatter(df, ev$predictor, ev$criterion),
      exp_plot = plot_expectancy(df_exp(), ev$predictor, input$cutoffInput),
      icon_plot = icon_plot_obj
    )
  }

  output$report <- downloadHandler(
    filename = function() {
      paste0("ESCAPE_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(selected_data())
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
<small>Generated by ESCAPE (Effect Size Calculator for Practical Effects), created by Don Zhang.</small>
'
      writeLines(rmd_content, tempReport)

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

*Generated by ESCAPE (Effect Size Calculator for Practical Effects), created by Don Zhang.*
'
      writeLines(rmd_pdf, tempReport)

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
          message = paste0(
            "PDF export needs a LaTeX engine (install TinyTeX: tinytex::install_tinytex()). ",
            "Alternatively export HTML and use Print to PDF. ",
            conditionMessage(err)
          ),
          type = "error"
        ))
        stop(conditionMessage(err))
      }
    }
  )
}

# ============================================
# RUN APP
# ============================================

shinyApp(ui = ui, server = server)
