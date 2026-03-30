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

    # Hero Section
    div(
      class = "hero-section",
        div(
          class = "hero-content",
          div(
            class = "app-name-badge",
            tags$strong("ESCAPE"),
            " — Effect Size Calculator for Practical Effects"
          ),
          h1(class = "hero-title", "Translate Statistics into Everyday Impact"),
          p(
            class = "hero-subtitle",
            "Traditional effect sizes like correlations are hard to understand. ESCAPE transforms abstract coefficients ",
            "into intuitive probabilities, success rates, and plain-language metrics that anyone can use to make informed decisions."
          ),
        div(
          class = "hero-actions",
          actionButton("try_sample", "Try with Sample Data"),
          fileInput("landing_file", NULL, accept = c(".csv", ".xlsx", ".sav", ".sas7bdat"), buttonLabel = "Upload Your Data", placeholder = NULL)
        )
      ),

      # Hero Visual - Icon array only (no text/labels)
      div(
        class = "hero-visual",
        div(
          class = "icon-array-card",
          div(
            class = "icon-array-container",
            # 69 high performer icons
            tagList(lapply(1:69, function(i) {
              div(
                class = paste0("array-icon success-icon delay-", ((i - 1) %% 10) + 1),
                tags$i(`data-lucide` = "user-check")
              )
            })),
            # 31 low performer icons
            tagList(lapply(1:31, function(i) {
              div(
                class = paste0("array-icon neutral-icon delay-", ((i - 1) %% 10) + 1),
                tags$i(`data-lucide` = "user")
              )
            }))
          )
        )
      )
    ),

    # Features Section
    div(
      class = "features-section",
      h2(class = "section-title", "Everything you need for effect size analysis"),
      div(
        class = "features-grid",
        feature_card(
          "bar-chart-3",
          "Expectancy Charts",
          "Show \"out of 100 people\" style probabilities instead of abstract coefficients, using intuitive bar charts."
        ),
        feature_card(
          "layers",
          "Multiple Effect Sizes",
          "Calculate Cohen's d, Hedges' g, CLES, and BESD, then present them as interpretable, side-by-side stories."
        ),
        feature_card(
          "trending-up",
          "Bridge Traditional Statistics",
          "Keep familiar descriptives and correlations, but pair them with consumer-friendly translations."
        ),
        feature_card(
          "download",
          "Export Reports",
          "Generate professional HTML reports with effect sizes explained in clear, shareable language."
        ),
        feature_card(
          "file-spreadsheet",
          "Multiple Formats",
          "Import data from CSV, Excel (.xlsx), SPSS (.sav), and SAS (.sas7bdat) files."
        ),
        feature_card(
          "shield-check",
          "Privacy First",
          "All analysis happens in your browser. Your data never leaves your computer."
        )
      )
    ),

    # Use Cases Section
    div(
      class = "use-cases-section",
      h2(class = "section-title", "Real-World Use Cases"),
      p(class = "section-subtitle", "See how ESCAPE can be used to make data-driven decisions in the real world"),
      div(
        class = "use-cases-carousel",
        # Carousel navigation
        tags$button(
          type = "button",
          class = "carousel-nav carousel-prev",
          `aria-label` = "Previous",
          tags$i(`data-lucide` = "chevron-left")
        ),
        # Carousel slides container
        div(
          class = "carousel-slides",
          use_case_card(
            "user-check",
            "Selection Tool Validity",
            "HR departments assess whether pre-employment tests predict job performance. Instead of reporting r = 0.35, ESCAPE shows that 67% of high-scoring applicants outperform low-scoring applicants—helping hiring managers make informed decisions.",
            "assets/selection.jpg"
          ),
          use_case_card(
            "heart-pulse",
            "Health Intervention Effectiveness",
            "Clinical trials evaluate treatment outcomes. A correlation of r = 0.42 becomes: \"Patients receiving the new treatment have a 71% chance of better outcomes compared to standard care.\" This makes results accessible to patients and policymakers.",
            "assets/health.jpg"
          ),
          use_case_card(
            "graduation-cap",
            "Educational Assessment",
            "Schools analyze whether entrance exams predict academic success. Rather than abstract correlations, educators can say: \"Students scoring above the 75th percentile are 2.3 times more likely to graduate on time.\"",
            "assets/education.jpg"
          ),
          use_case_card(
            "scale",
            "Psychological Scale Validation",
            "Researchers validate new measurement instruments. Instead of technical metrics, they can demonstrate: \"The new anxiety scale correctly identifies 78% of individuals who need clinical intervention.\"",
            "assets/validation.jpg"
          ),
          use_case_card(
            "target",
            "Program Evaluation",
            "Nonprofits and government agencies evaluate program impact. A correlation of r = 0.28 translates to: \"Participants are 64% more likely to achieve positive outcomes than non-participants.\"",
            "assets/program.jpg"
          ),
          use_case_card(
            "trending-up",
            "Predictive Analytics",
            "Business analysts forecast customer behavior. Rather than r = 0.45, stakeholders understand: \"Customers with high engagement scores are 73% more likely to make repeat purchases.\"",
            "assets/predictive.jpg"
          )
        ),
        # Carousel navigation
        tags$button(
          type = "button",
          class = "carousel-nav carousel-next",
          `aria-label` = "Next",
          tags$i(`data-lucide` = "chevron-right")
        ),
        # Carousel indicators
        div(
          class = "carousel-indicators",
          tags$button(class = "carousel-indicator active", `data-slide` = "0", `aria-label` = "Slide 1"),
          tags$button(class = "carousel-indicator", `data-slide` = "1", `aria-label` = "Slide 2"),
          tags$button(class = "carousel-indicator", `data-slide` = "2", `aria-label` = "Slide 3"),
          tags$button(class = "carousel-indicator", `data-slide` = "3", `aria-label` = "Slide 4"),
          tags$button(class = "carousel-indicator", `data-slide` = "4", `aria-label` = "Slide 5"),
          tags$button(class = "carousel-indicator", `data-slide` = "5", `aria-label` = "Slide 6")
        )
      )
    ),

    # Sample Data Preview
    div(
      class = "sample-section",
      h2(class = "section-title", "See it in action"),
      p(class = "section-subtitle", "Our sample dataset shows the relationship between SAT scores and college GPA"),
      div(
        class = "sample-preview",
        div(
          class = "sample-table-container",
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
            p("Students scoring in the top 25% on SAT have a 22% chance of achieving above-average GPA (3.5+), compared to 4% for those in the bottom 75%."),
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

    # Footer
    div(
      class = "landing-footer",
      p(
        tags$strong("ESCAPE"), " — Effect Size Calculator for Practical Effects",
        tags$br(),
        "Built with ",
        tags$i(`data-lucide` = "heart", style = "width: 14px; height: 14px; color: #F87171;"),
        " using R Shiny"
      )
    )
  )
}

# Feature Card Helper
feature_card <- function(icon, title, description) {
  div(
    class = "feature-card",
    div(
      class = "feature-icon",
      tags$i(`data-lucide` = icon)
    ),
    h3(class = "feature-title", title),
    p(class = "feature-description", description)
  )
}

# Use Case Card Helper
use_case_card <- function(icon, title, description, image) {
  div(
    class = "use-case-card",
    div(
      class = "use-case-image",
      tags$img(src = image, alt = title, class = "use-case-img")
    ),
    div(
      class = "use-case-content",
      div(
        class = "use-case-icon-badge",
        tags$i(`data-lucide` = icon)
      ),
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
      "ESCAPE"
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
              "Export Report"
            ),
            class = "btn-primary w-100"
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
        id = "main_tabs",

        # ---- Tab 0: Start Here ----
        nav_panel(
          title = tags$span(
            tags$i(`data-lucide` = "compass", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Start Here"
          ),
          value = "start",
          div(
            class = "analysis-page",
            card(
              card_header("Guided Workflow"),
              card_body(
                class = "start-guide-card",
                div(
                  class = "start-guide",
                  tags$ol(
                    tags$li("Choose Predictor (X) and Criterion (Y) in the sidebar."),
                    tags$li("Set Criterion cutoff and Predictor percentile."),
                    tags$li("Read practical interpretation in Summary."),
                    tags$li("Review technical details in Effect Sizes and BESD."),
                    tags$li("Export report when ready.")
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

        # ---- Tab 1: Summary ----
        nav_panel(
          title = tags$span(
            tags$i(`data-lucide` = "layout-dashboard", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Summary"
          ),
          value = "overview",
          div(
            class = "analysis-page",
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

        # ---- Tab 2: Data & Descriptives ----
        nav_panel(
          title = tags$span(
            tags$i(`data-lucide` = "table", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Data & Descriptives"
          ),
          value = "data",
          div(
            class = "analysis-page",
            card(
              card_header("Raw Data"),
              card_body(style = "overflow-x: auto;", DTOutput("contents"))
            ),
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

        # ---- Tab 3: Expectancy ----
        nav_panel(
          title = tags$span(
            tags$i(`data-lucide` = "bar-chart-3", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Expectancy"
          ),
          value = "expectancy",
          div(
            class = "analysis-page",
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
            )
          )
        ),

        # ---- Tab 4: Effect Sizes ----
        nav_panel(
          title = tags$span(
            tags$i(`data-lucide` = "layers", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Effect Sizes"
          ),
          value = "effects",
          div(
            class = "analysis-page",
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

        # ---- Tab 5: Icon Array ----
        nav_panel(
          title = tags$span(
            tags$i(`data-lucide` = "users", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Icon Array"
          ),
          value = "iconarray",
          div(
            class = "analysis-page",
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
                # Icon Array Controls - Horizontal Layout
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
        ),

        nav_panel(
          title = tags$span(
            tags$i(`data-lucide` = "shuffle", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Converter"
          ),
          value = "converter",
          div(
            class = "analysis-page",
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
        ),
        # ---- Tab 7: Help ----
        nav_panel(
          title = tags$span(
            tags$i(`data-lucide` = "help-circle", style = "width: 14px; height: 14px; margin-right: 6px;"),
            "Help"
          ),
          value = "help",
          div(
            class = "analysis-page analysis-page--help",
            card(
              card_body(class = "help-body", htmlOutput("instructions"))
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

  uiOutput("main_ui")
)

# ============================================
# SERVER LOGIC
# ============================================

server <- function(input, output, session) {

  # --- App State ---
  app_state <- reactiveValues(
    view = "landing",  # "landing" or "analysis"
    data_source = NULL,  # "sample" or "uploaded"
    landing_file = NULL   # preserved file from landing upload (lost when DOM switches)
  )

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

  # File upload from landing page (store file so it survives UI switch)
  observeEvent(input$landing_file, {
    req(input$landing_file)
    app_state$landing_file <- input$landing_file
    app_state$view <- "analysis"
    app_state$data_source <- "uploaded"
  })

  # Back to landing
  observeEvent(input$back_to_landing, {
    app_state$view <- "landing"
    app_state$data_source <- NULL
    app_state$landing_file <- NULL
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
      # Use sample data if in analysis view
      if (app_state$view == "analysis") {
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
    pred_name <- names(raw)[1]
    crit_name <- names(raw)[2]
    df <- raw %>%
      dplyr::select(Predictor = 1, Criterion = 2) %>%
      na.omit()
    list(df = df, predictor_name = pred_name, criterion_name = crit_name)
  })

  # Expectancy chart in landing Key Insight (sample data, fixed bins=5, cutoff=3.5)
  output$landing_expectancy_plot <- renderPlot({
    preview <- landing_preview_data()
    if (is.null(preview) || nrow(preview$df) < 2) return(invisible(NULL))
    exp_df <- calc_expectancy(preview$df, bins = 5, cutoff_y = 3.5)
    plot_expectancy_landing(exp_df, preview$predictor_name, 3.5)
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
        round((1 - input$cutoff.X) * 100), "%. ",
        "Practical takeaway: treat this as one decision input alongside context, constraints, and professional judgment."
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
      tags$p(tags$strong("Recommended path: "), "Summary -> Expectancy -> Effect Sizes -> Converter (if raw data are unavailable).")
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

  output$report <- downloadHandler(
    filename = function() {
      paste0("ESCAPE_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(selected_data())
      tempReport <- file.path(tempdir(), "report.Rmd")

      rmd_content <- '
---
title: "ESCAPE Analysis Report"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    toc: true
---

## Analysis Summary

**Predictor:** `r params$predictor`
**Criterion:** `r params$criterion`

### Correlation

`r paste0("r = ", round(params$r, 3), " (", round(params$r^2 * 100, 1), "% variance explained)")`

### Descriptive Statistics

```{r, echo=FALSE}
knitr::kable(params$desc_stats, digits = 3)
```

### Effect Size Indices

```{r, echo=FALSE}
knitr::kable(params$effect_sizes, digits = 3)
```

### Scatterplot

```{r, echo=FALSE, fig.width=8, fig.height=5}
params$scatter_plot
```

### Expectancy Chart

```{r, echo=FALSE, fig.width=8, fig.height=4}
params$exp_plot
```

---
*Generated by ESCAPE*
'
      writeLines(rmd_content, tempReport)

      df <- selected_data()
      df_grouped <- df_cles()
      above_vals <- df_grouped %>% filter(Group == "Above") %>% pull(Criterion)
      below_vals <- df_grouped %>% filter(Group == "Below") %>% pull(Criterion)

      rmarkdown::render(
        tempReport,
        output_file = file,
        params = list(
          predictor = effective_vars()$predictor,
          criterion = effective_vars()$criterion,
          r = validity_stats()$r,
          desc_stats = psych::describe(df) %>% as.data.frame() %>% select(n, mean, sd, median, min, max),
          effect_sizes = calc_all_effect_sizes(above_vals, below_vals, validity_stats()$r),
          scatter_plot = plot_scatter(df, effective_vars()$predictor, effective_vars()$criterion),
          exp_plot = plot_expectancy(df_exp(), effective_vars()$predictor, input$cutoffInput)
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# ============================================
# RUN APP
# ============================================

shinyApp(ui = ui, server = server)
