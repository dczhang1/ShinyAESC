# ShinyAESC v3.0 - The "Greenfield" Re-Design
# Narrative Statistics & Explorable Explanations

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(readxl)
library(haven)

# Custom CSS for "Mad Libs" Interface
mad_libs_css <- "
  .mad-lib-container {
    font-size: 1.5rem;
    line-height: 2.5rem;
    font-family: 'Georgia', serif;
    color: #2c3e50;
  }
  .mad-lib-input {
    display: inline-block;
    border-bottom: 2px dashed #3498db;
    margin: 0 5px;
    font-weight: bold;
    color: #3498db;
    cursor: pointer;
  }
  .mad-lib-input .selectize-input {
    border: none !important;
    box-shadow: none !important;
    background: transparent !important;
    font-weight: bold;
    color: #3498db;
    padding: 0 !important;
    font-size: 1.5rem !important;
    line-height: 1.5rem !important;
  }
  .mad-lib-input .selectize-dropdown {
    font-size: 1rem;
    font-weight: normal;
  }
  .mad-lib-input input {
    display: none !important;
  }
  .mad-lib-static {
    font-weight: bold;
    color: #e74c3c;
  }
  .section-card {
    margin-bottom: 3rem;
    border: none;
    background: transparent;
  }
  .big-number {
    font-size: 3rem;
    font-weight: 800;
    color: #2c3e50;
  }
  .settings-btn {
    position: absolute;
    top: 1rem;
    right: 1rem;
    font-size: 1.5rem;
    color: #95a5a6;
    cursor: pointer;
  }
  .settings-btn:hover {
    color: #2c3e50;
  }
"

ui <- page_fluid(
    theme = bs_theme(version = 5, bootswatch = "litera"),
    tags$head(tags$style(HTML(mad_libs_css))),

    # Settings Button
    actionButton("settings_btn", icon("cog"), class = "settings-btn", title = "Settings"),
    div(
        class = "container", style = "max-width: 800px; margin-top: 3rem;",

        # --- Header ---
        h1("ShinyAESC", style = "font-weight: 900; letter-spacing: -1px;"),
        p("An interactive story about your data.", style = "color: #7f8c8d; margin-bottom: 2rem;"),

        # --- Chapter 1: The Question (Mad Libs) ---
        div(
            class = "card section-card",
            div(
                class = "card-body",
                div(
                    class = "mad-lib-container",
                    "I want to see how",
                    div(
                        class = "mad-lib-input", style = "width: 200px;",
                        selectInput("predictorVar", NULL, choices = NULL, width = "100%")
                    ),
                    "predicts",
                    div(
                        class = "mad-lib-input", style = "width: 200px;",
                        selectInput("criterionVar", NULL, choices = NULL, width = "100%")
                    ),
                    "."
                ),
                div(
                    style = "margin-top: 1rem; font-size: 0.9rem; color: #95a5a6;",
                    fileInput("file1", "Upload your own data (CSV, Excel, SPSS)",
                        accept = c(".csv", ".xlsx", ".sav"),
                        buttonLabel = "Change Data", placeholder = "Using Sample Data"
                    )
                )
            )
        ),

        # --- Chapter 2: The Landscape ---
        div(
            class = "section-card",
            h3("1. The Landscape"),
            p("First, let's look at the raw relationship. Each point is a person."),
            plotlyOutput("plot_scatter", height = "400px"),
            div(
                style = "text-align: center; margin-top: 1rem;",
                htmlOutput("correlation_text")
            )
        ),

        # --- Chapter 3: The Prediction ---
        div(
            class = "section-card",
            h3("2. The Prediction"),
            div(
                class = "mad-lib-container",
                "If we select people in the top",
                div(
                    class = "mad-lib-input", style = "width: 100px; vertical-align: middle;",
                    sliderInput("cutoff_pct", NULL, min = 1, max = 99, value = 50, step = 1, ticks = FALSE, post = "%")
                ),
                "of", textOutput("pred_name_inline", inline = TRUE), "..."
            ),
            div(
                class = "mad-lib-container", style = "margin-top: 1rem;",
                "...how many of them will",
                div(
                    class = "mad-lib-input", style = "width: 150px;",
                    textInput("outcome_verb", NULL, value = "succeed")
                ),
                "?"
            ),
            div(
                class = "row", style = "margin-top: 2rem;",
                div(
                    class = "col-md-6",
                    div(
                        class = "card", style = "background: #f8f9fa;",
                        div(
                            class = "card-body text-center",
                            h5(textOutput("label_success_top")),
                            div(class = "big-number", textOutput("success_rate_top")),
                            p("of selected people", textOutput("outcome_verb_1", inline = TRUE), ".")
                        )
                    )
                ),
                div(
                    class = "col-md-6",
                    div(
                        class = "card", style = "background: #f8f9fa;",
                        div(
                            class = "card-body text-center",
                            h5(textOutput("label_success_bottom")),
                            div(class = "big-number", textOutput("success_rate_bottom")),
                            p("of unselected people", textOutput("outcome_verb_2", inline = TRUE), ".")
                        )
                    )
                )
            ),
            div(
                class = "mad-lib-container", style = "margin-top: 1rem; font-size: 1.2rem;",
                span(textOutput("outcome_noun", inline = TRUE), style = "font-weight:bold; text-transform: capitalize;"),
                "is defined as having", textOutput("crit_name_inline", inline = TRUE),
                div(
                    class = "mad-lib-input", style = "width: 120px;",
                    selectInput("direction", NULL, choices = c("above", "below"), selected = "above")
                ),
                "average."
            ),
            uiOutput("cles_box")
        ),

        # --- Chapter 4: The Breakdown (Expectancy Chart) ---
        div(
            class = "section-card",
            h3("3. The Breakdown"),
            p("Let's look at the probability of", textOutput("outcome_noun_2", inline = TRUE), "across the entire range of scores."),
            plotlyOutput("plot_expectancy", height = "350px")
        ),

        # --- Chapter 5: The Impact (Icon Array) ---
        div(
            class = "section-card",
            h3("4. The Impact"),
            p("Imagine 100 people from the top group. Here is what their outcomes look like:"),
            plotOutput("icon_array", height = "300px"),
            div(
                style = "text-align: center; color: #7f8c8d; font-size: 0.9rem;",
                uiOutput("icon_legend")
            )
        ),

        # --- Chapter 6: Summary Statistics ---
        div(
            class = "section-card",
            h3("5. Summary Statistics"),
            tableOutput("summary_stats")
        ),
        div(style = "height: 100px;") # Spacing at bottom
    )
)

server <- function(input, output, session) {
    # --- Settings Modal ---
    observeEvent(input$settings_btn, {
        showModal(modalDialog(
            title = "Analysis Settings",
            textInput("label_failure", "Label for 'Failure' (Opposite of Outcome)", value = "Failure"),
            colourpicker::colourInput("col_success", "Color for Outcome", value = "#3498db"),
            colourpicker::colourInput("col_failure", "Color for Opposite", value = "#bdc3c7"),
            numericInput("n_bins", "Number of Bins (Expectancy Chart)", value = 5, min = 3, max = 10),
            easyClose = TRUE,
            footer = modalButton("Close")
        ))
    })

    # Defaults for settings if not yet set
    settings <- reactive({
        list(
            label_failure = if (is.null(input$label_failure)) "Failure" else input$label_failure,
            col_success = if (is.null(input$col_success)) "#3498db" else input$col_success,
            col_failure = if (is.null(input$col_failure)) "#bdc3c7" else input$col_failure,
            n_bins = if (is.null(input$n_bins)) 5 else input$n_bins
        )
    })

    # --- Data Loading ---
    data_set <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) {
            if (file.exists("sampleData.csv")) {
                return(read.csv("sampleData.csv", header = TRUE))
            }
            return(NULL)
        }
        ext <- tools::file_ext(inFile$name)
        tryCatch(
            {
                switch(ext,
                    csv = read.csv(inFile$datapath, header = TRUE),
                    xlsx = readxl::read_excel(inFile$datapath),
                    sav = haven::read_sav(inFile$datapath),
                    NULL
                )
            },
            error = function(e) NULL
        )
    })

    observe({
        df <- data_set()
        if (!is.null(df)) {
            updateSelectInput(session, "predictorVar", choices = names(df), selected = names(df)[1])
            updateSelectInput(session, "criterionVar", choices = names(df), selected = names(df)[2])
        }
    })

    selected_data <- reactive({
        req(data_set(), input$predictorVar, input$criterionVar)
        df <- data_set()
        req(input$predictorVar %in% names(df), input$criterionVar %in% names(df))
        df %>%
            select(Predictor = !!sym(input$predictorVar), Criterion = !!sym(input$criterionVar)) %>%
            na.omit()
    })

    # --- Inline Text Helpers ---
    output$pred_name_inline <- renderText({
        input$predictorVar
    })
    output$crit_name_inline <- renderText({
        input$criterionVar
    })
    output$outcome_verb_1 <- renderText({
        input$outcome_verb
    })
    output$outcome_verb_2 <- renderText({
        input$outcome_verb
    })
    output$outcome_noun <- renderText({
        input$outcome_verb
    }) # Using verb as noun for now (e.g. "Theft" works for both)
    output$outcome_noun_2 <- renderText({
        input$outcome_verb
    })

    output$label_success_top <- renderText({
        paste(tools::toTitleCase(input$outcome_verb), "Rate (Top Group)")
    })
    output$label_success_bottom <- renderText({
        paste(tools::toTitleCase(input$outcome_verb), "Rate (Bottom Group)")
    })

    # --- Stats ---
    stats <- reactive({
        df <- selected_data()
        co_X_prob <- 1 - (input$cutoff_pct / 100)
        co_X <- quantile(df$Predictor, probs = co_X_prob, na.rm = TRUE)
        co_Y <- mean(df$Criterion, na.rm = TRUE)

        # Direction Logic
        is_above <- input$direction == "above"

        df <- df %>%
            mutate(
                Selected = Predictor >= co_X,
                Success = if (is_above) Criterion > co_Y else Criterion < co_Y
            )

        rate_top <- mean(df$Success[df$Selected])
        rate_bottom <- mean(df$Success[!df$Selected])

        # CLES Calculation (Direction Aware)
        # If "Success" is high score, CLES is P(Top > Bottom).
        # If "Success" is low score (e.g. Theft), CLES should reflect P(Top < Bottom) or similar?
        # Actually, CLES is usually defined as P(X1 > X2).
        # If we want "Probability that Top Group has MORE of the outcome", it depends on the outcome.
        # If Outcome is "Theft" (Low Integrity), we want P(Top Group < Bottom Group) maybe?
        # Let's stick to the standard CLES definition: P(Top > Bottom) on the raw Criterion metric.
        # But we should explain it carefully in the text box.

        m1 <- mean(df$Criterion[df$Selected], na.rm = T)
        m2 <- mean(df$Criterion[!df$Selected], na.rm = T)
        sd_pooled <- sqrt((var(df$Criterion[df$Selected], na.rm = T) + var(df$Criterion[!df$Selected], na.rm = T)) / 2)
        d <- (m1 - m2) / sd_pooled
        cles <- pnorm(d / sqrt(2))

        list(
            co_X = co_X,
            co_Y = co_Y,
            rate_top = rate_top,
            rate_bottom = rate_bottom,
            r = cor(df$Predictor, df$Criterion),
            cles = cles,
            is_above = is_above
        )
    })

    # --- Outputs ---

    output$plot_scatter <- renderPlotly({
        df <- selected_data()
        s <- stats()
        sett <- settings()

        df <- df %>%
            mutate(Group = ifelse(Predictor >= s$co_X, "Selected (Top)", "Not Selected"))

        p <- ggplot(df, aes(x = Predictor, y = Criterion, color = Group)) +
            geom_point(alpha = 0.6, size = 2) +
            scale_color_manual(values = c("Selected (Top)" = sett$col_success, "Not Selected" = sett$col_failure)) +
            geom_smooth(method = "lm", se = FALSE, color = "#2c3e50", size = 0.8, aes(group = 1)) +
            geom_vline(xintercept = s$co_X, linetype = "dashed", color = sett$col_success) +
            geom_hline(yintercept = s$co_Y, linetype = "dotted", color = "#7f8c8d") +
            labs(x = input$predictorVar, y = input$criterionVar) +
            theme_minimal() +
            theme(legend.position = "top")

        ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = 1.1))
    })

    output$correlation_text <- renderUI({
        r <- round(stats()$r, 2)
        strength <- case_when(
            abs(r) < 0.1 ~ "no real",
            abs(r) < 0.3 ~ "a weak",
            abs(r) < 0.5 ~ "a moderate",
            TRUE ~ "a strong"
        )
        HTML(paste0("There is <span class='mad-lib-static'>", strength, " correlation</span> (r = ", r, ") between these variables."))
    })

    output$success_rate_top <- renderText({
        paste0(round(stats()$rate_top * 100), "%")
    })
    output$success_rate_bottom <- renderText({
        paste0(round(stats()$rate_bottom * 100), "%")
    })

    output$cles_box <- renderUI({
        s <- stats()
        sett <- settings()

        # Adjust CLES text based on direction
        # If direction is "below", then "Success" means LOWER score.
        # But CLES calculation (pnorm(d/sqrt(2))) gives prob that Group 1 > Group 2.
        # If d is negative (Group 1 < Group 2), CLES < 0.5.
        # We want to phrase it intuitively.

        prob_higher <- round(s$cles * 100, 1)

        div(
            class = "alert alert-info", style = "margin-top: 2rem; border-left: 5px solid #3498db; background-color: #eaf2f8;",
            h5(icon("info-circle"), "Common Language Effect Size", style = "color: #2980b9; font-weight: bold;"),
            p(
                style = "font-size: 1.1rem; margin-bottom: 0;",
                "A randomly chosen person from the ", span(style = paste0("color:", sett$col_success, "; font-weight:bold;"), "Top Group"),
                " has a ", span(class = "mad-lib-static", paste0(prob_higher, "%")),
                " chance of having a higher ", input$criterionVar, " score than a randomly chosen person from the ",
                span(style = paste0("color:", sett$col_failure, "; font-weight:bold;"), "Bottom Group"), "."
            )
        )
    })

    output$plot_expectancy <- renderPlotly({
        df <- selected_data()
        s <- stats()
        sett <- settings()
        bins <- sett$n_bins

        df_exp <- df %>%
            mutate(
                ntile_X = ntile(Predictor, bins),
                Success = as.numeric(if (s$is_above) Criterion > s$co_Y else Criterion < s$co_Y)
            ) %>%
            group_by(ntile_X) %>%
            summarise(
                proportion = mean(Success),
                lower = min(Predictor),
                upper = max(Predictor)
            ) %>%
            mutate(
                label = paste0(round(lower, 1), " - ", round(upper, 1))
            )

        p <- ggplot(df_exp, aes(x = reorder(label, ntile_X), y = proportion, text = paste("Range:", label, "<br>Rate:", round(proportion * 100, 1), "%"))) +
            geom_bar(stat = "identity", fill = sett$col_success) +
            scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
            labs(x = paste(input$predictorVar, "Range"), y = paste("Probability of", tools::toTitleCase(input$outcome_verb))) +
            theme_minimal()

        ggplotly(p, tooltip = "text")
    })

    output$icon_legend <- renderUI({
        sett <- settings()
        HTML(paste0(
            "<span style='color:", sett$col_success, "; font-size: 1.2rem;'>●</span> ", tools::toTitleCase(input$outcome_verb),
            "<span style='color:", sett$col_failure, "; font-size: 1.2rem; margin-left: 1rem;'>●</span> ", sett$label_failure
        ))
    })

    output$icon_array <- renderPlot({
        rate <- round(stats()$rate_top * 100)
        sett <- settings()

        dots <- expand.grid(x = 1:10, y = 1:10)
        dots <- dots %>% arrange(y, x)
        dots$Success <- c(rep(TRUE, rate), rep(FALSE, 100 - rate))

        ggplot(dots, aes(x = x, y = y, color = Success)) +
            geom_point(size = 10) +
            scale_color_manual(values = c("TRUE" = sett$col_success, "FALSE" = sett$col_failure)) +
            theme_void() +
            theme(legend.position = "none") +
            coord_fixed() +
            labs(title = paste0(rate, " out of 100 selected people ", input$outcome_verb)) +
            theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"))
    })

    output$summary_stats <- renderTable(
        {
            s <- stats()
            df <- selected_data()

            # Calculate Cohen's d
            m1 <- mean(df$Criterion[df$Predictor >= s$co_X], na.rm = T)
            m2 <- mean(df$Criterion[df$Predictor < s$co_X], na.rm = T)
            sd_pooled <- sqrt((var(df$Criterion[df$Predictor >= s$co_X], na.rm = T) + var(df$Criterion[df$Predictor < s$co_X], na.rm = T)) / 2)
            d <- (m1 - m2) / sd_pooled

            # CLES
            cles <- pnorm(d / sqrt(2))

            # Base Rate
            base_rate <- mean(if (s$is_above) df$Criterion > s$co_Y else df$Criterion < s$co_Y, na.rm = T)

            data.frame(
                Statistic = c("Correlation (r)", "Cohen's d", "Common Language Effect Size (CLES)", "Overall Base Rate"),
                Value = c(
                    round(s$r, 3),
                    round(d, 3),
                    paste0(round(cles * 100, 1), "%"),
                    paste0(round(base_rate * 100, 1), "%")
                )
            )
        },
        width = "100%"
    )
}

shinyApp(ui, server)
