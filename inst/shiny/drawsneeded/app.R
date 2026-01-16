library(shiny)
library(drawsneeded) # Zorg dat je package geïnstalleerd is
library(tibble)
library(dplyr)
library(readr)
library(rhandsontable)
library(ggplot2)

# ==============================================================================
# HULPFUNCTIES
# ==============================================================================

# 1. Parseer NL tekst (0,05) naar getal (0.05)
parse_dutch_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.null(x) || x == "") return(NA)
  x_char <- as.character(x)
  # parse_number is slim genoeg om punten als duizendtal en komma als decimaal te zien
  readr::parse_number(x_char, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
}

# 2. Format getal naar NL tekst voor grafiek labels (en output)
label_nl <- function(x) {
  format(x, decimal.mark = ",", big.mark = ".", scientific = FALSE, trim = TRUE)
}

# ==============================================================================
# UI
# ==============================================================================

ui <- navbarPage("DrawsNeeded",

                 # CSS voor nette tabellen (zwarte tekst in Handsontable)
                 header = tags$head(
                   tags$style(HTML("
      .handsontable td { color: #000000 !important; }
      .handsontable .htRight { text-align: right; }
    "))
                 ),

                 # --- TAB 1: Calculator ---
                 tabPanel("Calculator",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Parameters"),
                              textInput("calc_posited", "Verwachte Foutfractie:", value = "0,005"),
                              textInput("calc_allowed", "Toelaatbare Foutfractie:", value = "0,05"),
                              textInput("calc_cert", "Betrouwbaarheid (0,95 = 95%):", value = "0,95"),

                              selectInput("calc_dist", "Verdeling:",
                                          choices = c("Binomiaal" = "binomial",
                                                      "Poisson" = "Poisson",
                                                      "Poisson Geïnterpoleerd" = "Poisson_interpolated"))
                            ),
                            mainPanel(
                              h3("Resultaat"),
                              uiOutput("calc_result_ui"),
                              hr(),
                              h4("Gevoeligheidsanalyse"),
                              p("De grafiek toont de benodigde steekproef (y-as) bij oplopende verwachte fouten (x-as)."),
                              plotOutput("plot_sensitivity")
                            )
                          )
                 ),

                 # --- TAB 2: Scenario's (Aangepaste Naam) ---
                 tabPanel("Scenario's",
                          fluidPage(

                            # --- Scenario Generator ---
                            wellPanel(
                              h4("Nieuw Vergelijkings-Groepje Toevoegen"),
                              p("Vul hieronder een scenario in en klik op 'Voeg Groep Toe'. Er worden automatisch 3 regels aangemaakt (voor elke verdeling één)."),
                              fluidRow(
                                column(3, textInput("batch_name", "Naam Scenario:", value = "Scenario A")),
                                column(2, textInput("batch_posited", "Verwacht:", value = "0,005")),
                                column(2, textInput("batch_allowed", "Toelaatbaar:", value = "0,05")),
                                column(2, textInput("batch_cert", "Zekerheid:", value = "0,95")),
                                column(3, br(), actionButton("add_group", "Voeg Groep Toe", class = "btn-info", icon = icon("plus")))
                              )
                            ),

                            h4("Scenario Tabel"),
                            # De tabel
                            rHandsontableOutput("hot_scenarios"),
                            br(),

                            # Knoppen naast elkaar
                            div(style="display:inline-block",
                                actionButton("calc_batch", "Bereken Tabel", class = "btn-primary", icon = icon("calculator"))),
                            div(style="display:inline-block",
                                downloadButton("download_csv", "Download CSV", class = "btn-success")),
                            div(style="display:inline-block; margin-left: 20px;",
                                actionButton("reset_table", "Tabel Leegmaken", class = "btn-warning", icon = icon("trash")))
                          )
                 )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # --- LOGICA TAB 1: Calculator ---

  calc_params <- reactive({
    list(
      posited = parse_dutch_num(input$calc_posited),
      allowed = parse_dutch_num(input$calc_allowed),
      cert    = parse_dutch_num(input$calc_cert),
      dist    = input$calc_dist
    )
  })

  output$calc_result_ui <- renderUI({
    params <- calc_params()

    if (is.na(params$posited) || is.na(params$allowed) || is.na(params$cert)) {
      return(h4("Vul geldige getallen in (bijv. 0,05)", style = "color: grey;"))
    }

    n <- tryCatch({
      drawsneeded(
        posited_defect_rate = params$posited,
        allowed_defect_rate = params$allowed,
        cert = params$cert,
        distribution = params$dist
      )
    }, error = function(e) return(NA))

    if (is.na(n) || is.infinite(n)) {
      return(h2("Resultaat: N.v.t. (Oneindig of Fout)", style = "color: red;"))
    }

    tagList(
      h1(format(n, big.mark = ".", decimal.mark = ","), style = "color: #2c3e50; font-weight: bold;"),
      p(paste0("Benodigde steekproefomvang (n) op basis van ", params$dist, " verdeling."))
    )
  })

  output$plot_sensitivity <- renderPlot({
    params <- calc_params()
    req(params$allowed, params$cert, !is.na(params$posited))

    max_x <- params$allowed * 0.95
    x_vals <- seq(0, max_x, length.out = 100)

    y_vals <- sapply(x_vals, function(x) {
      drawsneeded(x, params$allowed, params$cert, params$dist)
    })

    df_plot <- data.frame(Verwacht = x_vals, n = y_vals)
    curr_point <- df_plot[which.min(abs(df_plot$Verwacht - params$posited)), ]

    ggplot(df_plot, aes(x = Verwacht, y = n)) +
      geom_line(color = "#3498db", linewidth = 1.2) +
      geom_vline(aes(xintercept = params$allowed, linetype = "Toelaatbare Fout"),
                 color = "black", linewidth = 0.8) +
      geom_point(data = curr_point, aes(x = Verwacht, y = n, color = "Huidige Invoer"),
                 size = 4) +

      # KLEUREN & LIJNEN
      scale_linetype_manual(name = "", values = c("Toelaatbare Fout" = "dashed")) +
      scale_color_manual(name = "", values = c("Huidige Invoer" = "red")) +

      # ASSEN
      scale_x_continuous(labels = label_nl) +
      scale_y_continuous(labels = label_nl) +

      # ZOOM: Hier kappen we de y-as af op 2.500
      coord_cartesian(ylim = c(0, 2500)) +

      labs(x = "Verwachte Foutfractie", y = "Steekproefomvang (n)", title = "Gevoeligheidsanalyse") +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.background = element_rect(fill = "white", color = NA)
      )
  })

  # --- LOGICA TAB 2: Scenario's ---

  create_default_table <- function() {
    data.frame(
      Naam = c("Voorbeeld (Bin)", "Voorbeeld (Pois)", "Voorbeeld (PoisInt)"),
      Verwacht = c("0,005", "0,005", "0,005"),
      Toelaatbaar = c("0,05", "0,05", "0,05"),
      Betrouwbaarheid = c("0,95", "0,95", "0,95"),
      Verdeling = c("binomial", "Poisson", "Poisson_interpolated"),
      n_Resultaat = c(NA, NA, NA),
      stringsAsFactors = FALSE
    )
  }

  scenario_data <- reactiveVal(create_default_table())

  output$hot_scenarios <- renderRHandsontable({
    df <- scenario_data()
    rhandsontable(df, stretchH = "all") %>%
      hot_col("Naam", type = "text") %>%
      hot_col("Verwacht", type = "text") %>%
      hot_col("Toelaatbaar", type = "text") %>%
      hot_col("Betrouwbaarheid", type = "text") %>%
      hot_col("Verdeling", type = "dropdown", source = c("binomial", "Poisson", "Poisson_interpolated")) %>%
      hot_col("n_Resultaat", readOnly = TRUE)
  })

  observeEvent(input$add_group, {
    nm <- input$batch_name
    vp <- input$batch_posited
    va <- input$batch_allowed
    vz <- input$batch_cert

    new_rows <- data.frame(
      Naam = paste0(nm, " (", c("Bin", "Pois", "PoisInt"), ")"),
      Verwacht = rep(vp, 3),
      Toelaatbaar = rep(va, 3),
      Betrouwbaarheid = rep(vz, 3),
      Verdeling = c("binomial", "Poisson", "Poisson_interpolated"),
      n_Resultaat = c(NA, NA, NA),
      stringsAsFactors = FALSE
    )

    current_df <- hot_to_r(input$hot_scenarios)
    updated_df <- rbind(current_df, new_rows)
    scenario_data(updated_df)
  })

  observeEvent(input$reset_table, {
    empty_df <- create_default_table()[0, ]
    scenario_data(empty_df)
  })

  observeEvent(input$calc_batch, {
    req(input$hot_scenarios)
    raw_df <- hot_to_r(input$hot_scenarios) %>% as_tibble()

    if(nrow(raw_df) == 0) return()

    res_df <- raw_df %>%
      rowwise() %>%
      mutate(
        p_val = parse_dutch_num(Verwacht),
        a_val = parse_dutch_num(Toelaatbaar),
        c_val = parse_dutch_num(Betrouwbaarheid),

        n_calc = tryCatch({
          drawsneeded(p_val, a_val, c_val, Verdeling)
        }, error = function(e) NA_real_)
      ) %>%
      ungroup() %>%
      mutate(
        n_Resultaat = ifelse(is.infinite(n_calc), "Inf",
                             format(n_calc, big.mark = ".", decimal.mark = ",", scientific = FALSE))
      ) %>%
      select(Naam, Verwacht, Toelaatbaar, Betrouwbaarheid, Verdeling, n_Resultaat)

    scenario_data(res_df)
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste("steekproef_scenarios_", format(Sys.Date(), "%d-%m-%Y"), ".csv", sep = "")
    },
    content = function(file) {
      req(scenario_data())
      write.csv2(scenario_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
