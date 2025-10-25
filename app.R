#### Sterb's NFL QB Negative Plays Analyzer (2015–present) ####

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(bslib)
library(DT)
library(rlang)
library(scales)
library(readr)

`%||%` <- function(x, y) if (is.null(x)) y else x

#### Theme ####
sterb_theme <- function(base_size = 12, base_family = "Roboto"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position = "plot",
      plot.title    = element_text(face = "bold", hjust = 0.5, colour = "black"),
      plot.subtitle = element_text(hjust = 0.5, colour = "black"),
      axis.title    = element_text(face = "bold", colour = "black"),
      axis.text     = element_text(face = "bold", colour = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#d0d0d0"),
      plot.background  = element_rect(fill = "#f7f7f7", colour = NA),
      panel.background = element_rect(fill = "#f7f7f7", colour = NA)
    )
}

SEASON_RANGE <- 2015:2025

#### UI ####
ui <- fluidPage(
  title = "Sterb's NFL QB Negative Plays Analyzer",
  theme = bs_theme(
    version = 5,
    bg  = "#2B2B2B",
    fg  = "rgb(234,234,234)",
    primary = "#43B6FF",
    secondary = "#F0F0F0",
    success = "#28B62C",
    info    = "#75CAEB",
    warning = "#FF851B"
  ),
  tags$head(
    tags$title("Sterb's NFL QB Negative Plays Analyzer"),
    tags$style(HTML("
      .form-label, .control-label, label, .form-check-label { color: rgba(234,234,234,.95) !important; }
      .form-control, .form-select { color: rgba(234,234,234,.95) !important; }
      .form-control::placeholder { color: rgba(234,234,234,.7) !important; opacity:1 !important; }
      .selectize-input, .selectize-input input { color: rgba(234,234,234,.95) !important; }
      .selectize-control .item,
      .selectize-control.single .selectize-input > .item { color: rgba(234,234,234,.95) !important; }
      .selectize-input > input::placeholder { color: rgba(234,234,234,.7) !important; opacity:1 !important; }
      .selectize-dropdown .option { color: rgba(234,234,234,.95) !important; }
      table.dataTable thead th, table.dataTable tbody td { text-align:center !important; }
    "))
  ),
  div(style = "padding-top:10px;", titlePanel("Sterb's NFL QB Negative Plays Analyzer")),
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("season", "Season(s)", choices = as.character(SEASON_RANGE),
                  selected = as.character(max(SEASON_RANGE)), multiple = TRUE),
      tags$small("Hold Ctrl/Cmd to multi-select."), br(), br(),
      checkboxGroupInput("season_type", "Season Type",
                         choices = c("Regular" = "REG", "Postseason" = "POST"),
                         selected = c("REG")),
      uiOutput("weeks_ui"),
      checkboxGroupInput("downs", "Downs", choices = c(1,2,3,4),
                         selected = c(1,2,3,4), inline = TRUE),
      checkboxGroupInput("quarters", "Quarters",
                         choices = c("1"=1,"2"=2,"3"=3,"4"=4,"OT"=5),
                         selected = c(1,2,3,4,5), inline = TRUE),
      sliderInput("wp_range", "Win Probability range",
                  min = 0, max = 1, value = c(0.05, 0.95), step = 0.01),
      sliderInput("xpass_range", "Dropback Prob %",
                  min = 0, max = 1, value = c(0, 1), step = 0.01),
      numericInput("min_dropbacks", "Min default dropbacks for qualification",
                   value = 200, min = 1, max = 5000, step = 1),
      uiOutput("highlight_ui"),
      actionButton("recalc","Recalculate", class = "btn btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chart",
                 br(),
                 downloadButton("dl_plot", "Download Plot (PNG)", class = "btn btn-secondary"),
                 br(), br(),
                 plotOutput("neg_scatter", height = "800px")),
        tabPanel("Summary", DTOutput("tbl_summary")),
        tabPanel("About",
                 tags$pre("Negative plays = sacks + interceptions + fumbles on QB dropbacks.
If multiple seasons are selected, each QB is a single dot (aggregated across seasons)."))
      )
    )
  )
)

#### Server ####
server <- function(input, output, session){
  
  #### Data Load ####
  pbp_all <- read_csv("data/condensed.csv", show_col_types = FALSE)
  teams <- nflreadr::load_teams() %>% select(team_abbr, team_color, team_color2)
  
  fmt_vec   <- function(v) paste0(unique(v), collapse = ",")
  fmt_range <- function(v, digits = 2){
    if (length(v) == 2) sprintf("%s–%s", signif(v[1], digits), signif(v[2], digits))
    else paste(v, collapse = ",")
  }
  
  #### Reactive Bases ####
  pbp_base_rx <- reactive({
    req(input$season)
    pbp_all %>% filter(season %in% as.integer(input$season))
  })
  
  output$weeks_ui <- renderUI({
    pbp <- pbp_base_rx(); req(nrow(pbp) > 0, input$season_type)
    wk_df <- pbp %>% filter(season_type %in% input$season_type, !is.na(week)) %>%
      distinct(week) %>% arrange(week)
    min_wk <- min(wk_df$week, na.rm = TRUE)
    max_wk <- max(wk_df$week, na.rm = TRUE)
    cur_ws <- isolate(input$week_start) %||% min_wk
    cur_we <- isolate(input$week_end)   %||% max_wk
    cur_ws <- max(min_wk, min(cur_ws, max_wk))
    cur_we <- max(min_wk, min(cur_we, max_wk))
    fluidRow(
      column(6, selectInput("week_start","Start Week",
                            choices = min_wk:max_wk, selected = cur_ws)),
      column(6, selectInput("week_end","End Week",
                            choices = min_wk:max_wk, selected = cur_we))
    )
  })
  
  baseline_plays_rx <- reactive({
    pbp <- pbp_base_rx(); req(nrow(pbp) > 0, input$season_type)
    ws <- input$week_start %||% min(pbp$week, na.rm = TRUE)
    we <- input$week_end   %||% max(pbp$week, na.rm = TRUE)
    ws <- min(as.integer(ws), as.integer(we)); we <- max(as.integer(ws), as.integer(we))
    pbp %>%
      filter(season_type %in% input$season_type,
             !is.na(week), week >= ws, week <= we,
             down %in% 1:4,
             qtr %in% 1:5,
             qb_dropback == 1,
             !is.na(wp), wp >= 0.05, wp <= 0.95)
  })
  
  observeEvent(list(input$season, input$season_type, input$week_start, input$week_end), {
    pbp <- pbp_base_rx(); req(nrow(pbp) > 0)
    seasons_sel <- as.integer(input$season)
    st_sel <- input$season_type %||% character(0)
    ws <- input$week_start %||% min(pbp$week, na.rm = TRUE)
    we <- input$week_end   %||% max(pbp$week, na.rm = TRUE)
    ws <- min(as.integer(ws), as.integer(we)); we <- max(as.integer(ws), as.integer(we))
    
    reg_weeks_total <- pbp %>%
      filter(season %in% seasons_sel, season_type == "REG", !is.na(week), week >= ws, week <= we) %>%
      summarise(total_weeks = n_distinct(week)) %>% pull(total_weeks) %||% 0L
    
    n_seasons <- length(unique(seasons_sel))
    default_db <- if (identical(sort(st_sel), "POST")) {
      10L * n_seasons
    } else if (identical(sort(st_sel), "REG")) {
      10L * as.integer(reg_weeks_total)
    } else {
      10L * as.integer(reg_weeks_total + n_seasons)
    }
    updateNumericInput(session, "min_dropbacks", value = max(1L, as.integer(default_db)))
  }, ignoreInit = FALSE)
  
  qualified_ids_rx <- reactive({
    base <- baseline_plays_rx(); req(nrow(base) > 0)
    thr <- input$min_dropbacks %||% 1L
    base %>% count(passer_id, name = "db") %>% filter(db >= thr) %>% pull(passer_id)
  })
  
  plays_rx <- reactive({
    req(input$season, input$season_type, input$downs, input$quarters, input$wp_range, input$xpass_range)
    pbp <- pbp_base_rx(); if (!nrow(pbp)) return(pbp[0, ])
    ws <- input$week_start %||% min(pbp$week, na.rm = TRUE)
    we <- input$week_end   %||% max(pbp$week, na.rm = TRUE)
    ws <- min(as.integer(ws), as.integer(we)); we <- max(as.integer(ws), as.integer(we))
    pbp %>%
      filter(season_type %in% input$season_type,
             !is.na(week), week >= ws, week <= we,
             down %in% as.integer(input$downs),
             qtr %in% as.integer(input$quarters),
             qb_dropback == 1,
             !is.na(wp), wp >= input$wp_range[1], wp <= input$wp_range[2]) %>%
      {rng <- input$xpass_range; if (isTRUE(rng[1] > 0 || rng[2] < 1))
        filter(., !is.na(xpass), xpass >= rng[1], xpass <= rng[2]) else .}
  })
  
  mode_team <- function(x){
    if (!length(x)) return(NA_character_)
    tb <- sort(table(x), decreasing = TRUE)
    names(tb)[1]
  }
  
  qb_summary_rx <- reactive({
    pbp <- plays_rx(); ids <- qualified_ids_rx()
    if (!nrow(pbp) || !length(ids)) return(tibble())
    pbp %>%
      group_by(passer_id, passer) %>%
      summarise(
        posteam = mode_team(na.omit(posteam)),
        sacks   = sum(sack, na.rm = TRUE),
        sack_epa = sum(sack * epa, na.rm = TRUE),
        sack_wpa = sum(sack * wpa, na.rm = TRUE),
        ints    = sum(interception, na.rm = TRUE),
        int_epa = sum(interception * epa, na.rm = TRUE),
        int_wpa = sum(interception * wpa, na.rm = TRUE),
        fums    = sum(fumble, na.rm = TRUE),
        fum_epa = sum(fumble * epa, na.rm = TRUE),
        fum_wpa = sum(fumble * wpa, na.rm = TRUE),
        dropbacks = n(),
        .groups = "drop"
      ) %>%
      mutate(
        neg_plays = sacks + ints + fums,
        neg_rate  = neg_plays / dropbacks,
        neg_epa   = sack_epa + int_epa + fum_epa,
        neg_wpa   = sack_wpa + int_wpa + fum_wpa,
        neg_epa_per_db = neg_epa / dropbacks,
        neg_wpa_per_db = neg_wpa / dropbacks
      ) %>%
      left_join(teams, by = c("posteam" = "team_abbr")) %>%
      filter(passer_id %in% ids)
  })
  
  #### Outputs ####
  output$highlight_ui <- renderUI({
    dat <- qb_summary_rx()
    if (!nrow(dat)) return(NULL)
    selectizeInput("highlight", "Highlight QB (optional)",
                   choices = setNames(dat$passer_id, dat$passer),
                   selected = character(0),
                   options = list(placeholder = "Search…", onInitialize = I('function(){ this.clear(true); }')))
  })
  
  title_bits_rx <- reactive({
    pbp <- pbp_base_rx()
    seasons_txt <- fmt_vec(input$season)
    st_txt <- fmt_vec(input$season_type)
    ws <- input$week_start %||% min(pbp$week, na.rm = TRUE)
    we <- input$week_end   %||% max(pbp$week, na.rm = TRUE)
    wk_txt <- sprintf("Weeks %s–%s", ws, we)
    wp_txt <- sprintf("WP %s", fmt_range(input$wp_range, 2))
    xp_txt <- sprintf("Dropback Prob: %s%%–%s%%",
                      round(100*input$xpass_range[1]), round(100*input$xpass_range[2]))
    dn_txt <- paste0("D", paste(sort(as.integer(input$downs)), collapse = "/"))
    qt_map <- c(`1`="Q1", `2`="Q2", `3`="Q3", `4`="Q4", `5`="OT")
    qt_txt <- paste(qt_map[as.character(sort(as.integer(input$quarters)))], collapse = "/")
    sub <- sprintf("%s | %s | %s | %s | %s | %s",
                   seasons_txt, st_txt, wk_txt, wp_txt, xp_txt, paste(dn_txt, qt_txt, sep=" | "))
    list(
      subtitle = sub,
      filename_stub = sprintf("qb_negplays_%s_%s_w%s-%s_wp%s_xp%s_%s_%s_min%s",
                              gsub(",", "-", seasons_txt),
                              gsub(",", "-", st_txt),
                              ws, we,
                              gsub("\\.", "", fmt_range(input$wp_range, 2)),
                              paste0(round(100*input$xpass_range[1]),"-",round(100*input$xpass_range[2])),
                              gsub("/", "-", dn_txt),
                              gsub("/", "-", qt_txt),
                              input$min_dropbacks)
    )
  })
  
  x_breaks_fixed <- function(xmin, xmax){
    lo <- floor(xmin / 0.10) * 0.10
    hi <- ceiling(xmax / 0.10) * 0.10
    seq(lo, hi, by = 0.10)
  }
  y_breaks_quarterpct <- function(ymin, ymax){
    lo <- floor(ymin / 0.0025) * 0.0025
    hi <- ceiling(ymax / 0.0025) * 0.0025
    seq(lo, hi, by = 0.0025)
  }
  
  #### Plot ####
  neg_plot <- reactive({
    dat <- qb_summary_rx(); validate(need(nrow(dat) > 0, "No data for current filters."))
    xbar <- mean(dat$neg_epa_per_db, na.rm = TRUE)
    ybar <- mean(dat$neg_wpa_per_db, na.rm = TRUE)
    is_hi <- if (!is.null(input$highlight) && nzchar(input$highlight)) dat$passer_id == input$highlight else rep(FALSE, nrow(dat))
    label_col <- ifelse(is_hi, "red3", "black")
    bits <- title_bits_rx()
    xr <- range(dat$neg_epa_per_db, na.rm = TRUE)
    yr <- range(dat$neg_wpa_per_db, na.rm = TRUE)
    
    ggplot(dat, aes(x = neg_epa_per_db, y = neg_wpa_per_db)) +
      geom_hline(yintercept = ybar, linetype = "dashed", color = "grey16") +
      geom_vline(xintercept = xbar, linetype = "dashed", color = "grey16") +
      geom_point(aes(size = dropbacks),
                 shape = 21,
                 fill  = dat$team_color %||% "#999999",
                 color = dat$team_color2 %||% "#333333",
                 stroke = 1, alpha = 0.95) +
      ggrepel::geom_text_repel(aes(label = passer),
                               size = 4, box.padding = 0.64,
                               color = label_col, max.overlaps = 200) +
      scale_size_continuous(range = c(3, 10)) +
      scale_x_continuous(breaks = x_breaks_fixed(xr[1], xr[2]),
                         labels = function(v) sprintf("%.2f", v)) +
      scale_y_continuous(breaks = y_breaks_quarterpct(yr[1], yr[2]),
                         labels = percent_format(accuracy = 0.01)) +
      labs(
        title = "QB Negative Plays (INT, FUM, SK): EPA/WPA per Dropback",
        subtitle = bits$subtitle,
        x = "Negative EPA / Dropback",
        y = "Negative WPA / Dropback",
        caption = "Data via nflreadr | App & Plot: @EthanSterbis"
      ) +
      sterb_theme(base_size = 14, base_family = "Roboto") +
      theme(legend.position = "none")
  })
  output$neg_scatter <- renderPlot({ neg_plot() }, res = 110)
  
  #### Summary Table ####
  output$tbl_summary <- DT::renderDT({
    base_dat <- qb_summary_rx()
    if (!nrow(base_dat)) {
      return(DT::datatable(data.frame(Message = "No data for current filters."),
                           rownames = FALSE, options = list(dom = "t")))
    }
    
    dat <- base_dat %>%
      mutate(
        INT_RATE = ints / dropbacks,
        FUM_RATE = fums / dropbacks,
        SK_RATE  = sacks / dropbacks,
        INT_EPA_DB = int_epa / dropbacks,
        FUM_EPA_DB = fum_epa / dropbacks,
        SK_EPA_DB  = sack_epa / dropbacks,
        INT_WPA_DB = int_wpa / dropbacks,
        FUM_WPA_DB = fum_wpa / dropbacks,
        SK_WPA_DB  = sack_wpa / dropbacks
      ) %>%
      arrange(desc(neg_epa_per_db)) %>%
      mutate(RANK = row_number()) %>%
      select(
        RANK, PLAYER = passer, TEAM = posteam, DROPBACKS = dropbacks,
        NEG_RATE = neg_rate, NEG_EPA_DB = neg_epa_per_db, NEG_WPA_DB = neg_wpa_per_db,
        INT_RATE, FUM_RATE, SK_RATE, INT_EPA_DB, FUM_EPA_DB, SK_EPA_DB,
        INT_WPA_DB, FUM_WPA_DB, SK_WPA_DB,
        NEG_PLAYS = neg_plays, INTS = ints, FUMS = fums, SACKS = sacks
      )
    
    tbl <- DT::datatable(dat, rownames = FALSE,
                         options = list(pageLength = 25,
                                        order = list(list(0, "asc")),
                                        class = "cell-border stripe hover",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    tbl <- DT::formatRound(tbl, c("DROPBACKS","NEG_PLAYS","INTS","FUMS","SACKS"), 0)
    tbl <- DT::formatPercentage(tbl, c("NEG_RATE","INT_RATE","FUM_RATE","SK_RATE"), 2)
    tbl <- DT::formatRound(tbl, c("NEG_EPA_DB","NEG_WPA_DB","INT_EPA_DB","FUM_EPA_DB","SK_EPA_DB",
                                  "INT_WPA_DB","FUM_WPA_DB","SK_WPA_DB"), 4)
    tbl
  })
  
  #### Download ####
  output$dl_plot <- downloadHandler(
    filename = function(){ paste0(title_bits_rx()$filename_stub, ".png") },
    content  = function(file){ ggsave(file, plot = neg_plot(), width = 12, height = 8, dpi = "retina") }
  )
}

#### Run App ####
shinyApp(ui, server)
