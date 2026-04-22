library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)
library(rlang)

# ---------
# LOAD DATA
# ---------
bcdc_path <- "Data/Community_Vulnerability_(BCDC_2023)/Community_Vulnerability_(BCDC_2023).shp"
shoreline_csv_path <- "Data/Shoreline-Access-Pts_v2-1-attribute-table.csv"
shoreline_shp_path <- "Data/Shoreline_access_points/Shoreline_Access_Pts_v2_1.shp"

bcdc <- st_read(bcdc_path, quiet = TRUE) |>
  st_transform(4326)

shoreline_csv <- read.csv(shoreline_csv_path)
shoreline_shp <- st_read(shoreline_shp_path, quiet = TRUE) |>
  st_transform(4326)

# Make IDs comparable
shoreline_csv <- shoreline_csv |>
  mutate(Access_Point_ID = as.integer(Access_Point_ID))

shoreline_shp <- shoreline_shp |>
  mutate(Access_Poi = as.integer(Access_Poi))

# ----------------
# HELPER FUNCTIONS
# ----------------
safe_ratio <- function(num, den) {
  out <- rep(NA_real_, length(num))
  keep <- !is.na(num) & !is.na(den) & den > 0
  out[keep] <- num[keep] / den[keep]
  out
}

safe_pct_label <- function(x) {
  ifelse(is.na(x), "NA", paste0(round(100 * x, 1), "%"))
}

fmt_num <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "NA",
    format(round(x, digits), big.mark = ",", trim = TRUE, scientific = FALSE)
  )
}

fmt_pct_points <- function(x) {
  ifelse(is.na(x), "NA", paste0(round(x, 1), "%"))
}

minmax01 <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

make_weight_id <- function(prefix, x) {
  paste0(prefix, make.names(x))
}

first_nonmissing <- function(x) {
  x2 <- x[!is.na(x)]
  if (length(x2) == 0) NA else x2[[1]]
}

# Collapse duplicate CSV rows per access point
shoreline_csv_1 <- shoreline_csv |>
  group_by(Access_Point_ID) |>
  summarise(
    across(where(is.numeric), first_nonmissing),
    across(where(is.character), first_nonmissing),
    .groups = "drop"
  )

# Service mode flags
service_modes <- shoreline_csv |>
  group_by(Access_Point_ID) |>
  summarise(
    Walk  = any(Service_Type == "Walk", na.rm = TRUE),
    Bike  = any(Service_Type == "Bike", na.rm = TRUE),
    Drive = any(Service_Type == "Drive", na.rm = TRUE),
    .groups = "drop"
  )

shoreline_csv_1 <- shoreline_csv_1 |>
  left_join(service_modes, by = "Access_Point_ID") |>
  mutate(
    Walk  = replace_na(Walk, FALSE),
    Bike  = replace_na(Bike, FALSE),
    Drive = replace_na(Drive, FALSE)
  )

# Join access-point attributes to geometry
access_pts <- shoreline_shp |>
  left_join(shoreline_csv_1, by = c("Access_Poi" = "Access_Point_ID"))

# ----------------
# FEATURE CATALOGS
# ----------------
demo_catalog <- tibble::tribble(
  ~label, ~est_col, ~moe_col,
  "People of Color",                  "pct_PoC",    "pct_moe_Po",
  "Renters",                          "pct_renter", "pct_moe_re",
  "People Under 5 Years Old",         "pct_under5", "pct_moe_un",
  "Very Low Income (Below 200% FPL)", "pct_B200Pv", "pct_moe_B2",
  "Households Below 50% AMI",         "pct_50Medi", "pct_moe_50",
  "Non-U.S. Citizens",                "pct_NoCtz",  "pct_moe__3",
  "Households Without a Vehicle",     "pct_noVeh",  "pct_moe_no",
  "People with a Disability",         "pct_disabH", "pct_moe_di",
  "Single Parent Families",           "pct_SglPar", "pct_moe_Sg",
  "Seniors (65+) Living Alone",       "pct_65Alon", "pct_moe_65",
  "Limited English Proficiency",      "pct_LEP_HH", "pct_moe_LE",
  "Without High School Degree",       "pct_noHS",   "pct_moe__1",
  "Severely Rent-Cost Burdened",      "pct_RentHC", "pct_moe__2",
  "Severely Owner-Cost Burdened",     "pct_Mortga", "pct_moe_Mo"
)

shoreline_catalog <- tibble::tribble(
  ~metric,               ~label,                              ~deficit,
  "n_points",           "Fewer shoreline access points",    TRUE,
  "walk_pct",           "Lower walk-access share",          TRUE,
  "bike_pct",           "Lower bike-access share",          TRUE,
  "drive_pct",          "Lower drive-access share",         TRUE,
  "transit_stops_sum",  "Lower transit-stop availability",  TRUE,
  "route_count_sum",    "Lower transit-route availability", TRUE,
  "trail_quality_mean", "Lower trail quality",              TRUE
)

# ----------------------------------
# PREP FOR CENTROID-BASED ACCESSIBLE
# ----------------------------------
bcdc_proj <- st_transform(bcdc, 3310)
bg_centroids <- st_centroid(bcdc_proj)
access_pts_proj <- st_transform(access_pts, 3310)

has_transit_stops <- "Public_Transit_Stops" %in% names(access_pts_proj)
has_route_count <- "sum_n_routes" %in% names(access_pts_proj)
has_trail_quality <- "mean_Trail_Quality_Score" %in% names(access_pts_proj)

summarize_access_within_radius <- function(radius_km) {
  idx_list <- st_is_within_distance(
    bg_centroids,
    access_pts_proj,
    dist = radius_km * 1000
  )

  tibble(
    GEOID = bcdc$GEOID,
    idx = idx_list
  ) |>
    mutate(
      n_points = purrr::map_int(idx, length),
      walk_n = purrr::map_int(idx, ~ sum(access_pts_proj$Walk[.x], na.rm = TRUE)),
      bike_n = purrr::map_int(idx, ~ sum(access_pts_proj$Bike[.x], na.rm = TRUE)),
      drive_n = purrr::map_int(idx, ~ sum(access_pts_proj$Drive[.x], na.rm = TRUE)),
      walk_pct = safe_ratio(walk_n, n_points),
      bike_pct = safe_ratio(bike_n, n_points),
      drive_pct = safe_ratio(drive_n, n_points),
      transit_stops_sum = purrr::map_dbl(
        idx,
        ~ if (has_transit_stops && length(.x) > 0) {
          sum(access_pts_proj$Public_Transit_Stops[.x], na.rm = TRUE)
        } else {
          0
        }
      ),
      route_count_sum = purrr::map_dbl(
        idx,
        ~ if (has_route_count && length(.x) > 0) {
          sum(access_pts_proj$sum_n_routes[.x], na.rm = TRUE)
        } else {
          0
        }
      ),
      trail_quality_mean = purrr::map_dbl(
        idx,
        ~ {
          if (!has_trail_quality || length(.x) == 0) return(NA_real_)
          vals <- access_pts_proj$mean_Trail_Quality_Score[.x]
          if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
        }
      )
    ) |>
    select(-idx)
}

# -----
# UI
# -----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      #controls {
        position: absolute; top: 10px; left: 50px; z-index: 1000;
        background: white; padding: 12px 16px; border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.25); width: 360px;
        max-height: 92vh; overflow-y: auto;
      }
      h4 { margin-top: 0; margin-bottom: 10px; font-size: 15px; font-weight: 600; }
      h5 { margin-top: 10px; margin-bottom: 6px; font-size: 13px; font-weight: 600; }
      .legend-note { font-size: 11px; color: #555; margin-top: 6px; line-height: 1.4; }
      .unreliable-note {
        font-size: 11px; color: #c0392b; margin-top: 6px;
        border-left: 3px solid #c0392b; padding-left: 6px;
      }
      .section-note { font-size: 11px; color: #555; line-height: 1.4; }
      .selectize-input { font-size: 13px; }
      .control-label { font-size: 12px; }
      .small-table { font-size: 11px; }
      #detail_panel {
        position: absolute; top: 10px; right: 20px; z-index: 1000;
        background: white; padding: 12px 16px; border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.25); width: 430px;
        max-height: 92vh; overflow-y: auto;
      }
      .panel-close {
        float: right; border: none; background: transparent; font-size: 22px;
        line-height: 1; cursor: pointer; color: #666; padding: 0;
      }
      .panel-close:hover { color: #000; }
    "))
  ),

  leafletOutput("map", width = "100%", height = "100vh"),

  absolutePanel(
    id = "controls", top = 10, left = 50, width = 360,

    h4("SF Bay Shoreline Access Priority Map"),

    div(
      class = "legend-note",
      "Fill color shows a composite priority score built from selected demographic burden ",
      "and shoreline access metrics."
    ),

    div(
      class = "unreliable-note",
      "Red outlines mark block groups where at least one selected demographic indicator has ",
      "margin of error greater than 50% of its estimate."
    ),

    hr(style = "margin: 8px 0;"),

    h5("Demographic indicators"),

    div(
      class = "section-note",
      "These use BCDC block-group percentages as the underlying burden variables."
    ),

    div(
      class = "legend-note",
      " - FPL = Federal Poverty Level", tags$br(),
      " - AMI = Area Median Income", tags$br()
    ),

    selectizeInput(
      "demo_features",
      label = NULL,
      choices = demo_catalog$label,
      selected = c(
        "Households Without a Vehicle",
        "People of Color"
      ),
      multiple = TRUE,
      width = "100%"
    ),

    uiOutput("demo_weight_sliders"),

    hr(style = "margin: 10px 0;"),

    h5("Shoreline access metrics"),

    div(
      class = "section-note",
      "These are summarized from shoreline access points within the selected distance of each ",
      "block-group centroid. All selected shoreline metrics are treated as deficits, so lower ",
      "access implies higher priority."
    ),

    selectizeInput(
      "shoreline_features",
      label = NULL,
      choices = setNames(shoreline_catalog$metric, shoreline_catalog$label),
      selected = c("transit_stops_sum"),
      multiple = TRUE,
      width = "100%"
    ),

    uiOutput("shoreline_weight_sliders"),

    sliderInput(
      "radius_km",
      "Nearby shoreline access radius (km)",
      min = 0.5,
      max = 10,
      value = 2,
      step = 0.5,
      width = "100%"
    ),

    actionButton("apply_radius", "Apply shoreline settings"),

    div(
      class = "legend-note",
      "Shoreline metric selections, weights, and radius only update after clicking this button."
    ),

    hr(style = "margin: 10px 0;"),

    checkboxInput("show_points", "Show shoreline access points", value = FALSE),

    hr(style = "margin: 10px 0;"),

    uiOutput("map_summary")
  ),

  conditionalPanel(
    condition = "output.detail_panel_visible",
    div(
      id = "detail_panel",
      tags$button(
        type = "button",
        class = "panel-close",
        onclick = "Shiny.setInputValue('close_detail_panel', Math.random(), {priority: 'event'})",
        HTML("&times;")
      ),
      h5("Clicked block group summary"),
      uiOutput("clicked_bg_summary")
    )
  )
)

# ------
# SERVER
# ------
server <- function(input, output, session) {

  detail_open <- reactiveVal(FALSE)

  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)
    detail_open(TRUE)
  })

  observeEvent(input$close_detail_panel, {
    detail_open(FALSE)
  })

  output$detail_panel_visible <- reactive({
    detail_open()
  })
  outputOptions(output, "detail_panel_visible", suspendWhenHidden = FALSE)

  output$demo_weight_sliders <- renderUI({
    req(input$demo_features)

    tagList(
      lapply(input$demo_features, function(lab) {
        sliderInput(
          inputId = make_weight_id("w_demo_", lab),
          label = paste("Weight:", lab),
          min = 0,
          max = 1,
          value = 1,
          step = 0.1,
          width = "100%"
        )
      })
    )
  })

  output$shoreline_weight_sliders <- renderUI({
    if (length(input$shoreline_features %||% character(0)) == 0) {
      return(NULL)
    }

    tagList(
      lapply(input$shoreline_features, function(met) {
        lab <- shoreline_catalog |>
          filter(metric == met) |>
          pull(label)

        sliderInput(
          inputId = make_weight_id("w_shore_", met),
          label = paste("Weight:", lab),
          min = 0,
          max = 1,
          value = 1,
          step = 0.1,
          width = "100%"
        )
      })
    )
  })

  applied_shoreline_settings <- eventReactive(input$apply_radius, {
    feats <- input$shoreline_features %||% character(0)

    if (length(feats) == 0) {
      return(tibble(metric = character(0), weight = numeric(0)))
    }

    tibble(
      metric = feats,
      weight = purrr::map_dbl(
        feats,
        ~ {
          val <- input[[make_weight_id("w_shore_", .x)]]
          if (is.null(val)) 1 else val
        }
      )
    )
  }, ignoreInit = FALSE)

  bg_access_reactive <- eventReactive(input$apply_radius, {
    summarize_access_within_radius(input$radius_km)
  }, ignoreInit = FALSE)

  applied_radius_km <- eventReactive(input$apply_radius, {
    input$radius_km
  }, ignoreInit = FALSE)

  map_data <- reactive({
    shore_settings <- applied_shoreline_settings()
    req(length(input$demo_features) > 0 || nrow(shore_settings) > 0)

    d <- bcdc |>
      left_join(bg_access_reactive(), by = "GEOID") |>
      mutate(
        n_points = replace_na(n_points, 0),
        walk_n = replace_na(walk_n, 0),
        bike_n = replace_na(bike_n, 0),
        drive_n = replace_na(drive_n, 0),
        walk_pct = replace_na(walk_pct, 0),
        bike_pct = replace_na(bike_pct, 0),
        drive_pct = replace_na(drive_pct, 0),
        transit_stops_sum = replace_na(transit_stops_sum, 0),
        route_count_sum = replace_na(route_count_sum, 0)
      )

    unreliable_any <- rep(FALSE, nrow(d))
    demo_score <- rep(0, nrow(d))

    for (lab in input$demo_features) {
      row <- demo_catalog |>
        filter(label == lab)

      est_col <- row$est_col[[1]]
      moe_col <- row$moe_col[[1]]
      est <- as.numeric(d[[est_col]])
      moe <- as.numeric(d[[moe_col]])
      ratio <- ifelse(!is.na(est) & est > 0, moe / est, NA_real_)

      unreliable_any <- unreliable_any | (!is.na(ratio) & ratio > 0.5)

      w <- input[[make_weight_id("w_demo_", lab)]]
      if (!is.null(w) && w > 0) {
        demo_score <- demo_score + w * minmax01(est)
      }

      d[[paste0("est__", make.names(lab))]] <- est
      d[[paste0("moe__", make.names(lab))]] <- moe
      d[[paste0("ratio__", make.names(lab))]] <- ratio
    }

    shore_score <- rep(0, nrow(d))

    for (i in seq_len(nrow(shore_settings))) {
      met <- shore_settings$metric[[i]]
      w <- shore_settings$weight[[i]]

      x <- as.numeric(d[[met]])
      z <- minmax01(x)
      z <- 1 - z

      if (met == "trail_quality_mean") {
        z[is.na(x)] <- 0
      }

      if (!is.na(w) && w > 0) {
        shore_score <- shore_score + w * z
      }
    }

    d |>
      mutate(
        demo_score = demo_score,
        shoreline_score = shore_score,
        score = demo_score + shoreline_score,
        unreliable_any = unreliable_any
      )
  })

  pal <- reactive({
    d <- map_data()
    colorNumeric(
      palette = "viridis",
      domain = d$score,
      na.color = "#cccccc"
    )
  })

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -122.2, lat = 37.7, zoom = 9)
  })

  observe({
    d <- map_data()
    pal_fn <- pal()

    popup_text <- paste0(
      "<strong>Block group:</strong> ", d$GEOID,
      "<br/><strong>Priority score:</strong> ", ifelse(is.na(d$score), "NA", sprintf("%.2f", d$score)),
      "<br/><strong>Nearby access points:</strong> ", fmt_num(d$n_points, 0),
      "<br/><strong>Transit stops:</strong> ", fmt_num(d$transit_stops_sum, 0),
      "<br/><strong>Click block group for full summary</strong>"
    )

    leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      addPolygons(
        data = d,
        layerId = ~GEOID,
        fillColor = ~pal_fn(score),
        fillOpacity = 0.72,
        color = "#666666",
        weight = 0.5,
        opacity = 0.6,
        popup = popup_text
      ) |>
      addPolygons(
        data = d |>
          filter(unreliable_any),
        fill = FALSE,
        color = "#c0392b",
        weight = 2,
        opacity = 1
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal_fn,
        values = d$score,
        title = "Priority score",
        labFormat = labelFormat(digits = 2),
        na.label = "No data",
        opacity = 0.85
      )

    if (isTRUE(input$show_points)) {
      leafletProxy("map") |>
        addCircleMarkers(
          data = access_pts,
          color = "#2F4F4F",
          radius = 2.5,
          stroke = FALSE,
          fillOpacity = 0.6,
          group = "Shoreline access points"
        )
    }
  })

  output$map_summary <- renderUI({
    d <- map_data()

    n_total <- nrow(d)
    n_unreliable <- sum(d$unreliable_any, na.rm = TRUE)
    avg_points <- round(mean(d$n_points, na.rm = TRUE), 2)
    pct_unreliable <- round(100 * n_unreliable / n_total, 1)

    tagList(
      tags$div(
        style = "font-size: 12px; color: #333;",
        tags$b("Map summary"), tags$br(),
        sprintf("Total block groups: %d", n_total), tags$br(),
        sprintf("Block groups outlined as unreliable: %d (%.1f%%)", n_unreliable, pct_unreliable), tags$br(),
        sprintf("Mean nearby shoreline access points per block group: %.2f", avg_points), tags$br(),
        sprintf("Current nearby-access radius: %.1f km", applied_radius_km())
      )
    )
  })

  selected_bg <- reactive({
    click <- input$map_shape_click
    req(click$id)

    map_data() |>
      filter(GEOID == click$id)
  })

  output$clicked_bg_summary <- renderUI({
    if (is.null(input$map_shape_click$id) || !isTRUE(detail_open())) {
      return(NULL)
    }

    row <- selected_bg()

    tagList(
      tags$div(
        style = "font-size: 12px; color: #333; margin-bottom: 8px;",
        tags$b("Block group: "), row$GEOID, tags$br(),
        tags$span(
          style = if (isTRUE(row$unreliable_any)) "color:#c0392b; font-weight:600;" else "color:#2c3e50;",
          if (isTRUE(row$unreliable_any)) {
            "At least one selected demographic indicator is unreliable."
          } else {
            "Selected demographic indicators are reliable."
          }
        )
      ),
      tags$b("Demographic indicators"),
      div(
        class = "small-table",
        tableOutput("demo_detail_table")
      ),
      tags$br(),
      tags$b("Shoreline access metrics"),
      div(
        class = "small-table",
        tableOutput("shore_detail_table")
      )
    )
  })

  output$demo_detail_table <- renderTable({
    req(input$map_shape_click$id)
    row <- selected_bg()

    purrr::map_dfr(input$demo_features, function(lab) {
      est <- row[[paste0("est__", make.names(lab))]][[1]]
      moe <- row[[paste0("moe__", make.names(lab))]][[1]]
      ratio <- row[[paste0("ratio__", make.names(lab))]][[1]]

      tibble(
        Feature = lab,
        Estimate = fmt_pct_points(est),
        MOE = ifelse(is.na(moe), "NA", paste0("±", fmt_pct_points(moe))),
        `MOE / Estimate` = ifelse(is.na(ratio), "NA", sprintf("%.2f", ratio)),
        Reliable = ifelse(!is.na(ratio) & ratio > 0.5, "No", ifelse(is.na(ratio), "No data", "Yes"))
      )
    })
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")

  output$shore_detail_table <- renderTable({
    req(input$map_shape_click$id)
    row <- selected_bg()

    tibble(
      Metric = c(
        "Priority score",
        "Demographic score component",
        "Shoreline score component",
        "Nearby access points",
        "Walk access",
        "Bike access",
        "Drive access",
        "Transit stops",
        "Route count",
        "Mean trail quality"
      ),
      Value = c(
        ifelse(is.na(row$score), "NA", sprintf("%.2f", row$score)),
        ifelse(is.na(row$demo_score), "NA", sprintf("%.2f", row$demo_score)),
        ifelse(is.na(row$shoreline_score), "NA", sprintf("%.2f", row$shoreline_score)),
        fmt_num(row$n_points, 0),
        paste0(fmt_num(row$walk_n, 0), "/", fmt_num(row$n_points, 0), " (", safe_pct_label(row$walk_pct), ")"),
        paste0(fmt_num(row$bike_n, 0), "/", fmt_num(row$n_points, 0), " (", safe_pct_label(row$bike_pct), ")"),
        paste0(fmt_num(row$drive_n, 0), "/", fmt_num(row$n_points, 0), " (", safe_pct_label(row$drive_pct), ")"),
        fmt_num(row$transit_stops_sum, 0),
        fmt_num(row$route_count_sum, 0),
        fmt_num(row$trail_quality_mean, 2)
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%")
}

shinyApp(ui, server)