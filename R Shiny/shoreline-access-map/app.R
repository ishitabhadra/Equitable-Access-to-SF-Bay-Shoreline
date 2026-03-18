library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(rlang)

# -------------
# Load datasets
# -------------
csv <- read.csv("Data/Shoreline-Access-Pts_v2-1-attribute-table.csv")
shp <- st_read("Data/Shoreline_access_points/Shoreline_Access_Pts_v2_1.shp", quiet = TRUE) |>
  st_transform(4326)  # Convert geometry to latitude/longitude

# Make IDs comparable (sometimes one side is int and the other is character)
csv <- csv |> mutate(Access_Point_ID = as.integer(Access_Point_ID))
shp <- shp |> mutate(Access_Poi = as.integer(Access_Poi))

# Take means of sums per Access_Point_ID
csv_1 <- csv |>
  group_by(Access_Point_ID) |>
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.character), ~ dplyr::first(.x)),
    .groups = "drop"
  ) 

# Append service mode accessbility
service_modes <- csv |>
  group_by(Access_Point_ID) |>
  summarise(
    Walk  = any(Service_Type == "Walk", na.rm = TRUE),
    Bike  = any(Service_Type == "Bike", na.rm = TRUE),
    Drive = any(Service_Type == "Drive", na.rm = TRUE),
    .groups = "drop"
  )

csv_1 <- csv_1 |>
  left_join(service_modes, by = "Access_Point_ID") |>
  mutate(
    Walk  = replace_na(Walk, FALSE),
    Bike  = replace_na(Bike, FALSE),
    Drive = replace_na(Drive, FALSE)
  )

# Join full-name CSV fields onto the geometry (csv column names better for readability)
access_pts <- shp |>
  left_join(csv_1, by = c("Access_Poi" = "Access_Point_ID"))


# ----------------------
# Shiny helper functions
# ----------------------
safe_pct <- function(numer, denom) {
  # Vectorized: Takes percentage, returns NA where denominator is NA or <= 0
  invalid <- is.na(denom) | denom <= 0
  result <- numer / denom
  result[invalid] <- NA_real_

  result
}

minmax01 <- function(x) {
  # Normalize minimum and maximum to 0 and 1 to compare different features
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) return(rep(0, length(x)))
  
  return (x - rng[1]) / (rng[2] - rng[1])
}

fmt_num <- function(x) {
  # Formatted numbers for labels/tooltips
  ifelse(
    is.na(x),
    "NA",
    format(round(x, 0), big.mark = ",", trim = TRUE, scientific = FALSE)
  )
}

fmt_pct <- function(x) {
  # Formatted percentages for labels/tooltips
  ifelse(
    is.na(x),
    "NA",
    paste0(round(100 * x, 1), "%")
  )
}


# ---------------
# Feature Catalog
# ---------------
feature_catalog <- tibble::tribble(
  ~var, ~label, ~category, ~denom,

  # Financial status
  "SUM_Estimated_Low_Income_Households", "Low-income households", "Financial status", "tot_hh",
  "SUM_Estimated_Households_With_No_Vehicle", "Households with no vehicle", "Financial status", "tot_hh",
  "SUM_Estimated_Households_Below_50_Percent_Median_AMI", "Households below 50% median AMI", "Financial status", "tot_hh",

  # Vulnerability
  "SUM_Estimated_Households_With_Disability", "Households with disability", "Vulnerability", "tot_hh",
  "SUM_Estimated_Single_Parent_Households", "Single-parent households", "Vulnerability", "tot_hh",
  "SUM_Estimated_Seniors_Living_Alone", "Seniors living alone", "Vulnerability", "tot_hh",
  "SUM_Estimated_Population_With_No_High_School_Diploma", "Population with no high school diploma", "Vulnerability", "tot_pop",
  "SUM_Estimated_Limited_English_Proficiency_Households", "Limited-English-proficiency households", "Vulnerability", "tot_hh",
  "SUM_High_Vuln_Households", "High vulnerability households", "Vulnerability", "tot_hh",
  "SUM_Highest_Vuln_Households", "Highest vulnerability households", "Vulnerability", "tot_hh",

  # Race / ethnicity / immigration
  "SUM_Estimated_People_of_Color", "People of color", "Race / ethnicity / immigration", "tot_pop",
  "SUM_Estimated_Non_Citizens", "Non-citizens", "Race / ethnicity / immigration", "tot_pop",
  "SUM_Estimated_Foreign_Born", "Foreign-born population", "Race / ethnicity / immigration", "tot_pop",
  "SUM_Estimated_Latino_Population", "Latino population", "Race / ethnicity / immigration", "tot_pop",
  "SUM_Estimated_Black_Population", "Black population", "Race / ethnicity / immigration", "tot_pop",
  "SUM_Estimated_American_Indian_Population", "American Indian population", "Race / ethnicity / immigration", "tot_pop",
  "SUM_Estimated_Asian_Population", "Asian population", "Race / ethnicity / immigration", "tot_pop",
  "SUM_Estimated_Pacific_Islander_Population", "Pacific Islander population", "Race / ethnicity / immigration", "tot_pop"
)

# ----------
# UI
# ----------
ui <- fluidPage(
  titlePanel("SF Bay Shoreline Access: Priority Target Regions"),
  sidebarLayout(
    sidebarPanel(
      # -------------
      # User controls
      # -------------
      # Switch between percent-based and count-based ranking
      radioButtons(
        "scale_mode", "Rank by:",
        choices = c("Percent" = "pct", "Count" = "count"),
        inline = TRUE
      ),

      # Grid resolution control: slider to tune hex size / aggregation
      sliderInput(
        "hex_km", "Hex size (km):",
        min = 0.5, max = 10, value = 2, step = 0.5
      ),

      tags$hr(),

      # Feature selectors: categorized selectize inputs for demographic features
      selectizeInput(
        "features_financial",
        "Financial / household burden:",
        choices = setNames(
          feature_catalog$var[feature_catalog$category == "Financial status"],
          feature_catalog$label[feature_catalog$category == "Financial status"]
        ),
        selected = c(
          "SUM_Estimated_Low_Income_Households",
          "SUM_Estimated_Households_With_No_Vehicle"
        ),
        multiple = TRUE
      ),

      selectizeInput(
        "features_vulnerability",
        "Vulnerability / social barriers:",
        choices = setNames(
          feature_catalog$var[feature_catalog$category == "Vulnerability"],
          feature_catalog$label[feature_catalog$category == "Vulnerability"]
        ),
        selected = c(
          "SUM_Estimated_Households_With_Disability"
        ),
        multiple = TRUE
      ),

      selectizeInput(
        "features_race",
        "Race / ethnicity / immigration:",
        choices = setNames(
          feature_catalog$var[feature_catalog$category == "Race / ethnicity / immigration"],
          feature_catalog$label[feature_catalog$category == "Race / ethnicity / immigration"]
        ),
        selected = c(
          "SUM_Estimated_People_of_Color"
        ),
        multiple = TRUE
      ),

      uiOutput("weight_sliders"),

      tags$hr(),

      # General display controls
      sliderInput("top_n", "Highlight top N hexes by priority score:", min = 0, max = 100, value = 20, step = 5),
      checkboxInput("show_points", "Show access points", value = TRUE)
    ),

    mainPanel(
      # -----------------------
      # Leaflet interactive map
      # -----------------------
      leafletOutput("map", height = "90vh"),

      tags$hr(),

      # --------------------------
      # Context / explanation text
      # --------------------------
      tags$div(
        style = "font-size: 13px; line-height: 1.35;",

        tags$b("Description"),
        tags$p(
          "This prototype dashboard aggregates shoreline access points into a hexagonal grid ",
          "and attempts to quantify the need for improved shoreline access. ",
          "Each hex contains the access points that fall within its boundary. ",
          "We then summarize the demographic features associated with those points and compute a ",
          tags$b("priority score"),
          " per hex based on user-chosen weights."
        ),

        tags$b("Percent vs Count"),
        tags$ul(
          tags$li(
            tags$b("Percent mode"),
            ": Ranks hexes based on the share of people or households served ",
            " that fall into each group (e.g., percentage of households served that are low-income). "
          ),
          tags$li(
            tags$b("Count mode"),
            ": Ranks hexes based on raw totals ",
            " (e.g., number of low-income households served). ",
          )
        ),

        tags$b("Priority Score"),
        tags$p(
          "For each hex, the app computes whichever demographic features the user selects from the three categories above. ",
          "Depending on the selected mode, each feature is evaluated either as a percentage of the relevant denominator ",
          "(total population or total households) or as a raw count."
        ),
        tags$ol(
          tags$li(
            tags$b("Compute metric values per hex"),
            ": Sums are taken across all access points inside each hex. ",
            "In Percent mode, each selected feature is divided by either total population or total households, ",
            "depending on the feature."
          ),
          tags$li(
            tags$b("Normalize each selected metric to a 0–1 scale"),
            ": Since counts and percentages can be on different scales, ",
            "each selected metric is normalized across all hexes in the current view:",
            tags$br(),
            tags$code("z = (x - min(x)) / (max(x) - min(x))"),
            tags$br(),
            "If a metric has no variation, it contributes 0 everywhere."
          ),
          tags$li(
            tags$b("Weight and combine"),
            ": The final score is a weighted sum of the normalized selected metrics:",
            tags$br(),
            tags$code("score = sum(weight_j * z_j)"),
            tags$br(),
            "If all selected weights are 0, the score defaults to 0 for all hexes."
          ),
          tags$li(
            tags$b("Rank"),
            ": Hexes are ranked by priority score in descending order, where rank 1 is the highest-priority hex under the current settings."
          )
        ),

        tags$b("Other Features + Interpreting"),
        tags$ul(
          tags$li("Higher score (yellow hexes) means higher priority under the current mode and weight inputs."),
          tags$li("The ", tags$b("top N hexes"), " input draws a red outline around the highest-ranked hexes."),
          tags$li("The ", tags$b("hex size"), " input changes the analysis resolution. Larger hex size will reduce the number of hexes.")
        ),

        tags$hr()
      )
    )
  )
)

# ----------
# Server
# ----------
server <- function(input, output, session) {

  selected_features <- reactive({
    unique(c(
      input$features_financial,
      input$features_vulnerability,
      input$features_race
    ))
  })

  output$weight_sliders <- renderUI({
    req(selected_features())

    selected_tbl <- feature_catalog |>
      filter(var %in% selected_features())

    if (nrow(selected_tbl) == 0) {
      return(tags$em("Select at least one demographic feature to compute a priority score."))
    }

    sliders <- lapply(seq_len(nrow(selected_tbl)), function(i) {
      row <- selected_tbl[i, ]

      sliderInput(
        inputId = paste0("w_", row$var),
        label = paste("Weight:", row$label),
        min = 0, max = 1, value = 0.5, step = 0.05
      )
    })

    do.call(tagList, sliders)
  })

  # ----------------------------------------
  # Build hex grid + aggregate access points
  # Actively updates to hex size changes
  # ----------------------------------------
  hex_agg <- reactive({
    req(input$hex_km)

    # Use a projected CRS in meters for grid construction and spatial operations
    pts_proj <- st_transform(access_pts, 3310)

    # Build a padded bounding box around the points so edge points aren't clipped tightly
    bb <- st_bbox(pts_proj)
    pad <- input$hex_km * 1000 * 1.5
    bb_pad <- bb
    bb_pad[c("xmin","ymin")] <- bb[c("xmin","ymin")] - pad
    bb_pad[c("xmax","ymax")] <- bb[c("xmax","ymax")] + pad

    # Create a hexagonal grid over the padded bbox
    hex_sfc <- st_make_grid(
      st_as_sfc(bb_pad),
      cellsize = input$hex_km * 1000,
      what = "polygons",
      square = FALSE
    )

    # Convert grid polygons to an sf object and assign each hex an id
    hex_grid <- st_sf(hex_id = seq_along(hex_sfc), geometry = hex_sfc)

    # Spatially assign each point to the hex it falls within
    pts_join <- st_join(pts_proj, hex_grid, join = st_within)

    # Aggregate demographic totals per hex (sum across all points in that hex)
    demo_vars <- feature_catalog$var

    agg <- pts_join |>
      st_drop_geometry() |>
      filter(!is.na(hex_id)) |>
      group_by(hex_id) |>
      summarise(
        n_points = n(),

        # Service mode availability (counts of access points in hex)
        walk_n  = sum(Walk, na.rm = TRUE),
        bike_n  = sum(Bike, na.rm = TRUE),
        drive_n = sum(Drive, na.rm = TRUE),

        # Mode availability (share of access points in hex)
        walk_pct  = safe_pct(walk_n, n_points),
        bike_pct  = safe_pct(bike_n, n_points),
        drive_pct = safe_pct(drive_n, n_points),

        # Denominators for percent mode
        tot_pop = sum(SUM_Estimated_Total_Population, na.rm = TRUE),
        tot_hh = sum(SUM_Estimated_Total_Households, na.rm = TRUE),

        # Aggregate all selectable demographic columns
        across(all_of(demo_vars), ~ sum(.x, na.rm = TRUE)),
        mean_trail_quality = mean(mean_Trail_Quality_Score, na.rm = TRUE),
        transit_stops = sum(Public_Transit_Stops, na.rm = TRUE),
        .groups = "drop"
      )

    # Attach aggregated values back to the hex geometry and compute percent metrics
    hex_sf <- inner_join(hex_grid, agg, by = "hex_id")

    # Return hex polygons in WGS84 format for Leaflet
    st_transform(hex_sf, 4326)
  })

  # -------------------------------------------------
  # Compute priority score + rank for each hex
  # Actively updates to percent/count + weight inputs
  # -------------------------------------------------
  scored_hex <- reactive({
    h <- hex_agg()

    vars <- selected_features()

    if (length(vars) == 0) {
      return(h |> mutate(score = 0, rank = dense_rank(desc(score))))
    }

    selected_tbl <- feature_catalog |>
      filter(var %in% vars)

    score <- rep(0, nrow(h))
    weight_sum <- 0

    for (i in seq_len(nrow(selected_tbl))) {
      row <- selected_tbl[i, ]
      w <- input[[paste0("w_", row$var)]]

      if (is.null(w) || w == 0) next

      metric <- if (input$scale_mode == "pct") {
        # Percent mode only works if a denominator is defined
        safe_pct(h[[row$var]], h[[row$denom]])
      } else {
        # Count mode
        h[[row$var]]
      }

      # Normalize each metric across hexes to [0, 1] so weights for different features are comparable
      z_metric <- minmax01(metric)

      # Weighted sum: score is higher when a hex has high values of weighted metrics
      score <- score + w * z_metric
      weight_sum <- weight_sum + w
    }

    if (weight_sum == 0) {
      score <- rep(0, nrow(h))
    }

    # Rank: 1 for highest priority score
    h |>
      mutate(score = score, rank = dense_rank(desc(score)))
  })

  # --------------------------
  # Initialize Leaflet basemap
  # --------------------------
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -122.3, lat = 37.8, zoom = 10)  # Default view to Bay Area
  })

  # -----------------------------------------------
  # Redraw map layers when reactive controls update
  # -----------------------------------------------
  observe({
    h <- scored_hex()

    # HTML formatted tooltip statistics
    selected_tbl <- feature_catalog |>
      filter(var %in% selected_features())

    build_hex_label <- function(row, selected_tbl) {
      # Header (priority score + rank, counts of population + households)
      header_lines <- c(
        paste0(
          "<strong>Score:</strong> ", round(row$score, 3),
          " | <strong>Rank:</strong> ", row$rank,
          " | <strong>Access Points:</strong> ", row$n_points
        ),
        paste0(
          "<strong>Total population:</strong> ", fmt_num(row$tot_pop),
          " | <strong>Total households:</strong> ", fmt_num(row$tot_hh)
        )
      )

      # Tooltip labels for selected features
      feature_lines <- purrr::map_chr(seq_len(nrow(selected_tbl)), function(i) {
        feat <- selected_tbl[i, ]

        raw_val <- row[[feat$var]]
        pct_val <- safe_pct(raw_val, row[[feat$denom]])

        denom_text <- if (feat$denom == "tot_pop") "population" else "households"

        paste0(
          "<strong>", feat$label, ":</strong> ",
          fmt_num(raw_val),
          " (", fmt_pct(pct_val), " of ", denom_text, ")"
        )
      })

      # Tooltip labels for service mode (walk/bike/drive) access
      service_mode_lines <- c(
        paste0(
          "<strong>Walk access:</strong> ", row$walk_n, "/", row$n_points,
          " (", fmt_pct(row$walk_pct), ")"
        ),
        paste0(
          "<strong>Bike access:</strong> ", row$bike_n, "/", row$n_points,
          " (", fmt_pct(row$bike_pct), ")"
        ),
        paste0(
          "<strong>Drive access:</strong> ", row$drive_n, "/", row$n_points,
          " (", fmt_pct(row$drive_pct), ")"
        )
      )

      htmltools::HTML(paste(
        c(
          header_lines,
          "",
          feature_lines,
          "",
          service_mode_lines
        ),
        collapse = "<br/>"
      ))
    }

    h$tooltip_html <- purrr::map(
      seq_len(nrow(h)),
      ~ build_hex_label(h[.x, ], selected_tbl)
    )

    # Color scale for the priority score
    pal <- colorNumeric("viridis", domain = h$score, na.color = "transparent")

    # Select top N priority score hexes to outline in red
    top_hex <- if (input$top_n > 0) {
      h |> arrange(rank) |> slice_head(n = input$top_n)
    } else h[0, ]

    leafletProxy("map") |>
      clearShapes() |>
      clearControls() |>
      clearMarkers() |>

      # Base choropleth layer: all hexes filled by score
      addPolygons(
        data = h,
        fillColor = ~pal(score),
        fillOpacity = 0.65,
        color = "#444444",
        weight = 0.5,
        opacity = 0.4,
        label = ~tooltip_html,
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "13px"
        )
      ) |>

      # Highlight layer: outlines top N hexes
      addPolygons(
        data = top_hex,
        fill = FALSE,
        color = "#FF0000",
        weight = 2,
        opacity = 0.9
      ) |>

      # Legend: priority score color scale
      addLegend(
        position = "bottomright",
        pal = pal,
        values = h$score,
        title = "Priority score",
        opacity = 1
      )

    # Optional overlay: show individual shoreline access points
    if (isTRUE(input$show_points)) {
      leafletProxy("map") |>
        addCircleMarkers(
          data = access_pts,
          color = "#2F4F4F",
          radius = 2.5,
          stroke = FALSE,
          fillOpacity = 0.6
        )
    }
  })
}

shinyApp(ui, server)