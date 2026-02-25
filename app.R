library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(rlang)

# -------------
# Load datasets
# -------------
csv <- read.csv("Shoreline-Access-Pts_v2-1-attribute-table.csv")
shp <- st_read("Shoreline_access_points/Shoreline_Access_Pts_v2_1.shp", quiet = TRUE) |>
  st_transform(4326)  # Convert geometry to latitude/longitude

# Make IDs comparable (sometimes one side is int and the other is character)
csv <- csv |> mutate(Access_Point_ID = as.integer(Access_Point_ID))
shp <- shp |> mutate(Access_Poi = as.integer(Access_Poi))

# If IDs repeat, collapse to 1 row per Access_Point_ID
csv_1 <- csv |>
  group_by(Access_Point_ID) |>
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.character), ~ dplyr::first(.x)),
    .groups = "drop"
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

      # Weights: sliders to tune the demographic feature and priority score
      sliderInput("w_poc", "Weight: People of color", min = 0, max = 1, value = 0.5, step = 0.05),
      sliderInput("w_lowinc", "Weight: Low income households", min = 0, max = 1, value = 0.5, step = 0.05),
      sliderInput("w_noveh", "Weight: No-vehicle households", min = 0, max = 1, value = 0.5, step = 0.05),

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
          "For each hex we compute three underlying metrics (people of color, low-income status, vehicle ownership). ",
          "Depending on the selected mode, these metrics are either percentages or counts."
        ),
        tags$ol(
          tags$li(
            tags$b("Compute metric values per hex"),
            ": Sums are taken across all access points inside each hex. ",
            "Percent metrics are computed as:",
            tags$ul(
              tags$li(tags$code("poc_pct = poc_pop / total_pop")),
              tags$li(tags$code("lowinc_pct = lowinc_households / total_households")),
              tags$li(tags$code("noveh_pct = noveh_households / total_households"))
            )
          ),
          tags$li(
            tags$b("Normalize each metric to a 0–1 scale"),
            ": Since counts and percentages can be on different scales, ",
            "each metric is normalized according to its minimum and maximum", 
            "across all hexes in the current view:",
            tags$br(),
            tags$code("z = (x - min(x)) / (max(x) - min(x))"),
            tags$br(),
            "If a metric has no variation, i.e., min = max, it contributes 0 everywhere."
          ),
          tags$li(
            tags$b("Weight and combine"),
            ": The final score is a weighted sum of the normalized metrics (z-scores):",
            tags$br(),
            tags$code("score = weight_poc*z_poc + weight_lowinc*z_lowinc + weight_noveh*z_noveh"),
            tags$br(),
            "If all weights are set to 0, scores default to 0 (no rankings given)."
          ),
          tags$li(
            tags$b("Rank"),
            ": Hexes are ranked by priority score in descending order (e.g., highest priority score is rank 1)."
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
    agg <- pts_join |>
      st_drop_geometry() |>
      filter(!is.na(hex_id)) |>
      group_by(hex_id) |>
      summarise(
        # Count how many access points fall in the hex
        n_points = n(),

        # Totals used as denominators for percent metrics
        tot_pop = sum(SUM_Estimated_Total_Population, na.rm = TRUE),
        tot_hh = sum(SUM_Estimated_Total_Households, na.rm = TRUE),

        # Total counts for demographics (used as numerators for percent metrics)
        poc_pop = sum(SUM_Estimated_People_of_Color, na.rm = TRUE),
        lowinc_hh = sum(SUM_Estimated_Low_Income_Households, na.rm = TRUE),
        noveh_hh = sum(SUM_Estimated_Households_With_No_Vehicle, na.rm = TRUE),

        # Extra unused metrics
        mean_trail_quality = mean(mean_Trail_Quality_Score, na.rm = TRUE),
        transit_stops = sum(Public_Transit_Stops, na.rm = TRUE),
        .groups = "drop"
      )

    # Attach aggregated values back to the hex geometry and compute percent metrics
    hex_sf <- inner_join(hex_grid, agg, by = "hex_id") |>
      mutate(
        poc_pct    = safe_pct(poc_pop, tot_pop),
        lowinc_pct = safe_pct(lowinc_hh, tot_hh),
        noveh_pct  = safe_pct(noveh_hh, tot_hh)
      )

    # Return hex polygons in WGS84 format for Leaflet
    st_transform(hex_sf, 4326)
  })

  # -------------------------------------------------
  # Compute priority score + rank for each hex
  # Actively updates to percent/count + weight inputs
  # -------------------------------------------------
  scored_hex <- reactive({
    h <- hex_agg()

    # Select which metrics to score:
    # Percent option uses percentages
    # Count option uses raw totals
    if (input$scale_mode == "pct") {
      m_poc <- h$poc_pct
      m_low <- h$lowinc_pct
      m_noveh <- h$noveh_pct
    } else {
      m_poc <- h$poc_pop
      m_low <- h$lowinc_hh
      m_noveh <- h$noveh_hh
    }

    # Normalize each metric across hexes to [0, 1] so weights for different features are comparable
    z_poc <- minmax01(m_poc)
    z_low <- minmax01(m_low)
    z_noveh <- minmax01(m_noveh)

    # Weighted sum: score is higher when a hex has high values of weighted metrics
    wsum <- input$w_poc + input$w_lowinc + input$w_noveh
    score <- if (wsum == 0) rep(0, nrow(h)) else (
      input$w_poc * z_poc +
        input$w_lowinc * z_low +
        input$w_noveh * z_noveh
    )

    # Rank: 1 = highest priority score
    h |>
      mutate(score = score, rank = dense_rank(desc(score)))
  })

  # --------------------------
  # Initialize Leaflet basemap
  # --------------------------
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -122.3, lat = 37.8, zoom = 9)  # Default view to Bay Area
  })

  # -----------------------------------------------
  # Redraw map layers when reactive controls update
  # -----------------------------------------------
  observe({
    h <- scored_hex()

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
        label = ~paste0("Score: ", round(score, 3), " | Rank: ", rank, " | Points: ", n_points)
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