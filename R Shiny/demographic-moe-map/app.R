library(shiny)
library(sf)
library(tidyverse)
library(leaflet)

# ----------
# LOAD DATA
# ----------
shp_path <- "Data/Community_Vulnerability_(BCDC_2023)/Community_Vulnerability_(BCDC_2023).shp"

bcdc <- st_read(shp_path, quiet = TRUE) |>
  st_transform(4326)  # WGS84 for Leaflet

# Link vulnerability indicator descriptions to shapefile column names
indicators <- list(
  # Shapefile field names are truncated to 10 chars from the full CSV names.
  # Collision fields (pct_moe__1/2/3) resolve in column order as:
  #   pct_moe__1 = pct_moe_noHS
  #   pct_moe__2 = pct_moe_RentHCB
  #   pct_moe__3 = pct_moe_NoCtz
  "People of Color"                   = c("pct_PoC",     "pct_moe_Po"),
  "Renters"                           = c("pct_renter",  "pct_moe_re"),
  "People Under 5 Years Old"          = c("pct_under5",  "pct_moe_un"),
  "Very Low Income (Below 200% FPL)"  = c("pct_B200Pv",  "pct_moe_B2"),  # FPL = Federal Poverty Level
  "Households Below 50% AMI"          = c("pct_50Medi",  "pct_moe_50"),  # AMI = Area Median Income
  "Non-U.S. Citizens"                 = c("pct_NoCtz",   "pct_moe__3"),
  "Households Without a Vehicle"      = c("pct_noVeh",   "pct_moe_no"),
  "People with a Disability"          = c("pct_disabH",  "pct_moe_di"),
  "Single Parent Families"            = c("pct_SglPar",  "pct_moe_Sg"),
  "Seniors (65+) Living Alone"        = c("pct_65Alon",  "pct_moe_65"),
  "Limited English Proficiency"       = c("pct_LEP_HH",  "pct_moe_LE"),
  "Without High School Degree"        = c("pct_noHS",    "pct_moe__1"),
  "Severely Rent-Cost Burdened"       = c("pct_RentHC",  "pct_moe__2"),
  "Severely Owner-Cost Burdened"      = c("pct_Mortga",  "pct_moe_Mo")
)

valid_indicators <- names(indicators)

# ----
# UI
# ----
ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      #controls {
        position: absolute; top: 10px; left: 50px; z-index: 1000;
        background: white; padding: 12px 16px; border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.25); width: 310px;
      }
      h4 { margin-top: 0; margin-bottom: 10px; font-size: 14px; font-weight: 600; }
      .legend-note { font-size: 11px; color: #555; margin-top: 8px; line-height: 1.4; }
      .unreliable-note {
        font-size: 11px; color: #c0392b; margin-top: 6px;
        border-left: 3px solid #c0392b; padding-left: 6px;
      }
      .selectize-input { font-size: 13px; }
    "))
  ),

  # Full-screen map
  leafletOutput("map", width = "100%", height = "100vh"),

  # Floating control panel
  absolutePanel(
    id = "controls", top = 10, left = 50, width = 310,

    h4("Demographic Data Reliability Map"),

    div(class = "legend-note",
      "This map shows the ratio of ",
      strong("margin of error / estimate"),
      " for the selected indicator within each census block group. ",
      "Lower ratios indicate more reliable data."
    ),

    div(class = "unreliable-note",
      "Outlined block groups have a MOE > 50% of the estimate and should be ",
      "treated as unreliable."
    ),

    hr(style = "margin: 8px 0;"),

    selectInput(
      "indicator",
      label   = "Social Vulnerability Indicator",
      choices = valid_indicators,
      width   = "100%"
    ),

    div(class = "legend-note",
      span("- FPL = Federal Poverty Level"),
      tags$br(),
      span("- AMI = Area Median Income")
    ),

    hr(style = "margin: 8px 0;"),

    uiOutput("summary_stats")
  )
)

# -------
# SERVER
# -------
server <- function(input, output, session) {

  # Reactively compute MOE ratio for selected indicator
  map_data <- reactive({
    req(input$indicator)
    fields <- indicators[[input$indicator]]
    est_col <- fields[1]
    moe_col <- fields[2]

    bcdc |>
      mutate(
        .estimate   = as.numeric(.data[[est_col]]),
        .moe        = as.numeric(.data[[moe_col]]),
        .ratio      = ifelse(.estimate > 0, .moe / .estimate, NA_real_),
        .unreliable = !is.na(.ratio) & .ratio > 0.5  # MOE/Estimate ratio above which data is considered unreliable
      )
  })

  # Color palette
  pal <- reactive({
    d <- map_data()
    colorNumeric(
      palette = "YlOrRd",
      domain  = d$.ratio,
      na.color = "#cccccc"
    )
  })

  # Base map (renders once)
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -122.2, lat = 37.7, zoom = 9)
  })

  # Update polygons when indicator changes
  observe({
    d   <- map_data()
    pal_fn <- pal()

    # Popup text
    popup_text <- sprintf(
      "<b>Block Group:</b> %s<br/>
       <b>Estimate:</b> %.1f%%<br/>
       <b>Margin of Error:</b> ±%.1f%%<br/>
       <b>MOE / Estimate ratio:</b> %.2f<br/>
       <b>Reliability:</b> %s",
      ifelse("GEOID" %in% names(d), d$GEOID,
             ifelse("geoid" %in% names(d), d$geoid, "N/A")),
      d$.estimate,
      d$.moe,
      ifelse(is.na(d$.ratio), NA, d$.ratio),
      ifelse(d$.unreliable, "Unreliable (MOE > 50% of estimate)",
             ifelse(is.na(d$.ratio), "No data", "Reliable"))
    )

    # Reliable block groups
    leafletProxy("map") |>
      clearShapes() |>
      clearControls() |>

      addPolygons(
        data        = d,
        fillColor   = ~pal_fn(.ratio),
        fillOpacity = 0.75,
        color       = "#888888",
        weight      = 0.4,
        opacity     = 0.6,
        popup       = popup_text,
        label       = lapply(popup_text, htmltools::HTML),
        labelOptions = labelOptions(
          style     = list("font-size" = "12px"),
          direction = "auto"
        )
      ) |>

      # Overlay bold red outline for unreliable block groups
      addPolygons(
        data        = filter(d, .unreliable),
        fill        = FALSE,
        color       = "#c0392b",
        weight      = 2,
        opacity     = 1
      ) |>

      addLegend(
        position = "bottomright",
        pal      = pal_fn,
        values   = d$.ratio,
        title    = "MOE / Estimate Ratio",
        labFormat = labelFormat(digits = 2),
        na.label = "No data",
        opacity  = 0.85
      )
  })

  # Summary stats panel
  output$summary_stats <- renderUI({
    d <- map_data()
    n_total      <- nrow(d)
    n_unreliable <- sum(d$.unreliable, na.rm = TRUE)
    n_nodata     <- sum(is.na(d$.ratio))
    pct_unrel    <- round(100 * n_unreliable / (n_total - n_nodata), 1)

    tagList(
      tags$div(style = "font-size: 12px; color: #333;",
        tags$b("Summary for selected indicator"),
        tags$br(),
        sprintf("Total block groups: %d", n_total),
        tags$br(),
        sprintf("No data: %d", n_nodata),
        tags$br(),
        tags$span(style = "color: #c0392b;",
          sprintf("Unreliable (MOE > 50%%): %d of %d (%.1f%%)",
                  n_unreliable, n_total - n_nodata, pct_unrel)
        )
      )
    )
  })
}

shinyApp(ui, server)