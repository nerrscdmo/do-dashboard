library(shiny)
library(bslib)
library(bsicons)
library(here)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)

# bring in data frames (calculations have been made outside the app)
load(here::here("data_wq", "do_dataframes.RData"))

stn_trends_long <- stn_trends |> 
    pivot_longer(-c(station, nYears),
                 names_to = c("param", ".value"),
                 names_sep = "\\.") |> 
    mutate(significant = case_when(pval <= 0.05 ~ "yes",
                                   is.na(pval) ~ "no",  # these are proportions when all values were 0
                                   pval > 0.05 ~ "no"),
           direction = case_when(trend < 0 ~ "decreasing",
                                 trend > 0 ~ "increasing",
                                 trend == 0 ~ "none",
                                 is.na(trend) ~ "not calculated"),
           map_color = case_when(is.na(trend) ~ "not calculated",
                                 significant == "no" ~ "no trend",
                                 direction == "increasing" ~ "increasing",
                                 direction == "decreasing" ~ "decreasing")) |> 
    left_join(distinct(select(tomap, station, lat, long)),
              by = "station")

# color palettes
# palette <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
palette_unus <- colorFactor(palette = c("#2166AC", "#B2182B"),  # from Tol's BuRd
                            levels = c(0, 1))

palette_trnd.mgl <- colorFactor(palette = c("#2166AC", "#B2182B", "#FFEE99", "#7F7F7F"),  # from Tol's BuRd EXCEPT for 'not calcd' - need to check this
                            levels = c("increasing", "decreasing", "no trend", "not calculated"))

palette_trnd.thrsh <- colorFactor(palette = c("#762A83", "#1B7837", "#FFEE99", "#7F7F7F"),  # from Tol's PRGn
                                levels = c("increasing", "decreasing", "no trend", "not calculated"))

# UI ----
ui <- page_fillable(
    
    # header info
    layout_columns(
        col_widths = c(4, 3, 5),
        h1("Can our estuaries breathe?"),
        checkboxInput("sync_maps", "Zoom and Pan Maps Together", value = TRUE),
        p("Select a year to focus on, and your threshold of interest for 'low DO'. The default display shows values compared to 5 mg/L for the most recent year. Code is available on ", a('GitHub', href='https://github.com/swmpkim/estuary-dashboard', target = '_blank'), ".")
        
    ),
    
    # css/html styling ---- 
    tags$head(
        tags$style(HTML("
        /* Main input labels - 14px */
        .shiny-input-container > label,  /* Main labels */
        .form-label { 
            font-size: 14px important;
        }
        
        p {
        font-size: 14px !important;
        }
        
        .accordion-button {
        font-size: 14px !important;
        color: blue;
        }
        
        /* Option text - 12px */
        .form-check-label,
        div.shiny-options-group label,  /* Radio and checkbox group options */
        .radio label,
        .checkbox label {
            font-size: 14px !important;
        }
        
         /* Style for two-column checkbox group */
        .two-col-checks .form-check {
            display: inline-block;
            width: 50%;
        }
        
        /* Slider text */
        .irs-grid-text, .irs-min, .irs-max, .irs-single, .irs-from, .irs-to {
            font-size: 12px !important;
        }
        
        
    /* Slider bar styles */
    .irs-bar {
      height: 0px !important;
      border: 0px solid #444444 !important;
      background: #444444 !important;
    }
    .irs-bar-edge {
      display: none !important;
    }
    .irs-line {
      height: 2px !important;
      border: 0px solid transparent !important;
      background: #444444 !important;
    }
    .irs-slider {
      border: 1px solid #aaa !important;
      background: #fff !important;
      width: 14px !important;
      height: 14px !important;
      border-radius: 50%;
    }

  "))
    ),  # end styling
    
    
    # column layout so map cards are side-by-side
    layout_columns(
        
        
        # card 1: trend map ----
        card(
            full_screen = TRUE,
            
            card_header("Where is DO changing over time?",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "If a station had at least 5 years of data, a trend through time was calculated using simple linear regression."
                        ), # end tooltip
             ), # end header
            
            
            # selections
            # inputs in a popover; div so text can trigger
            div(
                style = "text-align: right; margin: 0; padding: 0.1rem 1rem;",
                popover(
                    title = "Map Options",
                    span(
                        "Map Options ", 
                        bsicons::bs_icon("gear"),
                        style = "color: #333333; font-weight: bold; font-size: 14px;"
                    ),
                    
                    # column layout for inputs
                    layout_columns(
                        # choose trend parameter
                        div(
                            "Select trend to view: ",
                            tooltip(
                                bsicons::bs_icon("info-circle"),
                                "Decreases in DO are more common than changes in time spent below thresholds. Here you can choose which metric you would like to examine."
                            ),
                            radioButtons("trendParam_sel", label = NULL,
                                         choiceNames = c("Median DO Concentration",
                                                         "Time DO < 2",
                                                         "Time DO < 5"),
                                         choiceValues = c("domgl_median",
                                                          "LT2",
                                                          "LT5"),
                                         selected = "domgl_median")
                        ),
                        
                        # choose which values to see
                        div(class = "two-col-checks",
                            checkboxGroupInput("trendShow_sel", "Select results to include:",
                                               choices = c("increasing",
                                                           "decreasing",
                                                           "no trend",
                                                           "not calculated"),
                                               selected = c("increasing",
                                                            "decreasing",
                                                            "no trend",
                                                            "not calculated"),
                                               inline = TRUE)
                        )
                    ),  # end column layout
                    placement = "right",
                )   # end popover
            ),
            
            # sidebar layout, for station popups
            layout_sidebar(
                sidebar = sidebar(p("Sidebar! Find the arrow to collapse me!"),
                                  br(),
                                  p("This is where information about a station will be displayed when someone clicks the station on the map."),
                                  position = "left",
                                  open = FALSE),
                # map
                leafletOutput("map_trends")
            )
        ), # end card 1
        
        # card 2: low do map ----
        card(
            full_screen = TRUE,
            
            card_header("In the selected year, how much time was DO below the selected threshold?",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Info here about % of readings, and how typical/unusual was determined"
                        ) # end tooltip
            ), # end header
            
            # inputs in a popover; div so text can trigger
            div(
                style = "text-align: left; margin: 0; padding: 0.1rem 1rem;",
                popover(
                    span(
                        bsicons::bs_icon("gear"),
                        "Map Options ", 
                        style = "color: #333333; font-weight: bold; font-size: 14px;"
                    ),
                    
                    title = "Map Options",
                    div(
                   
                    # stack slider bars for year and range selection
                    layout_column_wrap(
                        width = 1/2,
                        # year selection
                        sliderInput(
                            "year",
                            "Select Year:",
                            min = min(tomap$year),
                            max = max(tomap$year),
                            value = max(tomap$year),
                            step = 1,
                            sep = ""
                        ),
                        # range selection
                        sliderInput("cutoff_range", 
                                    "Limit to stations in this range of low DO frequency", 
                                    min = 0, 
                                    max = 100, 
                                    value = c(0, 100), 
                                    step = 1)
                    ),
                    
                    layout_column_wrap(
                        # col_widths = c(6, 2, 2, 2),
                        width = 1/3,
                        # choose threshold
                        radioButtons("threshold_sel", "DO threshold",
                                     choiceNames = c("2 mg/L", "5 mg/L"),
                                     choiceValues = c("LT2", "LT5"),
                                     selected = "LT5"),
                        
                        # station type
                        checkboxGroupInput("unus_sel", "Show stations where this low DO frequency is:",
                                           choiceNames = c("typical", "unusual"),
                                           choiceValues = c(0, 1),
                                           selected = c(0, 1)),
                        
                        # size typical points by amount?
                        div(
                            style = "display: flex; align-items: top; gap: 5px;",
                            checkboxInput("typicalSize_sel", "Size 'typical' points by % time",
                                          value = FALSE),
                            tooltip(
                                bsicons::bs_icon("info-circle", 
                                                 width = "50", height = "20",
                                                 # style = " color: blue;",
                                                 color = "blue"
                                                 ),
                                "Points will be sized according to the percentage of time DO was below the threshold"
                            )
                        )
                        
                    )
                    ), # end wrapper div
                    placement = "left",
                    options = list(
                        html = TRUE,
                        container = "body",
                        template = '<div class="popover" role="tooltip" style="max-width: 600px; width: 600px;"><div class="arrow"></div><h3 class="popover-header"></h3><div class="popover-body"></div></div>'
                    )
                ) # end popover
            ), # end div
            
            
            # sidebar layout, for station popups
            layout_sidebar(
                sidebar = sidebar(p("Sidebar! Find the arrow to collapse me!"),
                                  br(),
                                  p("This is where information about a station will be displayed when someone clicks the station on the map."),
                                  position = "right",
                                  open = FALSE),
                # map
                leafletOutput("map_timeLow")
            )
        )
        
    ), # end column layout
    
    card(
        p("Maybe this is where things should show up when a point is clicked on the maps")
    )
    
)  # end ui


# Server ----
server <- function(input, output, session) {
    
    # maps ----
    output$map_trends <- renderLeaflet({
        leaflet() |> 
            addTiles(group = "Default (OpenStreetMap") |> 
            addProviderTiles(provider = providers$CartoDB.Positron,
                             group = "Positron (CartoDB)") |> 
            addProviderTiles(provider = providers$Esri,
                             group = "Esri") |> 
            setView(lng = -98.5, lat = 39.8, zoom = 2)  |>  # Central US, zoomed out to include AK and HI
            addLayersControl(baseGroups = c("Default (OpenStreetMap)",
                                            "Positron (CartoDB)",
                                            "Esri"))
    })
    
    output$map_timeLow <- renderLeaflet({
        leaflet() |> 
            addTiles(group = "Default (OpenStreetMap") |> 
            addProviderTiles(provider = providers$CartoDB.Positron,
                             group = "Positron (CartoDB)") |> 
            addProviderTiles(provider = providers$Esri,
                             group = "Esri") |> 
            setView(lng = -98.5, lat = 39.8, zoom = 2)  |>  # Central US, zoomed out to include AK and HI
            addLayersControl(baseGroups = c("Default (OpenStreetMap)",
                                            "Positron (CartoDB)",
                                            "Esri"))
        })
    
    # Update map_timeLow based on selections
    observe({
        # Filter data based on the selected year and cutoff value
        tomap_sub <- tomap |> 
            filter(year == input$year, 
                   threshold == input$threshold_sel,
                   pct >= input$cutoff_range[1],
                   pct <= input$cutoff_range[2],
                   unusual %in% input$unus_sel) |> 
            mutate(size1 = case_when(pct <= 3 ~ 3,
                                     3 < pct & pct <= 7 ~ pct,
                                     7 < pct ~ 4 + sqrt(pct)),
                   # if user wants to size 'typical' points by pct, use size 1. if they don't, make it 3.
                   size1 = case_when(unusual == 0 & input$typicalSize_sel == FALSE ~ 3,
                                     .default = size1))

        
        rows_unusual <- which(tomap_sub$unusual == 1)
        rows_typical <- which(tomap_sub$unusual == 0)
        
        leafletProxy("map_timeLow", data = tomap_sub) |>
            clearMarkers() |> 
            addCircleMarkers(
                data = tomap_sub[rows_typical, ],
                group = "in typical range",
                lng = ~long,
                lat = ~lat,
                radius = ~size1,
                stroke = FALSE,
                popup = ~as.character(round(pct, 1)),
                opacity = 0.5,
                fill = TRUE,
                fillColor = ~palette_unus(0),
                fillOpacity = 0.5
            ) |> 
            addCircleMarkers(
                data = tomap_sub[rows_unusual, ],
                lng = ~long,
                lat = ~lat,
                radius = ~size1,
                stroke = FALSE,
                popup = ~as.character(round(pct, 1)),
                opacity = 0.5,
                fill = TRUE,
                fillColor = ~palette_unus(1),
                fillOpacity = 0.7 
            ) |> 
            clearControls() |> 
            addLegend(position = "bottomright",
                      colors = palette_unus(c(0, 1)),
                      labels = c("no", "yes"),
                      title = "Unusual?",
                      opacity = 0.7)
    })
    
    # update trend map based on selections
    observe({
        filtered2 <- stn_trends_long |> 
            filter(param == input$trendParam_sel,
                   map_color %in% input$trendShow_sel)
        
        # use the right color palette function
        palette_trnd <- if(input$trendParam_sel == "domgl_median") {
            palette_trnd.mgl
        } else {
            palette_trnd.thrsh
        }
        
        leafletProxy("map_trends", data = filtered2) |> 
            clearMarkers() |> 
            addCircleMarkers(
                lng = ~long,
                lat = ~lat,
                color = ~palette_trnd(map_color),
                fillOpacity = 0.7,
                radius = 4
            )  |> 
            clearControls() |>
            addLegend(position = "bottomright",
                      colors = palette_trnd(c("increasing",
                                              "decreasing",
                                              "no trend",
                                              "not calculated")),
                      labels = c("increasing",
                                 "decreasing",
                                 "no trend",
                                 "not calculated"),
                      title = "Trend",
                      opacity = 0.7)
        
    })
    
    # Zoom and Center maps together----
    
    # Reactive values to track zoom and center
    zoom_level <- reactiveVal(2)
    map_center <- reactiveVal(c(lng = -98.5, lat = 39.8))
    
    # Sync map2 when map1 changes
    observeEvent(input$map_trends_zoom, {
        if (input$sync_maps) zoom_level(input$map_trends_zoom)
    })
    
    observeEvent(input$map_trends_center, {
        if (input$sync_maps) map_center(input$map_trends_center)
    })
    
    observe({
        if (input$sync_maps) {
            leafletProxy("map_timeLow") |> 
                setView(lng = map_center()[1], lat = map_center()[2], zoom = zoom_level())
        }
    })
    
    # Sync map1 when map2 changes
    observeEvent(input$map_timeLow_zoom, {
        if (input$sync_maps) zoom_level(input$map_timeLow_zoom)
    })
    
    observeEvent(input$map_timeLow_center, {
        if (input$sync_maps) map_center(input$map_timeLow_center)
    })
    
    observe({
        if (input$sync_maps) {
            leafletProxy("map_trends") |> 
                setView(lng = map_center()[1], lat = map_center()[2], zoom = zoom_level())
        }
    })
    
    
}

# Run the app
shinyApp(ui, server)
