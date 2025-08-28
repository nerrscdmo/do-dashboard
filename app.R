library(shiny)
library(bslib)
library(bsicons)
library(here)
library(plotly)
library(leaflet)
library(leaflegend)
library(htmltools)
library(reactable)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(waiter)
library(glue)

# setup ----
source(here::here("R", "functions.R"))
source("global.R")
source("explanations.R")

# UI ----
ui <- page_fillable(
    useWaiter(),
    autoWaiter(html = spin_3(),
               color = transparent(0.5)),
    
    
    # css/html styling ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # Header ----
    layout_columns(
        col_widths = c(8, 4),
        fill = FALSE,
        
        # Header, github link, description
        div(
            # Header + GitHub link
            div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                h3("NERRS Monitoring: Dissolved Oxygen"),
                tags$a(
                    icon("github"), 
                    href = "https://github.com/nerrscdmo/do-dashboard", 
                    target = "_blank", 
                    style = "color: inherit;"
                )
            ),
            
            # Description
            p("Choose a tab to explore DO nationally. Change options in the left sidebar, and click on a station to see more detail in a right sidebar.")
        ),
        
        # Value box
        value_box(
            title = "Stations with Decreasing DO",
            value = textOutput("station_count"),
            # value = "49 of 105 stations",
            showcase = bsicons::bs_icon("graph-down-arrow"),
            showcase_layout = "left center",
            theme = "danger",
            max_height = "120px"
        )
    ),
    
    # main body
    navset_card_tab(
        full_screen = TRUE,
        
        # sidebar: stn info ----
        sidebar = sidebar(id = "stn_sidebar",
                          position = "right",
                          width = "50%",
                          height = "90vh",
                          open = FALSE,
                          # title = "Station Information",
                          uiOutput("station_section")
        ),
        
        
        
        # map tabs ----
        # panel 1: trend map ----
        nav_panel(
            "Trends",
            full_screen = TRUE,
            
            card_header("Where is DO changing over time?",
                        # tooltip(
                        #     bsicons::bs_icon("info-circle"),
                        #     "These trends were calculated as part of a SWMP Synthesis project. Stations were included only if they had at least 10 years of data and were active as of 2022."
                        # ), # end tooltip
                        # details popover
                        popover(
                            actionButton("btnTrendMap", "Map Details", 
                                         icon = icon("map-location-dot"),
                                         width = 175,
                                         class = "small-btn"),
                            
                            p(strong("What do we want to see?"), "It depends on the selected trend to view. We don't want to see oxygen decreasing - if 'median DO concentration' was selected, we don't want to see much red, because red means median DO level is decreasing. If time below a threshold was selected, we don't want to see much purple, because purple represents MORE time where DO is low."),
                            # p(strong("Clicking on a point"), "will open a sidebar with more details about the selected station: e.g., how long has data been recorded; what are the trend results; graphics of DO through time; and details on how DO in a single year compares to DO in other years at this station."),
                            p(strong("Each point"), "represents a long-term trend value for a single station. A trend has been calculated for each station with enough data. Stations without enough data are labeled 'not calculated' and are represented by the color gray. Each station with a calculated trend was categorized based on whether the variable was increasing, decreasing, or not significantly changing through time. See the map legend for details on colors. Clicking on a point will open a sidebar with more information about that station."),
                            br(),
                            p("See the 'About' tab for more detail on data sources and calculations."),
                            title = "Trends map details",
                            placement = "right"
                        ), # end popover
            ), # end header
            
            
            
            # sidebar layout
            
            layout_sidebar(
                sidebar = sidebar(
                    # title = "Map Options",
                    width = "30%",
                    position = "left",
                    open = TRUE,
                    
                    div(
                        "Select trend to view: ",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Decreases in DO are more common than changes in time spent below thresholds. Here you can choose which metric you would like to examine."
                        ),
                        radioButtons("trendParam_sel", label = NULL,
                                     choiceNames = c("Median DO Concentration",
                                                     "Time DO < 5",
                                                     "Time DO < 2"),
                                     choiceValues = c("domgl_median",
                                                      "LT5",
                                                      "LT2"),
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
                ), # end sidebar
                
                # map
                leafletOutput("map_trends")
                
            ) # end tab's layout_sidebar
            
        ), # end nav panel 1
        
        # panel 3: medians map ----
        nav_panel(
            "Long-term medians",
            full_screen = TRUE,
            
            card_header("What is 'normal' for each station?",
                        # tooltip(
                        #     bsicons::bs_icon("info-circle"),
                        #     "The values shown on the map are the long-term median values of each annual value (for time < 5 or 2 mg/L, this is '% of the year'; for DO mg/L, it is median annual mg/L."
                        # ), # end tooltip
                        popover(
                            actionButton("btnMediansMap", "Map Details", 
                                         icon = icon("map-location-dot"),
                                         width = 175,
                                         class = "small-btn"),
                            p("The values shown on the map are the long-term median values of each annual value (for time < 5 or 2 mg/L, this is '% of the year'; for DO mg/L, it is median annual mg/L."),
                            p(strong("What do we want to see?"), "It depends on the selected variable. Ideally, we see more higher levels of median DO than lower values - more blues than greens and yellows. For time below thresholds, we want lower values; more yellow and orange than red."),
                            # p(strong("Clicking on a point"), "will open a sidebar with more details about the selected station: e.g., how long has data been recorded; what are long-term median values; what are the trend results; graphics of DO through time; and details on how DO in a single year compares to DO in other years at this station."),
                            p(strong("Each point"), "represents a long-term summary value for a single station. Clicking on a point will open a sidebar with more information about that station."),
                            br(),
                            p("See the 'About' tab for more detail on data sources and calculations."),
                            title = "Medians map details",
                            placement = "right"
                        ), # end popover
            ), # end header
            
            
            
            # sidebar layout
            
            layout_sidebar(
                sidebar = sidebar(
                    # title = "Map Options",
                    width = "30%",
                    position = "left",
                    open = TRUE,
                    
                    div(
                        "Select DO metric to view: ",
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Decreases in DO are more common than changes in time spent below thresholds. Here you can choose which metric you would like to examine."
                        ),
                        radioButtons("medianParam_sel", label = NULL,
                                     choiceNames = c("Median DO Concentration",
                                                     "Time DO < 5",
                                                     "Time DO < 2"),
                                     choiceValues = c("domgl_median",
                                                      "LT5",
                                                      "LT2"),
                                     selected = "domgl_median")
                    ),
                    
                    
                ), # end sidebar
                
                # map
                leafletOutput("map_medians")
                
            ) # end tab's layout_sidebar
            
        ), # end nav panel 3        
        
        
        # panel 2: low do map ----
        nav_panel(
            "Explore by Year",
            full_screen = TRUE,
            
            card_header(span("In the selected year (", 
                             textOutput("selected_year", inline = TRUE),
                             "), how much of the time was DO below the selected threshold?"),
                        # tooltip(
                        #     bsicons::bs_icon("info-circle"),
                        #     "Info here about % of readings, and how typical/unusual was determined"
                        # ) # end tooltip
                        popover(
                            actionButton("btnYearlyMap", "Map Details", 
                                         icon = icon("map-location-dot"),
                                         width = 175,
                                         class = "small-btn"),
                            p("A single year and various other options can be selected in the left sidebar. The values shown on the map are the percent of readings that year where the DO measurement fell below the selected threshold."),
                            p(strong("What do we want to see?"), "Lighter, more yellow colors are better. Low DO can be a normal event, but we still don't want it occurring much of the year."),
                            # p(strong("Clicking on a point"), "will open a sidebar with more details about the selected station: e.g., how long has data been recorded; what are long-term median values; what are the trend results; graphics of DO through time; and details on how DO in a single year compares to DO in other years at this station."),
                            br(),
                            p(strong("Each point"), "represents a single year summary of low DO at a single station. Clicking on a point will open a sidebar with more information about that station."),
                            p(strong("Point color"), "represents the percent of the year with low DO. In the left sidebar, you can change this to represent whether the amount of low DO was typical for a station, or unusually high."),
                            p(strong("Point shape"), "represents whether this amount of low DO is typical for a station, or if it was an unusually high value compared to other values at the station."),
                            
                            br(),
                            p("See the 'About' tab for more detail on data sources and calculations."),
                            title = "Yearly map details",
                            placement = "right"
                        ), # end popover
            ), # end header
            
            p("'Typical' and 'Unusual' were defined for each station based on data since 2002. For some stations, human impacts that cause low DO have existed longer than this dataset. So, just because a year here is 'typical' for a station, that doesn't necessarily mean it is good."),
            
            # sidebar layout, for map options
            layout_sidebar(
                sidebar = sidebar(
                    # title = "Map Options",
                    width = "30%",
                    position = "left",
                    open = TRUE,
                    
                    
                    # choose threshold
                    radioButtons("threshold_sel", "Define 'low' DO as:",
                                 choiceNames = c("< 5 mg/L", "< 2 mg/L"),
                                 choiceValues = c("LT5", "LT2"),
                                 selected = "LT5"),
                    
                    # year selection
                    div(
                        class = "highlight-slider",
                        sliderInput(
                            "year",
                            "Select Year:",
                            min = min(tomap$year),
                            max = max(tomap$year),
                            value = max(tomap$year),
                            step = 1,
                            sep = "",
                            animate = FALSE
                        )
                    ),
                    
                    # size points by % of time where DO was low?
                    div(
                        style = "display: flex; align-items: top;",
                        checkboxInput("size_sel", "Size points by % time",
                                      value = FALSE),
                        tooltip(
                            bsicons::bs_icon("info-circle", 
                                             width = "50", height = "20",
                                             # style = " color: blue;",
                                             color = "blue"
                            ),
                            "Points will be sized according to the percent of time DO was below the selected threshold"
                        )
                        
                    ),
                    
                    # station type
                    checkboxGroupInput("unus_sel", "Show stations where the amount of low DO was:",
                                       choiceNames = c("typical", "unusual"),
                                       choiceValues = c(0, 1),
                                       selected = c(0, 1)),
                    
                    
                    sliderInput("cutoff_range", 
                                "Limit to stations in this range of low DO frequency", 
                                min = 0, 
                                max = 100, 
                                value = c(0, 100), 
                                step = 1),
                    
                    
                    
                    # choose how to color points
                    radioButtons("color_sel", "Color points by:",
                                 choiceNames = c("typical or unusual",
                                                 "% of year with low DO"),
                                 choiceValues = c("col_unus",
                                                  "col_time.hypoxic"),
                                 selected = "col_time.hypoxic")
                ), # end sidebar
                
                # map
                leafletOutput("map_timeLow")
                
            ) # end tab's layout_sidebar
            
        ), # end nav-panel 2
        
        # panel 4: Instructions ----
        about_ui
        
    ) # end nav-panel layout
    
)  # end ui


# Server ----
server <- function(input, output, session) {
    # waiter new ----
    w <- Waiter$new(
        html = spin_3(),
        color = transparent(0.5)
    )
    
    # medians map setup----
    output$map_medians <- renderLeaflet({
        # base map
        m <- leaflet() |> 
            addTiles(group = "Default (OpenStreetMap") |> 
            addProviderTiles(provider = providers$CartoDB.Positron,
                             group = "Positron (CartoDB)") |> 
            addProviderTiles(provider = providers$Esri,
                             group = "Esri") |> 
            setView(lng = -98.5, lat = 42.8, zoom = 2.4)  |>  # Central US, zoomed out to include AK and HI
            addLayersControl(baseGroups = c("Default (OpenStreetMap)",
                                            "Positron (CartoDB)",
                                            "Esri"))
        
        # add what I want to render the first time the map shows
        # filter the data to median do
        filtered2 <- tomap_medians |>
            filter(param == "domgl_median") |>
            mutate(
                # if user wants to size points by pct, use size 1. if they don't, make it 4.
                # size1 = case_when(input$median.size_sel == FALSE ~ 6,
                #                   .default = value)
                size1 = 6
            ) 
        
        # use the right color palette function
        palette_trnd <- palette_median.mgl
        
        # add the markers to the map
        m <- m |>
            addCircleMarkers(
                data = filtered2,
                lng = ~long,
                lat = ~lat,
                layerId = ~station,
                color = "black",
                weight = 1,
                fillColor = ~palette_trnd(value),
                fillOpacity = 0.7,
                radius = ~size1,  # base it on the value
                label = ~paste(station, param, round(value, 1))
            )  |>
            clearControls()
        
        # add the legend
        m <- m |>
            addLegend(position = "bottomright",
                      colors = palette_median.mgl(c(2, 4, 6, 8, 10)),
                      labels = c(2, 4, 6, 8, 10),
                      title = "median DO (mg/L)",
                      opacity = 0.7)
        
        # return the map
        m
        
    })
    
    # trends map setup----
    output$map_trends <- renderLeaflet({
        # base map
        m <- leaflet() |> 
            addTiles(group = "Default (OpenStreetMap") |> 
            addProviderTiles(provider = providers$CartoDB.Positron,
                             group = "Positron (CartoDB)") |> 
            addProviderTiles(provider = providers$Esri,
                             group = "Esri") |> 
            setView(lng = -98.5, lat = 42.8, zoom = 2.4)  |>  # Central US, zoomed out to include AK and HI
            addLayersControl(baseGroups = c("Default (OpenStreetMap)",
                                            "Positron (CartoDB)",
                                            "Esri"))
        
        # filter the data
        filtered2 <- stn_trends_long |> 
            filter(param == "domgl_median")
        
        # use the right color palette function
        palette_trnd <- palette_trnd.mgl
        
        # add markers to the map
        m <- m |> 
            addCircleMarkers(
                data = filtered2,
                lng = ~long,
                lat = ~lat,
                layerId = ~station,
                color = "black",
                weight = 1,
                fillColor = ~palette_trnd(map_color),
                fillOpacity = 0.7,
                radius = 5,
                label = ~paste(station, param, round(trend, 2))
            )  |> 
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
    
    # time low map setup ----
    output$map_timeLow <- renderLeaflet({
        tomap_sub <- tomap |> 
            mutate(size1 = 6,                   # default to *not* sizing by time low
                   symbol = symbol_time) |>     # symbol by unus; color by time
            filter(year == max(tomap$year),
                   threshold == "LT5")
        
        # figure out typical and unusual rows
        rows_unusual <- which(tomap_sub$unusual == 1)
        rows_typical <- which(tomap_sub$unusual == 0)
        
        m <- leaflet(tomap_sub) |> 
            addTiles(group = "Default (OpenStreetMap") |> 
            addProviderTiles(provider = providers$CartoDB.Positron,
                             group = "Positron (CartoDB)") |> 
            addProviderTiles(provider = providers$Esri,
                             group = "Esri") |> 
            setView(lng = -98.5, lat = 42.8, zoom = 2.4)  |>  # Central US, zoomed out to include AK and HI
            addLayersControl(baseGroups = c("Default (OpenStreetMap)",
                                            "Positron (CartoDB)",
                                            "Esri"))
        # add markers for the initial subset of data
        m <- m |> 
            addMarkers(
                data = tomap_sub[rows_typical, ],
                group = "in typical range",
                lng = ~long,
                lat = ~lat,
                layerId = ~station,
                icon = ~icons(
                    iconUrl = symbol,
                    iconWidth = size1*2,   # because size1 is for radius, and icons use diameter
                    iconHeight = size1*2
                ),
                label = ~paste(station, year, round(pct, 1))
            ) |> 
            addMarkers(
                data = tomap_sub[rows_unusual, ],
                lng = ~long,
                lat = ~lat,
                layerId = ~station,
                icon = ~icons(
                    iconUrl = symbol,
                    iconWidth = size1*2,   # because size1 is for radius, and icons use diameter
                    iconHeight = size1*2
                ),
                label = ~paste(station, year, round(pct, 1)) 
            ) |> 
            clearControls() 
        
        # add legends
        m <- m |> 
            addLegendSymbol(title = "Shape",
                            values = c('typical', 'unusual'), 
                            shape = c('circle', 'rect'), 
                            fillColor = palette_time.hypoxic(50),
                            color = 'black',
                            opacity = 0.7,
                            width = 15,
                            position = "bottomright") |> 
            addLegend(position = "bottomright",
                      colors = palette_time.hypoxic(c(0, 25, 50, 75, 100)),
                      labels = c(0, 25, 50, 75, 100),
                      title = "Color: % of year",
                      opacity = 0.7)
        
        m
    })
    
    
    # medians map update ----
    observe({
        filtered2 <- tomap_medians |>
            filter(param == input$medianParam_sel) |>
            mutate(
                # if user wants to size points by pct, use size 1. if they don't, make it 4.
                # size1 = case_when(input$median.size_sel == FALSE ~ 6,
                #                   .default = value)
                size1 = 6
            ) 
        
        
        # use the right color palette function
        palette_trnd <- if(input$medianParam_sel == "domgl_median") {
            palette_median.mgl
        } else {
            palette_time.hypoxic
        }
        
        # see if a station has been selected
        current_station_id <- selected_station()
        
        # if no station selected, normal map:
        if(is.null(current_station_id)){
            
            m <- leafletProxy("map_medians", data = filtered2) |>
                clearMarkers() |>
                addCircleMarkers(
                    lng = ~long,
                    lat = ~lat,
                    layerId = ~station,
                    color = "black",
                    weight = 1,
                    fillColor = ~palette_trnd(value),
                    fillOpacity = 0.7,
                    radius = ~size1,  # base it on the value
                    label = ~paste(station, param, round(value, 1))
                )  |>
                clearControls()
            
        } else {
            # make the selected station stand out
            m <- leafletProxy("map_medians", data = filtered2) |>
                clearMarkers() |>
                addCircleMarkers(
                    lng = ~long,
                    lat = ~lat,
                    layerId = ~station,
                    color = ~ifelse(station == current_station_id, "#E67E22", "black"),
                    weight = ~ifelse(station == current_station_id, 4, 1),
                    opacity = ~ifelse(station == current_station_id, 1.0, 0.7),
                    fillColor = ~palette_trnd(value),
                    fillOpacity = ~ifelse(station == current_station_id, 0.9, 0.7),
                    radius = ~ifelse(station == current_station_id, 10, 6),
                    label = ~paste(station, param, round(value, 1))
                )  |>
                clearControls()
        }
        
        
        if(input$medianParam_sel != "domgl_median"){
            m <- m |>
                addLegend(position = "bottomright",
                          colors = palette_time.hypoxic(c(0, 25, 50, 75, 100)),
                          labels = c(0, 25, 50, 75, 100),
                          title = "median % of year with low DO",
                          opacity = 0.7)
        } else {
            m <- m |>
                addLegend(position = "bottomright",
                          colors = palette_median.mgl(c(2, 4, 6, 8, 10)),
                          labels = c(2, 4, 6, 8, 10),
                          title = "median DO (mg/L)",
                          opacity = 0.7)
        }
        
        
        m
        
    })
    
    
    # trends map update ----
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
        
        # see if a station has been selected
        current_station_id <- selected_station()
        
        # if not, normal map:
        if(is.null(current_station_id)){
            
            leafletProxy("map_trends", data = filtered2) |> 
                clearMarkers() |> 
                addCircleMarkers(
                    lng = ~long,
                    lat = ~lat,
                    layerId = ~station,
                    color = "black",
                    weight = 1,
                    fillColor = ~palette_trnd(map_color),
                    fillOpacity = 0.7,
                    radius = 6,
                    label = ~paste(station, param, round(trend, 2))
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
            
        } else {
            # make the selected station a different size and weight
            leafletProxy("map_trends", data = filtered2) |> 
                clearMarkers() |> 
                addCircleMarkers(
                    lng = ~long,
                    lat = ~lat,
                    layerId = ~station,
                    color = ~ifelse(station == current_station_id, "#E67E22", "black"),
                    weight = ~ifelse(station == current_station_id, 4, 1),
                    opacity = ~ifelse(station == current_station_id, 1.0, 0.7),
                    fillColor = ~palette_trnd(map_color),
                    fillOpacity = ~ifelse(station == current_station_id, 0.9, 0.7),
                    radius = ~ifelse(station == current_station_id, 10, 6),
                    label = ~paste(station, param, round(trend, 2))
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
        }
        
        
    })
    
    # timeLow map update----
    
    # make text output for card header
    output$selected_year <- renderText({
        input$year
    })
    
    # Shiny assistant's version to circle selected station:
    observe({
        # waiter show ----
        # spinner while everything is recalculating
        w$show()
        on.exit(w$hide())
        
        # Filter data based on the selected year and cutoff value
        
        tomap_sub <- tomap |> rowwise() |> 
            mutate(
                # if user wants to size points by pct, use size 1. if they don't, make it 4.
                size1 = case_when(input$size_sel == FALSE ~ 6,
                                  .default = size)) |> 
            filter(year == input$year, 
                   threshold == input$threshold_sel,
                   pct >= input$cutoff_range[1],
                   pct <= input$cutoff_range[2],
                   unusual %in% input$unus_sel)  
        
        
        # choose symbols based on what user wants to color by:
        tomap_sub <- tomap_sub |> 
            mutate(symbol = case_when(
                input$color_sel == "col_unus" ~ symbol_unus,
                input$color_sel == "col_time.hypoxic" ~ symbol_time
            ))
        
        # Get the currently selected station ID
        current_station_id <- selected_station()
        
        # figure out typical and unusual rows
        rows_unusual <- which(tomap_sub$unusual == 1)
        rows_typical <- which(tomap_sub$unusual == 0)
        
        # Clear all markers
        m <- leafletProxy("map_timeLow", data = tomap_sub) |>
            clearMarkers() |> 
            clearControls()
        
        # If a station is selected, add a highlight circle around its marker
        if(!is.null(current_station_id)) {
            # Find the selected station in the data
            selected_row <- which(tomap_sub$station == current_station_id)
            
            if(length(selected_row) > 0) {
                # Add a circle marker underneath the icon for the selected station
                selected_data <- tomap_sub[selected_row, ]
                
                m <- m |> addCircleMarkers(
                    data = selected_data,
                    lng = ~long,
                    lat = ~lat,
                    radius = 10,  # Larger than the icon to create a highlight effect
                    color = "#E67E22",  # Orange outline
                    weight = 4,
                    opacity = 1.0,
                    fillOpacity = 0,  # Transparent fill
                    label = ~paste(station, year, round(pct, 1))
                )
            }
        }
        
        # Add typical markers only if there are any
        if(length(rows_typical) > 0){
            m <- m |> addMarkers(
                data = tomap_sub[rows_typical, ],
                group = "in typical range",
                lng = ~long,
                lat = ~lat,
                layerId = ~station,
                icon = ~icons(
                    iconUrl = symbol,
                    iconWidth = size1*2,   # because size1 is for radius, and icons use diameter
                    iconHeight = size1*2
                ),
                label = ~paste(station, year, round(pct, 1))
            ) 
        }
        
        # Add unusual markers only if there are any
        if(length(rows_unusual) > 0) {
            m <- m |> addMarkers(
                data = tomap_sub[rows_unusual, ],
                lng = ~long,
                lat = ~lat,
                layerId = ~station,
                icon = ~icons(
                    iconUrl = symbol,
                    iconWidth = size1*2,   # because size1 is for radius, and icons use diameter
                    iconHeight = size1*2
                ),
                label = ~paste(station, year, round(pct, 1)) 
            )
        }
        
        # deal with legends
        if(input$size_sel == TRUE){
            m <- m |> 
                addLegendCustom(
                    sizes = legend_sizes$size, 
                    labels = legend_sizes$label,
                    colors = "black",
                    position = "bottomleft",   # Custom position
                    opacity = 0.5            # Custom opacity (0 to 1)
                )
        }
        
        if(input$color_sel == "col_unus"){
            m <- m |> 
                addLegendSymbol(title = "Shape & Fill",
                                values = c('typical', 'unusual'), 
                                shape = c('circle', 'rect'), 
                                fillColor = palette_unus(c(0, 1)),
                                color = 'black',
                                opacity = 0.7,
                                width = 15,
                                position = "bottomright")
        }
        
        if(input$color_sel == "col_time.hypoxic"){
            m <- m |> 
                addLegendSymbol(title = "Shape",
                                values = c('typical', 'unusual'), 
                                shape = c('circle', 'rect'), 
                                fillColor = palette_time.hypoxic(50),
                                color = 'black',
                                opacity = 0.7,
                                width = 15,
                                position = "bottomright") |> 
                addLegend(position = "bottomright",
                          colors = palette_time.hypoxic(c(0, 25, 50, 75, 100)),
                          labels = c(0, 25, 50, 75, 100),
                          title = "Fill: % of year",
                          opacity = 0.7)
        }
        
        m
    })
    
    # map clicks ----
    # Create reactive value to store selected station
    selected_station <- reactiveVal(NULL)
    
    # Add click observers to all maps
    observeEvent(input$map_trends_marker_click, {
        click <- input$map_trends_marker_click
        
        # waiter show ----
        w$show()
        on.exit(w$hide())
        
        station_id <- click$id
        selected_station(station_id)
        
        # Open the sidebar when a station is clicked
        sidebar_toggle("stn_sidebar", open = TRUE)
    })
    
    observeEvent(input$map_medians_marker_click, {
        click <- input$map_medians_marker_click
        
        # waiter show ----
        w$show()
        on.exit(w$hide())
        
        
        station_id <- click$id
        selected_station(station_id)
        # Open the sidebar when a station is clicked
        sidebar_toggle("stn_sidebar", open = TRUE)
        
    })
    
    observeEvent(input$map_timeLow_marker_click, {
        click <- input$map_timeLow_marker_click
        
        # waiter show ----
        w$show()
        on.exit(w$hide())
        
        station_id <- click$id
        selected_station(station_id)
        # Open the sidebar when a station is clicked
        sidebar_toggle("stn_sidebar", open = TRUE)
        
    })
    
    
    # Plotly graphs----
    output$stn_timeSeries <- renderPlotly({
        
        # identify the station
        stn <- selected_station()
        
        # subset the data frames
        stn_timeSeries <- mgl_timeSeries |> 
            filter(station == stn)
        stn_hypox_annual <- hypoxia_annual |> 
            filter(station == stn)
        
        line_2 <- ifelse("2 mg/L" %in% input$thresh_sel == TRUE, TRUE, FALSE)
        line_5 <- ifelse("5 mg/L" %in% input$thresh_sel == TRUE, TRUE, FALSE)
        ribb_var <- ifelse(input$var_sel == TRUE, TRUE, FALSE)
        ribb_minmax <- ifelse(input$minmax_sel == TRUE, TRUE, FALSE)
        
        
        graph_do(data_monthly = stn_timeSeries,
                 data_yearly = stn_hypox_annual,
                 station = stn,
                 thresh2 = line_2,
                 thresh5 = line_5,
                 ribbon_var = ribb_var,
                 ribbon_minmax = ribb_minmax)
        
    })
    
    # selected year distn graphs ----
    output$lowDOdist <- renderPlot({
        req(selected_station(), input$year)
        
        # identify the station
        stn <- selected_station()
        yr <- input$year
        
        col_lowDO <- "#A50026"
        
        # subset the data frame
        stn_hypox_annual <- hypoxia_annual |> 
            filter(station == stn)
        
        p1 <- plot_yrdist(stn_hypox_annual, LT2,
                          yr, col_lowDO) +
            labs(subtitle = "< 2 mg/L")
        
        p2 <- plot_yrdist(stn_hypox_annual, LT5,
                          yr, col_lowDO) +
            labs(subtitle = "< 5 mg/L")
        
        # combine low DO plots
        p3 <- p1 / p2 +
            plot_layout(axes = "collect") +
            plot_annotation(
                title = "% of year with low DO, compared to other years at this station"
            )
        
        p3
    })
    
    # reserve info ----
    output$reserve_info <- renderText({
        req(selected_station())
        
        res_inf <- stn_summaries |> 
            filter(station == selected_station()) |> 
            select(station, StationName, ReserveCode, ReserveName, ReserveState, ReserveWebsite, NERRAPage) |> 
            distinct()
        
        res_out <- res_inf |> 
            glue_data("<b>{station}</b> is the {StationName} monitoring station at {ReserveName} ({ReserveCode}) NERR in {ReserveState}. 
                      For more information about the reserve, please visit <a href='{ReserveWebsite}' target='_blank'>their website</a>.")
        
        HTML(res_out)
    })
    
    
    # station table ----
    output$stn_tbl <- renderReactable({
        req(selected_station())
        
        tbl <- stn_summaries |> 
            filter(station == selected_station()) |> 
            select(1:5) |> 
            mutate(`Time Series Length (years)` = ifelse(row_number() == 1,
                                                         `Time Series Length (years)`, "")) |> 
            select(-station)
        
        reactable(tbl,
                  sortable = FALSE,
                  columns = list(
                      `Time Series Length (years)` = colDef(align = "center"),
                      `Long Term Median` = colDef(align = "center")
                  ))
    })
    
    # station sidebar ui ----
    # with accordion
    output$station_section <- renderUI({
        req(selected_station())
        
        accordion(
            id = "station_accordion",
            open = FALSE,
            
            h4(paste0("Selected Station: ", selected_station())),
            
            htmlOutput("reserve_info"),
            br(),
            br(),
            
            span("Station details:",
                 tooltip(
                     bsicons::bs_icon("info-circle"),
                     HTML("<p>Pop graphs out to full screen from the bottom right corner.</p>
                    <p>Below the graphs is a slider bar to let you change how much of the x-axis is visible.</p>")
                 )),
            br(),
            
            
            # numeric outputs
            accordion_panel(
                title = "Numeric Summary",
                reactableOutput("stn_tbl")
            ),
            
            # Plotly graph, with accordioned options
            accordion_panel(
                title = "Time Series Graphs",
                tags$small("The graph section can be popped out to full screen from the bottom right corner."),
                
                card(
                    full_screen = TRUE,
                    height = "50vh",
                    
                    
                    # options popover
                    popover(
                        actionButton("btn", "Graph options", 
                                     icon = icon("sliders"),
                                     width = 200,
                                     class = "small-btn"),
                        div(
                            checkboxGroupInput("thresh_sel", "Select threshold(s) of interest:",
                                               choices = c("2 mg/L", "5 mg/L"),
                                               selected = c("2 mg/L", "5 mg/L")),
                            checkboxInput("var_sel", "Add middle 50% of values",
                                          value = FALSE),
                            checkboxInput("minmax_sel", label = "Add min/max",
                                          value = FALSE)
                        ),
                        title = "Graph Options",
                        placement = "right"
                    ),
                    
                    # graph
                    withWaiter(plotlyOutput("stn_timeSeries"))
                )
            ),
            
            accordion_panel(
                title = paste("Selected Year:", input$year),
                tags$small("Choose a year from the 'Select-a-Year' tab."),
                
                withWaiter(plotOutput("lowDOdist", height = "300px", width = "100%"))
            )
        )
    })
    
    # value box ----
    output$station_count <- renderText({
        tmp <- stn_trends_long |> 
            filter(param == "domgl_median",
                   direction != "not calculated") |> 
            summarize(total = n(),
                      decreasing = sum(significant == "yes" & direction == "decreasing"))
        pct <- round(tmp$decreasing / tmp$total * 100)
        glue("{tmp$decreasing} of {tmp$total}")
    })
    
    
}

# Run the app
shinyApp(ui, server)
