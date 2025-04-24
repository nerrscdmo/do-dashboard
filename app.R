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

# setup ----
source(here::here("R", "functions.R"))
source("global.R")

# UI ----
ui <- page_fluid(
    
    # css/html styling ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # header
    h5("NERRS Monitoring: Dissolved Oxygen"),
    p("Choose a tab to explore DO nationally. Change options in the left sidebar, and click on a station to see more detail in a right sidebar."),

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
                                tooltip(
                                    bsicons::bs_icon("info-circle"),
                                    "If a station had at least 5 years of data, a trend through time was calculated using simple linear regression."
                                ), # end tooltip
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
                        ), # end sidebar
                        
                        # map
                        leafletOutput("map_trends")
                    
                        ) # end tab's layout_sidebar
                
                    ), # end nav panel 1
                
                
                # panel 2: low do map ----
                nav_panel(
                    "Low DO by year",
                    full_screen = TRUE,
                    
                    card_header("In the selected year, how much of the time was DO below the selected threshold?",
                                tooltip(
                                    bsicons::bs_icon("info-circle"),
                                    "Info here about % of readings, and how typical/unusual was determined"
                                ) # end tooltip
                    ), # end header
                    
                    p("'Typical' and 'Unusual' were defined based on data since 2002. For some stations, human impacts that cause low DO have existed longer than this dataset. So, just because a year here is 'typical' for a station, that doesn't necessarily mean it is good."),
                    
                    # sidebar layout, for map options
                    layout_sidebar(
                        sidebar = sidebar(
                            # title = "Map Options",
                            width = "30%",
                            position = "left",
                            open = TRUE,
                            
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
                            
                            # choose how to color points
                            radioButtons("color_sel", "Color points by:",
                                         choiceNames = c("typical or unusual",
                                                         "% of year with low DO"),
                                         choiceValues = c("col_unus",
                                                          "col_time.hypoxic"),
                                         selected = "col_time.hypoxic"),
                            
                            # size typical points by amount?
                            div(
                                style = "display: flex; align-items: top; gap: 5px;",
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
                            
                            # choose threshold
                            radioButtons("threshold_sel", "DO threshold",
                                         choiceNames = c("<2 mg/L", "<5 mg/L"),
                                         choiceValues = c("LT2", "LT5"),
                                         selected = "LT5"),
                            
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
                                        step = 1)
                        ), # end sidebar
                        
                        # map
                        leafletOutput("map_timeLow")
                    
                    ) # end tab's layout_sidebar
                    
                )  # end nav-panel 2
                
            ) # end nav-panel layout

)  # end ui


# Server ----
server <- function(input, output, session) {
    
    # map setup ----
    output$map_trends <- renderLeaflet({
        leaflet() |> 
            addTiles(group = "Default (OpenStreetMap") |> 
            addProviderTiles(provider = providers$CartoDB.Positron,
                             group = "Positron (CartoDB)") |> 
            addProviderTiles(provider = providers$Esri,
                             group = "Esri") |> 
            setView(lng = -98.5, lat = 42.8, zoom = 2.4)  |>  # Central US, zoomed out to include AK and HI
            addLayersControl(baseGroups = c("Default (OpenStreetMap)",
                                            "Positron (CartoDB)",
                                            "Esri"))
    })
    
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
                popup = ~paste(station, year, round(pct, 1))
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
                popup = ~as.character(round(pct, 1)) 
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
                      title = "Fill: % of year",
                      opacity = 0.7)
        
        m
    })
    
    
    
    # trends ----
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
                layerId = ~station,
                color = "black",
                weight = 1,
                fillColor = ~palette_trnd(map_color),
                fillOpacity = 0.7,
                radius = 5
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
    
    # timeLow ----
    # Update map_timeLow based on selections
    observe({
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
        
        # figure out typical and unusual rows
        rows_unusual <- which(tomap_sub$unusual == 1)
        rows_typical <- which(tomap_sub$unusual == 0)
        
        
        m <- leafletProxy("map_timeLow", data = tomap_sub) |>
            clearMarkers() |> 
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
                popup = ~paste(station, year, round(pct, 1))
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
                popup = ~paste(station, year, round(pct, 1)) 
            ) |> 
            clearControls() 
        
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
    
    # Add click observers to both maps
    observeEvent(input$map_trends_marker_click, {
        click <- input$map_trends_marker_click
        station_id <- click$id
        selected_station(station_id)
        # Open the sidebar when a station is clicked
        sidebar_toggle("stn_sidebar", open = TRUE)
        
    })
    
    observeEvent(input$map_timeLow_marker_click, {
        click <- input$map_timeLow_marker_click
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
    
    # station table ----
    output$stn_tbl <- renderReactable({
        req(selected_station())
        
        tbl <- stn_summaries |> 
            filter(station == selected_station()) |> 
            select(-station) |> 
            mutate(`Time Series Length (years)` = ifelse(row_number() == 1,
                                                         `Time Series Length (years)`, ""))
        
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
            span("Some info here about options",
                 tooltip(
                     bsicons::bs_icon("info-circle"),
                     HTML("<p>Pop graphs out to full screen from the bottom right corner.</p>
                    <p>Below the graphs is a slider bar to let you change how much of the x-axis is visible.</p>")
                 )),
            
            # Plotly options sidebar
            
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
                    plotlyOutput("stn_timeSeries")
                )
            ),
            
            # numeric outputs
            accordion_panel(
                title = "Numeric Summary",
                reactableOutput("stn_tbl")
            )
        )
    })
    
    
}

# Run the app
shinyApp(ui, server)
