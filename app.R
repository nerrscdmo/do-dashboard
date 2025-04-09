library(shiny)
library(bslib)
library(bsicons)
library(here)
library(plotly)
library(leaflet)
library(leaflegend)
library(htmltools)
library(dplyr)
library(tidyr)
library(ggplot2)

# setup ----

# bring in data frames (calculations have been made outside the app)
load(here::here("data_wq", "do_dataframes.RData"))
source(here::here("R", "functions.R"))

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

hypoxia_annual <- tomap |> 
    select(station, year, threshold, pct) |> 
    pivot_wider(names_from = threshold,
                values_from = pct)

mgl_timeSeries <- stn_mmyr |> 
    select(station, year, month,
           domgl_median, domgl_p25, domgl_p75,
           domgl_min, domgl_max) |> 
    mutate(date = lubridate::ymd(paste(year, month, "01")))


# color palettes and shapes ----
# palette <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
palette_unus <- colorFactor(palette = c("#2166AC", "#B2182B"),  # from Tol's BuRd
                            levels = c(0, 1))

palette_time.hypoxic <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))

palette_trnd.mgl <- colorFactor(palette = c("#2166AC", "#B2182B", "#FFEE99", "#7F7F7F"),  # from Tol's BuRd EXCEPT for 'not calcd' - need to check this
                                levels = c("increasing", "decreasing", "no trend", "not calculated"))

palette_trnd.thrsh <- colorFactor(palette = c("#762A83", "#1B7837", "#FFEE99", "#7F7F7F"),  # from Tol's PRGn
                                  levels = c("increasing", "decreasing", "no trend", "not calculated"))
# use circles for typical and squares for unusual - change here if desired
shape_assignment <- function(x){
    case_when(is.na(x) ~ "diamond",
              x == 0 ~ "circle",
              x == 1 ~ "rect",
              .default = "triangle")
}


# big map data frame ----
# define everything, for later custom creation of symbols
tomap <- tomap |> 
    mutate(size = case_when(sqrt(pct) <= 3 ~ 3,
                            is.na(pct) ~ 3,
                            .default = sqrt(pct)),
           size = size + 1,                           # for slightly bigger symbols
           shape = shape_assignment(unusual),
           color_unus = palette_unus(unusual),
           color_time.hypoxic = palette_time.hypoxic(pct),
           pct_rounded = round(pct))

# size legend data frame ----
legend_sizes <- data.frame(
    value = c(3, 25, 50, 75),
    label = c("<10", "25", "50", "75")
) |> 
    mutate(size = case_when(sqrt(value) <= 3 ~ 3,
                            .default = sqrt(value)),
           size = size + 1)

# symbol setup----

# TYPICAL VS UNUSUAL
# make the grid for symbols
symbols_unus.grid <- data.frame(
    unusual = c(0, 1)
) |> 
    mutate(shape = shape_assignment(unusual),
           color = palette_unus(unusual))
# make the symbols
symbols_unus <- Map(
    f = makeSymbol,
    shape = symbols_unus.grid$shape,
    fillColor = symbols_unus.grid$color,
    color = "black",
    opacity = 0.5,
    height = 20,
    width = 20
)
# add the url to the time grid symbol data frame for joining with 'tomap'
symbols_unus.grid$symbol_unus <- unlist(symbols_unus)
# join to the mapping df
tomap <- left_join(tomap, symbols_unus.grid,
                   by = "unusual")

# TIME HYPOXIC
# make combinations of typical/unusual and every rounded percent
symbols_time.grid <- expand.grid(
    unusual = c(0, 1),
    pcts = c(0:100)
) |> 
    mutate(shape = shape_assignment(unusual),
           color = palette_time.hypoxic(pcts))    
# generate all those symbols
symbols_time.hypoxic <- Map(
    f = makeSymbol,
    shape = symbols_time.grid$shape,
    fillColor = symbols_time.grid$color,
    color = "black",
    opacity = 0.5,
    height = 20,
    width = 20
)
# add the url to the time grid symbol data frame for joining with 'tomap'
symbols_time.grid$symbol_time <- unlist(symbols_time.hypoxic)
# join to the mapping df
tomap <- left_join(tomap, symbols_time.grid,
                   by = c("unusual",
                          "pct_rounded" = "pcts"))


# UI ----
ui <- page_fluid(
    
    # header info
    layout_columns(
        col_widths = c(4, 8),
        h1("Can our estuaries breathe?"),
        # checkboxInput("sync_maps", "Zoom and Pan Maps Together", value = TRUE),
        p("Here is where we might say something about how time spent below thresholds seems to be patchy, and is different every year. The default for the map is to show how much of the most recent year was spent with DO < 5 mg/L. Maybe make an info box for why this is important? Code is available on ", a('GitHub', href='https://github.com/swmpkim/estuary-dashboard', target = '_blank'), ".")
        
    ),
    
    # css/html styling ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    
    layout_sidebar(
        # sidebar: stn info ----
        sidebar = sidebar(id = "stn_sidebar",
                          position = "right",
                          width = "50%",
                          open = FALSE,
                          # title = "Station Information",
                          uiOutput("station_section")
        ),
        
        
        # column wrap for navsets so they can be fillable 
        layout_column_wrap(
            width = "100%",
            height = "80vh", 
            
            # map tabs ----
            navset_card_tab(
                full_screen = TRUE,
                
                # card 1: trend map ----
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
                        sidebar = sidebar(title = "Map Options",
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
                        ),
                        # map
                        leafletOutput("map_trends")
                    )
                ), # end card 1
                
                
                # card 2: low do map ----
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
                        sidebar = sidebar(title = "Map Options",
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
                                                  "Points will be sized according to the percentage of time DO was below the selected threshold"
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
                    )
                )  # end card 2
                
                
            ) # end nav-panel layout
            
        ) # end column-wrap layout
        
        
    ) # end layout sidebar
    
    
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
    
    # Station table ----
    output$station_info <- renderUI({
        stn <- selected_station()
        
        if(is.null(stn)) {
            return(tagList(
                "Click on a station from either map to see detailed information here."
            ))
        }
        
        # Filter stn_trends for the selected station
        station_data <- stn_trends %>% 
            filter(station == stn)
        
        if(nrow(station_data) == 0) {
            return(tagList(
                h4("Station: ", stn),
                "No detailed information available for this station."
            ))
        }
        
        # Create info display
        tagList(
            h4("Station: ", station_data$station[1]),
            
            hr(),
            
            h5("Trends:"),
            tags$ul(
                tags$li(strong("DO Concentration: "), 
                        ifelse(is.na(station_data$domgl_median.trend), "Not calculated", 
                               paste0(round(station_data$domgl_median.trend, 2), " mg/L per year"))),
                tags$li(strong("Time DO < 2 mg/L: "), 
                        ifelse(is.na(station_data$LT2.trend), "Not calculated", 
                               paste0(round(station_data$LT2.trend, 1), "% per year"))),
                tags$li(strong("Time DO < 5 mg/L: "), 
                        ifelse(is.na(station_data$LT5.trend), "Not calculated", 
                               paste0(round(station_data$LT5.trend, 1), "% per year")))
                # ),
                # 
                # hr(),
                # 
                # h5("Typical values:"),
                # tags$ul(
                #     tags$li(strong("Median DO: "), paste0(round(station_data$domgl_median_typical, 1), " mg/L")),
                #     tags$li(strong("Typical % time DO < 2 mg/L: "), paste0(round(station_data$LT2_typical, 1), "%")),
                #     tags$li(strong("Typical % time DO < 5 mg/L: "), paste0(round(station_data$LT5_typical, 1), "%"))
            )
        )
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
                
                card(
                    full_screen = TRUE,
                    
                    # options popover
                    popover(
                        actionButton("btn", "Graph options", 
                                     icon = icon("sliders"),
                                     width = 200),
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
                "some text here"
            )
        )
    })
    
    
}

# Run the app
shinyApp(ui, server)
