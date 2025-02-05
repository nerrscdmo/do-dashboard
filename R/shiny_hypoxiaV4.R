library(shiny)
library(bslib)
library(bsicons)
library(here)
library(leaflet)
library(dplyr)
library(ggplot2)

# Sample data (replace with your hyp_summer_cat data)
load(here::here("data_wq", "summer_hypoxia.RDS"))
load(here::here("data_wq", "hypox_by_reserve_annual.RDS"))

reserve_coords <- hyp_summer_cat |> 
    mutate(reserve = substr(station, 1, 3)) |> 
    select(reserve, latitude, longitude) |> 
    distinct() |> 
    summarize(.by = reserve,
              latitude = mean(latitude, na.rm = TRUE),
              longitude = mean(longitude, na.rm = TRUE))

hypox_by_reserve <- left_join(hypox_by_reserve, reserve_coords) |> 
    mutate(hypox_rad = nStations_LT2BadYear * 3)

hypox_by_year <- hypox_by_reserve |> 
    summarize(.by = year,
              nStations_LT2BadYear = sum(nStations_LT2BadYear, na.rm = TRUE),
              nStations_LT5BadYear = sum(nStations_LT5BadYear, na.rm = TRUE),
              nStationsReporting = sum(nStationsReporting, na.rm = TRUE))



# Create color palette
palette <- colorFactor(palette = "YlOrRd", domain = hyp_summer_cat$category)

# UI ----
ui <- page_fillable(
    
    layout_columns(
        col_widths = c("5, 7"),
        h1("Can our estuaries breathe?"),
        p("Here's some info about the data display. The default display shows the most recent year, but you can change that with the slider bar below. Code is available on ", a('GitHub', href='https://github.com/swmpkim/estuary-dashboard', target = '_blank'), ".")
        
    ),
       
    # styling 
    tags$head(
        tags$style(HTML("
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
    ),
    
    
    # value boxes
    card(
        max_height = "200px",
        layout_columns(
            fill = TRUE,
            
            list(
                # year selection
                sliderInput(
                    "year",
                    "Select Year:",
                    min = min(hyp_summer_cat$year),
                    max = max(hyp_summer_cat$year),
                    value = max(hyp_summer_cat$year),
                    step = 1,
                    sep = ""
                ),
                
                # time series graph
                div(
                    style = "aspect-ratio: 2/1; width: 100%;",
                    plotOutput("p_ts", width = "100%", height = "50%")
                )
            ),
            
            
            uiOutput("context_box"), 
            
            # value_box(
            #     title = "# stations reporting",
            #     value = textOutput("n_reporting"),
            #     showcase = bsicons::bs_icon("droplet-fill"),
            #     theme = "text-blue"
            # ),

            value_box(
                title = "# stations worse than normal",
                value = textOutput("n_bad"),
                showcase = bsicons::bs_icon("exclamation-triangle"),
                p("11 is typical"),
                theme = "text-red"
            )
        )
    ),
    
 # Maps in tabs
        layout_columns(
            card(
                full_screen = TRUE,
                card_header("Where was hypoxia worse than usual?",
                            tooltip(
                                bsicons::bs_icon("info-circle"),
                                "'Worse than usual' means the % of time hypoxic this year was more than 2 standard deviations above the mean annual % time hypoxic for the station."
                            )
                ), 
                leafletOutput("map2")
            ),
            card(
                full_screen = TRUE,
                card_header("How much time in summer was hypoxic?",
                            tooltip(
                                bsicons::bs_icon("info-circle"),
                                "% of valid readings for the year where DO < 2 mg/L"
                            ),
                            
                            popover(# actionButton("btn", "Change display"),
                                    # bsicons::bs_icon("filter-square"),
                                    bsicons::bs_icon("layers"),
                                    checkboxGroupInput(
                                        "categories",
                                        "Select 'time spent hypoxic in summer' categories to display:",
                                        choices = levels(hyp_summer_cat$category),
                                        selected = unique(hyp_summer_cat$category)[c(1, 2, 3, 4)],
                                        inline = FALSE)
                            )
                ),
                
                leafletOutput("map")
            )
        )

)  # end ui


# Server ----
server <- function(input, output, session) {
    
    # maps ----
    output$map <- renderLeaflet({
        leaflet() |> 
            addTiles() |> 
            setView(lng = -98.5, lat = 39.8, zoom = 2) |>  # Central US, zoomed out to include AK and HI
            addLegend(position = "topright",
                      pal = palette,
                      values = hyp_summer_cat$category,
                      title = "Summer hypoxia",
                      opacity = 0.7)
    })
    
    output$map2 <- renderLeaflet({
        leaflet() |> 
            addTiles() |> 
            setView(lng = -98.5, lat = 39.8, zoom = 2) # |>  # Central US, zoomed out to include AK and HI
        # addLegend(position = "topright",
        #           opacity = 0.7)
    })
    
    # Update outputs based on selected categories and year
    observe({
        # proportion of time map
        filtered_data <- hyp_summer_cat |> 
            filter(year == input$year, category %in% input$categories)
        
        leafletProxy("map", data = filtered_data) |>
            clearMarkers() |> 
            addCircleMarkers(
                lng = ~longitude,
                lat = ~latitude,
                color = ~palette(category),
                fillOpacity = 0.7,
                radius = 7
            )
        
        # "bad year" map
        filtered2 <- hypox_by_reserve |> 
            filter(year == input$year,
                   nStations_LT2BadYear > 0)
        
        leafletProxy("map2", data = filtered2) |> 
            clearMarkers() |> 
            addCircleMarkers(
                lng = ~longitude,
                lat = ~latitude,
                color = "red",
                fillOpacity = 0.7,
                radius = ~hypox_rad
            )
        
    })
    
    # value boxes ----
    filtered3 <- reactive({
        req(input$year)
        hypox_by_year |> 
            filter(year == input$year)
    })
    
    output$n_bad <- renderText({
        sum(filtered3()$nStations_LT2BadYear)
    })
    output$n_reporting <- renderText({
        sum(filtered3()$nStationsReporting)
    })
    output$n_hours <- renderText({
        hrs <- sum(filtered3()$nStationsReporting) * 24 * 365
        prettyNum(hrs, big.mark = ",")
    })
    output$context_box <- renderUI({
        if(sum(filtered3()$nStations_LT2BadYear) > 13){
            value_box(
                title = paste("The year", input$year ,"was"),
                value = "WORSE than normal",
                theme = "warning",
                showcase = bsicons::bs_icon("emoji-frown")
            )
        } else {
            value_box(
                title = paste("The year", input$year ,"was"),
                value = "about normal",
                theme = "success",
                showcase = bsicons::bs_icon("emoji-smile")
            )
        }
    })
    
    # graphs ----
    output$p_ts <- renderPlot({
        ggplot(hypox_by_year,
               aes(x = year,
                   y = nStations_LT2BadYear)) +
            geom_ribbon(aes(x = year,
                            ymin = 12,
                            ymax = max(hypox_by_year$nStations_LT2BadYear) + 1),
                        fill = "red3",
                        alpha = 0.2) +
            geom_ribbon(aes(x = year,
                            ymin = 9,
                            ymax = 12),
                        fill = "darkgreen",
                        alpha = 0.4) +
            geom_ribbon(aes(x = year,
                            ymin = min(hypox_by_year$nStations_LT2BadYear) - 1,
                            ymax = 9),
                        fill = "green",
                        alpha = 0.4) +
            geom_line(linewidth = 1) +
            geom_vline(xintercept = input$year,
                       col = "navy") +
            coord_cartesian(clip = "off") +
            theme_minimal() +
            theme(plot.margin = margin(0, 0, 0, 0, "pt")) +
            theme(axis.text.y = element_blank()) +
            labs(y = NULL,
                 x = NULL,
                 subtitle = "# stations with 'bad' hypoxia")
    })
}

# Run the app
shinyApp(ui, server)
