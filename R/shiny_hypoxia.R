library(shiny)
library(bslib)
library(here)
library(leaflet)
library(dplyr)

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
ui <- page_sidebar(
    
    title = "Interactive Map of Hypoxia",
    # p("This map shows, for each of 150 SWMP stations, how much time is spent hypoxic (DO < 2 mg/L) in July and August of any selected year. The vast majority of stations report <2 mg/L less than 5% of the time. You can select which time groupings to view via checkboxes in the sidebar, and change the year you see with the slider. Each reserve has 4 stations, so you may need to zoom in fairly far to see separation within a reserve."),
    
    sidebar = sidebar(
        title = "Selections",
        
        # Slider for year selection
        sliderInput(
            "year",
            "Select Year:",
            min = min(hyp_summer_cat$year),
            max = max(hyp_summer_cat$year),
            value = max(hyp_summer_cat$year),
            step = 1,
            sep = ""
        )
        
    ), # end sidebar
    
    # value boxes
    layout_columns(
        fill = FALSE,
        
        value_box(
            title = "# stations reporting",
            value = textOutput("n_reporting"),
            showcase = bsicons::bs_icon("droplet-fill"),
            theme = "text-blue"
        ),
        
        value_box(
            title = "# stations worse than normal",
            value = textOutput("n_bad"),
            showcase = bsicons::bs_icon("exclamation-triangle"),
            p("11 is typical"),
            theme = "text-red"
        ),
        
        uiOutput("context_box")
    ),
    # https://icons.getbootstrap.com/
    
    
    # "bad year" map
    card(
        card_header("Where was hypoxia worse than normal?"),
        leafletOutput("map2"),
        min_height = 200
    ),

    # proportion of time map
    card(
        card_header("How much of the summer months was spent hypoxic?"),
        layout_sidebar(
            sidebar = sidebar(
                position = "right",

                # Checkbox for category selection
                checkboxGroupInput(
                    "categories",
                    "Select 'time spent hypoxic in summer' categories to display:",
                    choices = levels(hyp_summer_cat$category),
                    selected = unique(hyp_summer_cat$category)[-1]
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
        years_stations <- hyp_summer_cat |> 
            filter(year == input$year)
        
        # proportion of time map
        filtered_data <- hyp_summer_cat |> 
            filter(year == input$year, category %in% input$categories)
        
        leafletProxy("map", data = years_stations) |>
            clearMarkers() |> 
            addCircleMarkers(data = years_stations,
                             lng = ~longitude,
                             lat = ~latitude,
                             color = "gray20",
                             radius = 2) |> 
            addCircleMarkers(
                data = filtered_data,
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
        
        leafletProxy("map2") |> 
            clearMarkers() |> 
            addCircleMarkers(data = years_stations,
                             lng = ~longitude,
                             lat = ~latitude,
                             color = "gray20",
                             radius = 2) |> 
            addCircleMarkers(
                data = filtered2,
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
}

# Run the app
shinyApp(ui, server)
