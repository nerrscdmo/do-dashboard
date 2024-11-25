library(shiny)
library(here)
library(leaflet)
library(dplyr)

# Sample data (replace with your hyp_summer_cat data)
load(here::here("data_wq", "summer_hypoxia.RDS"))

# Create color palette
palette <- colorFactor(palette = "YlOrRd", domain = hyp_summer_cat$category)

# Define UI
ui <- fluidPage(
    titlePanel("Interactive Map of Hypoxia"),
    
    p("This map shows, for each of 150 SWMP stations, how much time is spent hypoxic (DO < 2 mg/L) in July and August of any selected year. The vast majority of stations report <2 mg/L less than 5% of the time. You can select which time groupings to view via checkboxes in the sidebar, and change the year you see with the slider. Each reserve has 4 stations, so you may need to zoom in fairly far to see separation within a reserve."),
    
    # Sidebar layout
    
    sidebarLayout(
        sidebarPanel(
            
            # Slider for year selection
            sliderInput(
                "year",
                "Select Year:",
                min = min(hyp_summer_cat$year),
                max = max(hyp_summer_cat$year),
                value = max(hyp_summer_cat$year),
                step = 1,
                sep = ""
            ),
            
            # Checkbox for category selection
            checkboxGroupInput(
                "categories", 
                "Select 'time spent hypoxic in summer' categories to display:",
                choices = levels(hyp_summer_cat$category),
                selected = unique(hyp_summer_cat$category)
            )
            
        ), # end sidebar panel
        
        mainPanel(
            
            # Leaflet output for the map
            leafletOutput("map")
        )  # end main panel
    ),  # end sidebar layout
    
    br(),
    p("If this sort of map works, we can add things - like, when you click on a point, you see the station code and % of time spent hypoxic. We can also have other information pop up for a station, like a graph of summer hypoxia over many years (like how we have things happen when you click on a pie chart in the SETr demo). Additional useful station information might be the trend in dissolved oxygen (so you could see if, e.g., a station had a lot of hypoxia in summer 2022 AND is also experiencing an overall decrease in DO). Another option for the big map might be some way of combining information for all the stations at a reserve.")
    
)  # end ui

# Define server logic
server <- function(input, output, session) {
    
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
    
    # Update map based on selected categories and year
    observe({
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
    })
}

# Run the app
shinyApp(ui, server)
