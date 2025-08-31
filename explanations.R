

# about description ----

about_ui <- nav_panel(
    "About",
    full_screen = FALSE,
    
    h5("Expand a section below for more detail."),
    
    accordion(
        id = "explanation_accordion",
        open = FALSE,
        
        accordion_panel(
            title = "Using the Maps",
            p("Just about everything in these maps is interactive - click something interesting and see what happens! Some tips are below."),
            tags$ul(
                tags$li(strong("Choose a tab."), "The default tab allows you to explore long-term change in Dissolved Oxygen. The next tab shows long-term median values. The final tab is an exploration of how much time Dissolved Oxygen was below certain thresholds in a given year, and you can change which year you'd like to view."),
                tags$li(strong("Click on the 'Map Details' button"), "for more information about what is on the map."),
                tags$li(strong("Adjust options in the left sidebar."), "Info buttons can be clicked on for more detail about certain options. In this dashboard, you can choose between different values to represent Dissolved Oxygen, and change which stations are showing, using a variety of filters."),
                tags$li(strong("Click on a point"), "and a sidebar on the right will open up with more detail. Expand any of the sections by clicking on it. The sidebar can be closed again by clicking on the arrow in the upper left corner of the sidebar."),
                tags$li(strong("Sidebars and expandable information"), "can be closed again by clicking on the arrows at the top of them."),
                tags$li(strong("Graphs in the sidebar"), "can be popped out by clicking on the symbol in the bottom right corner of the graph pane. The entire tab's contents can also be popped out, so if you don't see what you want, try looking for the symbol in a slightly different place."),
                tags$li(strong("Zoom in and out"), "using the buttons in the upper left corner of the map."),
                tags$li(strong("Change the map background"), "if desired, by using the layers button in the upper right corner of the map.")
                
            )
        ),
        
        accordion_panel(
            title = "Data source(s) and Definitions",
            
            h5("Data source"),
            p("Data files from all water quality stations were downloaded from the NERRS Centralized Data Management Office", 
              tags$a("Advanced Query System, Zip Download", href = "https://cdmo.baruch.sc.edu/aqs/zips.cfm", target = "_blank"),
              "and processed using code in this", tags$a("do-data-processing repository", href = "https://github.com/nerrscdmo/do-data-processing", target = "_blank"), ".
              Monthly values were calculated when at least a week's worth of readings were present and passed QA/QC criteria: median DO concentration (mg/L) as the median of all monthly readings in the month, and % of readings below the thresholds of 5 and 2 mg/L. 
              Annual values were also calculated: annual median DO was the median of the monthly medians within the year; and % time below 5 and 2 mg/L was the % of readings for the entire year (not based on monthly values) below the thresholds."),
            p("The time period included in this dashboard is 2002 - 2023."),

            h5("Variable definitions"),
            p("here, discuss median DO and the thresholds, and trends in the Reinl et al. synthesis"),

            h5("Category definitions"),
            p("increasing/decreasing/no trend; typical/unusual (boxplot outlier definition - per station)")
            
        ),
        
        accordion_panel(
            title = "Trend Calculations",
            p("Trends were calculated as part of a larger SWMP Data Synthesis project (Reinl et al., in review) and re-used here for simplicity of data processing."),

            h5("When trends were calculated and declared"),
            p(strong("Calculation:"), "Trends were calculated for a station when that station had at least 10 yrs of data. Otherwise, the station's results for each trend are listed as 'not calculated', and the station represented on the maps by the color gray)."),
            p(strong("Significance:"), "For stations where trend was calculated: if the p-value was <0.05, a significant trend was recognized, as either increasing or decreasing. Otherwise, 'no trend' was listed as the result for that station."),

            h5("How trends were calculated"),
            p("Generalized Additive Models (GAMs) were used to calculate trends at each station based on the monthly summary values described above. 
              A seasonal smoothing term was included, with 12 knots if possible and the number of months represented in the station's data otherwise (e.g. stations where sondes are removed part of the year due to ice). 
              Using a custom function, autocorrelation of residuals was automatically checked for and if present, the model was re-run to account for the autocorrelation (this affects p-values and thus declaration of trends vs. no trend). 
              The reported trend in the outputs is the linear trend through time (per year) of the parameter, after accounting for autocorrelation and seasonality through the GAM."),
            p("All trends were calculated using the `bam` function from the R `mgcv` package. The gaussian family was used for median DO trends. Time below 2 and 5 mg/L was a proportion, so for these trend calculations, the `betar` family was used.")
            
        )
        
    ),  # end accordion
    
    
    hr(),
    p("This app was developed in support of the National Estuarine Research Reserve System (NERRS), a partnership program between the National Oceanic and Atmospheric Administration and coastal states."),
    p("Developed by ", tags$a("Catbird Stats, LLC", href = "https://www.catbirdstats.com", target = "_blank"), "under a subaward from NOAA [NA23NOS4200321] to the University of South Carolina /", tags$a("NERRS Centralized Data Management Office", href = "https://cdmo.baruch.sc.edu", target = "_blank"), "."),
    p("For questions about this app, please contact ", tags$a("kim@catbirdstats.com", href = "mailto:kim@catbirdstats.com"), ".")
    
)

