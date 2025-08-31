

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
              Monthly values were calculated when at least a week's worth of readings were present and passed QA/QC criteria: median DO concentration (mg/L) as the median of all valid readings within the month, and % of valid readings below the thresholds of 5 and 2 mg/L."),
            p("The time period included in this dashboard is 2002 - 2023."),

            h5("Variable definitions"),
            p(strong("DO concentration:"), "All calculations involving DO concentration (mg/L) were based on monthly median DO mg/L values."),
            tags$ul(
                tags$li("Trend in median DO concentration (as in the 'Trends' tab) is the long-term change in DO mg/L, calculated as described below, based on monthly median DO mg/L."),
                tags$li("Annual median DO concentration is the median of all monthly medians within a given year."),
                tags$li("Long-term median DO concentration (as in the 'Long-Term Medians' tab) is the median of all annual medians at a station.")
            ),
            p(strong("Time < 5 or 2 mg/L:"), " All calculations involving time below given thresholds involved a proportion or percent of valid readings that were below these numbers."),
            tags$ul(
                tags$li("Trend in time below thresholds (as in the 'Trends' tab) is the long-term change in proportion of time below the thresholds, calculated as described below, based on monthly proportions of time below thresholds."),
                tags$li("Annual time below thresholds (as in the 'Explore by Year' tab) represents the total percent of valid readings within a year that were below the thresholds."),
                tags$li("Long-term median time below thresholds (as in the 'Long-Term Medians' tab) is the median of the annual percents at a station.")
            ),
            p(strong("Typical vs. Unusual Low DO"), "Low DO is not necessarily rare or bad. It can be bad, but for some stations, it is a natural occurrence, particularly in summer.
              As such, in addition to showing only the amount of time that DO was below thresholds in any given year, this dashboard shows whether that amount is 'typical' for a station or
              unusually high. An annual time below either threshold was determined to be", strong("unusual"), "if it would appear as an outlier point on a boxplot
              of all annual values for that threshold. The calculation was [3rd quantile + 1.5*IQR].")
            
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
    div(class = "footer-text",
        p("This app was developed in support of the National Estuarine Research Reserve System (NERRS), a partnership program between the National Oceanic and Atmospheric Administration and coastal states."),
        p("Developed by ", tags$a("Catbird Stats, LLC", href = "https://www.catbirdstats.com", target = "_blank"), "under a subaward from NOAA [NA23NOS4200321] to the University of South Carolina /", tags$a("NERRS Centralized Data Management Office", href = "https://cdmo.baruch.sc.edu", target = "_blank"), "."),
        p("For questions about this app, please contact ", tags$a("kim@catbirdstats.com", href = "mailto:kim@catbirdstats.com"), ".")
    ) 
)

