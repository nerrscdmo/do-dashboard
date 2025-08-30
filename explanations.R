

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
              "and processed using code in this", tags$a("do-data-processing repository", href = "https://github.com/nerrscdmo/do-data-processing", target = "_blank"), "."),
            p("The time period included in this dashboard is 2002 - 2023."),
            br(),
            
            h5("Variable definitions"),
            p("here, discuss median DO and the thresholds, and trends in the Reinl et al. synthesis"),
            br(),
            
            h5("Category definitions"),
            p("increasing/decreasing/no trend; typical/unusual (boxplot outlier definition - per station)")
            
        ),
        
        accordion_panel(
            title = "Trend Calculations",
            p("again mention Reinl et al.;"),
            br(),
            
            h5("When trends were calculated"),
            p("trends only calculated when >10 yrs of data (if <10, declared 'not calculated' and represented by the color gray)."),
            p("For stations where trend was calculated: if p-value was <0.05, significant trend was recognized, as either increasing or decreasing. Otherwise, it was declared 'no trend' at that station."),
            br(),
            
            h5("How trends were calculated"),
            p("We used GAMs (generalized additive models) to calculate trends. A seasonal term is included, with 12 knots if possible and the number of months represented in the data frame otherwise (e.g. stations where sondes are removed part of the year due to ice). Using a custom function, autocorrelation of residuals was automatically checked for and if present, the model was re-run to account for the autocorrelation. The reported trend in the outputs is the LINEAR trend through time (per year) of the parameter, after accounting for autocorrelation and seasonality through the GAM."),
            p("For WQ proportion of DO below 2 and 5: These calculations were made before monthly aggregation - each valid 15-minute data point was marked TRUE/FALSE for below 2 and 5, respectively (in separate columns). During monthly aggregation, the total TRUE for each month was divided by the total number of valid DO points for the month, leading to a proportion per month. Trends were again calculated in `mgcv::bam()` with a seasonal term and an autocorrelation term if necessary. Because this response is a proportion, we used `family = betar()`. The `eps` option, which adjusts exact 0s and 1s, was set to 1/10th of the minimum number of readings per month (1/27900)."),
            p("All trends were calculated using the `bam` function from the R `mgcv` package.")
            
            
        )
        
    ),  # end accordion
    
    
    hr(),
    p("This app was developed in support of the National Estuarine Research Reserve System (NERRS), a partnership program between the National Oceanic and Atmospheric Administration and coastal states."),
    p("Funding was provided by NOAA under a subaward from [NA23NOS4200321] to the University of South Carolina /", tags$a("NERRS Centralized Data Management Office", href = "https://cdmo.baruch.sc.edu", target = "_blank"), "."),
    p("Developed by ", tags$a("Catbird Stats, LLC", href = "https://www.catbirdstats.com", target = "_blank"), ". For questions about this app, please contact ", tags$a("kim@catbirdstats.com", href = "mailto:kim@catbirdstats.com"), ".")
    
)

