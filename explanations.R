

# about description ----

about_ui <- nav_panel(
    "About",
    full_screen = FALSE,
    
    # card_header("Tips and Tricks",
    #             tooltip(
    #                 bsicons::bs_icon("info-circle"),
    #                 "Info here"
    #             ) # end tooltip
    # ), # end header
    
    h5("Expand a section below for more detail."),
    
    accordion(
        id = "explanation_accordion",
        open = FALSE,
        
        accordion_panel(
            title = "Data source(s) and Definitions",
            p("here, discuss median DO and the thresholds, and trends in the Reinl et al. synthesis"),
            p("definitely link to CDMO SWMP data here")
            
        ),
        
        accordion_panel(
            title = "Trend Calculations",
            p("only calculated when >10 yrs of data; GAMs and other details; again mention Reinl et al."),
            
        ),
        
        accordion_panel(
            title = "Using the Maps",
            p("what options can you modify; sidebars will open up when something is clicked on; what's in the sidebars; how to expand accordions; how to pop out the graphs"),
            
        )
        
    ),  # end accordion
    
    
    hr(),
    p("This app was developed in support of the National Estuarine Research Reserve System (NERRS), a partnership program between the National Oceanic and Atmospheric Administration and coastal states."),
    p("Funding was provided by NOAA under a subaward from [NA23NOS4200321] to the University of South Carolina /", tags$a("NERRS Centralized Data Management Office", href = "https://cdmo.baruch.sc.edu", target = "_blank"), "."),
    p("Developed by ", tags$a("Catbird Stats, LLC", href = "https://www.catbirdstats.com", target = "_blank"), ". For questions about this app, please contact ", tags$a("kim@catbirdstats.com", href = "mailto:kim@catbirdstats.com"), ".")
    
)

# description text ----
# store as variables so the wording can be used in multiple places  
data_preview_desc1 <- "View, sort, filter, and search the raw data in the 'Cover' worksheet of your file. This table is laid out exactly the same as your original spreadsheet." 
data_preview_desc2 <- HTML(
    "<p>Columns can be sorted by clicking on their name, or filtered by typing into the box below the name.</p>
    <p>You probably only need this table if you see anything unexpected in your data via the other tables and graphs. Search for the values here without having to return to your original spreadsheet. Any data updates however will need to be made in the original spreadsheet.</p>"
)

column_summary_desc1 <- "This table shows you how R 'sees' your data. This table is good to look through to make sure values in your columns align with your expectations (e.g. you do not have any vegetation cover values of 500)."
column_summary_desc2 <- HTML(
    "<p>The table contains one row for each column of the data. It shows you what each column type is and summarizes the values in the column. Note, empty columns are typically seen as 'logical' (true or false). Every column type displays information about how many cells are full and empty, and what the completeness rate is (number of non-empty cells divided by number of rows).</p>
    <p>For character columns, you see how many unique entries exist. For numeric columns, you see numeric summaries like the min, mean, median, and max.</p>"
)

sampling_summary_desc1 <- "This table provides a summary of sampling events and flags any vegetation plot-date combinations where there is no vegetation cover recorded."
sampling_summary_desc2 <- HTML(
    "<p>For each vegetation plot on each date, true or false is assigned to denote whether each of cover, height, and density were collected. If cover has a 'false' value, the row is orange to draw your attention.</p>
    <p>Rows are initially shown at only the site/date level, and can be expanded all the way down to vegetation plot level so you can find which row is causing the flagging. Any issues you find need to be addressed in the data file."
)

time_series_desc1 <- "See how a variable changes over time at a site. In the sidebar, choose your site and any numeric variable from your file."
time_series_desc2 <- HTML(
    "<p><strong>x-axis:</strong> date</p>
    <p><strong>y-axis:</strong> the selected variable's value</p>
    <p><strong>points:</strong> one for each vegetation plot on each date, showing the variable's value</p>
    <p><strong>lines:</strong> one for each vegetation plot, showing the variable through time</p>
    <p><strong>panels:</strong> each panel represents a transect, and contains all vegetation plots in that transect</p>
    <p><strong>selections:</strong> vegetation plots can be removed and added using the checkboxes, if you want to focus on one or a few.</p>"
)

transect_profiles_desc1 <- "See how a variable changes along a cross-section of your transect. In the sidebar, choose your site and any numeric variable from your file."
transect_profiles_desc2 <- HTML(
    "<p><strong>x-axis:</strong> vegetation plot (numerically ordered; presumably either water-to-upland or vice versa)</p>
    <p><strong>y-axis:</strong> the selected variable's value</p>
    <p><strong>points:</strong> one for each vegetation plot on each date, showing the variable's value</p>
    <p><strong>lines:</strong> one for each year</p>
    <p><strong>panels:</strong> each panel represents a transect, and contains all vegetation plots in that transect</p>
    <p><strong>selections:</strong> years can be removed and added using the checkboxes, if you want to focus on one or a few.</p>"
)

correlation_scatterplots_desc1 <- "Explore relationships between variables, across all sites. This graph only updates when you click the 'Use these choices' button. This is the only graph that is not interactive."
correlation_scatterplots_desc2 <- HTML(
    "<p>You choose the variables to display on each axis.</p>
    <p><strong>points:</strong> one for each vegetation plot on each date</p>
    <p><strong>shape:</strong> represents site - are there differences in the relationship between sites?</p>
    <p><strong>color:</strong> represents missing vs. non-missing values. If a 'missing' colored point is near the origin, it is missing for both variables. If a value is missing for only one of the two variables, it will be near 0 for the variable that is missing but at the appropriate value for the axis where a variable is present. e.g., if a missing value is placed at 80 along the x-axis, and is near the axis, the y-variable was not measured (and is presumably 0, unless it was truly missing data).</p>
    <p><strong>line:</strong> if selected, a linear regression line is added to the graph.</p>"
)

table_interactivity_desc <- "This table is interactive. Columns can be sorted by clicking on their name or filtered by typing into the box below the name."


# data/technical details ----
# don't think I'll end up using this, but here's the structure
# the list structure is especially important
techDetails_ui <- nav_panel(
    "Data/Technical Details",
    full_screen = FALSE,
    
    card_header("How was this information generated?",
                tooltip(
                    bsicons::bs_icon("info-circle"),
                    "Info here"
                ) # end tooltip
    ), # end header
    
    p("Put text here about different options"),
    p("...what each tab does, how to interact with the maps, what pops up in the sidebar and how you can interact with that"),
    p("...about SWMP data"),
    p("...text about dashboard development and for more information, etc."),
    
    h4("How to use this app:"),
    tags$ol(
        tags$li(strong("Upload your vegetation data file"), "using the sidebar. This information will not be retained by the app once you close the session."),
        tags$li(
            span(strong("See tabular summaries"), " of your data by selecting 'Tables' from the navigation bar at the top of the app."),
            tags$ol(
                style = "list-style-type: lower-alpha; margin-top: 8px;",
                tags$li(em(strong("Data preview:")), " ", data_preview_desc1),
                tags$li(em(strong("Column summary:")), " ", column_summary_desc1),
                tags$li(em(strong("Sampling summary:")), " ", sampling_summary_desc1)
            )
        ),
        tags$li(
            span(strong("Explore graphs"), " of your data by selecting 'Graphs' from the navigation bar at the top of the app."),
            tags$ol(
                style = "list-style-type: lower-alpha; margin-top: 8px;",
                tags$li(em(strong("Time series:")), " ", time_series_desc1),
                tags$li(em(strong("Transect Profiles:")), " ", transect_profiles_desc1),
                tags$li(em(strong("Correlation Scatterplots:")), " ", correlation_scatterplots_desc1)
            )
        )
        
    )
)
