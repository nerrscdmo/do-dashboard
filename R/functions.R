# Custom legend for leaflet with sizes ----
addLegendCustom <- function(map, sizes, labels, colors = "black", position = "bottomright", opacity = 0.8) {
    legend_html <- paste0(
        "<div style='background-color: white; padding: 10px; border-radius: 5px; width: 100px;'>",
        "<div style='text-align: center; font-weight: bold; margin-bottom: 5px;'>% of year</div>",  # Centered title
        paste0(
            "<div style='display: flex; align-items: center; justify-content: space-between; margin-bottom: 5px;'>",
            # Wrapper div to center circles
            "<div style='display: flex; justify-content: center; align-items: center; width: 50px;'>",
            "<div style='width: ", sizes * 2, "px; height: ", sizes * 2, "px; ",
            "background-color: ", colors, "; border-radius: 50%; opacity: ", opacity, ";'></div>",
            "</div>",
            # Right-aligned text
            "<div style='flex-grow: 1; text-align: right; padding-left: 10px;'>", labels, "</div>",
            "</div>"
            , collapse = ""),
        "</div>"
    )
    
    return(addControl(map, html = HTML(legend_html), position = position))
}




# Stacked DO graphs ----

# Utility function to convert hex to rgba with alpha
hex_to_rgba <- function(hex, alpha = 1) {
    rgb <- col2rgb(hex)
    sprintf("rgba(%d, %d, %d, %.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

# time series ----
graph_do <- function(data_monthly, data_yearly, 
                      station, 
                     thresh2, thresh5,
                     ribbon_var, ribbon_minmax) {  
    
    # Define colors and line types for DO thresholds 
    # so they are the same in both graphs
    # these colors from Tol's 'sunset' palette  
    # https://packages.tesselle.org/khroma/articles/tol.html#qualitative-data
    main_color <- "#364b9a"
    thresh2_color <- "#dd3d2d"
    thresh5_color <- "#fdb366"

    thresh2_line <- "dash"
    thresh5_line <- "dashdot"
    

    # initiate empty graph ----
    p <- plot_ly(data_monthly,
                 # type = "scatter",
                 # mode = "lines",
                 # trace = NULL,
                 x = ~date,
                 y = ~domgl_median,
                 color = NULL,
                 name = NULL)
    
    # ribbons ----
    if(ribbon_minmax == TRUE){
        p <- p |> 
            add_ribbons(ymin = ~domgl_min,
                        ymax = ~domgl_max,
                        fillcolor = hex_to_rgba(main_color, alpha = 0.1),
                        # fillcolor = "rgba(0, 0, 128, 0.1)",
                        line = list(color = "transparent"),
                        name = "min/max") 
    }
    
    if(ribbon_var == TRUE){
        p <- p |> 
            add_ribbons(ymin = ~domgl_p25,
                        ymax = ~domgl_p75,
                        fillcolor = hex_to_rgba(main_color, alpha = 0.3),
                        # fillcolor = "rgba(0, 0, 128, 0.2)",
                        line = list(color = "transparent"),
                        name = "middle 50% of values") 
    }
    
    # threshold lines ----
    if(thresh2 == TRUE){
        p <- p |> 
            add_lines(y = 2,
                      color = I(thresh2_color),
                      line = list(dash = thresh2_line),
                      showlegend = FALSE)
    }
    
    if(thresh5 == TRUE){
        p <- p |> 
            add_lines(y = 5,
                      color = I(thresh5_color),
                      line = list(dash = thresh5_line),
                      showlegend = FALSE)
    }
    
    # median do ----
    p <- p |> 
        add_lines(y = ~domgl_median,
                  x = ~date,
                  color = I(main_color),
                  name = "Median")
    
    
    p <- p |> 
        layout(xaxis = list(title = "Date"),
               yaxis = list(title = "DO (mg/L)"))
    
    
    # annual hypoxia graph ----
    p3 <- plot_ly(data_yearly,
                  x = ~year,
                  y = ~LT5,
                  type = "scatter",
                  mode = "none",
                  name = " ") 
    
    if(thresh2 == TRUE){
        p3 <- p3 |> 
            add_trace(type = "scatter",
                      mode = "markers+lines",
                      x = ~year,
                      y = ~LT2,
                      color = I(thresh2_color),
                      marker = list(size = 4),
                      line = list(dash = thresh2_line),
                      name = "DO < 2")
    }
    
    if(thresh5 == TRUE){
        p3 <- p3 |> 
            add_trace(y = ~LT5,
                      type = "scatter",
                      mode = "markers+lines",
                      color = I(thresh5_color),
                      marker = list(size = 4),
                      line = list(dash = thresh5_line),
                      name = "DO < 5") 
    }
    
    p3 <- p3  |> 
        layout(xaxis = list(title = "Date"),  
               yaxis = list(title = "% of year \nbelow threshold")
        )
    
    # plot titles
    annotations = list(
        list(
            x = 0,
            y = 0.96,
            text = "<b>Monthly DO Concentrations</b> (higher is better)",
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 13, color = "black")
        ),
        list(
            x = 0,
            y = 0.46,
            text = "<b>Annual time spent with low DO</b> (lower is better)",
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 13, color = "black")
        )
    )
    
    # output
    out <- plotly::subplot(p, p3, nrows = 2, shareX = TRUE, titleY = TRUE, shareY = FALSE) |> 
        layout(
            # title = list(text = paste0("<b>", station, "</b>"), x = 0.5, xanchor = "center", y = 0.98),
            xaxis = list(
                title = "Date",
                # range = year_range,  
                rangeslider = list(type = "date",
                                   thickness = 0.05),
                zerolinecolor = '#ffff', 
                zerolinewidth = 1
            ),
            annotations = annotations
        )
    
    out
}


# selected year's DO at station ----
monthly_stn_do <- function(data, 
                     station, 
                     thresh2, thresh5,
                     ribbon_var, ribbon_minmax) {  
    # data should contain monthly summaries of DO concentration and hypoxia
    
    # Define colors and line types for DO thresholds 
    # so they are the same in both graphs
    main_color <- "#364b9a"
    thresh2_color <- "#dd3d2d"
    thresh5_color <- "#fdb366"
    
    thresh2_line <- "dash"
    thresh5_line <- "dashdot"
    
    
    # monthly conc ----
    p <- plot_ly(data,
                 type = "scatter",
                 mode = "lines",
                 x = ~month,
                 y = ~mean,
                  name = "overall mean",
                  color = I("rgba(102, 102, 102, 1)")) # gray40
        
    
    if(ribbon_minmax == TRUE){
        p <- p |> 
            add_ribbons(ymin = ~min,
                        ymax = ~max,
                        fillcolor = "rgba(102, 102, 102, 0.1)",  
                        line = list(color = "transparent"),
                        name = "min/max") 
    }
    
    if(ribbon_var == TRUE){
        p <- p |> 
            add_ribbons(ymin = ~p25,
                        ymax = ~p75,
                        fillcolor = "rgba(102, 102, 102, 0.2)",  
                        line = list(color = "transparent"),
                        name = "middle 50% of values") 
    }
    
    if(thresh2 == TRUE){
        p <- p |> 
            add_lines(y = 2,
                      color = I(thresh2_color),
                      line = list(dash = thresh2_line),
                      showlegend = FALSE)
    }
    
    if(thresh5 == TRUE){
        p <- p |> 
            add_lines(y = 5,
                      color = I(thresh5_color),
                      line = list(dash = thresh5_line),
                      showlegend = FALSE)
    }
    
    p <- p |> 
        add_trace(type = "scatter",
                  mode = "markers+lines",
                  x = ~month,
                  y = ~domgl_median,
                  name = "selected year median",
                  color = I(main_color)) |> 
        layout(xaxis = list(title = "Month"),
               yaxis = list(title = "Median DO (mg/L)"))
    
    
    # monthly hypoxia graph ----
    p3 <- plot_ly(data,
                  x = ~month,
                  y = ~LT5,
                  type = "scatter",
                  mode = "none",
                  name = " ") 
    
    if(thresh2 == TRUE){
        # ribbons
        if(ribbon_minmax == TRUE){
            p3 <- p3 |> 
                add_ribbons(ymin = ~min.lt2,
                            ymax = ~max.lt2,
                            fillcolor = hex_to_rgba(thresh2_color, 0.1),  
                            line = list(color = "transparent"),
                            name = "min/max")  
        }
        
        if(ribbon_var == TRUE){
            p3 <- p3 |> 
                add_ribbons(ymin = ~p25.lt2,
                            ymax = ~p75.lt2,
                            fillcolor = hex_to_rgba(thresh2_color, 0.3),  
                            line = list(color = "transparent"),
                            name = "middle 50% of values")  
        }
        
        # line
        p3 <- p3 |> 
            add_trace(type = "scatter",
                      mode = "markers+lines",
                      x = ~month,
                      y = ~LT2,
                      color = I(thresh2_color),
                      marker = list(size = 6),
                      line = list(dash = thresh2_line),
                      name = "DO < 2")
    }
    
    if(thresh5 == TRUE){
        # ribbons
        if(ribbon_minmax == TRUE){
            p3 <- p3 |> 
                add_ribbons(ymin = ~min.lt5,
                            ymax = ~max.lt5,
                            fillcolor = hex_to_rgba(thresh5_color, 0.1),  
                            line = list(color = "transparent"),
                            name = "min/max")  
        }
        
        if(ribbon_var == TRUE){
            p3 <- p3 |> 
                add_ribbons(ymin = ~p25.lt5,
                            ymax = ~p75.lt5,
                            fillcolor = hex_to_rgba(thresh5_color, 0.3),  
                            line = list(color = "transparent"),
                            name = "middle 50% of values")  
        }
        
        # line
        p3 <- p3 |> 
            add_trace(y = ~LT5,
                      type = "scatter",
                      mode = "markers+lines",
                      color = I(thresh5_color),
                      marker = list(size = 6),
                      line = list(dash = thresh5_line),
                      name = "DO < 5") 
    }
    
    
    p3 <- p3  |> 
        layout(xaxis = list(title = "Date"),  
               yaxis = list(title = "% of month \nbelow threshold")
        )
    
    # plot titles
    annotations = list(
        list(
            x = 0,
            y = 0.96,
            text = "<b>Monthly DO Concentrations</b> (higher is better)",
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 13, color = "black")
        ),
        list(
            x = 0,
            y = 0.46,
            text = "<b>Monthly time spent with low DO</b> (lower is better)",
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 13, color = "black")
        )
    )
    
    # output
    out <- plotly::subplot(p, p3, nrows = 2, shareX = TRUE, titleY = TRUE, shareY = FALSE) |> 
        layout(
            title = list(text = paste0("<b>", station, "</b>"), x = 0.5, xanchor = "center", y = 0.98),
            xaxis = list(
                title = "Month",
                zerolinecolor = '#ffff', 
                zerolinewidth = 1
            ),
            annotations = annotations
        )
    
    out
}


