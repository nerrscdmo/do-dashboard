# make the stacked DO graphs

# time series ----
graph_do <- function(data_monthly, data_yearly, 
                      station, 
                     thresh2, thresh5,
                     ribbon_var, ribbon_minmax) {  
    
    # Define colors and line types for DO thresholds 
    # so they are the same in both graphs
    thresh2_color <- "#e6550d"
    thresh5_color <- "#fdae6b"
    
    thresh2_line <- "dash"
    thresh5_line <- "dashdot"
    

    # monthly conc ----
    p <- plot_ly(data_monthly,
                 type = "scatter",
                 mode = "lines",
                 x = ~date,
                 y = ~domgl_median,
                 color = I("navy"),
                 name = "Average")  

    
    if(ribbon_minmax == TRUE){
        p <- p |> 
            add_ribbons(ymin = ~domgl_min,
                        ymax = ~domgl_max,
                        fillcolor = "rgba(0, 0, 128, 0.1)",
                        line = list(color = "transparent"),
                        name = "min/max") 
    }
    
    if(ribbon_var == TRUE){
        p <- p |> 
            add_ribbons(ymin = ~domgl_p25,
                        ymax = ~domgl_p75,
                        fillcolor = "rgba(0, 0, 128, 0.2)",
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
            title = list(text = paste0("<b>", station, "</b>"), x = 0.5, xanchor = "center", y = 0.98),
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
    thresh2_color <- "rgba(230, 85, 13, 1)"
    thresh5_color <- "rgba(253, 174, 107, 1)"
    
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
                  y = ~do_mgl_mean,
                  name = "selected year mean",
                  color = I("rgba(0, 0, 200, 1)")) |> 
        layout(xaxis = list(title = "Month"),
               yaxis = list(title = "Average DO (mg/L)"))
    
    
    # monthly hypoxia graph ----
    p3 <- plot_ly(data,
                  x = ~month,
                  y = ~doLessThan5_percent,
                  type = "scatter",
                  mode = "none",
                  name = " ") 
    
    if(thresh2 == TRUE){
        # ribbons
        if(ribbon_minmax == TRUE){
            p3 <- p3 |> 
                add_ribbons(ymin = ~min.lt2,
                            ymax = ~max.lt2,
                            fillcolor = stringr::str_replace(thresh2_color, "1\\)", "0.1\\)"),  
                            line = list(color = "transparent"),
                            name = "min/max")  
        }
        
        if(ribbon_var == TRUE){
            p3 <- p3 |> 
                add_ribbons(ymin = ~p25.lt2,
                            ymax = ~p75.lt2,
                            fillcolor = stringr::str_replace(thresh2_color, "1\\)", "0.2\\)"),  
                            line = list(color = "transparent"),
                            name = "middle 50% of values")  
        }
        
        # line
        p3 <- p3 |> 
            add_trace(type = "scatter",
                      mode = "markers+lines",
                      x = ~month,
                      y = ~doLessThan2_percent,
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
                            fillcolor = stringr::str_replace(thresh5_color, "1\\)", "0.1\\)"),  
                            line = list(color = "transparent"),
                            name = "min/max")  
        }
        
        if(ribbon_var == TRUE){
            p3 <- p3 |> 
                add_ribbons(ymin = ~p25.lt5,
                            ymax = ~p75.lt5,
                            fillcolor = stringr::str_replace(thresh5_color, "1\\)", "0.2\\)"),  
                            line = list(color = "transparent"),
                            name = "middle 50% of values")  
        }
        
        # line
        p3 <- p3 |> 
            add_trace(y = ~doLessThan5_percent,
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


