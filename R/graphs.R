library(tidyverse)
library(plotly)

# data ----
dat <- readRDS(here::here("data_wq", "monthly_DO.RDS")) |> 
    mutate(date = lubridate::ymd(paste(year, month, "01")),
           doLessThan2_percent = doLessThan2_proportion * 100,
           doLessThan5_percent = doLessThan5_proportion * 100)


st = "gndpcwq"


dat_sub <- dat |> 
    filter(station == st) |> 
    rowwise() |> 
    mutate(do_mgl_ribbonmin = max(do_mgl_min, do_mgl_mean - do_mgl_sd, na.rm = TRUE),
           do_mgl_ribbonmax = min(do_mgl_max, do_mgl_mean + do_mgl_sd, na.rm = TRUE),
           do_mgl_ribbonmin = case_when(do_mgl_ribbonmin == "-Inf" ~ NA_real_,
                                        .default = do_mgl_ribbonmin),
           do_mgl_ribbonmax = case_when(do_mgl_ribbonmax == "Inf" ~ NA_real_,
                                        .default = do_mgl_ribbonmax))

# Mean DO mg/L graph ----
p <- plot_ly(dat_sub,
        type = "scatter",
        mode = "lines",
        x = ~date,
        y = ~do_mgl_mean,
        color = I("navy"),
        name = "Monthly Mean") |> 
    add_ribbons(ymin = ~do_mgl_min,
                ymax = ~do_mgl_max,
                fillcolor = "rgba(0, 0, 128, 0.1)",
                line = list(color = "transparent"),
                name = "min/max") |> 
    add_ribbons(ymin = ~do_mgl_ribbonmin,
                ymax = ~do_mgl_ribbonmax,
                fillcolor = "rgba(0, 0, 128, 0.2)",
                line = list(color = "transparent"),
                name = "+/- 1 SD") |>
    add_lines(y = 2,
              color = I("#e6550d"),
              line = list(dash = "dash"),
              showlegend = FALSE) |> 
    add_lines(y = 5,
              color = I("#fdae6b"),
              line = list(dash = "dash"),
              showlegend = FALSE) |> 
    layout(xaxis = list(title = "Date"),
           yaxis = list(title = "Dissolved Oxygen (mg/L)"),
           title = list(text = st,
                        x = 0.09,
                        xanchor = "left",
                        y = 0.97))
# p

# amount of hypoxia graph ----
p2 <- plot_ly(dat_sub,
              type = "scatter",
              mode = "markers+lines",
              x = ~date,
              y = ~doLessThan2_percent,
              color = I("#e6550d"),
              marker = list(size = 4),
              name = "DO < 2") |> 
    add_trace(y = ~doLessThan5_percent,
              type = "scatter",
              mode = "markers+lines",
              color = I("#fdae6b"),
              marker = list(size = 4),
              name = "DO < 5") |> 
    layout(xaxis = list(title = "Date"),
           yaxis = list(title = "% of month below threshold"),
           title = list(text = st,
                        x = 0.09,
                        xanchor = "left",
                        y = 0.97))
# p2

# from selsit_plo(), https://github.com/tbep-tech/wq-dash/blob/master/R/funcs.R
# selsit is an input in the .Rmd for the dashboard
# https://github.com/tbep-tech/wq-dash/blob/cd0839d0a27fa0729c56af857190ab7679ca888a/wq-dash.Rmd#L380
# selsit <- input$map_marker_click$id  
plotly::subplot(p, p2, nrows = 2, shareX = T, titleY = T) |> 
    plotly::rangeslider(thickness = 0.03)

