library(shiny)
library(here)
library(leaflet)
library(leaflegend)
library(htmltools)
library(dplyr)
library(tidyr)
library(ggplot2)

# bring in data frames (calculations have been made outside the app)
load(here::here("data_wq", "do_dataframes.RData"))


stn_trends_long <- stn_trends |> 
    pivot_longer(-c(station, nYears),
                 names_to = c("param", ".value"),
                 names_sep = "\\.") |> 
    mutate(significant = case_when(pval <= 0.05 ~ "yes",
                                   is.na(pval) ~ "no",  # these are proportions when all values were 0
                                   pval > 0.05 ~ "no"),
           direction = case_when(trend < 0 ~ "decreasing",
                                 trend > 0 ~ "increasing",
                                 trend == 0 ~ "none",
                                 is.na(trend) ~ "not calculated"),
           map_color = case_when(is.na(trend) ~ "not calculated",
                                 significant == "no" ~ "no trend",
                                 direction == "increasing" ~ "increasing",
                                 direction == "decreasing" ~ "decreasing")) |> 
    left_join(distinct(select(tomap, station, lat, long)),
              by = "station")

hypoxia_annual <- tomap |> 
    select(station, year, threshold, pct) |> 
    pivot_wider(names_from = threshold,
                values_from = pct)

mgl_timeSeries <- stn_mmyr |> 
    select(station, year, month,
           domgl_median, domgl_p25, domgl_p75,
           domgl_min, domgl_max) |> 
    mutate(date = lubridate::ymd(paste(year, month, "01")))


# color palettes and shapes ----
# palette <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
palette_unus <- colorFactor(palette = c("#2166AC", "#B2182B"),  # from Tol's BuRd
                            levels = c(0, 1))

palette_time.hypoxic <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))

palette_trnd.mgl <- colorFactor(palette = c("#2166AC", "#B2182B", "#FFEE99", "#7F7F7F"),  # from Tol's BuRd EXCEPT for 'not calcd' - need to check this
                                levels = c("increasing", "decreasing", "no trend", "not calculated"))

palette_trnd.thrsh <- colorFactor(palette = c("#762A83", "#1B7837", "#FFEE99", "#7F7F7F"),  # from Tol's PRGn
                                  levels = c("increasing", "decreasing", "no trend", "not calculated"))
# use circles for typical and squares for unusual - change here if desired
shape_assignment <- function(x){
    case_when(is.na(x) ~ "diamond",
              x == 0 ~ "circle",
              x == 1 ~ "rect",
              .default = "triangle")
}


# big map data frame ----
# define everything, for later custom creation of symbols
tomap <- tomap |> 
    mutate(size = case_when(sqrt(pct) <= 3 ~ 3,
                            is.na(pct) ~ 3,
                            .default = sqrt(pct)),
           size = size + 1,                           # for slightly bigger symbols
           shape = shape_assignment(unusual),
           color_unus = palette_unus(unusual),
           color_time.hypoxic = palette_time.hypoxic(pct),
           pct_rounded = round(pct))

# size legend data frame ----
legend_sizes <- data.frame(
    value = c(3, 25, 50, 75),
    label = c("<10", "25", "50", "75")
) |> 
    mutate(size = case_when(sqrt(value) <= 3 ~ 3,
                            .default = sqrt(value)),
           size = size + 1)

# symbol setup----

# TYPICAL VS UNUSUAL
# make the grid for symbols
symbols_unus.grid <- data.frame(
    unusual = c(0, 1)
) |> 
    mutate(shape = shape_assignment(unusual),
           color = palette_unus(unusual))
# make the symbols
symbols_unus <- Map(
    f = makeSymbol,
    shape = symbols_unus.grid$shape,
    fillColor = symbols_unus.grid$color,
    color = "black",
    opacity = 0.5,
    height = 20,
    width = 20
)
# add the url to the time grid symbol data frame for joining with 'tomap'
symbols_unus.grid$symbol_unus <- unlist(symbols_unus)
# join to the mapping df
tomap <- left_join(tomap, symbols_unus.grid,
                   by = "unusual")

# TIME HYPOXIC
# make combinations of typical/unusual and every rounded percent
symbols_time.grid <- expand.grid(
    unusual = c(0, 1),
    pcts = c(0:100)
) |> 
    mutate(shape = shape_assignment(unusual),
           color = palette_time.hypoxic(pcts))    
# generate all those symbols
symbols_time.hypoxic <- Map(
    f = makeSymbol,
    shape = symbols_time.grid$shape,
    fillColor = symbols_time.grid$color,
    color = "black",
    opacity = 0.5,
    height = 20,
    width = 20
)
# add the url to the time grid symbol data frame for joining with 'tomap'
symbols_time.grid$symbol_time <- unlist(symbols_time.hypoxic)
# join to the mapping df
tomap <- left_join(tomap, symbols_time.grid,
                   by = c("unusual",
                          "pct_rounded" = "pcts"))