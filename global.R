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


# stn_trends_long <- stn_trends |> 
#     pivot_longer(-c(station, nYears),
#                  names_to = c("param", ".value"),
#                  names_sep = "\\.") |> 
#     mutate(significant = case_when(pval <= 0.05 ~ "yes",
#                                    is.na(pval) ~ "no",  # these are proportions when all values were 0
#                                    pval > 0.05 ~ "no"),
#            direction = case_when(trend < 0 ~ "decreasing",
#                                  trend > 0 ~ "increasing",
#                                  trend == 0 ~ "none",
#                                  is.na(trend) ~ "not calculated"),
#            map_color = case_when(is.na(trend) ~ "not calculated",
#                                  significant == "no" ~ "no trend",
#                                  direction == "increasing" ~ "increasing",
#                                  direction == "decreasing" ~ "decreasing")) |> 
#     left_join(distinct(select(tomap, station, lat, long)),
#               by = "station")
# 
# hypoxia_annual <- tomap |> 
#     select(station, year, threshold, pct) |> 
#     pivot_wider(names_from = threshold,
#                 values_from = pct)
# 
# mgl_timeSeries <- stn_mmyr |> 
#     select(station, year, month,
#            domgl_median) |> 
#     mutate(date = lubridate::ymd(paste(year, month, "01")))
# 
# # table of summary stats by station
# stn_trends2 <- stn_trends_long |> 
#     mutate(units = case_when(param == "domgl_median" ~ "mg/L per year",
#                              param == "LT2" ~ "% per year",
#                              param == "LT5" ~ "% per year"),
#            desc = case_when(significant == "yes" ~ stringr::str_to_sentence(direction),
#                             .default = "No trend"),
#            significant = case_when(significant == "no" ~ "not significant",
#                                    .default = "statistically significant"),
#            trend = round(trend, 3),
#            pval = round(pval, 3),
#            trend_description = glue::glue("{desc}. Estimate: {trend} {units}; p = {pval}."),
#            trend_description = case_when(direction == "not calculated" ~ "Not calculated.",
#                                          .default = trend_description),
#            param = case_when(param == "domgl_median" ~ "DO (mg/L) median",
#                              param == "LT2" ~ "% of year under 2 mg/L",
#                              param == "LT5" ~ "% of year under 5 mg/L")) |> 
#     select(station, nYears, param, Trend = trend_description) 
# 
# stnMedians <- stn_yr |> 
#     select(station,
#            domgl_median = annual_median.mgl,
#            LT2 = annual_LT2_percent,
#            LT5 = annual_LT5_percent) |> 
#     summarize(.by = station,
#               domgl_median = median(domgl_median, na.rm = TRUE),
#               LT2 = median(LT2, na.rm = TRUE),
#               LT5 = median(LT5, na.rm = TRUE)) |> 
#     mutate(domgl_median = round(domgl_median, 1),
#            LT2 = round(LT2, 2),
#            LT5 = round(LT5, 2)) |> 
#     pivot_longer(-station,
#                  names_to = "param",
#                  values_to = "Median") |> 
#     mutate(param = case_when(param == "domgl_median" ~ "DO (mg/L) median",
#                              param == "LT2" ~ "% of year under 2 mg/L",
#                              param == "LT5" ~ "% of year under 5 mg/L"))
# 
# stn_summaries <- full_join(stnMedians, stn_trends2, by = c("station", "param")) |> 
#     relocate(nYears, .after = station) |> 
#     rename("Time Series Length (years)" = nYears,
#            "Long Term Median" = Median,
#            Variable = param)



# color palettes and shapes ----
# palette <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
palette_unus <- colorFactor(palette = c("#2166AC", "#B2182B"),  # from Tol's BuRd
                            levels = c(0, 1))

palette_time.hypoxic <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))

palette_median.mgl <- colorNumeric(palette = "YlGnBu", domain = c(0, 14))

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

# rm(stn_mmyr, stn_yr, stn_trends, stn_trends2)

# generate data frame to make map of medians
tomap_medians1 <- tomap |> 
    summarize(.by = c(station, threshold),
              medianLowDO = median(pct, na.rm = TRUE)) |> 
    pivot_wider(names_from = threshold,
                values_from = medianLowDO)
tomap_medians2 <- tomap |> 
    select(station, year, median.mgl, lat, long) |> 
    distinct() |> 
    summarize(.by = station,
              domgl_median = median(median.mgl, na.rm = TRUE),
              lat = median(lat, na.rm = TRUE),
              long = median(long, na.rm = TRUE))
tomap_medians <- full_join(tomap_medians1, tomap_medians2) |> 
    pivot_longer(2:4,
                 names_to = "param",
                 values_to = "value")
rm(tomap_medians1, tomap_medians2)
