library(shiny)
library(bslib)
library(bsicons)
library(here)
library(plotly)
library(leaflet)
library(leaflegend)
library(htmltools)
library(reactable)
library(dplyr)
library(tidyr)
library(ggplot2)

# setup ----
source(here::here("R", "functions.R"))
source("global.R")

# horizontal with ticks ----

stn = "tjroswq"
yr = 2020

stn_df <- hypoxia_annual |> 
    filter(station == stn)

stn_thresh <- stn_thresholds |> 
    filter(station == stn) |> 
    rename(LT2 = LT2pct_boxOutlier)

# colors for line and threshold come from Tol's nightfall palette (colorblind safe)
# 3 wide by 1 high is decent
# this is 280 x 88 px
ggplot(stn_df,
       aes(x = LT2,
           y = 1)) +
    # light gray lines for all years
    geom_point(
        shape = 124,
        col = "gray60",
        size = 5
    ) +
    geom_hline(yintercept = 1,
               col = "gray80") +
    # line for year to highlight
    geom_point(data = stn_df |> filter(year == yr),
               shape = 124,
               col = "#42A7C6",
               size = 9) +
    # line for threshold
    geom_point(data = stn_thresh,
               shape = 124,
               col = "#E94C1F",
               size = 6) +
    coord_cartesian(xlim = c(0, 100)) +
    scale_x_continuous(labels = scales::label_percent(scale = 1)) +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank())
    
    
# vertical, no ticks ----
test <- stn_mmyr |> 
    filter(station == "gndbhwq")


ggplot() +
    geom_segment(aes(x = 0,
                     xend = 0,
                     y = min(test$domgl_median, na.rm = TRUE),
                     yend = max(test$domgl_median, na.rm = TRUE)),
                 linewidth = 5,
                 col = "navy",
                 alpha = 0.5) +
    geom_segment(aes(x = -0.1,
                     xend = 0.1,
                     y = 7,
                     yend = 7),
                 linewidth = 3,
                 col = "red3") +
    # coord_fixed(ratio = 1, xlim = c(-1, 1), ylim = c(min(test$domgl_median, na.rm = TRUE), max(test$domgl_median, na.rm = TRUE))) +
    coord_cartesian(xlim = c(-1, 1)) +
    theme_void()