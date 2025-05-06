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
library(patchwork)

# setup ----
source(here::here("R", "functions.R"))
source("global.R")

# set up colors ----
# from Tol's 'sunset' palette
# col_lowDO <- "#DD3D2D"
# col_medDO <- "#4A7BB7"
# orign <- "#42A7C6"
col_lowDO <- "#A50026"
col_medDO <- "#364B9A"

# horizontal with ticks ----

stn = "tjroswq"
stn = "jacnewq"
yr = 2015

stn_mglByYear <- stn_mmyr |> 
    summarize(.by = c(station, year),
              medianDO = median(domgl_median, na.rm = TRUE))

stn_df <- hypoxia_annual |> 
    filter(station == stn) |> 
    left_join(stn_mglByYear)



p1 <- plot_yrdist(stn_df, LT2,
                  yr, col_lowDO) +
    labs(subtitle = "% of year DO < 2 mg/L")

p2 <- plot_yrdist(stn_df, LT5,
                  yr, col_lowDO) +
    labs(subtitle = "% of year DO < 5 mg/L")

# combine low DO plots
p3 <- p1 / p2 +
    plot_layout(axes = "collect")


# median mg/L
p4 <- plot_yrdist(stn_df, medianDO,
                  yr, col_medDO) +
    labs(subtitle = "Median DO concentration (mg/L)")


# combine all
p3 / plot_spacer() / p4 + 
    plot_layout(heights = c(1, 1, 0.5, 1))


# another option
p5 <- ggplot(stn_df, aes(y = medianDO, x = 1)) +
    geom_vline(xintercept = 1, col = "gray80") +
    
    # Light gray lines for all years
    geom_linerange(
        aes(xmin = 0.95, xmax = 1.05),
        col = "gray60",
        size = .5
    ) +
    
    # Line for year to highlight
    geom_linerange(
        data = stn_df |> filter(year == yr),
        aes(xmin = 0.9, xmax = 1.1),
        col = col_medDO,
        size = 1
    ) +
    
    theme(axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank()) +
    coord_cartesian(ylim = c(0, 13.5)) +
    labs(subtitle = "Median DO (mg/L)")

p5

p5 + p3


    
    
