# DO Dashboard  

This is the code behind an interactive dashboard of NERRS SWMP Dissolved Oxygen data. The dashboard can be found on the [Oxygen Levels page](https://sites.google.com/nerra.org/nerrs-state-of-the-estuary/oxygen-levels?authuser=0) of [this NERRS monitoring website](https://sites.google.com/nerra.org/nerrs-state-of-the-estuary/home).  


## To update in the future:  

This dashboard imported trends that had already been calculated as part of a SWMP Synthesis. A manuscript is currently under review at a scientific journal, and will be linked here once published. The data here is a subset.  

Processing scripts are in the [DO Data Processing Repository](https://github.com/nerrscdmo/do-data-processing). Follow instructions in that readme to update processed data. `do_dataframes.RData` needs to be copied from the processing repo into the `data_wq` folder of this `do-dashboard` project.  

In this `do-dashboard` directory, make sure `NERR Websites.csv` is up-to-date.  


### Colors and symbols  

In this dashboard, the color palettes for maps are defined at the top of `global.R`. Shapes and sizes are defined just below color palettes. Custom symbols (non-circles, with the selected color palette) are generated in this `global.R` as well, using functions that refer back to the color palette and shape designations earlier in the script.   

### Factor order  

The levels of trend results (increasing/decreasing/no trend/not calculated) are ordered in the definitions of the color palettes in `global.R`.  
