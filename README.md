## BoilingRiver_ThermalGradient_TreeDiversity

__Title:__ Hotter temperatures reduce the diversity and alter the composition of woody plants in an Amazonian forest

__Authors:__ Riley P. Fortier*, Alyssa T. Kullberg*, Roy D. Soria Ahuanari, Lauren Coombs, Andrés Ruzo, Kenneth J. Feeley

__Year:__ 2024


\* Please contact Riley P. Fortier (fortier.riley@gmail.com) with any questions regarding tree plot or soil nutrient data.

\* Please contact Alyssa T. Kullberg (alyssa.kullberg@gmail.com) with any questions regarding air temperature, soil temperature, or relative humidity data.


### This repository contains datasets and scripts associated with Fortier et al. (2024), including:


### Time-series air temperature, soil temperature, and relative humidity datasets
* __Boiling_River_temp_RH_datasets_README.docx__
* __tms_data_clean.csv__
* __hobo_data_clean.csv__
* __climate_metrics_clean.R__ 
   + calculate climate metrics (for TMS loggers at the Boiling River and for other forest sites) like mean annual temperature (MAT) and maximum temperature of the warmest month (MTWM, 'maximum temperature' in the manuscript)
   + create and save tms_clim15_clean.csv, used in temp_model_clean.R for response variables (MAT and MTWM)
* __temp_model_clean.R__ 
  + create linear models to predict MAT and MTWM across the Boiling River forest using Landsat land surface temperature as the predictor
  + extract the MAT and MTWM of each plot


### Tree diversity data and analyses from 70 3-m radius plots
* __Plot_chars.csv__
* __BR_plot_data.xlsx__
* __DETERMINACIONES BOTANICAS - RIO HIRVIENTE.xlsx__
* __occ_clean_final.csv__
* __BR Data analysis.R__ 
   + load and clean plot characteristic and tree composition data
   + characterize and calculate plot diversity, compare with plot temperature using linear models
   + run ordinations to visualize how plot composition compares with temperature, run partial Mantel test to test air temperature effect on tree composition
   + calculate thermal maxima for each species, calculate community temperature scores (CTS), compare plot CTS with air temperature using linear models 
