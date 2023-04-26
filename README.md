## BoilingRiver_ThermalGradient_TreeDiversity

__Title:__ Warming reduces diversity and alters composition of trees in the Amazon

__Authors:__ Riley P. Fortier*, Alyssa T. Kullberg*, Lauren Coombs, Roy Soria Ahuanari, Andrés Ruzo, Kenneth J. Feeley

__Year:__ 2023


\* Please contact Riley P. Fortier (fortier.riley@gmail.com) with any questions regarding tree plot or soil nutrient data.

\* Please contact Alyssa T. Kullberg (alyssa.kullberg@gmail.com) with any questions regarding air temperature, soil temperature, or relative humidity data.


## This repository contains datasets and scripts associated with Fortier et al. (2023), including:

### Time-series air temperature, soil temperature, and relative humidity datasets
* __Boiling_River_temp_RH_datasets_README.docx__
* __data_clean.csv__
* __hobo_data_clean.csv__
* __climate_metrics_clean.R__ 
   + calculate climate metrics (for TMS loggers at the Boiling River and for other forest sites) like mean annual temperature (MAT) and maximum temperature of the warmest month (MTWM)
   + plot climate metrics, plot temperature regimes
   + plot time-series temperature and humidity data
   + create and save tms_clim15_clean.csv, used in temp_model_clean.R for response variables (MAT and MTWM)
* __temp_model_clean.R__ 
  + create linear models to predict MAT and MTWM across the Boiling River forest using Landsat land surface temperature as the predictor


### Tree diversity data and analyses from 70 3-m radius plots
* __Coming soon!__
