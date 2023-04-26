
## Create linear models to predict mean annual temperature (MAT) and maximum temperature of the warmest month (MTWM) continuously across the Boiling River forest using remotely-sensed Landsat 9 imagery

## Prepared by: Alyssa T. Kullberg
## Last updated: April 26, 2023
## R version 4.2.2


## Load libraries
library(raster)
library(stringr)


## Load Landsat raster data (can be downloaded from USGS EarthExplorer)
lst <- raster("../Data/LST/LC09_L1TP_006066_20220626_20220802_02_T1.tif") #load LST raster


## Crop raster extent to the study area to reduce processing time
ext <- c(528000, 531500, -975000, -973000) 
lst <- crop(lst, ext)


## Load temperature data summarized into climate metrics by sensor type (file created in climate_metrics_clean.R)
tms.clim15 <- read.csv("../Data/Made_in_R/tms_clim15_clean.csv")



#######
## Prepare temperature logger data for LST extraction, append LST values, and run linear models
#######

## First, rename latitude and longitude from climate metric dataframe into new dataframe: coords
lats <- tms.clim15$Latitude
lons <- tms.clim15$Longitude
coords <- data.frame(x=lons, y=lats)


## Set lat and long onto the same coordinate system as the raster layer lst
points <- SpatialPoints(coords, proj4string=CRS("+proj=longlat"), bbox=NULL)
points.utm <- spTransform(points, CRS("+proj=utm +zone=18 ellps=WGS84 +datum=WGS84"))
  

## Extract LST values based on UTM points from each dataframe
values <- extract(lst, points.utm)
  

## Attach LST values to each climate metric dataframe in the list
tms.clim15$LST <- values
LST <- unlist(tms.clim15$LST)


## Create list of variable names to create models with in for-loop
var.names <- c("MAT_day", "Tmax")


## Prepare empty lists to store model results from for-loop
raster.maps <- list()
model.lm.results <- list()


## Run for-loop to create linear models for MAT (MAT_day) and MTWM (Tmax) to predict these variables across the Boiling River forest, using LST as the predictor
for(j in 1:length(var.names)){
    y.var <- as.numeric( unlist( tms.clim15[ names( tms.clim15) == var.names[j]]))

    ## Fit linear model
    model.lm <- lm(y.var ~ LST)
    
    ## Save LLS model results into lists, get R2 of model from these
    model.lm.results[[j]] <- summary(model.lm)
    names(model.lm.results) <- var.names[[j]]
    
    ## Predict climate metric of interest across LST (continuous surface) according to RF model
    names(lst) <- c("LST")
    raster.predictions <- predict(object = lst, model = model.lm)
    
    ## Write each raster map to a list
    raster.maps[[j]] <- raster.predictions
    names(raster.maps[[j]]) <- var.names[[j]]
    
    ## Write each raster map as a file, named after the sensor type
    writeRaster(raster.predictions, paste0("../Data/Made_in_R/Climate_maps_clean/", var.names[[j]], ".tif"),
                overwrite = TRUE)
    
}

## View linear model results to report statistics
# MAT ~ LST
model.lm.results[[1]]

# MTWM ~ LST
model.lm.results[[2]]
