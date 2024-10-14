
## Create linear models to predict mean annual temperature (MAT) and maximum temperature of the warmest month (MTWM) continuously across the Boiling River forest using remotely-sensed Landsat 9 imagery
## Then, extract MAT and MTWM for each plot location

## Prepared by: Alyssa T. Kullberg
## Last updated: October 14, 2024
## R version 4.3.1

set.seed(565)

## Load libraries
library(raster)
library(sp)
library(caret)
library(regclass)
library(ncf)
library(car)
library(vegan)
library(sf)


## Load Landsat raster data (can be downloaded from USGS EarthExplorer)
lst <- raster("../Data/LST/LC09_L1TP_006066_20220626_20220802_02_T1.tif") #load LST raster


## Crop raster extent to the study area to reduce processing time
ext <- c(528000, 531500, -975000, -973000) 
lst <- crop(lst, ext)


## Load temperature data summarized into climate metrics by sensor type (file created in climate_metrics_clean.R)
tms.clim15 <- read.csv("../Data/Made_in_R/tms_clim15_clean.csv")


## Remove two hilltop loggers to keep only those that are in the riparian forest
tms.clim15 <- subset(tms.clim15, Logger.ID != "T94223054" & Logger.ID != "T94223055")


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
NDVI <- unlist(tms.clim15$NDVI)
Dist_to_river <- unlist(tms.clim15$Dist_to_river)
long <- unlist(tms.clim15$Longitude)


names(lst) <- c("LST")


## Predict MAT (and MTWM) across study area and extract MAT (and MTWM) for each plot location
model.lm.mat <- lm(MAT ~ LST, data = tms.clim15)
model.sum.mat <- summary(model.lm.mat)

model.lm.mtwm <- lm(Tmax ~ LST, data = tms.clim15)
model.sum.mtwm <- summary(model.lm.mtwm)


## Predict climate metric of interest across LST (continuous surface) according to linear model
raster.predictions.mat <- predict(object = lst, model = model.lm.mat)
raster.predictions.mtwm <- predict(object = lst, model = model.lm.mtwm)

## Write each raster map as a file
writeRaster(raster.predictions.mat, paste0("../Data/Made_in_R/Climate_maps_rehash/MAT.tif"),
            overwrite = TRUE)
writeRaster(raster.predictions.mat, paste0("../Data/Made_in_R/Climate_maps_rehash/MTWM.tif"),
            overwrite = TRUE)


####################################################
####################################################

# Load plot locations
plots <- read.csv("../Data/BR_Plot_Locations.csv")

# Rename latitude and longitude from occ_all_3 into new dataframe: coords
lats <- plots$lat
lons <- plots$lon
coords <- data.frame(x=lons, y=lats)

# Set lat and long onto a coordinate system
points <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox=NULL)
points <- spTransform(points, CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"))

values.mat <- raster::extract(raster.predictions.mat, points)
values.mtwm <- raster::extract(raster.predictions.mtwm, points)

plots$MAT <- values.mat
plots$MTWM <- values.mtwm

#write.csv(plots, "../Data/Made_in_R/plot_locations_temp_model.csv")
