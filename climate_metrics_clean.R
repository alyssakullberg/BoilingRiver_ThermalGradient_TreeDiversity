
## Organize Boiling River climate data (Oct 2021 - Oct 2022) and calculate summary climate metrics, including mean annual temperature (MAT) and maximum temperature of the warmest month (MTWM -- 'maximum temperature' in the manuscript)
## MAT and MTWM, calculated with TOMST-TMS4 loggers at 15cm aboveground, are summarized by logger and are used in temp_model_clean.R to model MAT and MTWM across the study site
## temperature (TOMST) and humidity/vpd (HOBO) data are also summarized to monthly mean/sd/max/min values by logger for figures presented in the supplementary materials of the manuscript

## Prepared by: Alyssa T. Kullberg
## Last updated: October 14, 2024
## R version 4.3.1


## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(ggridges)
library(cowplot)


## Load HOBO data and TMS data (explained in associated README)
hobo <- read.csv("../Data/Made_in_R/hobo_data_clean.csv", sep = ",") # Tair at ~1m, RH
tms <- read.csv("../Data/Made_in_R/tms_data_clean.csv", sep = ",") # Tair at 15cm and 2cm, Tsoil at 6cm


## Rename TOMST/TMS loggers according to manuscript
tms$Logger.new <- rep(NA, nrow(tms))
tms[tms$Logger.ID == "T94223062",]$Logger.new <- "TMS_6"
tms[tms$Logger.ID == "T94223063",]$Logger.new <- "TMS_2"
tms[tms$Logger.ID == "T94223061",]$Logger.new <- "TMS_11"
tms[tms$Logger.ID == "T94223053",]$Logger.new <- "TMS_4"
tms[tms$Logger.ID == "T94223059",]$Logger.new <- "TMS_9"
tms[tms$Logger.ID == "T94223056",]$Logger.new <- "TMS_1"
tms[tms$Logger.ID == "T94223054",]$Logger.new <- "TMS_12"
tms[tms$Logger.ID == "T94223065",]$Logger.new <- "TMS_5"
tms[tms$Logger.ID == "T94223060",]$Logger.new <- "TMS_10"
tms[tms$Logger.ID == "T94223064",]$Logger.new <- "TMS_8"
tms[tms$Logger.ID == "T94223052",]$Logger.new <- "TMS_3"
tms[tms$Logger.ID == "T94223055",]$Logger.new <- "TMS_13"
tms[tms$Logger.ID == "T94223058",]$Logger.new <- "TMS_7"


## Rename HOBO loggers according to manuscript
hobo$Logger.new <- rep(NA, nrow(hobo))
hobo[hobo$Logger.ID == "H21189299",]$Logger.new <- "HOBO_2"
hobo[hobo$Logger.ID == "H21189297",]$Logger.new <- "HOBO_3"
hobo[hobo$Logger.ID == "H21181619",]$Logger.new <- "HOBO_4"
hobo[hobo$Logger.ID == "H21181620",]$Logger.new <- "HOBO_5"
hobo[hobo$Logger.ID == "H21189301",]$Logger.new <- "HOBO_7"
hobo[hobo$Logger.ID == "H21189300",]$Logger.new <- "HOBO_8"
hobo[hobo$Logger.ID == "H21189296",]$Logger.new <- "HOBO_10"
hobo[hobo$Logger.ID == "H21181621",]$Logger.new <- "HOBO_11"
hobo[hobo$Logger.ID == "H21189298",]$Logger.new <- "HOBO_12"


## Prepare datasets by classifying date and time columns and pulling out hour/min/month into separate columns
tms$Date <- as.POSIXct(tms$Date, format = "%m/%d/%Y")
tms$Time <- as.POSIXct(tms$time, format = "%m/%d/%Y %H:%M")
tms$Time <- format(tms$Time, "%H:%M:%S")
tms$Hour <- hour(as.POSIXct(tms$time, format = "%m/%d/%Y %H:%M"))
tms$Min <- minute(as.POSIXct(tms$time, format = "%m/%d/%Y %H:%M"))
tms$HourMin <- as.numeric(paste0(tms$Hour, ".", tms$Min))
tms$Month <- month(as.POSIXct(tms$time, format = "%m/%d/%Y %H:%M"))


hobo$Date <- as.POSIXct(hobo$Date, format = "%m/%d/%Y")
hobo$Time <- as.POSIXct(hobo$date_time, format = "%m/%d/%Y %H:%M")
hobo$Time <- format(hobo$Time, "%H:%M:%S")
hobo$Hour <- hour(as.POSIXct(hobo$date_time, format = "%m/%d/%Y %H:%M"))
hobo$Min <- minute(as.POSIXct(hobo$date_time, format = "%m/%d/%Y %H:%M"))
hobo$HourMin <- as.numeric(paste0(hobo$Hour, ".", hobo$Min))
hobo$Month <- month((as.POSIXct(hobo$date_time, format = "%m/%d/%Y %H:%M")))


## Summarize temperature data from HOBOs (Tair 1m) and TOMST/TMS (Tair 15cm, Tair 2cm, Tsoil 6cm), 
hobo.ids <- unique(hobo$Logger.ID) # extract unique HOBO IDs
tms.ids <- unique(tms$Logger.ID) # extract unique TOMST/TMS IDs

hobo.clim <- as.data.frame(hobo.ids) # convert HOBO IDs to a dataframe
names(hobo.clim) <- "Logger.ID"

tms.clim15 = tms.clim2 = tms.clim6 = as.data.frame(tms.ids) # convert TOMST/TMS IDs to dataframes (for all 3 strata)
names(tms.clim15) = names(tms.clim2) = names(tms.clim6) = "Logger.ID"



#######
### Climate metric key:
#######

## Mean annual temperature (MAT)
# mean temperature recorded over 1 year

## Maximum temperature of the warmest month (MTWM, 'maximum temperature')
# maximum temperature recorded during the hottest month on average across all loggers of each height

## Mean annual nighttime temperature (MAT_night)
# 6pm to 6am average temperature over 1 year

## First, subset the TMS dataset to just nighttime for MAT_night and Tmax_night for air and soil
tms_night <- subset(tms, HourMin >= 18 | HourMin <= 6)


## Calcualte MAT and MAT_night for TOMST/TMS loggers at 15 cm, 2 cm, and -6 cm
MAT15 = MAT2 = MAT6 = MAT_night15 = MAT_night2 = MAT_night6 = MAT15_sd = MAT2_sd = MAT6_sd = MAT_night15_sd = MAT_night2_sd = MAT_night6_sd = rep(NA, length(tms.ids))

for(i in 1:length(tms.ids)){
  id.sub <- subset(tms, Logger.ID == tms.ids[[i]])
  id.sub_mean <- subset(tms, Logger.ID == tms.ids[[i]])
  id.sub_night <- subset(tms_night, Logger.ID == tms.ids[[i]])
  
  if(length(unique(id.sub$Date)) > 274) { # don't calculate mean annual temp. if TOMST/TMS worked for <3/4 of a year
    MAT15[i] = mean(na.omit(id.sub$T3_air15))
    MAT15_sd[i] = sd(na.omit(id.sub$T3_air15))
    MAT2[i] = mean(na.omit(id.sub$T2_air2))
    MAT2_sd[i] = sd(na.omit(id.sub$T2_air2))
    MAT6[i] = mean(na.omit(id.sub$T1_soil6))
    MAT6_sd[i] = sd(na.omit(id.sub$T1_soil6))
    
    MAT_night15[i] = mean(na.omit(id.sub_night$T3_air15))
    MAT_night15_sd[i] = sd(na.omit(id.sub_night$T3_air15))
    MAT_night2[i] = mean(na.omit(id.sub_night$T2_air2))
    MAT_night2_sd[i] = sd(na.omit(id.sub_night$T2_air2))
    MAT_night6[i] = mean(na.omit(id.sub_night$T1_soil6))
    MAT_night6_sd[i] = sd(na.omit(id.sub_night$T1_soil6))
    
  }else(MAT15[i] = MAT2[i] = MAT6[i] = MAT_night15[i] = MAT_night2[i] = MAT_night6[i] = MAT15_sd[i] = MAT2_sd[i] = MAT6_sd[i] = MAT_night15_sd[i] = MAT_night2_sd[i] = MAT_night6_sd[i] = NA)
}


## Fill climate metric data frames according to results from above for-loops
tms.clim15$MAT <- MAT15
tms.clim2$MAT <- MAT2
tms.clim6$MAT <- MAT6

tms.clim15$MAT_sd <- MAT15_sd
tms.clim2$MAT_sd <- MAT2_sd
tms.clim6$MAT_sd <- MAT6_sd

tms.clim15$MAT_night <- MAT_night15
tms.clim2$MAT_night <- MAT_night2
tms.clim6$MAT_night <- MAT_night6

tms.clim15$MAT_night_sd <- MAT_night15_sd
tms.clim2$MAT_night_sd <- MAT_night2_sd
tms.clim6$MAT_night_sd <- MAT_night6_sd


## Calculate maximum temperature of the warmest month (Tmax) for each sensor type
## First, determine the hottest month for each logger type

monthly.mean <- tms %>%
  group_by(Month) %>%
  summarize(monthly.mean = mean(na.omit(T3_air15)))
# TOMST/TMS 15cm: September (26.3 C)

monthly.mean <- tms %>%
  group_by(Month) %>%
  summarize(monthly.mean = mean(na.omit(T2_air2)))
# TOMST/TMS 2cm: September or October -- going with October (26.6 C)

monthly.mean <- tms %>%
  group_by(Month) %>%
  summarize(monthly.mean = mean(na.omit(T1_soil6)))
# TOMST/TMS -6cm: October (27.9 C)


## Calculate Tmax for TOMST/TMS at 15cm, 2cm, -6cm
tms.sub <- subset(tms, Month == 9)
Tmax <- tms.sub %>%
  group_by(Logger.ID) %>%
  summarize(Tmax = max(na.omit(T3_air15)))
tms.clim15 <- left_join(tms.clim15, Tmax, by="Logger.ID")

# tms.sub <- subset(tms, Month == 10)
# Tmax <- tms.sub %>%
#   group_by(Logger.ID) %>%
#   summarize(Tmax = max(na.omit(T2_air2)))
# tms.clim2 <- left_join(tms.clim2, Tmax, by="Logger.ID")
# 
# tms.sub <- subset(tms, Month == 10)
# Tmax <- tms.sub %>%
#   group_by(Logger.ID) %>%
#   summarize(Tmax = max(na.omit(T1_soil6)))
# tms.clim6 <- left_join(tms.clim6, Tmax, by="Logger.ID")


## Add back in some of the metadata about each logger

tms.meta.unique <- unique(select(tms, Logger.ID, Logger.new, Latitude, Longitude)) # make a dataframe with the lat, long, and new logger ID for each unique logger

tms.clim15 <- left_join(tms.clim15, tms.meta.unique, by = "Logger.ID")

# tms.clim2 <- left_join(tms.clim2, tms.meta.unique, by = "Logger.ID")
# 
# tms.clim6 <- left_join(tms.clim6, tms.meta.unique, by = "Logger.ID")


## Save tms.clim15 to a .csv file to use for modelling MAT and MTWM across the Boiling River forest in temp_model_clean.R
# write.csv(tms.clim15, "../Data/Made_in_R/tms_clim15_clean.csv")



## Create new dataframes summarized by month for each type of logger for time-series climate figures (in supplementary material of manuscript)

tms_night$YearMonth <- substr(tms_night$Date, start = 1, stop = 7)
tms$YearMonth <- substr(tms$Date, start = 1, stop = 7)

# monthly max, mean, and SD of nighttime air temperatures for 3 TOMST-TMS strata
tms_night_mean <- tms_night %>%
  group_by(Logger.new, YearMonth) %>%
  summarise(nightly_mean = mean(T3_air15), 
            nightly_sd = sd(T3_air15), 
            nightly_max = max(T3_air15), 
            nightly_mean_s = mean(T1_soil6),
            nightly_s_sd = sd(T1_soil6),
            nightly_max_s = max(T1_soil6),
            .groups = "keep")

# monthly max, mean, and SD of 24-h air temperatures for 3 TOMST-TMS strata
tms_mean <- tms %>%
  group_by(Logger.new, YearMonth) %>%
  summarise(daily_mean = mean(T3_air15), 
            daily_sd = sd(T3_air15),
            daily_max = max(T3_air15), 
            daily_mean_s = mean(T1_soil6),
            daily_s_sd = sd(T1_soil6),
            daily_max_s = max(T1_soil6),
            .groups = "keep")


hobo$YearMonth <- substr(hobo$Date, start = 1, stop = 7)
hobo$VPDair <- (610.7*10^((7.5*hobo$Tair)/(237.3+hobo$Tair)))/1000 # calculate VPD from HOBO data

# monthly min, mean, and SD of relative humidity, and monthly max, mean, and SD of VPD from HOBO data
hobo_mean <- hobo %>%
  group_by(Logger.new, YearMonth) %>%
  summarise(Tair_mean = mean(Tair, na.rm = T), 
            RH_mean = mean(RH, na.rm = T), 
            RH_min = min(RH, na.rm = T), 
            RH_sd = sd(RH, na.rm = T),
            VPD_mean = mean(VPDair, na.rm = T), 
            VPD_max = max(VPDair, na.rm = T), 
            VPD_sd = sd(VPDair, na.rm = T),
            .groups = "keep")



