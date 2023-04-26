
## Organize Boiling River climate data (Oct 2021 - Oct 2022) and calculate summary climate metrics, including mean annual temperature (MAT) and maximum temperature of the warmest month (MTWM)

## Prepared by: Alyssa T. Kullberg
## Last updated: April 26, 2023
## R version 4.2.2


## Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(ggridges)
library(cowplot)


## Load HOBO data and TMS data (explained in associated README)
hobo <- read.csv("../Data/Made_in_R/hobo_data_clean.csv") # Tair at ~1m, RH
tms <- read.csv("../Data/Made_in_R/tms_data_clean.csv") # Tair at 15cm and 2cm, Tsoil at 6cm


## Give TOMST/TMS loggers simpler names according to manuscript
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


## Give HOBO loggers simpler names according to manuscript
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


## Prepare datasets
tms$Date <- as.Date(tms$Date, "%Y-%m-%d")
tms$Time <- as.POSIXct(tms$time, format = "%m/%d/%Y %H:%M")
tms$Time <- format(tms$Time, "%H:%M")
tms$Hour <- as.numeric(substr(tms$Time, start=1, stop=2))
tms$Min <- as.numeric(substr(tms$Time, start=4, stop=5))
tms$HourMin <- as.numeric(paste0(tms$Hour, ".", tms$Min))
tms$Month <- as.numeric(substr(tms$Date, start=6, stop=7))

hobo$Date <- as.Date(hobo$Date, "%m/%d/%Y")
hobo$Time <- as.POSIXct(hobo$date_time, format = "%m/%d/%Y %H:%M")
hobo$Time <- format(hobo$Time, "%H:%M")
hobo$Hour <- as.numeric(substr(hobo$Time, start=1, stop=2))
hobo$Min <- as.numeric(substr(hobo$Time, start=4, stop=5))
hobo$HourMin <- as.numeric(paste0(hobo$Hour, ".", hobo$Min))
hobo$Month <- as.numeric(substr(hobo$Date, start=6, stop=7))


## Summarize temperature data from HOBOs (Tair 1m) and TOMST/TMS (Tair 15cm, Tair 2cm, Tsoil 6cm), 
hobo.ids <- unique(hobo$Logger.ID) # extract unique HOBO IDs
tms.ids <- unique(tms$Logger.ID) # extract unique TOMST/TMS IDs

hobo.clim <- as.data.frame(hobo.ids) # convert HOBO IDs to a dataframe
names(hobo.clim) <- "Logger.ID"

tms.clim15 <- as.data.frame(tms.ids) # convert TOMST/TMS IDs to a dataframe (for 15 cm aboveground)
names(tms.clim15) <- "Logger.ID"

tms.clim2 <- tms.clim15 # create dataframe for TOMST/TMS (2 cm aboveground)

tms.clim6 <- tms.clim15 # create dataframe for TOMST/TMS (6 cm belowground)



#######
### Climate metric key:
#######
## Mean annual daytime temperature (MAT_day)
# 6am to 6pm average temperature over 1 year

## Mean annual temperature (MAT)
# mean temperature recorded over 1 year

## Maximum temperature of the warmest month (MTWM)
# maximum temperature recorded during the hottest month on average across all loggers of each height

## Mean annual nighttime temperature (MAT_night)
# 6pm to 6am average temperature over 1 year

## First, subset the temperature logger datasets to just daytime or just nighttime for MAT_day and MAT_night
hobo_6to6 <- subset(hobo, HourMin >= 6 & HourMin <= 18)
tms_6to6 <- subset(tms, HourMin >= 6 & HourMin <= 18)
tms_night <- subset(tms, HourMin >= 18 | HourMin <= 6)


## Calculate MAT and MAT_day for HOBO loggers
# MAT_day = MAT = rep(NA, length(hobo.ids))
# 
# for(i in 1:length(hobo.ids)){
#   id.sub <- subset(hobo, Logger.ID == hobo.ids[[i]])
#   id.sub_6to6 <- subset(hobo_6to6, Logger.ID == hobo.ids[[i]])
#   
#   if(length(unique(id.sub$Date)) > 274) { # don't calculate metrics if HOBO worked for <3/4 of a year
#     MAT[i] = mean(na.omit(id.sub$Tair))
#     MAT_day[i] = mean(na.omit(id.sub_6to6$Tair))
#     
#   }else(MAT_day[i] = MAT[i] = NA)
# }
# 
# hobo.clim$MAT <- MAT
# hobo.clim$MAT_day <- MAT_day


## Calcualte MAT, MAT_day, and MAT_night for TOMST/TMS loggers at 15 cm, 2 cm, and -6 cm
MAT_day15 = MAT_day2 = MAT_day6 = MAT15 = MAT2 = MAT6 = MAT_night15 = MAT_night2 = MAT_night6 = MAT15_sd = MAT2_sd = MAT6_sd = MAT_day15_sd = MAT_day2_sd = MAT_day6_sd = MAT_night15_sd = MAT_night2_sd = MAT_night6_sd = rep(NA, length(tms.ids))

for(i in 1:length(tms.ids)){
  id.sub <- subset(tms, Logger.ID == tms.ids[[i]])
  id.sub_6to6 <- subset(tms_6to6, Logger.ID == tms.ids[[i]])
  id.sub_night <- subset(tms_night, Logger.ID == tms.ids[[i]])
  
  if(length(unique(id.sub$Date)) > 274) { # don't calculate mean annual temp. if TOMST/TMS worked for <3/4 of a year
    MAT15[i] = mean(na.omit(id.sub$T3_air15))
    MAT15_sd[i] = sd(na.omit(id.sub$T3_air15))
    MAT2[i] = mean(na.omit(id.sub$T2_air2))
    MAT2_sd[i] = sd(na.omit(id.sub$T2_air2))
    MAT6[i] = mean(na.omit(id.sub$T1_soil6))
    MAT6_sd[i] = sd(na.omit(id.sub$T1_air6))
    
    MAT_day15[i] = mean(na.omit(id.sub_6to6$T3_air15))
    MAT_day15_sd[i] = sd(na.omit(id.sub_6to6$T3_air15))
    MAT_day2[i] = mean(na.omit(id.sub_6to6$T2_air2))
    MAT_day2_sd[i] = sd(na.omit(id.sub_6to6$T2_air2))
    MAT_day6[i] = mean(na.omit(id.sub_6to6$T1_soil6))
    MAT_day6_sd[i] = sd(na.omit(id.sub_6to6$T1_air6))
    
    MAT_night15[i] = mean(na.omit(id.sub_night$T3_air15))
    MAT_night15_sd[i] = sd(na.omit(id.sub_night$T3_air15))
    MAT_night2[i] = mean(na.omit(id.sub_night$T2_air2))
    MAT_night2_sd[i] = sd(na.omit(id.sub_night$T2_air2))
    MAT_night6[i] = mean(na.omit(id.sub_night$T1_soil6))
    MAT_night6_sd[i] = sd(na.omit(id.sub_night$T1_soil6))
    
  }else(MAT_day15[i] = MAT_day2[i] = MAT_day6[i] = MAT15[i] = MAT2[i] = MAT6[i] = MAT_night15[i] = MAT_night2[i] = MAT_night6[i] = MAT15_sd[i] = MAT2_sd[i] = MAT6_sd[i] = MAT_day15_sd[i] = MAT_day2_sd[i] = MAT_day6_sd[i] = MAT_night15_sd[i] = MAT_night2_sd[i] = MAT_night6_sd[i] = NA)
}


## Fill climate metric data frames according to results from above for-loops
tms.clim15$MAT <- MAT15
tms.clim2$MAT <- MAT2
tms.clim6$MAT <- MAT6

tms.clim15$MAT_sd <- MAT15_sd
tms.clim2$MAT_sd <- MAT2_sd
tms.clim6$MAT_sd <- MAT6_sd

tms.clim15$MAT_day <- MAT_day15
tms.clim2$MAT_day <- MAT_day2
tms.clim6$MAT_day <- MAT_day6

tms.clim15$MAT_day_sd <- MAT_day15_sd
tms.clim2$MAT_day_sd <- MAT_day2_sd
tms.clim6$MAT_day_sd <- MAT_day6_sd

tms.clim15$MAT_night <- MAT_night15
tms.clim2$MAT_night <- MAT_night2
tms.clim6$MAT_night <- MAT_night6

tms.clim15$MAT_night_sd <- MAT_night15_sd
tms.clim2$MAT_night_sd <- MAT_night2_sd
tms.clim6$MAT_night_sd <- MAT_night6_sd


## Calculate maximum temperature of the warmest month (Tmax) for each sensor type
## First, determine the hottest month for each logger type
# monthly.mean <- hobo %>%
#   group_by(Month) %>%
#   summarize(monthly.mean = mean(na.omit(Tair)))
# # HOBOs: September (26.4 C)

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


## Calculate Tmax for HOBOs
# hobo.sub <- subset(hobo, Month == 9)
# Tmax <- hobo.sub %>%
#   group_by(Logger.ID) %>%
#   summarize(Tmax = max(na.omit(Tair)))
# hobo.clim <- left_join(hobo.clim, Tmax, by="Logger.ID")


## Calculate Tmax for TOMST/TMS at 15cm, 2cm, -6cm
tms.sub <- subset(tms, Month == 9)
Tmax <- tms.sub %>%
  group_by(Logger.ID) %>%
  summarize(Tmax = max(na.omit(T3_air15)))
tms.clim15 <- left_join(tms.clim15, Tmax, by="Logger.ID")


## Add back in some of the metadata about each logger
# hobo.clim$Latitude <- unique(hobo$Latitude)
# hobo.clim$Longitude <- unique(hobo$Longitude)
# hobo.clim$Logger.new <- unique(hobo$Logger.new)

tms.meta.unique <- unique(select(tms, Logger.ID, Logger.new, Latitude, Longitude)) # make a dataframe with the lat, long, and new logger ID for each unique logger

tms.clim15 <- left_join(tms.clim15, tms.meta.unique, by = "Logger.ID")

tms.clim2 <- left_join(tms.clim2, tms.meta.unique, by = "Logger.ID")

tms.clim6 <- left_join(tms.clim6, tms.meta.unique, by = "Logger.ID")


## Save tms.clim15 to a .csv file to use for modelling MAT and MTWM across the Boiling River forest in temp_model_clean.R
# write.csv(tms.clim15, "../Data/Made_in_R/tms_clim15_clean.csv")


## Create new dataframes summarized by month for each logger for time-series climate figures
tms_night$YearMonth <- substr(tms_night$Date, start = 1, stop = 7)
tms_6to6$YearMonth <- substr(tms_6to6$Date, start = 1, stop = 7)

tms_night_mean <- tms_night %>%
  group_by(Logger.new, YearMonth) %>%
  summarise(nightly_mean = mean(T3_air15), 
            nightly_sd = sd(T3_air15), 
            nightly_max = max(T3_air15), 
            nightly_mean_s = mean(T1_soil6),
            nightly_s_sd = sd(T1_soil6),
            nightly_max_s = max(T1_soil6),
            .groups = "keep")

tms_day_mean <- tms_6to6 %>%
  group_by(Logger.new, YearMonth) %>%
  summarise(daily_mean = mean(T3_air15), 
            daily_sd = sd(T3_air15),
            daily_max = max(T3_air15), 
            daily_mean_s = mean(T1_soil6),
            daily_s_sd = sd(T1_soil6),
            daily_max_s = max(T1_soil6),
            .groups = "keep")

hobo_6to6$YearMonth <- substr(hobo_6to6$Date, start = 1, stop = 7)
hobo_6to6$VPDair <- (610.7*10^((7.5*hobo_6to6$Tair)/(237.3+hobo_6to6$Tair)))/1000
hobo_6to6$heatindex <- -8.78469475556 + (1.61139411*hobo_6to6$Tair) + (2.33854883889*hobo_6to6$RH) - 0.14611605*(hobo_6to6$Tair * hobo_6to6$RH) - 0.012308094*(hobo_6to6$Tair^2) - 0.0164248277778*(hobo_6to6$RH^2) + 0.002211732*(hobo_6to6$Tair^2 * hobo_6to6$RH) + 0.00072546*(hobo_6to6$Tair * hobo_6to6$RH^2) - 0.000003582*(hobo_6to6$Tair^2 * hobo_6to6$RH^2)

hobo_6to6_mean <- hobo_6to6 %>%
  group_by(Logger.new, YearMonth) %>%
  summarise(Tair_mean = mean(Tair, na.rm = T), 
            RH_mean = mean(RH, na.rm = T), 
            RH_min = min(RH, na.rm = T), 
            RH_sd = sd(RH, na.rm = T),
            VPD_mean = mean(VPDair, na.rm = T), 
            VPD_max = max(VPDair, na.rm = T), 
            VPD_sd = sd(VPDair, na.rm = T),
            .groups = "keep")


## Load external forest temperature datasets to compare MAT and MTWM of these forests with MAT and MTWM of Boiling River sites

## Biosphere 2
bio2 <- read.csv("../Data/HottestRainforest/b2tf_V1.csv") # load data
bio2 <- subset(bio2, Tair.C. > -9999) # remove NAs etc.
bio2_6to6 <- subset(bio2, hour >= 6 & hour <= 18) # keep only daytime temperatures
bio2MAT <- mean(bio2_6to6$Tair.C.) # calculate mean annual daytime temperature
bio2MAT_sd <- sd(bio2_6to6$Tair.C.) # calculate sd of annual daytime temperature
bio2MAT # 30.14

bio2_monthly <- bio2_6to6 %>% # determine the warmest month
  group_by(month) %>%
  summarise(monthly.mean = mean(Tair.C.))
# month 6 is the hottest on average during the day...
bio2_june <- subset(bio2_6to6, month == 6) # subset for the hottest month
bio2_june_yearly <- bio2_june %>% # calculate MTWM (Tmax)
  group_by(year) %>%
  summarise(yearly.max = max(Tair.C.))
bio2MTWM <- max(bio2_june_yearly$yearly.max) # maximum is 48.9


## Kilometer 34
k34 <- read.table("../Data/HottestRainforest/K34_CfluxBF.txt", sep = "\t", skip = 1) # load data
k34.names <- colnames(k34)[2:211]
k34.names[211] <- NA
colnames(k34) <- k34.names
k34 <- subset(k34, degK > -9999) # remove NAs etc.
k34_6to6 <- subset(k34, HR >= 6 & HR <= 18) # keep only daytime temperatures
k34MAT <- mean(k34_6to6$degK) - 273.15  # calculate mean annual daytime temperature
k34MAT_sd <- sd(k34_6to6$degK)  # calculate sd of annual daytime temperature
k34MAT # 26.84

k34_6to6$month <- rep(NA, nrow(k34_6to6)) # create an column for month so we can figure out the warmest month
k34_reg <- subset(k34_6to6, YYYY != 2004) # the following lines are to assign the correct month based on julian day and year
k34_leap <- subset(k34_6to6, YYYY == 2004)

k34_reg[k34_reg$JD >=1 & k34_reg$JD < 31,]$month <- 1
k34_reg[k34_reg$JD >=32 & k34_reg$JD <= 59,]$month <- 2
k34_reg[k34_reg$JD >=60 & k34_reg$JD <= 90,]$month <- 3
k34_reg[k34_reg$JD >=91 & k34_reg$JD <= 120,]$month <- 4
k34_reg[k34_reg$JD >=121 & k34_reg$JD <= 151,]$month <- 5
k34_reg[k34_reg$JD >=152 & k34_reg$JD <= 181,]$month <- 6
k34_reg[k34_reg$JD >=182 & k34_reg$JD <= 212,]$month <- 7
k34_reg[k34_reg$JD >=213 & k34_reg$JD <= 243,]$month <- 8
k34_reg[k34_reg$JD >=244 & k34_reg$JD <= 273,]$month <- 9
k34_reg[k34_reg$JD >=274 & k34_reg$JD <= 304,]$month <- 10
k34_reg[k34_reg$JD >=305 & k34_reg$JD <= 334,]$month <- 11
k34_reg[k34_reg$JD >=335 & k34_reg$JD <= 365,]$month <- 12

k34_leap[k34_leap$JD >=1 & k34_leap$JD < 31,]$month <- 1
k34_leap[k34_leap$JD >=32 & k34_leap$JD <= 60,]$month <- 2
k34_leap[k34_leap$JD >=61 & k34_leap$JD <= 91,]$month <- 3
k34_leap[k34_leap$JD >=92 & k34_leap$JD <= 121,]$month <- 4
k34_leap[k34_leap$JD >=122 & k34_leap$JD <= 152,]$month <- 5
k34_leap[k34_leap$JD >=153 & k34_leap$JD <= 182,]$month <- 6
k34_leap[k34_leap$JD >=183 & k34_leap$JD <= 213,]$month <- 7
k34_leap[k34_leap$JD >=214 & k34_leap$JD <= 244,]$month <- 8
k34_leap[k34_leap$JD >=245 & k34_leap$JD <= 274,]$month <- 9
k34_leap[k34_leap$JD >=275 & k34_leap$JD <= 305,]$month <- 10
k34_leap[k34_leap$JD >=306 & k34_leap$JD <= 335,]$month <- 11
k34_leap[k34_leap$JD >=336 & k34_leap$JD <= 366,]$month <- 12

k34_6to6 <- rbind(k34_reg, k34_leap) # recombine data with month attached
k34_sub <- select(k34_6to6, degK, month)
k34_monthly <- k34_sub %>% # determine the hottest month
  group_by(month) %>%
  summarise(monthly.mean = mean(degK))
as.data.frame(k34_monthly) # month 10 is hottest on average during day
k34MTWM <- max(subset(k34_6to6, month == 10)$degK) - 273.15 # calculate MTWM (Tmax)
k34MTWM # 34.275


## Kilometer 67
k67 <- read.table("../Data/HottestRainforest/K67_CfluxBF.txt", sep = "\t", skip = 1) # load data
k67.names <- colnames(k67)[2:211]
k67.names[211] <- NA
colnames(k67) <- k67.names
k67 <- subset(k67, degK > -9999)
k67_6to6 <- subset(k67, HR >= 6 & HR <= 18) # keep only daytime temperatures
k67MAT <- mean(k67_6to6$degK) - 273.15  # calculate mean annual daytime temperature
k67MAT_sd <- sd(k67_6to6$degK)  # calculate sd of annual daytime temperature
k67MAT # 26.35

k67_6to6$month <- rep(NA, nrow(k67_6to6)) # create an column for month so we can figure out the warmest month
k67_reg <- subset(k67_6to6, YYYY != 2004) # the following lines are to assign the correct month based on julian day and year
k67_leap <- subset(k67_6to6, YYYY == 2004)

k67_reg[k67_reg$JD >=1 & k67_reg$JD < 31,]$month <- 1
k67_reg[k67_reg$JD >=32 & k67_reg$JD <= 59,]$month <- 2
k67_reg[k67_reg$JD >=60 & k67_reg$JD <= 90,]$month <- 3
k67_reg[k67_reg$JD >=91 & k67_reg$JD <= 120,]$month <- 4
k67_reg[k67_reg$JD >=121 & k67_reg$JD <= 151,]$month <- 5
k67_reg[k67_reg$JD >=152 & k67_reg$JD <= 181,]$month <- 6
k67_reg[k67_reg$JD >=182 & k67_reg$JD <= 212,]$month <- 7
k67_reg[k67_reg$JD >=213 & k67_reg$JD <= 243,]$month <- 8
k67_reg[k67_reg$JD >=244 & k67_reg$JD <= 273,]$month <- 9
k67_reg[k67_reg$JD >=274 & k67_reg$JD <= 304,]$month <- 10
k67_reg[k67_reg$JD >=305 & k67_reg$JD <= 334,]$month <- 11
k67_reg[k67_reg$JD >=335 & k67_reg$JD <= 365,]$month <- 12

k67_leap[k67_leap$JD >=1 & k67_leap$JD < 31,]$month <- 1
k67_leap[k67_leap$JD >=32 & k67_leap$JD <= 60,]$month <- 2
k67_leap[k67_leap$JD >=61 & k67_leap$JD <= 91,]$month <- 3
k67_leap[k67_leap$JD >=92 & k67_leap$JD <= 121,]$month <- 4
k67_leap[k67_leap$JD >=122 & k67_leap$JD <= 152,]$month <- 5
k67_leap[k67_leap$JD >=153 & k67_leap$JD <= 182,]$month <- 6
k67_leap[k67_leap$JD >=183 & k67_leap$JD <= 213,]$month <- 7
k67_leap[k67_leap$JD >=214 & k67_leap$JD <= 244,]$month <- 8
k67_leap[k67_leap$JD >=245 & k67_leap$JD <= 274,]$month <- 9
k67_leap[k67_leap$JD >=275 & k67_leap$JD <= 305,]$month <- 10
k67_leap[k67_leap$JD >=306 & k67_leap$JD <= 335,]$month <- 11
k67_leap[k67_leap$JD >=336 & k67_leap$JD <= 366,]$month <- 12

k67_6to6 <- rbind(k67_reg, k67_leap) # recombine data with month attached
k67_sub <- select(k67_6to6, degK, month)
k67_monthly <- k67_sub %>% # determine the hottest month
  group_by(month) %>%
  summarise(monthly.mean = mean(degK))
as.data.frame(k67_monthly) # month 9 is hottest on average during day
k67MTWM <- max(subset(k67_6to6, month == 9)$degK) - 273.15 # calculate MTWM (Tmax)
k67MTWM # 31.494


## Kilometer 83
k83 <- read.table("../Data/HottestRainforest/K83_CfluxBF.txt", sep = "\t", skip = 1) # load data
k83.names <- colnames(k83)[2:211]
k83.names[211] <- NA
colnames(k83) <- k83.names
k83 <- subset(k83, degK > -9999)
k83_6to6 <- subset(k83, HR >= 6 & HR <= 18) # keep only daytime temperatures
k83MAT <- mean(k83_6to6$degK) - 273.15  # calculate mean annual daytime temperature
k83MAT_sd <- sd(k83_6to6$degK)  # calculate sd of annual daytime temperature
k83MAT # 26.85

k83_6to6$month <- rep(NA, nrow(k83_6to6)) # create an column for month so we can figure out the warmest month
k83_reg <- k83_6to6 # the following lines are to assign the correct month based on julian day and year

k83_reg[k83_reg$JD >=1 & k83_reg$JD < 31,]$month <- 1
k83_reg[k83_reg$JD >=32 & k83_reg$JD <= 59,]$month <- 2
k83_reg[k83_reg$JD >=60 & k83_reg$JD <= 90,]$month <- 3
k83_reg[k83_reg$JD >=91 & k83_reg$JD <= 120,]$month <- 4
k83_reg[k83_reg$JD >=121 & k83_reg$JD <= 151,]$month <- 5
k83_reg[k83_reg$JD >=152 & k83_reg$JD <= 181,]$month <- 6
k83_reg[k83_reg$JD >=182 & k83_reg$JD <= 212,]$month <- 7
k83_reg[k83_reg$JD >=213 & k83_reg$JD <= 243,]$month <- 8
k83_reg[k83_reg$JD >=244 & k83_reg$JD <= 273,]$month <- 9
k83_reg[k83_reg$JD >=274 & k83_reg$JD <= 304,]$month <- 10
k83_reg[k83_reg$JD >=305 & k83_reg$JD <= 334,]$month <- 11
k83_reg[k83_reg$JD >=335 & k83_reg$JD <= 365,]$month <- 12

k83_6to6 <- k83_reg
k83_sub <- select(k83_6to6, degK, month)
k83_monthly <- k83_sub %>% # determine the hottest month
  group_by(month) %>%
  summarise(monthly.mean = mean(degK))
as.data.frame(k83_monthly) # month 10 is hottest on average during day
k83MTWM <- max(subset(k83_6to6, month == 10)$degK) - 273.15  # calculate MTWM (Tmax)
k83MTWM # 33.28701


## Nova Xavantina
nx <- read.csv("../Data/HottestRainforest/nova xavantina 2015-19.csv") # load data
nx_6to6 <- subset(nx, Hora != 0) # subset for daytime temperatures only
nxMAT <- mean(nx_6to6$Temp, na.rm = T) # calculate mean daytime temperature
nxMAT_sd <- sd(nx_6to6$Temp, na.rm = T) # calculate sd of daytime temperature
nxMAT # 28.99

nx_6to6$month <- month(as.POSIXct(nx_6to6$Date, format = "%d/%m/%Y")) # create a column for month

nx_monthly <- nx_6to6 %>% # determine the hottest month
  group_by(month) %>%
  summarise(monthly.mean = mean(Temp, na.rm=T))
as.data.frame(nx_monthly) # month 9 is hottest on average during day
nxMTWM <- max(subset(nx_6to6, month == 9)$Temp, na.rm=T) # calculate MTWM (Tmax)
nxMTWM # 41.20



###########################################################################################################
###########################################################################################################



##########
## FIGURES
##########



##### 1. DOTPLOT


## Prepare and combine data for dotplots
tms.clim15 <- as.data.frame(tms.clim15)
tms.clim15[14,] <- c("Nova Xavantina", NA, NA, nxMAT, nxMAT_sd, NA, NA, nxMTWM, "Nova Xavantina", NA, NA)
tms.clim15[15,] <- c("Biosphere-2", NA, NA, bio2MAT, bio2MAT_sd, NA, NA, bio2MTWM, "Biosphere-2", NA, NA)
tms.clim15[16,] <- c("K34", NA, NA, k34MAT, k34MAT_sd, NA, NA, k34MTWM, "K34", NA, NA)
tms.clim15[17,] <- c("K67", NA, NA, k67MAT, k67MAT_sd, NA, NA, k67MTWM, "K67", NA, NA)
tms.clim15[18,] <- c("K83", NA, NA, k83MAT, k83MAT_sd, NA, NA, k83MTWM, "K83", NA, NA)


## Prepare plot aesthetics
my.colours <- c(
  "#99000d", #56
           "#cb181d", #63
           "#ef3b2c", #52
           "#fb6a4a", #53
           "#fc9272", #65
           "#fcbba1", #62
           "#d95f0e", #58
           "#fe9929", #64
           "#54278f", #59
           "#756bb1", #60
           "#9e9ac8", #61
           "#3182bd", #54
           "#6baed6" #55
)
my.legend.breaks <- c(
  "TMS_1",
  "TMS_2",
  "TMS_3",
  "TMS_4",
  "TMS_5",
  "TMS_6",
  "TMS_7",
  "TMS_8",
  "TMS_9",
  "TMS_10",
  "TMS_11",
  "TMS_12",
  "TMS_13")

my.colours3 <- c(rev(my.colours), "gray35", "gray35", "gray35", "gray35", "gray35")
my.legend.breaks3 <- c(rev(my.legend.breaks), "Nova Xavantina", "K34", "K67", "K83", "Biosphere-2")

## Create dotplot figure showing MAT and MTWM for each TOMST at Boiling River and for other forest sites
summary.dotplot <- ggplot(data = tms.clim15, aes(x = Logger.new)) +
  geom_errorbar(aes(ymin = as.numeric(MAT_day) - as.numeric(MAT_day_sd), ymax = as.numeric(MAT_day) + as.numeric(MAT_day_sd), colour = Logger.new), width = 0.4) +
  geom_point(aes(y = as.numeric(MAT_day), fill = Logger.new), pch = 21, size = 4) +
  geom_point(aes(y = as.numeric(Tmax), fill = Logger.new), pch = 24, size = 3) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1, size = 14),
        axis.title.y = element_text(size = 16),
        panel.border = element_rect(fill = NA)) +
  xlab("") +
  ylab(bquote("Temperature (°C)")) +
  scale_fill_manual(breaks = my.legend.breaks3, 
                    values = my.colours3) +
  scale_colour_manual(breaks = my.legend.breaks3, 
                    values = my.colours3) +
  scale_x_discrete(limits = my.legend.breaks3) +
  scale_y_continuous(breaks = c(25,30,35,40,45,50)) +
  geom_point(aes(x = 1.7, y = 46), pch = 21, fill = "gray90", size = 4) +
  geom_point(aes(x = 1.7, y = 48), pch = 24, fill = "gray90", size = 3) +
  annotate("text", label = "=  MAT", x = 2, y = 46, hjust = 0, size = 16/.pt) +
  annotate("text", label = "=  MTWM", x = 2, y = 48, hjust = 0, size = 16/.pt)
summary.dotplot
# save as climate_metrics_MAT_MTWM_summary_clean.pdf, 5x7 but landscape





##### 2. DENSITY PLOTS


## Prepare the data
tms_2 <- select(tms_6to6, Logger.new, T3_air15)
colnames(tms_2) <- c("Logger.new", "Tair")

k83_6to6$Logger.new <- rep("K83", nrow(k83_6to6))
k83_6to6$Tair <- k83_6to6$degK - 273.15
k83_2 <- select(k83_6to6, Logger.new, Tair)

k67_6to6$Logger.new <- rep("K67", nrow(k67_6to6))
k67_6to6$Tair <- k67_6to6$degK - 273.15
k67_2 <- select(k67_6to6, Logger.new, Tair)

k34_6to6$Logger.new <- rep("K34", nrow(k34_6to6))
k34_6to6$Tair <- k34_6to6$degK - 273.15
k34_2 <- select(k34_6to6, Logger.new, Tair)

nx_6to6$Logger.new <- rep("Nova Xavantina", nrow(nx_6to6))
nx_2 <- select(nx_6to6, Logger.new, Temp)
colnames(nx_2) <- c("Logger.new", "Tair")

bio2_6to6$Logger.new <- rep("Biosphere-2", nrow(bio2_6to6))
bio2_6to6$Tair <- bio2_6to6$Tair.C
bio_2 <- select(bio2_6to6, Logger.new, Tair)

temp.agg <- do.call(rbind, list(nx_2, bio_2, k34_2, k67_2, k83_2, tms_2))

temp.agg2 <- temp.agg %>%
  mutate(Logger.new = fct_relevel(Logger.new, 
                                 "Biosphere-2", "K83", "K67", "K34","Nova Xavantina", 
                                 my.legend.breaks
  ))


## Prepare plot aesthetics
my.colours4 <- c(rev(my.colours), "gray45", "gray45", "gray45", "gray45", "gray45")
my.legend.breaks4 <- c(rev(my.legend.breaks), "K34", "K67", "K83", "Nova Xavantina", "Biosphere-2")


## Create density plot figure showing temperature regime for each TOMST at Boiling River and for other forest sites
density.plot <- ggplot() +
  stat_density_ridges(data = temp.agg2,
                      aes(x = Tair,
                          y = Logger.new,
                          colour = Logger.new,
                          fill = Logger.new),
                      alpha = 0.2,
                      size = 0.8,
                      rel_min_height = 0.01,
                      quantile_lines = TRUE, quantiles = 2,
                      scale = 1) +
  theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_colour_manual("Logger ID",
                      breaks = my.legend.breaks4, 
                      values = my.colours4) +
  scale_fill_manual("Logger ID",
                    breaks = my.legend.breaks4, 
                    values = my.colours4) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank()) +
  ylab("") +
  xlab(bquote("T"[air]~"(°C)")) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))
density.plot
# save as climate_metrics_density_clean.pdf, 8x6 (portrait)





##### 3. TIME-SERIES CLIMATE PLOTS



##### 3A. TIME-SERIES AIR TEMPERATURE PLOTS
p1 <- ggplot() +
  geom_ribbon(data = tms_night_mean, aes(x = factor(YearMonth), ymax = nightly_mean+nightly_sd, ymin = nightly_mean-nightly_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = tms_night_mean, aes(x = factor(YearMonth), y = nightly_mean, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  theme_classic() +
  ylim(18, 43) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly nighttime T "[mean]~"(°C)")) +
  scale_colour_manual(values = my.colours,
                      breaks = my.legend.breaks) +
  scale_fill_manual(values = my.colours,
                    breaks = my.legend.breaks) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

p2 <- ggplot(data = tms_night_mean, aes(x = YearMonth, y = nightly_max, colour = Logger.new, group = Logger.new)) +
  geom_line(alpha = 0.7, size = .9) +
  ylim(18, 43) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly nighttime T "[max]~"(°C)")) +
  scale_colour_manual(values = my.colours,
                      breaks = my.legend.breaks) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

p3 <- ggplot() +
  geom_ribbon(data = tms_day_mean, aes(x = YearMonth, ymax = daily_mean+daily_sd, ymin = daily_mean-daily_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = tms_day_mean, aes(x = YearMonth, y = daily_mean, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  theme_classic() +
  ylim(18, 43) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly daytime T "[mean]~"(°C)")) +
  scale_colour_manual(values = my.colours,
                      breaks = my.legend.breaks) +
  scale_fill_manual(values = my.colours,
                    breaks = my.legend.breaks) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

p4 <- ggplot(data = tms_day_mean, aes(x = YearMonth, y = daily_max, colour = Logger.new, group = Logger.new)) +
  geom_line(alpha = 0.7, size = .9) +
  ylim(18, 43) +
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly daytime T "[max]~"(°C)")) +
  scale_colour_manual(values = my.colours,
                      breaks = my.legend.breaks) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

# dummy plot for legend
p5 <- ggplot() +
  geom_ribbon(data = tms_night_mean, aes(x = YearMonth, ymin = nightly_mean-nightly_sd, ymax = nightly_mean+nightly_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = tms_night_mean, aes(x = YearMonth, y = nightly_mean, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  ylim(18, 43) +
  theme_classic() +
  scale_colour_manual("Logger ID",
                      breaks = my.legend.breaks, 
                      values = my.colours) +
  scale_fill_manual("Logger ID",
                    breaks = my.legend.breaks, 
                    values = my.colours) +
  theme(legend.title = element_text(face = "bold"))
leg1 <- get_legend(p5)

# Plot TMS (15cm) monthly daytime mean, monthly daytime max, monthly nighttime mean, and monthly nighttime max where each line is a logger
plots_Tair <- cowplot::plot_grid(p3, p4, leg1,
                                 p1, p2, NULL,
                                 ncol = 3,
                                 nrow = 2,
                                 rel_widths = c(1,1,.3),
                                 labels = c("a", "b", "",
                                            "c", "d", ""))
plots_Tair
# save as climate_metrics_Tair_clean.pdf, 8x10 (landscape)




##### 3B. TIME-SERIES SOIL TEMPERATURE PLOTS

p1_s <- ggplot() +
  geom_ribbon(data = tms_night_mean, aes(x = YearMonth, ymin = nightly_mean_s-nightly_s_sd, ymax = nightly_mean_s+nightly_s_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = tms_night_mean, aes(x = factor(YearMonth), y = nightly_mean_s, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  theme_classic() +
  ylim(18, 43) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly nighttime soil T "[mean]~"(°C)")) +
  scale_colour_manual(values = my.colours,
                      breaks = my.legend.breaks) +
  scale_fill_manual("Logger ID",
                    breaks = my.legend.breaks, 
                    values = my.colours) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

p2_s <- ggplot() +
  geom_line(data = tms_night_mean, aes(x = YearMonth, y = nightly_max_s, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  ylim(18, 43) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly nighttime soil T "[max]~"(°C)")) +
  scale_colour_manual(values = my.colours,
                      breaks = my.legend.breaks) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

p3_s <- ggplot() +
  geom_ribbon(data = tms_day_mean, aes(x = YearMonth, ymin = daily_mean_s-daily_s_sd, ymax = daily_mean_s+daily_s_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = tms_day_mean, aes(x = YearMonth, y = daily_mean_s, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  theme_classic() +
  ylim(18, 43) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly daytime soil T "[mean]~"(°C)")) +
  scale_colour_manual(values = my.colours,
                      breaks = my.legend.breaks) +
  scale_fill_manual("Logger ID",
                    breaks = my.legend.breaks, 
                    values = my.colours) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

p4_s <- ggplot() +
  geom_line(data = tms_day_mean, aes(x = YearMonth, y = daily_max_s, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  ylim(18, 43) +
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly daytime soil T "[max]~"(°C)")) +
  scale_colour_manual(values = my.colours,
                      breaks = my.legend.breaks) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

# dummy plot for legend
p5_s <- ggplot() +
  geom_ribbon(data = tms_day_mean, aes(x = YearMonth, ymin = daily_mean_s-daily_s_sd, ymax = daily_mean_s+daily_s_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = tms_day_mean, aes(x = YearMonth, y = daily_mean_s, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  ylim(18, 43) +
  theme_classic() +
  scale_colour_manual("Logger ID",
                      breaks = my.legend.breaks, 
                      values = my.colours) +
  scale_fill_manual("Logger ID",
                    breaks = my.legend.breaks, 
                    values = my.colours) +
  theme(legend.title = element_text(face = "bold"))
leg1_s <- get_legend(p5_s)

# Plot TMS (6 cm belowground) monthly daytime mean, monthly daytime max, monthly nighttime mean, and monthly nighttime max where each line is a logger
plots_soil <- plot_grid(p3_s, p4_s, leg1_s,
                        p1_s, p2_s, NULL,
                        ncol = 3,
                        nrow = 2,
                        rel_widths = c(1,1,.3),
                        labels = c("a", "b", "",
                                   "c", "d", ""))
plots_soil
# save as climate_metrics_soil_clean.pdf, 8x10




##### 3C. TIME-SERIES RELATIVE HUMIDITY AND VPD PLOTS
my.colours2 <- c(
  #  "#99000d", #56 - mayantuyacu
  "#cb181d", #63
           "#ef3b2c", #52
           "#fb6a4a", #53
           "#fc9272", #65
           #  "#fcbba1", #62 - thermal glade
           "#d95f0e", #58
           "#fe9929", #64
           #  "#54278f", #59 - orange tree
           "#756bb1", #60
           "#9e9ac8", #61
           "#3182bd" #54
           #  "#6baed6" #55
)
my.legend.breaks2 <- c(
  "HOBO_2", #sumiruna
  "HOBO_3", #bubble pool
  "HOBO_4", #rock hop
  "HOBO_5", #escalera de raices
  "HOBO_7", #bubble pool across
  "HOBO_8", #bubble pool up
  "HOBO_10", #duende cave
  "HOBO_11", #siren hole
  "HOBO_12" #hilltop1
)


p1_v <- ggplot() +
  geom_ribbon(data = hobo_6to6_mean, aes(x = YearMonth, ymin = VPD_mean-VPD_sd, ymax = VPD_mean+VPD_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = hobo_6to6_mean, aes(x = factor(YearMonth), y = VPD_mean, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  theme_classic() +
  ylim(2.5, 6.5) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly mean VPD "[air]~"(kPa)")) +
  scale_colour_manual(values = my.colours2,
                      breaks = my.legend.breaks2) +
  scale_fill_manual(breaks = my.legend.breaks2, 
                    values = my.colours2) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))


p2_v <- ggplot() +
  geom_line(data = hobo_6to6_mean, aes(x = factor(YearMonth), y = VPD_max, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  theme_classic() +
  ylim(2.5, 6.5) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly max. VPD "[air]~"(kPa)")) +
  scale_colour_manual(values = my.colours2,
                      breaks = my.legend.breaks2) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

p1_r <- ggplot() +
  geom_ribbon(data = hobo_6to6_mean, aes(x = YearMonth, ymin = RH_mean-RH_sd, ymax = RH_mean+RH_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = hobo_6to6_mean, aes(x = factor(YearMonth), y = RH_mean, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  theme_classic() +
  ylim(40, 105) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly mean relative humidity (%)")) +
  scale_colour_manual(values = my.colours2,
                      breaks = my.legend.breaks2) +
  scale_fill_manual(breaks = my.legend.breaks2, 
                    values = my.colours2) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))

p2_r <- ggplot() +
  geom_line(data = hobo_6to6_mean, aes(x = factor(YearMonth), y = RH_min, colour = Logger.new, group = Logger.new), alpha = 0.7, size = .9) +
  theme_classic() +
  ylim(40, 105) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.border = element_rect(fill = NA)) +
  xlab("Month") +
  ylab(bquote("Monthly min. relative humidity (%)")) +
  scale_colour_manual(values = my.colours2,
                      breaks = my.legend.breaks2) +
  scale_x_discrete(labels = c("O", "N", "D", "J", "F", "M", "A", "M", 
                              "J", "J", "A", "S", "O"))


# dummy plot for legend
p5_rv <- ggplot() +
  geom_ribbon(data = hobo_6to6_mean, aes(x = YearMonth, ymin = RH_mean-RH_sd, ymax = RH_mean+RH_sd, fill = Logger.new, group = Logger.new), alpha = 0.1) +
  geom_line(data = hobo_6to6_mean, aes(x = YearMonth, y = RH_mean, colour = Logger.new, group = Logger.new), alpha = 0.7, size = 0.9) +
  theme_classic() +
  scale_colour_manual("Logger ID",
                      breaks = my.legend.breaks2, 
                      values = my.colours2) +
  scale_fill_manual("Logger ID",
                    breaks = my.legend.breaks2, 
                    values = my.colours2) +
  theme(legend.title = element_text(face = "bold"))
leg1_rv <- get_legend(p5_rv)


plots_rh_vpd <- plot_grid(p1_r, p2_r, leg1_rv,
                          p1_v, p2_v, NULL,
                          ncol = 3,
                          nrow = 2,
                          rel_widths = c(1,1,.3),
                          labels = c("a", "b", "",
                                     "c", "d", ""))
plots_rh_vpd
# save as climate_metrics_rh_vpd_clean.pdf, 8x10
