library(devtools)
library(remotes)
library(prism)
library(dplyr)
library(tidyr)
#library(terra) #AKIEM - doesn't seem to work or use raster
library(lubridate)
library(tidyverse)
library(dplyr)
library(lme4)
library(AICcmodavg)
library(scales)
library(DHARMa)
library(car)

#reading in downloaded weather data
weather <- read_csv("data/weather_data.csv")

#extracting month and year from date
str(weather)
weather$Date <- as.Date(weather$Date, format= "%m/%d/%Y")

weather$monthina <- as.numeric (format(weather$Date, "%m"))
weather$yearina <- as.numeric (format(weather$Date, "%Y")) + 2000

#creating census years for each data collection period     
weather$CensusYearMay <- ifelse(weather$monthina >= 5, (weather$yearina + 1), weather$yearina)
weather$CensusYearJul <- ifelse(weather$monthina >= 7, (weather$yearina + 1), weather$yearina)
weather$CensusYearSep <- ifelse(weather$monthina >= 9, (weather$yearina + 1), weather$yearina)

#generating summary stats for each census year
weatherMay <- weather %>%
  group_by(
    CensusYearMay) %>%
  mutate(CensusYearMay_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearMay_ppt_sd  = sd(`ppt (inches)`, na.rm = TRUE),
         CensusYearMay_tmean_mean = sum(`tmean (degrees F)`, na.rm = TRUE),
         CensusYearMay_tmean_sd  = sd(`tmean (degrees F)`, na.rm = TRUE))

weatherJul <- weather %>%
  group_by(
    CensusYearJul) %>%
  mutate(CensusYearJul_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearJul_ppt_sd  = sd(`ppt (inches)`, na.rm = TRUE),
         CensusYearJul_tmean_mean = sum(`tmean (degrees F)`, na.rm = TRUE),
         CensusYearJul_tmean_sd  = sd(`tmean (degrees F)`, na.rm = TRUE))

weatherSep <- weather %>%
  group_by(
    CensusYearSep) %>%
  mutate(CensusYearSep_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearSep_ppt_sd  = sd(`ppt (inches)`, na.rm = TRUE),
         CensusYearSep_tmean_mean = sum(`tmean (degrees F)`, na.rm = TRUE),
         CensusYearSep_tmean_sd  = sd(`tmean (degrees F)`, na.rm = TRUE))

#making data frames with only necessary data for each census year
weatherMay <- weatherMay %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
    -Date, -monthina, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
    -CensusYearJul, -CensusYearSep) %>% 
  distinct() 

weatherJul <- weatherJul %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
         -Date, -monthina, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
         -CensusYearMay, -CensusYearSep) %>% 
  distinct() 

weatherSep <- weatherSep %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
         -Date, -monthina, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
         -CensusYearJul, -CensusYearMay) %>% 
  distinct() 

gras <- read.csv("data/ltreb_allspp_2007_2025.csv")

CombinedMay <- left_join(x=gras, y=weatherMay, by=c("year_t" = "CensusYearMay")) %>% 
  rowwise()

CombinedJul <- left_join(x=CombinedMay, y=weatherJul, by=c("year_t" = "CensusYearJul")) %>%  
  rowwise()
  
CombinedData <- left_join(x=CombinedJul, y=weatherSep, by=c("year_t" = "CensusYearSep")) %>%  
  rowwise()


CombinedData$ppt_tot <- as.numeric (case_when(gras$species == "AGPE" ~ CombinedData$CensusYearSep_ppt_tot,
                                              gras$species == "ELRI" ~ CombinedData$CensusYearJul_ppt_tot,
                                              gras$species == "ELVI" ~ CombinedData$CensusYearJul_ppt_tot,
                                              gras$species == "FESU" ~ CombinedData$CensusYearMay_ppt_tot,
                                              gras$species == "LOAR" ~ CombinedData$CensusYearMay_ppt_tot, #fake
                                              gras$species == "POAL" ~ CombinedData$CensusYearMay_ppt_tot,
                                              gras$species == "POAU" ~ CombinedData$CensusYearMay_ppt_tot, #fake
                                              gras$species == "POSY" ~ CombinedData$CensusYearMay_ppt_tot))

CombinedData$ppt_sd <- as.numeric (case_when(gras$species == "AGPE" ~ CombinedData$CensusYearSep_ppt_sd,
                                              gras$species == "ELRI" ~ CombinedData$CensusYearJul_ppt_sd,
                                              gras$species == "ELVI" ~ CombinedData$CensusYearJul_ppt_sd,
                                              gras$species == "FESU" ~ CombinedData$CensusYearMay_ppt_sd,
                                              gras$species == "LOAR" ~ CombinedData$CensusYearMay_ppt_sd, #fake
                                              gras$species == "POAL" ~ CombinedData$CensusYearMay_ppt_sd,
                                              gras$species == "POAU" ~ CombinedData$CensusYearMay_ppt_sd, #fake
                                              gras$species == "POSY" ~ CombinedData$CensusYearMay_ppt_sd))

CombinedData$tmean_mean <- as.numeric (case_when(gras$species == "AGPE" ~ CombinedData$CensusYearSep_tmean_mean,
                                              gras$species == "ELRI" ~ CombinedData$CensusYearJul_tmean_mean,
                                              gras$species == "ELVI" ~ CombinedData$CensusYearJul_tmean_mean,
                                              gras$species == "FESU" ~ CombinedData$CensusYearMay_tmean_mean,
                                              gras$species == "LOAR" ~ CombinedData$CensusYearMay_tmean_mean, #fake
                                              gras$species == "POAL" ~ CombinedData$CensusYearMay_tmean_mean,
                                              gras$species == "POAU" ~ CombinedData$CensusYearMay_tmean_mean, #fake
                                              gras$species == "POSY" ~ CombinedData$CensusYearMay_tmean_mean))

CombinedData$tmean_sd <- as.numeric (case_when(gras$species == "AGPE" ~ CombinedData$CensusYearSep_tmean_sd,
                                             gras$species == "ELRI" ~ CombinedData$CensusYearJul_tmean_sd,
                                             gras$species == "ELVI" ~ CombinedData$CensusYearJul_tmean_sd,
                                             gras$species == "FESU" ~ CombinedData$CensusYearMay_tmean_sd,
                                             gras$species == "LOAR" ~ CombinedData$CensusYearMay_tmean_sd, #fake
                                             gras$species == "POAL" ~ CombinedData$CensusYearMay_tmean_sd,
                                             gras$species == "POAU" ~ CombinedData$CensusYearMay_tmean_sd, #fake
                                             gras$species == "POSY" ~ CombinedData$CensusYearMay_tmean_sd))


str(CombinedData)

CombinedDataRefined <- CombinedData %>% select(X, species, plot, endo_01, id, origin, original, birth,
                                               year_t, age, size_t, flw_count_t, mean_spike_t, 
                                               year_t1, surv_t1, size_t1, flw_count_t1, mean_spike_t1,
                                               dist_a, dist_b, ppt_tot, ppt_sd, tmean_mean, tmean_sd) 

