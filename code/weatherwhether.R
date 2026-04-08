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
weather$Date <- as.Date(weather$Date, format= "%m/%d/%Y")

weather$monthina <- as.numeric (format(weather$Date, "%m"))
weather$yearina <- as.numeric (format(weather$Date, "%Y")) + 2000

#creating census years for each data collection period     
weather$CensusYearMay <- ifelse(weather$monthina >= 5, (weather$yearina + 1), weather$yearina)
weather$CensusYearJun <- ifelse(weather$monthina >= 6, (weather$yearina + 1), weather$yearina)
weather$CensusYearJul <- ifelse(weather$monthina >= 7, (weather$yearina + 1), weather$yearina)
weather$CensusYearSep <- ifelse(weather$monthina >= 9, (weather$yearina + 1), weather$yearina)

#creating three month census blocks
weather$Census_three_month_May <- (case_when(weather$monthina == 5 ~ "firstthreeback",
                                             weather$monthina == 4 ~ "firstthreeback",
                                             weather$monthina == 3 ~ "firstthreeback",
                                             weather$monthina == 2 ~ "secondthreeback",
                                             weather$monthina == 1 ~ "secondthreeback",
                                             weather$monthina == 12 ~ "secondthreeback",
                                             weather$monthina == 11 ~ "thirdthreeback",
                                             weather$monthina == 10 ~ "thirdthreeback",
                                             weather$monthina == 9 ~ "thirdthreeback",
                                             weather$monthina == 8 ~ "fourththreeback",
                                             weather$monthina == 7 ~ "fourththreeback",
                                             weather$monthina == 6 ~ "fourththreeback"))

weather$Census_three_month_Jun <- (case_when(weather$monthina == 6 ~ "firstthreeback",
                                             weather$monthina == 5 ~ "firstthreeback",
                                             weather$monthina == 4 ~ "firstthreeback",
                                             weather$monthina == 3 ~ "secondthreeback",
                                             weather$monthina == 2 ~ "secondthreeback",
                                             weather$monthina == 1 ~ "secondthreeback",
                                             weather$monthina == 12 ~ "thirdthreeback",
                                             weather$monthina == 11 ~ "thirdthreeback",
                                             weather$monthina == 10 ~ "thirdthreeback",
                                             weather$monthina == 9 ~ "fourththreeback",
                                             weather$monthina == 8 ~ "fourththreeback",
                                             weather$monthina == 7 ~ "fourththreeback"))

weather$Census_three_month_Jul <- (case_when(weather$monthina == 7 ~ "firstthreeback",
                                             weather$monthina == 6 ~ "firstthreeback",
                                             weather$monthina == 5 ~ "firstthreeback",
                                             weather$monthina == 4 ~ "secondthreeback",
                                             weather$monthina == 3 ~ "secondthreeback",
                                             weather$monthina == 2 ~ "secondthreeback",
                                             weather$monthina == 1 ~ "thirdthreeback",
                                             weather$monthina == 12 ~ "thirdthreeback",
                                             weather$monthina == 11 ~ "thirdthreeback",
                                             weather$monthina == 10 ~ "fourththreeback",
                                             weather$monthina == 9 ~ "fourththreeback",
                                             weather$monthina == 8 ~ "fourththreeback"))

weather$Census_three_month_Sep <- (case_when(weather$monthina == 9 ~ "firstthreeback",
                                             weather$monthina == 8 ~ "firstthreeback",
                                             weather$monthina == 7 ~ "firstthreeback",
                                             weather$monthina == 6 ~ "secondthreeback",
                                             weather$monthina == 5 ~ "secondthreeback",
                                             weather$monthina == 4 ~ "secondthreeback",
                                             weather$monthina == 3 ~ "thirdthreeback",
                                             weather$monthina == 2 ~ "thirdthreeback",
                                             weather$monthina == 1 ~ "thirdthreeback",
                                             weather$monthina == 12 ~ "fourththreeback",
                                             weather$monthina == 11 ~ "fourththreeback",
                                             weather$monthina == 10 ~ "fourththreeback"))

#creating three month census blocks
weather$Census_six_month_May <- (case_when(weather$monthina == 5 ~ "firstsixback",
                                           weather$monthina == 4 ~ "firstsixback",
                                           weather$monthina == 3 ~ "firstsixback",
                                           weather$monthina == 2 ~ "firstsixback",
                                           weather$monthina == 1 ~ "firstsixback",
                                           weather$monthina == 12 ~ "firstsixback",
                                           weather$monthina == 11 ~ "secondsixback",
                                           weather$monthina == 10 ~ "secondsixback",
                                           weather$monthina == 9 ~ "secondsixback",
                                           weather$monthina == 8 ~ "secondsixback",
                                           weather$monthina == 7 ~ "secondsixback",
                                           weather$monthina == 6 ~ "secondsixback"))

weather$Census_six_month_Jun <- (case_when(weather$monthina == 6 ~ "firstsixback",
                                           weather$monthina == 5 ~ "firstsixback",
                                           weather$monthina == 4 ~ "firstsixback",
                                           weather$monthina == 3 ~ "firstsixback",
                                           weather$monthina == 2 ~ "firstsixback",
                                           weather$monthina == 1 ~ "firstsixback",
                                           weather$monthina == 12 ~ "secondsixback",
                                           weather$monthina == 11 ~ "secondsixback",
                                           weather$monthina == 10 ~ "secondsixback",
                                           weather$monthina == 9 ~ "secondsixback",
                                           weather$monthina == 8 ~ "secondsixback",
                                           weather$monthina == 7 ~ "secondsixback"))

weather$Census_six_month_Jul <- (case_when(weather$monthina == 7 ~ "firstsixback",
                                           weather$monthina == 6 ~ "firstsixback",
                                           weather$monthina == 5 ~ "firstsixback",
                                           weather$monthina == 4 ~ "firstsixback",
                                           weather$monthina == 3 ~ "firstsixback",
                                           weather$monthina == 2 ~ "firstsixback",
                                           weather$monthina == 1 ~ "secondsixback",
                                           weather$monthina == 12 ~ "secondsixback",
                                           weather$monthina == 11 ~ "secondsixback",
                                           weather$monthina == 10 ~ "secondsixback",
                                           weather$monthina == 9 ~ "secondsixback",
                                           weather$monthina == 8 ~ "secondsixback"))

weather$Census_six_month_Sep <- (case_when(weather$monthina == 9 ~ "firstsixback",
                                           weather$monthina == 8 ~ "firstsixback",
                                           weather$monthina == 7 ~ "firstsixback",
                                           weather$monthina == 6 ~ "firstsixback",
                                           weather$monthina == 5 ~ "firstsixback",
                                           weather$monthina == 4 ~ "firstsixback",
                                           weather$monthina == 3 ~ "secondsixback",
                                           weather$monthina == 2 ~ "secondsixback",
                                           weather$monthina == 1 ~ "secondsixback",
                                           weather$monthina == 12 ~ "secondsixback",
                                           weather$monthina == 11 ~ "secondsixback",
                                           weather$monthina == 10 ~ "secondsixback"))

#generating summary stats for each census month

#generating summary stats for each month
weather <- weather %>%
  group_by(
    monthina) %>%
  mutate(monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#generating summary stats for each three month block
weather <- weather %>%
  group_by(
    Census_three_month_May) %>%
  mutate(May_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         May_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    Census_three_month_Jun) %>%
  mutate(Jun_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         Jun_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    Census_three_month_Jul) %>%
  mutate(Jul_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         Jul_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    Census_three_month_Sep) %>%
  mutate(Sep_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         Sep_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#generating summary stats for each six month block
weather <- weather %>%
  group_by(
    Census_six_month_May) %>%
  mutate(May_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         May_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    Census_six_month_Jun) %>%
  mutate(Jun_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         Jun_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    Census_six_month_Jul) %>%
  mutate(Jul_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         Jul_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    Census_six_month_Sep) %>%
  mutate(Sep_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         Sep_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#generating summary stats for each census year
weather <- weather %>%
  group_by(
    CensusYearMay) %>%
  mutate(CensusYearMay_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearMay_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    CensusYearJun) %>%
  mutate(CensusYearJun_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearJun_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    CensusYearJul) %>%
  mutate(CensusYearJul_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearJul_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather <- weather %>%
  group_by(
    CensusYearSep) %>%
  mutate(CensusYearSep_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearSep_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#making data frames with only necessary data for each census year
weatherMay <- weather %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
    -Date, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
    -CensusYearJun, -CensusYearJul, -CensusYearSep) %>% 
  distinct() 

weatherJun <- weather %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
         -Date, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
         -CensusYearMay, -CensusYearJul, -CensusYearSep) %>% 
  distinct() 

weatherJul <- weather %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
         -Date, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
         -CensusYearMay, -CensusYearJun, -CensusYearSep) %>% 
  distinct() 

weatherSep <- weather %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
         -Date, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
         -CensusYearJun, -CensusYearJul, -CensusYearMay) %>% 
  distinct() 

#Combining demographic data with the weather data
gras <- read.csv("data/ltreb_allspp_2007_2025.csv")

CombinedMay <- left_join(x=gras, y=weatherMay, by=c("year_t" = "CensusYearMay")) %>% 
  rowwise()

CombinedJun <- left_join(x=CombinedMay, y=weatherJun, by=c("year_t" = "CensusYearJun")) %>% 
  rowwise()

CombinedJul <- left_join(x=CombinedJun, y=weatherJul, by=c("year_t" = "CensusYearJul")) %>%  
  rowwise()
  
CombinedData <- left_join(x=CombinedJul, y=weatherSep, by=c("year_t" = "CensusYearSep")) %>%  
  rowwise()


CombinedData$ppt_tot <- as.numeric (case_when(gras$species == "AGPE" ~ CombinedData$CensusYearSep_ppt_tot,
                                              gras$species == "ELRI" ~ CombinedData$CensusYearJul_ppt_tot,
                                              gras$species == "ELVI" ~ CombinedData$CensusYearJul_ppt_tot,
                                              gras$species == "FESU" ~ CombinedData$CensusYearJun_ppt_tot,
                                              gras$species == "LOAR" ~ CombinedData$CensusYearJul_ppt_tot,
                                              gras$species == "POAL" ~ CombinedData$CensusYearMay_ppt_tot,
                                              gras$species == "POAU" ~ CombinedData$CensusYearMay_ppt_tot, #fake (I don't know the census date)
                                              gras$species == "POSY" ~ CombinedData$CensusYearMay_ppt_tot))

CombinedData$tmean_mean <- as.numeric (case_when(gras$species == "AGPE" ~ CombinedData$CensusYearSep_tmean_mean,
                                              gras$species == "ELRI" ~ CombinedData$CensusYearJul_tmean_mean,
                                              gras$species == "ELVI" ~ CombinedData$CensusYearJul_tmean_mean,
                                              gras$species == "FESU" ~ CombinedData$CensusYearJun_tmean_mean,
                                              gras$species == "LOAR" ~ CombinedData$CensusYearJul_tmean_mean, #fake
                                              gras$species == "POAL" ~ CombinedData$CensusYearMay_tmean_mean,
                                              gras$species == "POAU" ~ CombinedData$CensusYearMay_tmean_mean, #fake (I don't know the census date)
                                              gras$species == "POSY" ~ CombinedData$CensusYearMay_tmean_mean))


str(CombinedData)

CombinedDataRefined <- CombinedData %>% select(X, species, plot, endo_01, id, origin, original, birth,
                                               year_t, age, size_t, flw_count_t, mean_spike_t, 
                                               year_t1, surv_t1, size_t1, flw_count_t1, mean_spike_t1,
                                               dist_a, dist_b, ppt_tot, ppt_sd, tmean_mean, tmean_sd) 

write.csv(CombinedDataRefined, "data/CombinedDataRefined")
