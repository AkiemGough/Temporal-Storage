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

#creating six month census blocks
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

#generating summary stats for each month
weather <- weather %>%
  group_by(
    monthina) %>%
  mutate(monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#generating summary stats for each three month block
weather_three_month_may <- weather %>%
  group_by(yearina,
    Census_three_month_May) %>%
  summarize(May_trimonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         May_trimonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_three_month_jun <- weather %>%
  group_by(yearina,
    Census_three_month_Jun) %>%
  summarize(Jun_trimonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Jun_trimonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_three_month_jul <- weather %>%
  group_by(yearina,
    Census_three_month_Jul) %>%
  summarize(Jul_trimonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Jul_trimonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_three_month_sep <- weather %>%
  group_by(yearina,
    Census_three_month_Sep) %>%
  summarize(Sep_trimonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Sep_trimonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

#generating summary stats for each six month block
weather_six_month_may <- weather %>%
  group_by(yearina,
    Census_six_month_May) %>%
  summarize(May_hexamonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         May_hexamonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_six_month_jun <- weather %>%
  group_by(yearina,
    Census_six_month_Jun) %>%
  summarize(Jun_hexamonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Jun_hexamonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_six_month_jul <- weather %>%
  group_by(yearina,
    Census_six_month_Jul) %>%
  summarize(Jul_hexamonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Jul_hexamonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_six_month_sep <- weather %>%
  group_by(yearina,
    Census_six_month_Sep) %>%
  summarize(Sep_hexamonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Sep_hexamonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

#generating summary stats for each census year
weatherMay <- weather %>%
  group_by(
    CensusYearMay) %>%
  summarize(CensusYearMay_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearMay_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherJun <- weather %>%
  group_by(
    CensusYearJun) %>%
  summarize(CensusYearJun_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearJun_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherJul <- weather %>%
  group_by(
    CensusYearJul) %>%
  summarize(CensusYearJul_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearJul_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherSep <- weather %>%
  group_by(
    CensusYearSep) %>%
  summarize(CensusYearSep_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
         CensusYearSep_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#making data frames with only necessary data for each census year
weatherMay <- weatherMay %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
    -Date, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
    -CensusYearJun, -CensusYearJul, -CensusYearSep) %>% 
  distinct() 

weatherJun <- weatherJun %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
         -Date, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
         -CensusYearMay, -CensusYearJul, -CensusYearSep) %>% 
  distinct() 

weatherJul <- weatherJul %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
         -Date, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
         -CensusYearMay, -CensusYearJun, -CensusYearSep) %>% 
  distinct() 

weatherSep <- weatherSep %>% 
  select(-`tmax (degrees F)`,-`tdmean (degrees F)`,-`vpdmin (hPa)`,-`vpdmax (hPa)`,-`tmin (degrees F)`,
         -Date, -yearina, -`ppt (inches)`,-`tmean (degrees F)`,
         -CensusYearJun, -CensusYearJul, -CensusYearMay) %>% 
  distinct() 


#making data frames with only necessary data for each 3 years back from May
weatherMar_May <- weather_three_month_may %>% filter(Census_three_month_May == "firstthreeback")
      
weatherDec_Feb <- weather_three_month_may %>% filter(Census_three_month_May == "secondthreeback") 

weatherSep_Nov <- weather_three_month_may %>% filter(Census_three_month_May == "thirdthreeback")
   
weatherJun_Aug <- weather_three_month_may %>% filter(Census_three_month_May == "fourththreeback") 

#making data frames with only necessary data for each 6 years back from May
weatherDec_May <- weather_six_month_may %>% filter(Census_six_month_May == "firstsixback") 

weatherJun_Nov <- weather_six_month_may %>% filter(Census_six_month_May == "secondsixback") 

#making data frames with only necessary data for each 3 years back from Jun
weather_J_Apr_Jun <- weather_three_month_jun %>% filter(Census_three_month_Jun == "firstthreeback")

weather_J_Jan_Mar <- weather_three_month_jun %>% filter(Census_three_month_Jun == "secondthreeback") 

weather_J_Oct_Dec <- weather_three_month_jun %>% filter(Census_three_month_Jun == "thirdthreeback") 

weather_J_Jul_Sep <- weather_three_month_jun %>% filter(Census_three_month_Jun == "fourththreeback") 

#making data frames with only necessary data for each 6 years back from Jun
weather_J_Jan_Jun <- weather_six_month_jun %>% filter(Census_six_month_Jun == "firstsixback") 

weather_J_Jul_Dec <- weather_six_month_jun %>% filter(Census_six_month_Jun == "secondsixback") 

#making data frames with only necessary data for each 3 years back from Jul
weatherMay_Jul <- weather_three_month_jul %>% filter(Census_three_month_Jul == "firstthreeback")

weatherFeb_Apr <- weather_three_month_jul %>% filter(Census_three_month_Jul == "secondthreeback")

weatherNov_Feb <- weather_three_month_jul %>% filter(Census_three_month_Jul == "thirdthreeback") 

weatherAug_Oct <- weather_three_month_jul %>% filter(Census_three_month_Jul == "fourththreeback") 

#making data frames with only necessary data for each 6 years back from Jul
weatherFeb_Jul <- weather_six_month_jul %>% filter(Census_six_month_Jul == "firstsixback") 

weatherAug_Jan <- weather_six_month_jul %>% filter(Census_six_month_Jul == "secondsixback")

#making data frames with only necessary data for each 3 years back from Sep
weather_S_Jul_Sep <- weather_three_month_sep %>% filter(Census_three_month_Sep == "firstthreeback") 

weather_S_Apr_Jun <- weather_three_month_sep %>% filter(Census_three_month_Sep == "secondthreeback") 

weather_S_Jan_Mar <- weather_three_month_sep %>% filter(Census_three_month_Sep == "thirdthreeback") 

weather_S_Oct_Dec <- weather_three_month_sep %>% filter(Census_three_month_Sep == "fourththreeback")

#making data frames with only necessary data for each 6 years back from Sep
weather_S_Apr_Sep <- weather_six_month_sep %>% filter(Census_six_month_Sep == "firstsixback") 

weather_S_Oct_Mar <- weather_six_month_sep %>% filter(Census_six_month_Sep == "secondsixback") 


#Combining demographic data with the weather data
gras <- read.csv("data/ltreb_allspp_2007_2025.csv")

CombinedMay <- left_join(x=gras, y=weatherMay, by=c("year_t" = "CensusYearMay"))

CombinedJun <- left_join(x=CombinedMay, y=weatherJun, by=c("year_t" = "CensusYearJun")) 

CombinedJul <- left_join(x=CombinedJun, y=weatherJul, by=c("year_t" = "CensusYearJul")) 
  
CombinedData <- left_join(x=CombinedJul, y=weatherSep, by=c("year_t" = "CensusYearSep")) 


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
