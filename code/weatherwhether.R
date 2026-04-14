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
  group_by(CensusYearMay,
    Census_three_month_May) %>%
  summarize(May_trimonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         May_trimonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_three_month_jul <- weather %>%
  group_by(CensusYearJul,
    Census_three_month_Jul) %>%
  summarize(Jul_trimonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Jul_trimonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_three_month_sep <- weather %>%
  group_by(CensusYearSep,
    Census_three_month_Sep) %>%
  summarize(Sep_trimonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Sep_trimonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

#generating summary stats for each six month block
weather_six_month_may <- weather %>%
  group_by(CensusYearMay,
    Census_six_month_May) %>%
  summarize(May_hexamonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         May_hexamonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_six_month_jul <- weather %>%
  group_by(CensusYearJul,
    Census_six_month_Jul) %>%
  summarize(Jul_hexamonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Jul_hexamonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))

weather_six_month_sep <- weather %>%
  group_by(CensusYearSep,
    Census_six_month_Sep) %>%
  summarize(Sep_hexamonthly_vpdmax = sum(`vpdmax (hPa)`, na.rm = TRUE),
         Sep_hexamonthly_vpdmin = mean(`vpdmin (hPa)`, na.rm = TRUE))


#making data frames with only necessary data for each 3 years back from May
weatherMar_May <- weather_three_month_may %>% filter(Census_three_month_May == "firstthreeback") %>%
  rename(firstthreeback_vpdmax = May_trimonthly_vpdmax, firstthreeback_vpdmin = May_trimonthly_vpdmin)

weatherDec_Feb <- weather_three_month_may %>% filter(Census_three_month_May == "secondthreeback") %>%
  rename(secondthreeback_vpdmax = May_trimonthly_vpdmax, secondthreeback_vpdmin = May_trimonthly_vpdmin)

weatherSep_Nov <- weather_three_month_may %>% filter(Census_three_month_May == "thirdthreeback")%>%
  rename(thirdthreeback_vpdmax = May_trimonthly_vpdmax, thirdthreeback_vpdmin = May_trimonthly_vpdmin)

weatherJun_Aug <- weather_three_month_may %>% filter(Census_three_month_May == "fourththreeback") %>%
  rename(fourththreeback_vpdmax = May_trimonthly_vpdmax, fourththreeback_vpdmin = May_trimonthly_vpdmin)

#making data frames with only necessary data for each 6 years back from May
weatherDec_May <- weather_six_month_may %>% filter(Census_six_month_May == "firstsixback") %>%
  rename(firstsixback_vpdmax = May_hexamonthly_vpdmax, firstsixback_vpdmin = May_hexamonthly_vpdmin)
weatherJun_Nov <- weather_six_month_may %>% filter(Census_six_month_May == "secondsixback") %>%
  rename(secondsixback_vpdmax = May_hexamonthly_vpdmax, secondsixback_vpdmin = May_hexamonthly_vpdmin)

#making data frames with only necessary data for each 3 years back from Jul
weatherMay_Jul <- weather_three_month_jul %>% filter(Census_three_month_Jul == "firstthreeback")
weatherFeb_Apr <- weather_three_month_jul %>% filter(Census_three_month_Jul == "secondthreeback")
weatherNov_Feb <- weather_three_month_jul %>% filter(Census_three_month_Jul == "thirdthreeback") 
weatherAug_Oct <- weather_three_month_jul %>% filter(Census_three_month_Jul == "fourththreeback") 

#making data frames with only necessary data for each 6 years back from Jul
weatherFeb_Jul <- weather_six_month_jul %>% filter(Census_six_month_Jul == "firstsixback") 
weatherAug_Jan <- weather_six_month_jul %>% filter(Census_six_month_Jul == "secondsixback")

#making data frames with only necessary data for each 3 years back from Sep
weatherJul_Sep <- weather_three_month_sep %>% filter(Census_three_month_Sep == "firstthreeback") 
weatherApr_Jun <- weather_three_month_sep %>% filter(Census_three_month_Sep == "secondthreeback") 
weatherJan_Mar <- weather_three_month_sep %>% filter(Census_three_month_Sep == "thirdthreeback") 
weatherOct_Dec <- weather_three_month_sep %>% filter(Census_three_month_Sep == "fourththreeback")

#making data frames with only necessary data for each 6 years back from Sep
weatherApr_Sep <- weather_six_month_sep %>% filter(Census_six_month_Sep == "firstsixback") 
weatherOct_Mar <- weather_six_month_sep %>% filter(Census_six_month_Sep == "secondsixback") 

#Combining demographic data with the weather data
gras <- read.csv("data/ltreb_allspp_2007_2025.csv")

#Grouping species with census timing
grasMayCensus <- gras %>% filter(species == c("FESU","POAL","POAU","POSY"))
grasJulCensus <- gras %>% filter(species == c("ELVI","ELRI","LOAR"))
grasSepCensus <- gras %>% filter(species == "AGPE")

#Combining corresponding weather and demographic data frames
#for May census
CombinedMar_May <- left_join(x=grasMayCensus, y=weatherMar_May, by=c("year_t" = "CensusYearMay"))
CombinedDec_Feb <- left_join(x=grasMayCensus, y=weatherDec_Feb, by=c("year_t" = "CensusYearMay"))
CombinedSep_Nov <- left_join(x=grasMayCensus, y=weatherSep_Nov, by=c("year_t" = "CensusYearMay"))
CombinedJun_Aug <- left_join(x=grasMayCensus, y=weatherJun_Aug, by=c("year_t" = "CensusYearMay"))

CombinedDec_May <- left_join(x=grasMayCensus, y=weatherDec_May, by=c("year_t" = "CensusYearMay"))
CombinedJun_Nov <- left_join(x=grasMayCensus, y=weatherJun_Nov, by=c("year_t" = "CensusYearMay"))

CombinedMayThree <- bind_rows(CombinedMar_May,CombinedDec_Feb,CombinedSep_Nov,CombinedJun_Aug,
                          .id="year_t")
CombinedMaySix <- bind_rows(CombinedDec_May,CombinedJun_Nov,
                          .id="year_t")

#for Jul census
CombinedMay_Jul <- left_join(x=grasJulCensus, y=weatherMay_Jul, by=c("year_t" = "CensusYearJul"))
CombinedFeb_Apr <- left_join(x=grasJulCensus, y=weatherFeb_Apr, by=c("year_t" = "CensusYearJul"))
CombinedNov_Feb <- left_join(x=grasJulCensus, y=weatherNov_Feb, by=c("year_t" = "CensusYearJul"))
CombinedAug_Oct <- left_join(x=grasJulCensus, y=weatherAug_Oct, by=c("year_t" = "CensusYearJul"))

CombinedFeb_Jul <- left_join(x=grasJulCensus, y=weatherFeb_Jul, by=c("year_t" = "CensusYearJul"))
CombinedAug_Jan <- left_join(x=grasJulCensus, y=weatherAug_Jan, by=c("year_t" = "CensusYearJul"))

CombinedJulThree <- bind_rows(CombinedMay_Jul,CombinedFeb_Apr,CombinedNov_Feb,CombinedAug_Oct,
                          .id="year_t")
CombinedJulSix <- bind_rows(CombinedFeb_Jul,CombinedAug_Jan,
                          .id="year_t")

#for Sep census
CombinedJul_Sep <- left_join(x=grasSepCensus, y=weatherJul_Sep, by=c("year_t" = "CensusYearSep"))
CombinedApr_Jun <- left_join(x=grasSepCensus, y=weatherApr_Jun, by=c("year_t" = "CensusYearSep"))
CombinedJan_Mar <- left_join(x=grasSepCensus, y=weatherJan_Mar, by=c("year_t" = "CensusYearSep"))
CombinedOct_Dec <- left_join(x=grasSepCensus, y=weatherOct_Dec, by=c("year_t" = "CensusYearSep"))

CombinedApr_Sep <- left_join(x=grasSepCensus, y=weatherApr_Sep, by=c("year_t" = "CensusYearSep"))
CombinedOct_Mar <- left_join(x=grasSepCensus, y=weatherOct_Mar, by=c("year_t" = "CensusYearSep"))

CombinedSepThree <- bind_rows(CombinedJul_Sep,CombinedApr_Jun,CombinedJan_Mar,CombinedOct_Dec,
                          .id="year_t")
CombinedSepSix <- bind_rows(CombinedApr_Sep,CombinedOct_Mar, 
                          .id="year_t")



CombinedData$tmean_mean <- as.numeric (case_when(gras$species == "AGPE" ~ CombinedData$CensusYearSep_tmean_mean,
                                              gras$species == "ELRI" ~ CombinedData$CensusYearJul_tmean_mean,
                                              gras$species == "ELVI" ~ CombinedData$CensusYearJul_tmean_mean,
                                              gras$species == "FESU" ~ CombinedData$CensusYearJun_tmean_mean,
                                              gras$species == "LOAR" ~ CombinedData$CensusYearJul_tmean_mean, 
                                              gras$species == "POAL" ~ CombinedData$CensusYearMay_tmean_mean,
                                              gras$species == "POAU" ~ CombinedData$CensusYearMay_tmean_mean, 
                                              gras$species == "POSY" ~ CombinedData$CensusYearMay_tmean_mean))


str(CombinedData)

CombinedDataRefined <- CombinedData %>% select(X, species, plot, endo_01, id, origin, original, birth,
                                               year_t, age, size_t, flw_count_t, mean_spike_t, 
                                               year_t1, surv_t1, size_t1, flw_count_t1, mean_spike_t1,
                                               dist_a, dist_b, ppt_tot, ppt_sd, tmean_mean, tmean_sd) 

write.csv(CombinedDataRefined, "data/CombinedDataRefined")
