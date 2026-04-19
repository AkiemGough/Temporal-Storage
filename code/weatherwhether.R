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

#generating summary stats for each three month block
weather_three_month_may <- weather %>%
  group_by(CensusYearMay,
    Census_three_month_May) %>%
  summarize(May_trimonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2),
                                          na.rm = TRUE)) 

weather_three_month_jul <- weather %>%
  group_by(CensusYearJul,
    Census_three_month_Jul) %>%
  summarize(Jul_trimonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2),
                                          na.rm = TRUE)) 

weather_three_month_sep <- weather %>%
  group_by(CensusYearSep,
    Census_three_month_Sep) %>%
  summarize(Sep_trimonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2),
                                          na.rm = TRUE)) 

#generating summary stats for each six month block
weather_six_month_may <- weather %>%
  group_by(CensusYearMay,
    Census_six_month_May) %>%
  summarize(May_hexamonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2),
                                           na.rm = TRUE)) 

weather_six_month_jul <- weather %>%
  group_by(CensusYearJul,
    Census_six_month_Jul) %>%
  summarize(Jul_hexamonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2),
                                           na.rm = TRUE)) 

weather_six_month_sep <- weather %>%
  group_by(CensusYearSep,
    Census_six_month_Sep) %>%
  summarize(Sep_hexamonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2),
                                           na.rm = TRUE)) 


#making data frames with only necessary data for each 3 years back from May
weatherMar_May <- weather_three_month_may %>% filter(Census_three_month_May == "firstthreeback") %>%
  rename(firstthreeback_vpdmean = May_trimonthly_vpdmean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherDec_Feb <- weather_three_month_may %>% filter(Census_three_month_May == "secondthreeback") %>%
  rename(secondthreeback_vpdmean = May_trimonthly_vpdmean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherSep_Nov <- weather_three_month_may %>% filter(Census_three_month_May == "thirdthreeback")%>%
  rename(thirdthreeback_vpdmean = May_trimonthly_vpdmean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherJun_Aug <- weather_three_month_may %>% filter(Census_three_month_May == "fourththreeback") %>%
  rename(fourththreeback_vpdmean = May_trimonthly_vpdmean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each 6 years back from May
weatherDec_May <- weather_six_month_may %>% filter(Census_six_month_May == "firstsixback") %>%
  rename(firstsixback_vpdmean = May_hexamonthly_vpdmean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

weatherJun_Nov <- weather_six_month_may %>% filter(Census_six_month_May == "secondsixback") %>%
  rename(secondsixback_vpdmean = May_hexamonthly_vpdmean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each 3 years back from Jul
weatherMay_Jul <- weather_three_month_jul %>% filter(Census_three_month_Jul == "firstthreeback") %>%
  rename(firstthreeback_vpdmean = Jul_trimonthly_vpdmean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherFeb_Apr <- weather_three_month_jul %>% filter(Census_three_month_Jul == "secondthreeback") %>%
  rename(secondthreeback_vpdmean = Jul_trimonthly_vpdmean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherNov_Feb <- weather_three_month_jul %>% filter(Census_three_month_Jul == "thirdthreeback") %>%
  rename(thirdthreeback_vpdmean = Jul_trimonthly_vpdmean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherAug_Oct <- weather_three_month_jul %>% filter(Census_three_month_Jul == "fourththreeback") %>%
  rename(fourththreeback_vpdmean = Jul_trimonthly_vpdmean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each 6 years back from Jul
weatherFeb_Jul <- weather_six_month_jul %>% filter(Census_six_month_Jul == "firstsixback") %>%
  rename(firstsixback_vpdmean = Jul_hexamonthly_vpdmean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherAug_Jan <- weather_six_month_jul %>% filter(Census_six_month_Jul == "secondsixback") %>%
  rename(secondsixback_vpdmean = Jul_hexamonthly_vpdmean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each 3 years back from Sep
weatherJul_Sep <- weather_three_month_sep %>% filter(Census_three_month_Sep == "firstthreeback") %>%
  rename(firstthreeback_vpdmean = Sep_trimonthly_vpdmean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherApr_Jun <- weather_three_month_sep %>% filter(Census_three_month_Sep == "secondthreeback") %>%
  rename(secondthreeback_vpdmean = Sep_trimonthly_vpdmean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherJan_Mar <- weather_three_month_sep %>% filter(Census_three_month_Sep == "thirdthreeback") %>%
  rename(thirdthreeback_vpdmean = Sep_trimonthly_vpdmean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherOct_Dec <- weather_three_month_sep %>% filter(Census_three_month_Sep == "fourththreeback") %>%
  rename(fourththreeback_vpdmean = Sep_trimonthly_vpdmean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

#making data frames with only necessary data for each 6 years back from Sep
weatherApr_Sep <- weather_six_month_sep %>% filter(Census_six_month_Sep == "firstsixback") %>%
  rename(firstsixback_vpdmean = Sep_hexamonthly_vpdmean,
         censusmonth = Census_six_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherOct_Mar <- weather_six_month_sep %>% filter(Census_six_month_Sep == "secondsixback") %>%
  rename(secondsixback_vpdmean = Sep_hexamonthly_vpdmean,
         censusmonth = Census_six_month_Sep) %>%
  mutate(censusmonth = "Sep")

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

#for Jul census
CombinedMay_Jul <- left_join(x=grasJulCensus, y=weatherMay_Jul, by=c("year_t" = "CensusYearJul"))
CombinedFeb_Apr <- left_join(x=grasJulCensus, y=weatherFeb_Apr, by=c("year_t" = "CensusYearJul"))
CombinedNov_Feb <- left_join(x=grasJulCensus, y=weatherNov_Feb, by=c("year_t" = "CensusYearJul"))
CombinedAug_Oct <- left_join(x=grasJulCensus, y=weatherAug_Oct, by=c("year_t" = "CensusYearJul"))

CombinedFeb_Jul <- left_join(x=grasJulCensus, y=weatherFeb_Jul, by=c("year_t" = "CensusYearJul"))
CombinedAug_Jan <- left_join(x=grasJulCensus, y=weatherAug_Jan, by=c("year_t" = "CensusYearJul"))

#for Sep census
CombinedJul_Sep <- left_join(x=grasSepCensus, y=weatherJul_Sep, by=c("year_t" = "CensusYearSep"))
CombinedApr_Jun <- left_join(x=grasSepCensus, y=weatherApr_Jun, by=c("year_t" = "CensusYearSep"))
CombinedJan_Mar <- left_join(x=grasSepCensus, y=weatherJan_Mar, by=c("year_t" = "CensusYearSep"))
CombinedOct_Dec <- left_join(x=grasSepCensus, y=weatherOct_Dec, by=c("year_t" = "CensusYearSep"))

CombinedApr_Sep <- left_join(x=grasSepCensus, y=weatherApr_Sep, by=c("year_t" = "CensusYearSep"))
CombinedOct_Mar <- left_join(x=grasSepCensus, y=weatherOct_Mar, by=c("year_t" = "CensusYearSep"))


CombinedDataSegments <- bind_rows(CombinedMar_May,CombinedDec_Feb,CombinedSep_Nov,CombinedJun_Aug,
                              CombinedDec_May,CombinedJun_Nov,
                              CombinedMay_Jul,CombinedFeb_Apr,CombinedNov_Feb,CombinedAug_Oct,
                              CombinedFeb_Jul,CombinedAug_Jan,
                              CombinedJul_Sep,CombinedApr_Jun,CombinedJan_Mar,CombinedOct_Dec,
                              CombinedApr_Sep,CombinedOct_Mar, 
                              .id="year_t")


write.csv(CombinedDataSegments, "data/CombinedDataSegments")
