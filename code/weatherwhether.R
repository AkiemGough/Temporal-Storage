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
weather$CensusYearMay <- ifelse(weather$monthina > 5, (weather$yearina + 1), weather$yearina)
weather$CensusYearJul <- ifelse(weather$monthina > 7, (weather$yearina + 1), weather$yearina)
weather$CensusYearSep <- ifelse(weather$monthina > 9, (weather$yearina + 1), weather$yearina)

#creating one month census blocks
weather$Census_one_month_May <- (case_when(weather$monthina == 5 ~ "firstoneback",
                                             weather$monthina == 4 ~ "secondoneback",
                                             weather$monthina == 3 ~ "thirdoneback",
                                             weather$monthina == 2 ~ "fourthoneback",
                                             weather$monthina == 1 ~ "fifthoneback",
                                             weather$monthina == 12 ~ "sixthoneback",
                                             weather$monthina == 11 ~ "seventhoneback",
                                             weather$monthina == 10 ~ "eighthoneback",
                                             weather$monthina == 9 ~ "ninthoneback",
                                             weather$monthina == 8 ~ "tenthoneback",
                                             weather$monthina == 7 ~ "eleventhoneback",
                                             weather$monthina == 6 ~ "twelvthoneback"))

weather$Census_one_month_Jul <- (case_when(weather$monthina == 7 ~ "firstoneback",
                                           weather$monthina == 6 ~ "secondoneback",
                                           weather$monthina == 5 ~ "thirdoneback",
                                           weather$monthina == 4 ~ "fourthoneback",
                                           weather$monthina == 3 ~ "fifthoneback",
                                           weather$monthina == 2 ~ "sixthoneback",
                                           weather$monthina == 1 ~ "seventhoneback",
                                           weather$monthina == 12 ~ "eighthoneback",
                                           weather$monthina == 11 ~ "ninthoneback",
                                           weather$monthina == 10 ~ "tenthoneback",
                                           weather$monthina == 9 ~ "eleventhoneback",
                                           weather$monthina == 8 ~ "twelvthoneback"))

weather$Census_one_month_Sep <- (case_when(weather$monthina == 9 ~ "firstoneback",
                                           weather$monthina == 8 ~ "secondoneback",
                                           weather$monthina == 7 ~ "thirdoneback",
                                           weather$monthina == 6 ~ "fourthoneback",
                                           weather$monthina == 5 ~ "fifthoneback",
                                           weather$monthina == 4 ~ "sixthoneback",
                                           weather$monthina == 3 ~ "seventhoneback",
                                           weather$monthina == 2 ~ "eighthoneback",
                                           weather$monthina == 1 ~ "ninthoneback",
                                           weather$monthina == 12 ~ "tenthoneback",
                                           weather$monthina == 11 ~ "eleventhoneback",
                                           weather$monthina == 10 ~ "twelvthoneback"))

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

#generating summary stats for each one month block
weather_one_month_may <- weather %>%
  group_by(CensusYearMay,
           Census_one_month_May) %>%
  summarize(May_monthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            May_monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather_one_month_jul <- weather %>%
  group_by(CensusYearJul,
           Census_one_month_Jul) %>%
  summarize(Jul_monthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            Jul_monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Jul_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather_one_month_sep <- weather %>%
  group_by(CensusYearSep,
           Census_one_month_Sep) %>%
  summarize(Sep_monthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            Sep_monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Sep_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#generating summary stats for each three month block
weather_three_month_may <- weather %>%
  group_by(CensusYearMay,
    Census_three_month_May) %>%
  summarize(May_trimonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            May_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather_three_month_jul <- weather %>%
  group_by(CensusYearJul,
    Census_three_month_Jul) %>%
  summarize(Jul_trimonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            Jul_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Jul_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather_three_month_sep <- weather %>%
  group_by(CensusYearSep,
    Census_three_month_Sep) %>%
  summarize(Sep_trimonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            Sep_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Sep_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#generating summary stats for each six month block
weather_six_month_may <- weather %>%
  group_by(CensusYearMay,
    Census_six_month_May) %>%
  summarize(May_hexamonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            May_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather_six_month_jul <- weather %>%
  group_by(CensusYearJul,
    Census_six_month_Jul) %>%
  summarize(Jul_hexamonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            Jul_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Jul_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weather_six_month_sep <- weather %>%
  group_by(CensusYearSep,
    Census_six_month_Sep) %>%
  summarize(Sep_hexamonthly_vpdmean = mean(((`vpdmax (hPa)`+`vpdmin (hPa)`)/2), na.rm = TRUE), 
            Sep_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Sep_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))


#making data frames with only necessary data for each one month back from May
weatherMayMay <- weather_one_month_may %>% filter(Census_one_month_May == "firstoneback") %>%
  rename(firstoneback_vpdmean = May_monthly_vpdmean,
         firstoneback_ppt = May_monthly_ppt_tot,
         firstoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayApr <- weather_one_month_may %>% filter(Census_one_month_May == "secondoneback") %>%
  rename(secondoneback_vpdmean = May_monthly_vpdmean,
         secondoneback_ppt = May_monthly_ppt_tot,
         secondoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayMar <- weather_one_month_may %>% filter(Census_one_month_May == "thirdoneback")%>%
  rename(thirdoneback_vpdmean = May_monthly_vpdmean,
         thirdoneback_ppt = May_monthly_ppt_tot,
         thirdoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayFeb <- weather_one_month_may %>% filter(Census_one_month_May == "fourthoneback") %>%
  rename(fourthoneback_vpdmean = May_monthly_vpdmean,
         fourthoneback_ppt = May_monthly_ppt_tot,
         fourthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayJan <- weather_one_month_may %>% filter(Census_one_month_May == "fifthoneback") %>%
  rename(fifthoneback_vpdmean = May_monthly_vpdmean,
         fifthoneback_ppt = May_monthly_ppt_tot,
         fifthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayDec <- weather_one_month_may %>% filter(Census_one_month_May == "sixthoneback") %>%
  rename(sixthoneback_vpdmean = May_monthly_vpdmean,
         sixthoneback_ppt = May_monthly_ppt_tot,
         sixthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayNov <- weather_one_month_may %>% filter(Census_one_month_May == "seventhoneback") %>%
  rename(seventhoneback_vpdmean = May_monthly_vpdmean,
         seventhoneback_ppt = May_monthly_ppt_tot,
         seventhoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayOct <- weather_one_month_may %>% filter(Census_one_month_May == "eighthoneback") %>%
  rename(eighthoneback_vpdmean = May_monthly_vpdmean,
         eighthoneback_ppt = May_monthly_ppt_tot,
         eighthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMaySep <- weather_one_month_may %>% filter(Census_one_month_May == "ninthoneback") %>%
  rename(ninthoneback_vpdmean = May_monthly_vpdmean,
         ninthoneback_ppt = May_monthly_ppt_tot,
         ninthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayAug <- weather_one_month_may %>% filter(Census_one_month_May == "tenthoneback") %>%
  rename(tenthoneback_vpdmean = May_monthly_vpdmean,
         tenthoneback_ppt = May_monthly_ppt_tot,
         tenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayJul <- weather_one_month_may %>% filter(Census_one_month_May == "eleventhoneback") %>%
  rename(eleventhoneback_vpdmean = May_monthly_vpdmean,
         eleventhoneback_ppt = May_monthly_ppt_tot,
         eleventhoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherMayJun <- weather_one_month_may %>% filter(Census_one_month_May == "twelvthoneback") %>%
  rename(twelvthoneback_vpdmean = May_monthly_vpdmean,
         twelvthoneback_ppt = May_monthly_ppt_tot,
         twelvthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each 3 months back from May
weatherMar_May <- weather_three_month_may %>% filter(Census_three_month_May == "firstthreeback") %>%
  rename(firstthreeback_vpdmean = May_trimonthly_vpdmean,
         firstthreeback_ppt = May_trimonthly_ppt_tot,
         firstthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherDec_Feb <- weather_three_month_may %>% filter(Census_three_month_May == "secondthreeback") %>%
  rename(secondthreeback_vpdmean = May_trimonthly_vpdmean,
         secondthreeback_ppt = May_trimonthly_ppt_tot,
         secondthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherSep_Nov <- weather_three_month_may %>% filter(Census_three_month_May == "thirdthreeback")%>%
  rename(thirdthreeback_vpdmean = May_trimonthly_vpdmean,
         thirdthreeback_ppt = May_trimonthly_ppt_tot,
         thirdthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherJun_Aug <- weather_three_month_may %>% filter(Census_three_month_May == "fourththreeback") %>%
  rename(fourththreeback_vpdmean = May_trimonthly_vpdmean,
         fourththreeback_ppt = May_trimonthly_ppt_tot,
         fourththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each 6 months back from May
weatherDec_May <- weather_six_month_may %>% filter(Census_six_month_May == "firstsixback") %>%
  rename(firstsixback_vpdmean = May_hexamonthly_vpdmean,
         firstsixback_ppt = May_hexamonthly_ppt_tot,
         firstsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

weatherJun_Nov <- weather_six_month_may %>% filter(Census_six_month_May == "secondsixback") %>%
  rename(secondsixback_vpdmean = May_hexamonthly_vpdmean,
         secondsixback_ppt = May_hexamonthly_ppt_tot,
         secondsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each one month back from Jul
weatherJulJul <- weather_one_month_jul %>% filter(Census_one_month_Jul == "firstoneback") %>%
  rename(firstoneback_vpdmean = Jul_monthly_vpdmean,
         firstoneback_ppt = Jul_monthly_ppt_tot,
         firstoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulJun <- weather_one_month_jul %>% filter(Census_one_month_Jul == "secondoneback") %>%
  rename(secondoneback_vpdmean = Jul_monthly_vpdmean,
         secondoneback_ppt = Jul_monthly_ppt_tot,
         secondoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulMay <- weather_one_month_jul %>% filter(Census_one_month_Jul == "thirdoneback")%>%
  rename(thirdoneback_vpdmean = Jul_monthly_vpdmean,
         thirdoneback_ppt = Jul_monthly_ppt_tot,
         thirdoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulApr <- weather_one_month_jul %>% filter(Census_one_month_Jul == "fourthoneback") %>%
  rename(fourthoneback_vpdmean = Jul_monthly_vpdmean,
         fourthoneback_ppt = Jul_monthly_ppt_tot,
         fourthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulMar <- weather_one_month_jul %>% filter(Census_one_month_Jul == "fifthoneback") %>%
  rename(fifthoneback_vpdmean = Jul_monthly_vpdmean,
         fifthoneback_ppt = Jul_monthly_ppt_tot,
         fifthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulFeb <- weather_one_month_jul %>% filter(Census_one_month_Jul == "sixthoneback") %>%
  rename(sixthoneback_vpdmean = Jul_monthly_vpdmean,
         sixthoneback_ppt = Jul_monthly_ppt_tot,
         sixthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulJan <- weather_one_month_jul %>% filter(Census_one_month_Jul == "seventhoneback") %>%
  rename(seventhoneback_vpdmean = Jul_monthly_vpdmean,
         seventhoneback_ppt = Jul_monthly_ppt_tot,
         seventhoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulDec <- weather_one_month_jul %>% filter(Census_one_month_Jul == "eighthoneback") %>%
  rename(eighthoneback_vpdmean = Jul_monthly_vpdmean,
         eighthoneback_ppt = Jul_monthly_ppt_tot,
         eighthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulNov <- weather_one_month_jul %>% filter(Census_one_month_Jul == "ninthoneback") %>%
  rename(ninthoneback_vpdmean = Jul_monthly_vpdmean,
         ninthoneback_ppt = Jul_monthly_ppt_tot,
         ninthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulOct <- weather_one_month_jul %>% filter(Census_one_month_Jul == "tenthoneback") %>%
  rename(tenthoneback_vpdmean = Jul_monthly_vpdmean,
         tenthoneback_ppt = Jul_monthly_ppt_tot,
         tenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulSep <- weather_one_month_jul %>% filter(Census_one_month_Jul == "eleventhoneback") %>%
  rename(eleventhoneback_vpdmean = Jul_monthly_vpdmean,
         eleventhoneback_ppt = Jul_monthly_ppt_tot,
         eleventhoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherJulAug <- weather_one_month_jul %>% filter(Census_one_month_Jul == "twelvthoneback") %>%
  rename(twelvthoneback_vpdmean = Jul_monthly_vpdmean,
         twelvthoneback_ppt = Jul_monthly_ppt_tot,
         twelvthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each 3 months back from Jul
weatherMay_Jul <- weather_three_month_jul %>% filter(Census_three_month_Jul == "firstthreeback") %>%
  rename(firstthreeback_vpdmean = Jul_trimonthly_vpdmean,
         firsththreeback_ppt = Jul_trimonthly_ppt_tot,
         firsththreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherFeb_Apr <- weather_three_month_jul %>% filter(Census_three_month_Jul == "secondthreeback") %>%
  rename(secondthreeback_vpdmean = Jul_trimonthly_vpdmean,
         secondthreeback_ppt = Jul_trimonthly_ppt_tot,
         secondthreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherNov_Feb <- weather_three_month_jul %>% filter(Census_three_month_Jul == "thirdthreeback") %>%
  rename(thirdthreeback_vpdmean = Jul_trimonthly_vpdmean,
         thirdthreeback_ppt = Jul_trimonthly_ppt_tot,
         thirdthreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherAug_Oct <- weather_three_month_jul %>% filter(Census_three_month_Jul == "fourththreeback") %>%
  rename(fourththreeback_vpdmean = Jul_trimonthly_vpdmean,
         fourththreeback_ppt = Jul_trimonthly_ppt_tot,
         fourththreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each 6 months back from Jul
weatherFeb_Jul <- weather_six_month_jul %>% filter(Census_six_month_Jul == "firstsixback") %>%
  rename(firstsixback_vpdmean = Jul_hexamonthly_vpdmean,
         fifthsixback_ppt = Jul_hexamonthly_ppt_tot,
         fifthsixback_tmean = Jul_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherAug_Jan <- weather_six_month_jul %>% filter(Census_six_month_Jul == "secondsixback") %>%
  rename(secondsixback_vpdmean = Jul_hexamonthly_vpdmean,
         secondsixback_ppt = Jul_hexamonthly_ppt_tot,
         secondsixback_tmean = Jul_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each one month back from Sep
weatherSepSep <- weather_one_month_sep %>% filter(Census_one_month_Sep == "firstoneback") %>%
  rename(firstoneback_vpdmean = Sep_monthly_vpdmean,
         firstoneback_ppt = Sep_monthly_ppt_tot,
         firstoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepAug <- weather_one_month_sep %>% filter(Census_one_month_Sep == "secondoneback") %>%
  rename(secondoneback_vpdmean = Sep_monthly_vpdmean,
         secondoneback_ppt = Sep_monthly_ppt_tot,
         secondoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepJul <- weather_one_month_sep %>% filter(Census_one_month_Sep == "thirdoneback")%>%
  rename(thirdoneback_vpdmean = Sep_monthly_vpdmean,
         thirdoneback_ppt = Sep_monthly_ppt_tot,
         thirdoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepJun <- weather_one_month_sep %>% filter(Census_one_month_Sep == "fourthoneback") %>%
  rename(fourthoneback_vpdmean = Sep_monthly_vpdmean,
         fourthoneback_ppt = Sep_monthly_ppt_tot,
         fourthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepMay <- weather_one_month_sep %>% filter(Census_one_month_Sep == "fifthoneback") %>%
  rename(fifthoneback_vpdmean = Sep_monthly_vpdmean,
         fifthoneback_ppt = Sep_monthly_ppt_tot,
         fifthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepApr <- weather_one_month_sep %>% filter(Census_one_month_Sep == "sixthoneback") %>%
  rename(sixthoneback_vpdmean = Sep_monthly_vpdmean,
         sixthoneback_ppt = Sep_monthly_ppt_tot,
         sixthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepMar <- weather_one_month_sep %>% filter(Census_one_month_Sep == "seventhoneback") %>%
  rename(seventhoneback_vpdmean = Sep_monthly_vpdmean,
         seventhoneback_ppt = Sep_monthly_ppt_tot,
         seventhoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepFeb <- weather_one_month_sep %>% filter(Census_one_month_Sep == "eighthoneback") %>%
  rename(eighthoneback_vpdmean = Sep_monthly_vpdmean,
         eighthoneback_ppt = Sep_monthly_ppt_tot,
         eighthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepJan <- weather_one_month_sep %>% filter(Census_one_month_Sep == "ninthoneback") %>%
  rename(ninthoneback_vpdmean = Sep_monthly_vpdmean,
         ninthoneback_ppt = Sep_monthly_ppt_tot,
         ninthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepDec <- weather_one_month_sep %>% filter(Census_one_month_Sep == "tenthoneback") %>%
  rename(tenthoneback_vpdmean = Sep_monthly_vpdmean,
         tenthoneback_ppt = Sep_monthly_ppt_tot,
         tenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepNov <- weather_one_month_sep %>% filter(Census_one_month_Sep == "eleventhoneback") %>%
  rename(eleventhoneback_vpdmean = Sep_monthly_vpdmean,
         eleventhoneback_ppt = Sep_monthly_ppt_tot,
         eleventhoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherSepOct <- weather_one_month_sep %>% filter(Census_one_month_Sep == "twelvthoneback") %>%
  rename(twelvthoneback_vpdmean = Sep_monthly_vpdmean,
         twelvthoneback_ppt = Sep_monthly_ppt_tot,
         twelvthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

#making data frames with only necessary data for each 3 months back from Sep
weatherJul_Sep <- weather_three_month_sep %>% filter(Census_three_month_Sep == "firstthreeback") %>%
  rename(firstthreeback_vpdmean = Sep_trimonthly_vpdmean,
         firstthreeback_ppt = Sep_trimonthly_ppt_tot,
         firstthreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherApr_Jun <- weather_three_month_sep %>% filter(Census_three_month_Sep == "secondthreeback") %>%
  rename(secondthreeback_vpdmean = Sep_trimonthly_vpdmean,
         secondthreeback_ppt = Sep_trimonthly_ppt_tot,
         secondthreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherJan_Mar <- weather_three_month_sep %>% filter(Census_three_month_Sep == "thirdthreeback") %>%
  rename(thirdthreeback_vpdmean = Sep_trimonthly_vpdmean,
         thirdthreeback_ppt = Sep_trimonthly_ppt_tot,
         thirdthreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherOct_Dec <- weather_three_month_sep %>% filter(Census_three_month_Sep == "fourththreeback") %>%
  rename(fourththreeback_vpdmean = Sep_trimonthly_vpdmean,
         fourththreeback_ppt = Sep_trimonthly_ppt_tot,
         fourththreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

#making data frames with only necessary data for each 6 months back from Sep
weatherApr_Sep <- weather_six_month_sep %>% filter(Census_six_month_Sep == "firstsixback") %>%
  rename(firstsixback_vpdmean = Sep_hexamonthly_vpdmean,
         firstsixback_ppt = Sep_hexamonthly_ppt_tot,
         firstsixback_tmean = Sep_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherOct_Mar <- weather_six_month_sep %>% filter(Census_six_month_Sep == "secondsixback") %>%
  rename(secondsixback_vpdmean = Sep_hexamonthly_vpdmean,
         secondsixback_ppt = Sep_hexamonthly_ppt_tot,
         secondsixback_tmean = Sep_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Sep) %>%
  mutate(censusmonth = "Sep")

#Combining demographic data with the weather data
gras <- read.csv("data/ltreb_allspp_2007_2025.csv")

#Grouping species with census timing
grasMayCensus <- gras %>% filter(species == c("FESU","POAL","POAU","POSY"))
grasJulCensus <- gras %>% filter(species == c("ELVI","ELRI","LOAR"))
grasSepCensus <- gras %>% filter(species == "AGPE")

#Combining corresponding weather and demographic data frames

#recombining weather data for May census
weatherAllMay <- left_join(x=weatherMayMay, y=weatherMayApr, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayMar, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayFeb, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayJan, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayDec, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayNov, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayOct, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMaySep, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayAug, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayJul, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherMayJun, by= c("censusmonth","CensusYearMay"))

weatherAllMay <- left_join(x=weatherAllMay, y=weatherMar_May, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherDec_Feb, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherSep_Nov, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherJun_Aug, by= c("censusmonth","CensusYearMay"))

weatherAllMay <- left_join(x=weatherAllMay, y=weatherDec_May, by= c("censusmonth","CensusYearMay"))
weatherAllMay <- left_join(x=weatherAllMay, y=weatherJun_Nov, by= c("censusmonth","CensusYearMay"))

#combing weather and demographic data for species censused in May
CombinedMay <- left_join(x=grasMayCensus, y=weatherAllMay, by=c("year_t" = "CensusYearMay"))

#recombining weather data for Jul census

weatherAllJul <- left_join(x=weatherJulJul, y=weatherJulJun, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulMay, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulApr, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulMar, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulFeb, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulJan, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulDec, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulNov, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulOct, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulSep, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherJulAug, by=c("censusmonth","CensusYearJul"))

weatherAllJul <- left_join(x=weatherAllJul, y=weatherMay_Jul, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherFeb_Apr, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherNov_Feb, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherAug_Oct, by=c("censusmonth","CensusYearJul"))

weatherAllJul <- left_join(x=weatherAllJul, y=weatherFeb_Jul, by=c("censusmonth","CensusYearJul"))
weatherAllJul <- left_join(x=weatherAllJul, y=weatherAug_Jan, by=c("censusmonth","CensusYearJul"))

#combing weather and demographic data for species censused in Jul
CombinedJul <- left_join(x=grasJulCensus, y=weatherAllJul, by=c("year_t" = "CensusYearJul"))

#recombining weather data for Sep census
weatherAllSep <- left_join(x=weatherSepSep, y=weatherSepAug, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepJul, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepJun, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepMay, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepApr, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepMar, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepFeb, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepJan, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepDec, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepNov, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherSepOct, by=c("censusmonth","CensusYearSep"))

weatherAllSep <- left_join(x=weatherAllSep, y=weatherJul_Sep, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherApr_Jun, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherJan_Mar, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherOct_Dec, by=c("censusmonth","CensusYearSep"))

weatherAllSep <- left_join(x=weatherAllSep, y=weatherApr_Sep, by=c("censusmonth","CensusYearSep"))
weatherAllSep <- left_join(x=weatherAllSep, y=weatherOct_Mar, by=c("censusmonth","CensusYearSep"))

#combing weather and demographic data for species censused in Sep
CombinedSep <- left_join(x=grasSepCensus, y=weatherAllSep, by=c("year_t" = "CensusYearSep"))

#Combining all species and censusmonths of data
CombinedDataSegments <- bind_rows(CombinedMay, CombinedJul, CombinedSep)


write.csv(CombinedDataSegments, "data/CombinedDataSegments")
