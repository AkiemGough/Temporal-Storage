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
weatherIND <- read_csv("data/weather_data_Indiana.csv")
weatherTEX <- read_csv("data/weather_data_Nacogdoches.csv")

#extracting month and year from date
weatherIND$Date <- as.Date(weatherIND$Date, format= "%m/%d/%Y")

weatherIND$monthina <- as.numeric (format(weatherIND$Date, "%m"))
weatherIND$yearina <- as.numeric (format(weatherIND$Date, "%Y")) + 2000

weatherTEX$Date <- as.Date(weatherTEX$Date, format= "%m/%d/%Y")

weatherTEX$monthina <- as.numeric (format(weatherTEX$Date, "%m"))
weatherTEX$yearina <- as.numeric (format(weatherTEX$Date, "%Y")) + 2000

#creating census years for each data collection period     
weatherIND$CensusYearMay <- ifelse(weatherIND$monthina > 5, (weatherIND$yearina + 1), weatherIND$yearina)
weatherIND$CensusYearJul <- ifelse(weatherIND$monthina > 7, (weatherIND$yearina + 1), weatherIND$yearina)
weatherIND$CensusYearSep <- ifelse(weatherIND$monthina > 9, (weatherIND$yearina + 1), weatherIND$yearina)

weatherTEX$CensusYearMay <- ifelse(weatherTEX$monthina > 5, (weatherTEX$yearina + 1), weatherTEX$yearina)

#creating one month census blocks
weatherIND$Census_one_month_May <- (case_when(weatherIND$monthina == 5 ~ "firstoneback",
                                             weatherIND$monthina == 4 ~ "secondoneback",
                                             weatherIND$monthina == 3 ~ "thirdoneback",
                                             weatherIND$monthina == 2 ~ "fourthoneback",
                                             weatherIND$monthina == 1 ~ "fifthoneback",
                                             weatherIND$monthina == 12 ~ "sixthoneback",
                                             weatherIND$monthina == 11 ~ "seventhoneback",
                                             weatherIND$monthina == 10 ~ "eighthoneback",
                                             weatherIND$monthina == 9 ~ "ninthoneback",
                                             weatherIND$monthina == 8 ~ "tenthoneback",
                                             weatherIND$monthina == 7 ~ "eleventhoneback",
                                             weatherIND$monthina == 6 ~ "twelvthoneback"))

weatherIND$Census_one_month_Jul <- (case_when(weatherIND$monthina == 7 ~ "firstoneback",
                                           weatherIND$monthina == 6 ~ "secondoneback",
                                           weatherIND$monthina == 5 ~ "thirdoneback",
                                           weatherIND$monthina == 4 ~ "fourthoneback",
                                           weatherIND$monthina == 3 ~ "fifthoneback",
                                           weatherIND$monthina == 2 ~ "sixthoneback",
                                           weatherIND$monthina == 1 ~ "seventhoneback",
                                           weatherIND$monthina == 12 ~ "eighthoneback",
                                           weatherIND$monthina == 11 ~ "ninthoneback",
                                           weatherIND$monthina == 10 ~ "tenthoneback",
                                           weatherIND$monthina == 9 ~ "eleventhoneback",
                                           weatherIND$monthina == 8 ~ "twelvthoneback"))

weatherIND$Census_one_month_Sep <- (case_when(weatherIND$monthina == 9 ~ "firstoneback",
                                           weatherIND$monthina == 8 ~ "secondoneback",
                                           weatherIND$monthina == 7 ~ "thirdoneback",
                                           weatherIND$monthina == 6 ~ "fourthoneback",
                                           weatherIND$monthina == 5 ~ "fifthoneback",
                                           weatherIND$monthina == 4 ~ "sixthoneback",
                                           weatherIND$monthina == 3 ~ "seventhoneback",
                                           weatherIND$monthina == 2 ~ "eighthoneback",
                                           weatherIND$monthina == 1 ~ "ninthoneback",
                                           weatherIND$monthina == 12 ~ "tenthoneback",
                                           weatherIND$monthina == 11 ~ "eleventhoneback",
                                           weatherIND$monthina == 10 ~ "twelvthoneback"))

weatherTEX$Census_one_month_May <- (case_when(weatherTEX$monthina == 5 ~ "firstoneback",
                                              weatherTEX$monthina == 4 ~ "secondoneback",
                                              weatherTEX$monthina == 3 ~ "thirdoneback",
                                              weatherTEX$monthina == 2 ~ "fourthoneback",
                                              weatherTEX$monthina == 1 ~ "fifthoneback",
                                              weatherTEX$monthina == 12 ~ "sixthoneback",
                                              weatherTEX$monthina == 11 ~ "seventhoneback",
                                              weatherTEX$monthina == 10 ~ "eighthoneback",
                                              weatherTEX$monthina == 9 ~ "ninthoneback",
                                              weatherTEX$monthina == 8 ~ "tenthoneback",
                                              weatherTEX$monthina == 7 ~ "eleventhoneback",
                                              weatherTEX$monthina == 6 ~ "twelvthoneback"))


#creating three month census blocks
weatherIND$Census_three_month_May <- (case_when(weatherIND$monthina == 5 ~ "firstthreeback",
                                             weatherIND$monthina == 4 ~ "firstthreeback",
                                             weatherIND$monthina == 3 ~ "firstthreeback",
                                             weatherIND$monthina == 2 ~ "secondthreeback",
                                             weatherIND$monthina == 1 ~ "secondthreeback",
                                             weatherIND$monthina == 12 ~ "secondthreeback",
                                             weatherIND$monthina == 11 ~ "thirdthreeback",
                                             weatherIND$monthina == 10 ~ "thirdthreeback",
                                             weatherIND$monthina == 9 ~ "thirdthreeback",
                                             weatherIND$monthina == 8 ~ "fourththreeback",
                                             weatherIND$monthina == 7 ~ "fourththreeback",
                                             weatherIND$monthina == 6 ~ "fourththreeback"))

weatherIND$Census_three_month_Jul <- (case_when(weatherIND$monthina == 7 ~ "firstthreeback",
                                             weatherIND$monthina == 6 ~ "firstthreeback",
                                             weatherIND$monthina == 5 ~ "firstthreeback",
                                             weatherIND$monthina == 4 ~ "secondthreeback",
                                             weatherIND$monthina == 3 ~ "secondthreeback",
                                             weatherIND$monthina == 2 ~ "secondthreeback",
                                             weatherIND$monthina == 1 ~ "thirdthreeback",
                                             weatherIND$monthina == 12 ~ "thirdthreeback",
                                             weatherIND$monthina == 11 ~ "thirdthreeback",
                                             weatherIND$monthina == 10 ~ "fourththreeback",
                                             weatherIND$monthina == 9 ~ "fourththreeback",
                                             weatherIND$monthina == 8 ~ "fourththreeback"))

weatherIND$Census_three_month_Sep <- (case_when(weatherIND$monthina == 9 ~ "firstthreeback",
                                             weatherIND$monthina == 8 ~ "firstthreeback",
                                             weatherIND$monthina == 7 ~ "firstthreeback",
                                             weatherIND$monthina == 6 ~ "secondthreeback",
                                             weatherIND$monthina == 5 ~ "secondthreeback",
                                             weatherIND$monthina == 4 ~ "secondthreeback",
                                             weatherIND$monthina == 3 ~ "thirdthreeback",
                                             weatherIND$monthina == 2 ~ "thirdthreeback",
                                             weatherIND$monthina == 1 ~ "thirdthreeback",
                                             weatherIND$monthina == 12 ~ "fourththreeback",
                                             weatherIND$monthina == 11 ~ "fourththreeback",
                                             weatherIND$monthina == 10 ~ "fourththreeback"))

weatherTEX$Census_three_month_May <- (case_when(weatherTEX$monthina == 5 ~ "firstthreeback",
                                                weatherTEX$monthina == 4 ~ "firstthreeback",
                                                weatherTEX$monthina == 3 ~ "firstthreeback",
                                                weatherTEX$monthina == 2 ~ "secondthreeback",
                                                weatherTEX$monthina == 1 ~ "secondthreeback",
                                                weatherTEX$monthina == 12 ~ "secondthreeback",
                                                weatherTEX$monthina == 11 ~ "thirdthreeback",
                                                weatherTEX$monthina == 10 ~ "thirdthreeback",
                                                weatherTEX$monthina == 9 ~ "thirdthreeback",
                                                weatherTEX$monthina == 8 ~ "fourththreeback",
                                                weatherTEX$monthina == 7 ~ "fourththreeback",
                                                weatherTEX$monthina == 6 ~ "fourththreeback"))

#creating six month census blocks
weatherIND$Census_six_month_May <- (case_when(weatherIND$monthina == 5 ~ "firstsixback",
                                           weatherIND$monthina == 4 ~ "firstsixback",
                                           weatherIND$monthina == 3 ~ "firstsixback",
                                           weatherIND$monthina == 2 ~ "firstsixback",
                                           weatherIND$monthina == 1 ~ "firstsixback",
                                           weatherIND$monthina == 12 ~ "firstsixback",
                                           weatherIND$monthina == 11 ~ "secondsixback",
                                           weatherIND$monthina == 10 ~ "secondsixback",
                                           weatherIND$monthina == 9 ~ "secondsixback",
                                           weatherIND$monthina == 8 ~ "secondsixback",
                                           weatherIND$monthina == 7 ~ "secondsixback",
                                           weatherIND$monthina == 6 ~ "secondsixback"))

weatherIND$Census_six_month_Jul <- (case_when(weatherIND$monthina == 7 ~ "firstsixback",
                                           weatherIND$monthina == 6 ~ "firstsixback",
                                           weatherIND$monthina == 5 ~ "firstsixback",
                                           weatherIND$monthina == 4 ~ "firstsixback",
                                           weatherIND$monthina == 3 ~ "firstsixback",
                                           weatherIND$monthina == 2 ~ "firstsixback",
                                           weatherIND$monthina == 1 ~ "secondsixback",
                                           weatherIND$monthina == 12 ~ "secondsixback",
                                           weatherIND$monthina == 11 ~ "secondsixback",
                                           weatherIND$monthina == 10 ~ "secondsixback",
                                           weatherIND$monthina == 9 ~ "secondsixback",
                                           weatherIND$monthina == 8 ~ "secondsixback"))

weatherIND$Census_six_month_Sep <- (case_when(weatherIND$monthina == 9 ~ "firstsixback",
                                           weatherIND$monthina == 8 ~ "firstsixback",
                                           weatherIND$monthina == 7 ~ "firstsixback",
                                           weatherIND$monthina == 6 ~ "firstsixback",
                                           weatherIND$monthina == 5 ~ "firstsixback",
                                           weatherIND$monthina == 4 ~ "firstsixback",
                                           weatherIND$monthina == 3 ~ "secondsixback",
                                           weatherIND$monthina == 2 ~ "secondsixback",
                                           weatherIND$monthina == 1 ~ "secondsixback",
                                           weatherIND$monthina == 12 ~ "secondsixback",
                                           weatherIND$monthina == 11 ~ "secondsixback",
                                           weatherIND$monthina == 10 ~ "secondsixback"))

weatherTEX$Census_six_month_May <- (case_when(weatherTEX$monthina == 5 ~ "firstsixback",
                                              weatherTEX$monthina == 4 ~ "firstsixback",
                                              weatherTEX$monthina == 3 ~ "firstsixback",
                                              weatherTEX$monthina == 2 ~ "firstsixback",
                                              weatherTEX$monthina == 1 ~ "firstsixback",
                                              weatherTEX$monthina == 12 ~ "firstsixback",
                                              weatherTEX$monthina == 11 ~ "secondsixback",
                                              weatherTEX$monthina == 10 ~ "secondsixback",
                                              weatherTEX$monthina == 9 ~ "secondsixback",
                                              weatherTEX$monthina == 8 ~ "secondsixback",
                                              weatherTEX$monthina == 7 ~ "secondsixback",
                                              weatherTEX$monthina == 6 ~ "secondsixback"))

#generating summary stats for each one month block
weatherIND_one_month_may <- weatherIND %>%
  group_by(CensusYearMay,
           Census_one_month_May) %>%
  summarize(May_monthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            May_monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherIND_one_month_jul <- weatherIND %>%
  group_by(CensusYearJul,
           Census_one_month_Jul) %>%
  summarize(Jul_monthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            Jul_monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Jul_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherIND_one_month_sep <- weatherIND %>%
  group_by(CensusYearSep,
           Census_one_month_Sep) %>%
  summarize(Sep_monthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            Sep_monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Sep_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#same but for Texas
weatherTEX_one_month_may <- weatherTEX %>%
  group_by(CensusYearMay,
           Census_one_month_May) %>%
  summarize(May_monthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            May_monthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#generating summary stats for each three month block
weatherIND_three_month_may <- weatherIND %>%
  group_by(CensusYearMay,
    Census_three_month_May) %>%
  summarize(May_trimonthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            May_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherIND_three_month_jul <- weatherIND %>%
  group_by(CensusYearJul,
    Census_three_month_Jul) %>%
  summarize(Jul_trimonthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            Jul_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Jul_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherIND_three_month_sep <- weatherIND %>%
  group_by(CensusYearSep,
    Census_three_month_Sep) %>%
  summarize(Sep_trimonthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            Sep_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Sep_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#same but for Texas
weatherTEX_three_month_may <- weatherTEX %>%
  group_by(CensusYearMay,
           Census_three_month_May) %>%
  summarize(May_trimonthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            May_trimonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#generating summary stats for each six month block
weatherIND_six_month_may <- weatherIND %>%
  group_by(CensusYearMay,
    Census_six_month_May) %>%
  summarize(May_hexamonthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            May_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherIND_six_month_jul <- weatherIND %>%
  group_by(CensusYearJul,
    Census_six_month_Jul) %>%
  summarize(Jul_hexamonthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            Jul_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Jul_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

weatherIND_six_month_sep <- weatherIND %>%
  group_by(CensusYearSep,
    Census_six_month_Sep) %>%
  summarize(Sep_hexamonthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            Sep_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            Sep_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#same but for Texas
weatherTEX_six_month_may <- weatherTEX %>%
  group_by(CensusYearMay,
           Census_six_month_May) %>%
  summarize(May_hexamonthly_vpdmax = mean(`vpdmax (hPa)`, na.rm = TRUE), 
            May_hexamonthly_ppt_tot = sum(`ppt (inches)`, na.rm = TRUE),
            May_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE))

#making data frames with only necessary data for each one month back from May for Indiana
weatherINDMayMay <- weatherIND_one_month_may %>% filter(Census_one_month_May == "firstoneback") %>%
  rename(firstoneback_vpdmax = May_monthly_vpdmax,
         firstoneback_ppt = May_monthly_ppt_tot,
         firstoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayApr <- weatherIND_one_month_may %>% filter(Census_one_month_May == "secondoneback") %>%
  rename(secondoneback_vpdmax = May_monthly_vpdmax,
         secondoneback_ppt = May_monthly_ppt_tot,
         secondoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayMar <- weatherIND_one_month_may %>% filter(Census_one_month_May == "thirdoneback")%>%
  rename(thirdoneback_vpdmax = May_monthly_vpdmax,
         thirdoneback_ppt = May_monthly_ppt_tot,
         thirdoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayFeb <- weatherIND_one_month_may %>% filter(Census_one_month_May == "fourthoneback") %>%
  rename(fourthoneback_vpdmax = May_monthly_vpdmax,
         fourthoneback_ppt = May_monthly_ppt_tot,
         fourthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayJan <- weatherIND_one_month_may %>% filter(Census_one_month_May == "fifthoneback") %>%
  rename(fifthoneback_vpdmax = May_monthly_vpdmax,
         fifthoneback_ppt = May_monthly_ppt_tot,
         fifthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayDec <- weatherIND_one_month_may %>% filter(Census_one_month_May == "sixthoneback") %>%
  rename(sixthoneback_vpdmax = May_monthly_vpdmax,
         sixthoneback_ppt = May_monthly_ppt_tot,
         sixthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayNov <- weatherIND_one_month_may %>% filter(Census_one_month_May == "seventhoneback") %>%
  rename(seventhoneback_vpdmax = May_monthly_vpdmax,
         seventhoneback_ppt = May_monthly_ppt_tot,
         seventhoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayOct <- weatherIND_one_month_may %>% filter(Census_one_month_May == "eighthoneback") %>%
  rename(eighthoneback_vpdmax = May_monthly_vpdmax,
         eighthoneback_ppt = May_monthly_ppt_tot,
         eighthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMaySep <- weatherIND_one_month_may %>% filter(Census_one_month_May == "ninthoneback") %>%
  rename(ninthoneback_vpdmax = May_monthly_vpdmax,
         ninthoneback_ppt = May_monthly_ppt_tot,
         ninthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayAug <- weatherIND_one_month_may %>% filter(Census_one_month_May == "tenthoneback") %>%
  rename(tenthoneback_vpdmax = May_monthly_vpdmax,
         tenthoneback_ppt = May_monthly_ppt_tot,
         tenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayJul <- weatherIND_one_month_may %>% filter(Census_one_month_May == "eleventhoneback") %>%
  rename(eleventhoneback_vpdmax = May_monthly_vpdmax,
         eleventhoneback_ppt = May_monthly_ppt_tot,
         eleventhoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayJun <- weatherIND_one_month_may %>% filter(Census_one_month_May == "twelvthoneback") %>%
  rename(twelvthoneback_vpdmax = May_monthly_vpdmax,
         twelvthoneback_ppt = May_monthly_ppt_tot,
         twelvthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each 3 months back from May for Indiana
weatherINDMar_May <- weatherIND_three_month_may %>% filter(Census_three_month_May == "firstthreeback") %>%
  rename(firstthreeback_vpdmax = May_trimonthly_vpdmax,
         firstthreeback_ppt = May_trimonthly_ppt_tot,
         firstthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherINDDec_Feb <- weatherIND_three_month_may %>% filter(Census_three_month_May == "secondthreeback") %>%
  rename(secondthreeback_vpdmax = May_trimonthly_vpdmax,
         secondthreeback_ppt = May_trimonthly_ppt_tot,
         secondthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherINDSep_Nov <- weatherIND_three_month_may %>% filter(Census_three_month_May == "thirdthreeback")%>%
  rename(thirdthreeback_vpdmax = May_trimonthly_vpdmax,
         thirdthreeback_ppt = May_trimonthly_ppt_tot,
         thirdthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherINDJun_Aug <- weatherIND_three_month_may %>% filter(Census_three_month_May == "fourththreeback") %>%
  rename(fourththreeback_vpdmax = May_trimonthly_vpdmax,
         fourththreeback_ppt = May_trimonthly_ppt_tot,
         fourththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each 6 months back from May for Indiana
weatherINDDec_May <- weatherIND_six_month_may %>% filter(Census_six_month_May == "firstsixback") %>%
  rename(firstsixback_vpdmax = May_hexamonthly_vpdmax,
         firstsixback_ppt = May_hexamonthly_ppt_tot,
         firstsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

weatherINDJun_Nov <- weatherIND_six_month_may %>% filter(Census_six_month_May == "secondsixback") %>%
  rename(secondsixback_vpdmax = May_hexamonthly_vpdmax,
         secondsixback_ppt = May_hexamonthly_ppt_tot,
         secondsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each one month back from Jul
weatherINDJulJul <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "firstoneback") %>%
  rename(firstoneback_vpdmax = Jul_monthly_vpdmax,
         firstoneback_ppt = Jul_monthly_ppt_tot,
         firstoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulJun <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "secondoneback") %>%
  rename(secondoneback_vpdmax = Jul_monthly_vpdmax,
         secondoneback_ppt = Jul_monthly_ppt_tot,
         secondoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulMay <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "thirdoneback")%>%
  rename(thirdoneback_vpdmax = Jul_monthly_vpdmax,
         thirdoneback_ppt = Jul_monthly_ppt_tot,
         thirdoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulApr <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "fourthoneback") %>%
  rename(fourthoneback_vpdmax = Jul_monthly_vpdmax,
         fourthoneback_ppt = Jul_monthly_ppt_tot,
         fourthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulMar <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "fifthoneback") %>%
  rename(fifthoneback_vpdmax = Jul_monthly_vpdmax,
         fifthoneback_ppt = Jul_monthly_ppt_tot,
         fifthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulFeb <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "sixthoneback") %>%
  rename(sixthoneback_vpdmax = Jul_monthly_vpdmax,
         sixthoneback_ppt = Jul_monthly_ppt_tot,
         sixthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulJan <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "seventhoneback") %>%
  rename(seventhoneback_vpdmax = Jul_monthly_vpdmax,
         seventhoneback_ppt = Jul_monthly_ppt_tot,
         seventhoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulDec <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "eighthoneback") %>%
  rename(eighthoneback_vpdmax = Jul_monthly_vpdmax,
         eighthoneback_ppt = Jul_monthly_ppt_tot,
         eighthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulNov <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "ninthoneback") %>%
  rename(ninthoneback_vpdmax = Jul_monthly_vpdmax,
         ninthoneback_ppt = Jul_monthly_ppt_tot,
         ninthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulOct <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "tenthoneback") %>%
  rename(tenthoneback_vpdmax = Jul_monthly_vpdmax,
         tenthoneback_ppt = Jul_monthly_ppt_tot,
         tenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulSep <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "eleventhoneback") %>%
  rename(eleventhoneback_vpdmax = Jul_monthly_vpdmax,
         eleventhoneback_ppt = Jul_monthly_ppt_tot,
         eleventhoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulAug <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "twelvthoneback") %>%
  rename(twelvthoneback_vpdmax = Jul_monthly_vpdmax,
         twelvthoneback_ppt = Jul_monthly_ppt_tot,
         twelvthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each 3 months back from Jul
weatherINDMay_Jul <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "firstthreeback") %>%
  rename(firstthreeback_vpdmax = Jul_trimonthly_vpdmax,
         firsththreeback_ppt = Jul_trimonthly_ppt_tot,
         firsththreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDFeb_Apr <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "secondthreeback") %>%
  rename(secondthreeback_vpdmax = Jul_trimonthly_vpdmax,
         secondthreeback_ppt = Jul_trimonthly_ppt_tot,
         secondthreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDNov_Feb <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "thirdthreeback") %>%
  rename(thirdthreeback_vpdmax = Jul_trimonthly_vpdmax,
         thirdthreeback_ppt = Jul_trimonthly_ppt_tot,
         thirdthreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDAug_Oct <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "fourththreeback") %>%
  rename(fourththreeback_vpdmax = Jul_trimonthly_vpdmax,
         fourththreeback_ppt = Jul_trimonthly_ppt_tot,
         fourththreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each 6 months back from Jul
weatherINDFeb_Jul <- weatherIND_six_month_jul %>% filter(Census_six_month_Jul == "firstsixback") %>%
  rename(firstsixback_vpdmax = Jul_hexamonthly_vpdmax,
         fifthsixback_ppt = Jul_hexamonthly_ppt_tot,
         fifthsixback_tmean = Jul_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDAug_Jan <- weatherIND_six_month_jul %>% filter(Census_six_month_Jul == "secondsixback") %>%
  rename(secondsixback_vpdmax = Jul_hexamonthly_vpdmax,
         secondsixback_ppt = Jul_hexamonthly_ppt_tot,
         secondsixback_tmean = Jul_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each one month back from Sep
weatherINDSepSep <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "firstoneback") %>%
  rename(firstoneback_vpdmax = Sep_monthly_vpdmax,
         firstoneback_ppt = Sep_monthly_ppt_tot,
         firstoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepAug <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "secondoneback") %>%
  rename(secondoneback_vpdmax = Sep_monthly_vpdmax,
         secondoneback_ppt = Sep_monthly_ppt_tot,
         secondoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepJul <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "thirdoneback")%>%
  rename(thirdoneback_vpdmax = Sep_monthly_vpdmax,
         thirdoneback_ppt = Sep_monthly_ppt_tot,
         thirdoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepJun <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "fourthoneback") %>%
  rename(fourthoneback_vpdmax = Sep_monthly_vpdmax,
         fourthoneback_ppt = Sep_monthly_ppt_tot,
         fourthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepMay <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "fifthoneback") %>%
  rename(fifthoneback_vpdmax = Sep_monthly_vpdmax,
         fifthoneback_ppt = Sep_monthly_ppt_tot,
         fifthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepApr <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "sixthoneback") %>%
  rename(sixthoneback_vpdmax = Sep_monthly_vpdmax,
         sixthoneback_ppt = Sep_monthly_ppt_tot,
         sixthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepMar <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "seventhoneback") %>%
  rename(seventhoneback_vpdmax = Sep_monthly_vpdmax,
         seventhoneback_ppt = Sep_monthly_ppt_tot,
         seventhoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepFeb <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "eighthoneback") %>%
  rename(eighthoneback_vpdmax = Sep_monthly_vpdmax,
         eighthoneback_ppt = Sep_monthly_ppt_tot,
         eighthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepJan <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "ninthoneback") %>%
  rename(ninthoneback_vpdmax = Sep_monthly_vpdmax,
         ninthoneback_ppt = Sep_monthly_ppt_tot,
         ninthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepDec <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "tenthoneback") %>%
  rename(tenthoneback_vpdmax = Sep_monthly_vpdmax,
         tenthoneback_ppt = Sep_monthly_ppt_tot,
         tenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepNov <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "eleventhoneback") %>%
  rename(eleventhoneback_vpdmax = Sep_monthly_vpdmax,
         eleventhoneback_ppt = Sep_monthly_ppt_tot,
         eleventhoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepOct <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "twelvthoneback") %>%
  rename(twelvthoneback_vpdmax = Sep_monthly_vpdmax,
         twelvthoneback_ppt = Sep_monthly_ppt_tot,
         twelvthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

#making data frames with only necessary data for each 3 months back from Sep
weatherINDJul_Sep <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "firstthreeback") %>%
  rename(firstthreeback_vpdmax = Sep_trimonthly_vpdmax,
         firstthreeback_ppt = Sep_trimonthly_ppt_tot,
         firstthreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDApr_Jun <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "secondthreeback") %>%
  rename(secondthreeback_vpdmax = Sep_trimonthly_vpdmax,
         secondthreeback_ppt = Sep_trimonthly_ppt_tot,
         secondthreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDJan_Mar <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "thirdthreeback") %>%
  rename(thirdthreeback_vpdmax = Sep_trimonthly_vpdmax,
         thirdthreeback_ppt = Sep_trimonthly_ppt_tot,
         thirdthreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDOct_Dec <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "fourththreeback") %>%
  rename(fourththreeback_vpdmax = Sep_trimonthly_vpdmax,
         fourththreeback_ppt = Sep_trimonthly_ppt_tot,
         fourththreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

#making data frames with only necessary data for each 6 months back from Sep
weatherINDApr_Sep <- weatherIND_six_month_sep %>% filter(Census_six_month_Sep == "firstsixback") %>%
  rename(firstsixback_vpdmax = Sep_hexamonthly_vpdmax,
         firstsixback_ppt = Sep_hexamonthly_ppt_tot,
         firstsixback_tmean = Sep_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDOct_Mar <- weatherIND_six_month_sep %>% filter(Census_six_month_Sep == "secondsixback") %>%
  rename(secondsixback_vpdmax = Sep_hexamonthly_vpdmax,
         secondsixback_ppt = Sep_hexamonthly_ppt_tot,
         secondsixback_tmean = Sep_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Sep) %>%
  mutate(censusmonth = "Sep")

#making data frames with only necessary data for each one month back from May for Texas
weatherTEXMayMay <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "firstoneback") %>%
  rename(firstoneback_vpdmax = May_monthly_vpdmax,
         firstoneback_ppt = May_monthly_ppt_tot,
         firstoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayApr <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "secondoneback") %>%
  rename(secondoneback_vpdmax = May_monthly_vpdmax,
         secondoneback_ppt = May_monthly_ppt_tot,
         secondoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayMar <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "thirdoneback")%>%
  rename(thirdoneback_vpdmax = May_monthly_vpdmax,
         thirdoneback_ppt = May_monthly_ppt_tot,
         thirdoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayFeb <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "fourthoneback") %>%
  rename(fourthoneback_vpdmax = May_monthly_vpdmax,
         fourthoneback_ppt = May_monthly_ppt_tot,
         fourthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayJan <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "fifthoneback") %>%
  rename(fifthoneback_vpdmax = May_monthly_vpdmax,
         fifthoneback_ppt = May_monthly_ppt_tot,
         fifthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayDec <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "sixthoneback") %>%
  rename(sixthoneback_vpdmax = May_monthly_vpdmax,
         sixthoneback_ppt = May_monthly_ppt_tot,
         sixthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayNov <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "seventhoneback") %>%
  rename(seventhoneback_vpdmax = May_monthly_vpdmax,
         seventhoneback_ppt = May_monthly_ppt_tot,
         seventhoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayOct <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "eighthoneback") %>%
  rename(eighthoneback_vpdmax = May_monthly_vpdmax,
         eighthoneback_ppt = May_monthly_ppt_tot,
         eighthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMaySep <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "ninthoneback") %>%
  rename(ninthoneback_vpdmax = May_monthly_vpdmax,
         ninthoneback_ppt = May_monthly_ppt_tot,
         ninthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayAug <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "tenthoneback") %>%
  rename(tenthoneback_vpdmax = May_monthly_vpdmax,
         tenthoneback_ppt = May_monthly_ppt_tot,
         tenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayJul <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "eleventhoneback") %>%
  rename(eleventhoneback_vpdmax = May_monthly_vpdmax,
         eleventhoneback_ppt = May_monthly_ppt_tot,
         eleventhoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayJun <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "twelvthoneback") %>%
  rename(twelvthoneback_vpdmax = May_monthly_vpdmax,
         twelvthoneback_ppt = May_monthly_ppt_tot,
         twelvthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each 3 months back from May for Texas
weatherTEXMar_May <- weatherTEX_three_month_may %>% filter(Census_three_month_May == "firstthreeback") %>%
  rename(firstthreeback_vpdmax = May_trimonthly_vpdmax,
         firstthreeback_ppt = May_trimonthly_ppt_tot,
         firstthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXDec_Feb <- weatherTEX_three_month_may %>% filter(Census_three_month_May == "secondthreeback") %>%
  rename(secondthreeback_vpdmax = May_trimonthly_vpdmax,
         secondthreeback_ppt = May_trimonthly_ppt_tot,
         secondthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXSep_Nov <- weatherTEX_three_month_may %>% filter(Census_three_month_May == "thirdthreeback")%>%
  rename(thirdthreeback_vpdmax = May_trimonthly_vpdmax,
         thirdthreeback_ppt = May_trimonthly_ppt_tot,
         thirdthreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXJun_Aug <- weatherTEX_three_month_may %>% filter(Census_three_month_May == "fourththreeback") %>%
  rename(fourththreeback_vpdmax = May_trimonthly_vpdmax,
         fourththreeback_ppt = May_trimonthly_ppt_tot,
         fourththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each 6 months back from May for Texas
weatherTEXDec_May <- weatherTEX_six_month_may %>% filter(Census_six_month_May == "firstsixback") %>%
  rename(firstsixback_vpdmax = May_hexamonthly_vpdmax,
         firstsixback_ppt = May_hexamonthly_ppt_tot,
         firstsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXJun_Nov <- weatherTEX_six_month_may %>% filter(Census_six_month_May == "secondsixback") %>%
  rename(secondsixback_vpdmax = May_hexamonthly_vpdmax,
         secondsixback_ppt = May_hexamonthly_ppt_tot,
         secondsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")


#Combining demographic data with the weather data
gras <- read.csv("data/ltreb_allspp_2007_2025.csv")

#Grouping species with census timing
grasMayCensus <- gras %>% filter(species == c("FESU","POAL","POSY"))
grasMayCensusTEX <- gras %>% filter(species == "POAU")
grasJulCensus <- gras %>% filter(species == c("ELVI","ELRI","LOAR"))
grasSepCensus <- gras %>% filter(species == "AGPE")

#Combining corresponding weather and demographic data frames

#recombining weather data for May census Indiana
weatherINDAllMay <- left_join(x=weatherINDMayMay, y=weatherINDMayApr, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayMar, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayFeb, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJan, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayDec, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayNov, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayOct, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMaySep, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayAug, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJul, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJun, by= c("censusmonth","CensusYearMay"))

weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMar_May, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDDec_Feb, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDSep_Nov, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDJun_Aug, by= c("censusmonth","CensusYearMay"))

weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDDec_May, by= c("censusmonth","CensusYearMay"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDJun_Nov, by= c("censusmonth","CensusYearMay"))

#combing weather and demographic data for species censused in May Indiana
CombinedMay <- left_join(x=grasMayCensus, y=weatherINDAllMay, by=c("year_t" = "CensusYearMay"))

#recombining weather data for Jul census

weatherINDAllJul <- left_join(x=weatherINDJulJul, y=weatherINDJulJun, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulMay, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulApr, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulMar, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulFeb, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulJan, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulDec, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulNov, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulOct, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulSep, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulAug, by=c("censusmonth","CensusYearJul"))

weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDMay_Jul, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDFeb_Apr, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDNov_Feb, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDAug_Oct, by=c("censusmonth","CensusYearJul"))

weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDFeb_Jul, by=c("censusmonth","CensusYearJul"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDAug_Jan, by=c("censusmonth","CensusYearJul"))

#combing weather and demographic data for species censused in Jul
CombinedJul <- left_join(x=grasJulCensus, y=weatherINDAllJul, by=c("year_t" = "CensusYearJul"))

#recombining weather data for Sep census
weatherINDAllSep <- left_join(x=weatherINDSepSep, y=weatherINDSepAug, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJul, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJun, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepMay, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepApr, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepMar, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepFeb, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJan, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepDec, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepNov, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepOct, by=c("censusmonth","CensusYearSep"))

weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDJul_Sep, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDApr_Jun, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDJan_Mar, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDOct_Dec, by=c("censusmonth","CensusYearSep"))

weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDApr_Sep, by=c("censusmonth","CensusYearSep"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDOct_Mar, by=c("censusmonth","CensusYearSep"))

#combing weather and demographic data for species censused in Sep
CombinedSep <- left_join(x=grasSepCensus, y=weatherINDAllSep, by=c("year_t" = "CensusYearSep"))

#recombining weather data for May census Texas
weatherTEXAllMay <- left_join(x=weatherTEXMayMay, y=weatherTEXMayApr, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayMar, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayFeb, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJan, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayDec, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayNov, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayOct, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMaySep, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayAug, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJul, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJun, by= c("censusmonth","CensusYearMay"))

weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMar_May, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXDec_Feb, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXSep_Nov, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXJun_Aug, by= c("censusmonth","CensusYearMay"))

weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXDec_May, by= c("censusmonth","CensusYearMay"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXJun_Nov, by= c("censusmonth","CensusYearMay"))

#combing weather and demographic data for species censused in May Texas
CombinedMayTEX <- left_join(x=grasMayCensusTEX, y=weatherTEXAllMay, by=c("year_t" = "CensusYearMay"))


#Combining all species and censusmonths of data
CombinedDataSegments <- bind_rows(CombinedMay, CombinedJul, CombinedSep, CombinedMayTEX)


write.csv(CombinedDataSegments, "data/CombinedDataSegments")
