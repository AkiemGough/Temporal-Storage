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

#Creating a lookup vector to automatically translate numeric month differences into text labels
#for one month back labels GOOGLE GEMINI SUGGESTIONS
one_month_back_labels <- c(
  "firstoneback", "secondoneback", "thirdoneback", "fourthoneback", "fifthoneback",
  "sixthoneback", "seventhoneback", "eighthoneback", "ninthoneback", "tenthoneback",
  "eleventhoneback", "twelvthoneback", "thirteenthoneback", "fourteenthoneback",
  "fifteenthoneback", "sixteenthoneback", "seventeenthoneback", "eighteenthoneback",
  "nineteenthoneback", "twentiethoneback", "twentyfirstoneback", "twentysecondoneback",
  "twentythirdoneback", "twentyfourthoneback")

#Creating a lookup vector to automatically translate numeric month differences into text labels
#for three month back labels GOOGLE GEMINI SUGGESTIONS
three_months_back_labels <- c(
  "firstthreeback", "secondthreeback", "thirdthreeback", "fourththreeback", "fifththreeback",
  "sixththreeback", "sevenththreeback", "eighththreeback")

#Creating a lookup vector to automatically translate numeric month differences into text labels
#for six month back labels GOOGLE GEMINI SUGGESTIONS
six_months_back_labels <- c(
  "firstsixback", "secondsixback", "thirdsixback", "fourthsixback")

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

# 3. Dynamically calculate months back (up to 24) using total month math SUGGESTED BY GEMINI
weatherIND <- weatherIND %>%
  mutate(
    # Total months from weather row to May of the Census Year
    diff_May = (CensusYearMay * 12 + 5) - (yearina * 12 + monthina) + 1,
    diff_Jul = (CensusYearJul * 12 + 7) - (yearina * 12 + monthina) + 1,
    diff_Sep = (CensusYearSep * 12 + 9) - (yearina * 12 + monthina) + 1,
    
        # --- KEEPING: Your original 1-month blocks (Optional) ---
    Census_one_month_May = ifelse(diff_May >= 1 & diff_May <= 24, one_month_back_labels[diff_May], NA),
    Census_one_month_Jul = ifelse(diff_Jul >= 1 & diff_Jul <= 24, one_month_back_labels[diff_Jul], NA),
    Census_one_month_Sep = ifelse(diff_Sep >= 1 & diff_Sep <= 24, one_month_back_labels[diff_Sep], NA),
    # --- NEW: 3-Month Census Blocks ---
    # (diff - 1) %/% 3 + 1 converts month 1-3 into index 1, month 4-7 into index 2, etc.
    Census_three_month_May = ifelse(diff_May >= 1 & diff_May <= 24, three_months_back_labels[(diff_May - 1) %/% 3 + 1], NA),
    Census_three_month_Jul = ifelse(diff_Jul >= 1 & diff_Jul <= 24, three_months_back_labels[(diff_Jul - 1) %/% 3 + 1], NA),
    Census_three_month_Sep = ifelse(diff_Sep >= 1 & diff_Sep <= 24, three_months_back_labels[(diff_Sep - 1) %/% 3 + 1], NA),
    # --- NEW: 6-Month Census Blocks ---
    # (diff - 1) %/% 6 + 1 converts month 1-6 into index 1, month 7-12 into index 2, etc.
    Census_six_month_May = ifelse(diff_May >= 1 & diff_May <= 24, six_months_back_labels[(diff_May - 1) %/% 6 + 1], NA),
    Census_six_month_Jul = ifelse(diff_Jul >= 1 & diff_Jul <= 24, six_months_back_labels[(diff_Jul - 1) %/% 6 + 1], NA),
    Census_six_month_Sep = ifelse(diff_Sep >= 1 & diff_Sep <= 24, six_months_back_labels[(diff_Sep - 1) %/% 6 + 1], NA),
  ) %>%
  # Clean up temporary difference columns
  select(-diff_May, -diff_Jul, -diff_Sep)

weatherTEX <- weatherTEX %>%
  mutate(
    # Total months from weather row to May of the Census Year
    diff_May = (CensusYearMay * 12 + 5) - (yearina * 12 + monthina) + 1,
    
    # --- KEEPING: Your original 1-month blocks (Optional) ---
    Census_one_month_May = ifelse(diff_May >= 1 & diff_May <= 24, one_month_back_labels[diff_May], NA),
    # --- NEW: 3-Month Census Blocks ---
    # (diff - 1) %/% 3 + 1 converts month 1-3 into index 1, month 4-7 into index 2, etc.
    Census_three_month_May = ifelse(diff_May >= 1 & diff_May <= 24, three_months_back_labels[(diff_May - 1) %/% 3 + 1], NA),
    # --- NEW: 6-Month Census Blocks ---
    # (diff - 1) %/% 6 + 1 converts month 1-6 into index 1, month 7-12 into index 2, etc.
    Census_six_month_May = ifelse(diff_May >= 1 & diff_May <= 24, six_months_back_labels[(diff_May - 1) %/% 6 + 1], NA)
  ) %>%
  # Clean up temporary difference columns
  select(-diff_May)

#generating summary stats for each one month block
weatherIND_one_month_may <- weatherIND %>%
  filter(!is.na(Census_one_month_May)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearMay, Census_one_month_May) %>%
  summarize(
    May_monthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    May_monthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    May_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherIND_one_month_jul <- weatherIND %>%
  filter(!is.na(Census_one_month_Jul)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearJul, Census_one_month_Jul) %>%
  summarize(
    Jul_monthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    Jul_monthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    Jul_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherIND_one_month_sep <- weatherIND %>%
  filter(!is.na(Census_one_month_Sep)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearSep, Census_one_month_Sep) %>%
  summarize(
    Sep_monthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    Sep_monthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    Sep_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherTEX_one_month_may <- weatherTEX %>%
  filter(!is.na(Census_one_month_May)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearMay, Census_one_month_May) %>%
  summarize(
    May_monthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    May_monthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    May_monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

#generating summary stats for each three month block
weatherIND_three_month_may <- weatherIND %>%
  filter(!is.na(Census_three_month_May)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearMay, Census_three_month_May) %>%
  summarize(
    May_trimonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    May_trimonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    May_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherIND_three_month_jul <- weatherIND %>%
  filter(!is.na(Census_three_month_Jul)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearJul, Census_three_month_Jul) %>%
  summarize(
    Jul_trimonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    Jul_trimonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    Jul_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherIND_three_month_sep <- weatherIND %>%
  filter(!is.na(Census_three_month_Sep)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearSep, Census_three_month_Sep) %>%
  summarize(
    Sep_trimonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    Sep_trimonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    Sep_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherTEX_three_month_may <- weatherTEX %>%
  filter(!is.na(Census_three_month_May)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearMay, Census_three_month_May) %>%
  summarize(
    May_trimonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    May_trimonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    May_trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")


#generating summary stats for each six month block
weatherIND_six_month_may <- weatherIND %>%
  filter(!is.na(Census_six_month_May)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearMay, Census_six_month_May) %>%
  summarize(
    May_hexamonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    May_hexamonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    May_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherIND_six_month_jul <- weatherIND %>%
  filter(!is.na(Census_six_month_Jul)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearJul, Census_six_month_Jul) %>%
  summarize(
    Jul_hexamonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    Jul_hexamonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    Jul_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherIND_six_month_sep <- weatherIND %>%
  filter(!is.na(Census_six_month_Sep)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearSep, Census_six_month_Sep) %>%
  summarize(
    Sep_hexamonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    Sep_hexamonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    Sep_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherTEX_six_month_may <- weatherTEX %>%
  filter(!is.na(Census_six_month_May)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYearMay, Census_six_month_May) %>%
  summarize(
    May_hexamonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    May_hexamonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    May_hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

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

weatherINDMayMay_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "thirteenthoneback") %>%
  rename(thirteenthoneback_vpdmax = May_monthly_vpdmax,
         thirteenthoneback_ppt = May_monthly_ppt_tot,
         thirteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayApr_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "fourteenthoneback") %>%
  rename(fourteenthoneback_vpdmax = May_monthly_vpdmax,
         fourteenthoneback_ppt = May_monthly_ppt_tot,
         fourteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayMar_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "fifteenthoneback") %>%
  rename(fifteenthoneback_vpdmax = May_monthly_vpdmax,
         fifteenthoneback_ppt = May_monthly_ppt_tot,
         fifteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayFeb_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "sixteenthoneback") %>%
  rename(sixteenthoneback_vpdmax = May_monthly_vpdmax,
         sixteenthoneback_ppt = May_monthly_ppt_tot,
         sixteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayJan_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "seventeenthoneback") %>%
  rename(seventeenthoneback_vpdmax = May_monthly_vpdmax,
         seventeenthoneback_ppt = May_monthly_ppt_tot,
         seventeenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayDec_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "eighteenthoneback") %>%
  rename(eighteenthoneback_vpdmax = May_monthly_vpdmax,
         eighteenthoneback_ppt = May_monthly_ppt_tot,
         eighteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayNov_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "nineteenthoneback") %>%
  rename(nineteenthoneback_vpdmax = May_monthly_vpdmax,
         nineteenthoneback_ppt = May_monthly_ppt_tot,
         nineteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayOct_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "twentiethoneback") %>%
  rename(twentiethoneback_vpdmax = May_monthly_vpdmax,
         twentiethoneback_ppt = May_monthly_ppt_tot,
         twentiethoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMaySep_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "twentyfirstoneback") %>%
  rename(twentyfirstoneback_vpdmax = May_monthly_vpdmax,
         twentyfirstoneback_ppt = May_monthly_ppt_tot,
         twentyfirstoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayAug_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "twentysecondoneback") %>%
  rename(twentysecondoneback_vpdmax = May_monthly_vpdmax,
         twentysecondoneback_ppt = May_monthly_ppt_tot,
         twentysecondoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayJul_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "twentythirdoneback") %>%
  rename(twentythirdoneback_vpdmax = May_monthly_vpdmax,
         twentythirdoneback_ppt = May_monthly_ppt_tot,
         twentythirdoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherINDMayJun_lastyear <- weatherIND_one_month_may %>% filter(Census_one_month_May == "twentyfourthoneback") %>%
  rename(twentyfourthoneback_vpdmax = May_monthly_vpdmax,
         twentyfourthoneback_ppt = May_monthly_ppt_tot,
         twentyfourthoneback_tmean = May_monthly_tmean_mean,
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

weatherINDMar_May_lastyear <- weatherIND_three_month_may %>% filter(Census_three_month_May == "fifththreeback") %>%
  rename(fifththreeback_vpdmax = May_trimonthly_vpdmax,
         fifththreeback_ppt = May_trimonthly_ppt_tot,
         fifththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherINDDec_Feb_lastyear <- weatherIND_three_month_may %>% filter(Census_three_month_May == "sixththreeback") %>%
  rename(sixththreeback_vpdmax = May_trimonthly_vpdmax,
         sixththreeback_ppt = May_trimonthly_ppt_tot,
         sixththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherINDSep_Nov_lastyear <- weatherIND_three_month_may %>% filter(Census_three_month_May == "sevenththreeback") %>%
  rename(sevenththreeback_vpdmax = May_trimonthly_vpdmax,
         sevenththreeback_ppt = May_trimonthly_ppt_tot,
         sevenththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherINDJun_Aug_lastyear <- weatherIND_three_month_may %>% filter(Census_three_month_May == "eightthreeback") %>%
  rename(eightthreeback_vpdmax = May_trimonthly_vpdmax,
         eightthreeback_ppt = May_trimonthly_ppt_tot,
         eightthreeback_tmean = May_trimonthly_tmean_mean,
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

weatherINDDec_May_lastyear <- weatherIND_six_month_may %>% filter(Census_six_month_May == "thirdsixback") %>%
  rename(thirdsixback_vpdmax = May_hexamonthly_vpdmax,
         thirdsixback_ppt = May_hexamonthly_ppt_tot,
         thirdsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

weatherINDJun_Nov_lastyear <- weatherIND_six_month_may %>% filter(Census_six_month_May == "fourthsixback") %>%
  rename(fourthsixback_vpdmax = May_hexamonthly_vpdmax,
         fourthsixback_ppt = May_hexamonthly_ppt_tot,
         fourthsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

#making data frames with only necessary data for each one month back from Jul for Indiana
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

weatherINDJulJul_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "thirteenthoneback") %>%
  rename(thirteenthoneback_vpdmax = Jul_monthly_vpdmax,
         thirteenthoneback_ppt = Jul_monthly_ppt_tot,
         thirteenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulJun_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "fourteenthoneback") %>%
  rename(fourteenthoneback_vpdmax = Jul_monthly_vpdmax,
         fourteenthoneback_ppt = Jul_monthly_ppt_tot,
         fourteenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulMay_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "fifteenthoneback") %>%
  rename(fifteenthoneback_vpdmax = Jul_monthly_vpdmax,
         fifteenthoneback_ppt = Jul_monthly_ppt_tot,
         fifteenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulApr_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "sixteenthoneback") %>%
  rename(sixteenthoneback_vpdmax = Jul_monthly_vpdmax,
         sixteenthoneback_ppt = Jul_monthly_ppt_tot,
         sixteenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulMar_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "seventeenthoneback") %>%
  rename(seventeenthoneback_vpdmax = Jul_monthly_vpdmax,
         seventeenthoneback_ppt = Jul_monthly_ppt_tot,
         seventeenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulFeb_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "eighteenthoneback") %>%
  rename(eighteenthoneback_vpdmax = Jul_monthly_vpdmax,
         eighteenthoneback_ppt = Jul_monthly_ppt_tot,
         eighteenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulJan_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "nineteenthoneback") %>%
  rename(nineteenthoneback_vpdmax = Jul_monthly_vpdmax,
         nineteenthoneback_ppt = Jul_monthly_ppt_tot,
         nineteenthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulDec_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "twentiethoneback") %>%
  rename(twentiethoneback_vpdmax = Jul_monthly_vpdmax,
         twentiethoneback_ppt = Jul_monthly_ppt_tot,
         twentiethoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulNov_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "twentyfirstoneback") %>%
  rename(twentyfirstoneback_vpdmax = Jul_monthly_vpdmax,
         twentyfirstoneback_ppt = Jul_monthly_ppt_tot,
         twentyfirstoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulOct_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "twentysecondoneback") %>%
  rename(twentysecondoneback_vpdmax = Jul_monthly_vpdmax,
         twentysecondoneback_ppt = Jul_monthly_ppt_tot,
         twentysecondoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulSep_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "twentythirdoneback") %>%
  rename(twentythirdoneback_vpdmax = Jul_monthly_vpdmax,
         twentythirdoneback_ppt = Jul_monthly_ppt_tot,
         twentythirdoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDJulAug_lastyear <- weatherIND_one_month_jul %>% filter(Census_one_month_Jul == "twentyfourthoneback") %>%
  rename(twentyfourthoneback_vpdmax = Jul_monthly_vpdmax,
         twentyfourthoneback_ppt = Jul_monthly_ppt_tot,
         twentyfourthoneback_tmean = Jul_monthly_tmean_mean,
         censusmonth = Census_one_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each 3 months back from Jul for Indiana
weatherINDMay_Jul <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "firstthreeback") %>%
  rename(firstthreeback_vpdmax = Jul_trimonthly_vpdmax,
         firstthreeback_ppt = Jul_trimonthly_ppt_tot,
         firstthreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDFeb_Apr <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "secondthreeback") %>%
  rename(secondthreeback_vpdmax = Jul_trimonthly_vpdmax,
         secondthreeback_ppt = Jul_trimonthly_ppt_tot,
         secondthreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDNov_Jan <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "thirdthreeback")%>%
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

weatherINDMay_Jul_lastyear <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "fifththreeback") %>%
  rename(fifththreeback_vpdmax = Jul_trimonthly_vpdmax,
         fifththreeback_ppt = Jul_trimonthly_ppt_tot,
         fifththreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDFeb_Apr_lastyear <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "sixththreeback") %>%
  rename(sixththreeback_vpdmax = Jul_trimonthly_vpdmax,
         sixththreeback_ppt = Jul_trimonthly_ppt_tot,
         sixththreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDNov_Jan_lastyear <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "sevenththreeback") %>%
  rename(sevenththreeback_vpdmax = Jul_trimonthly_vpdmax,
         sevenththreeback_ppt = Jul_trimonthly_ppt_tot,
         sevenththreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDAug_Oct_lastyear <- weatherIND_three_month_jul %>% filter(Census_three_month_Jul == "eightthreeback") %>%
  rename(eightthreeback_vpdmax = Jul_trimonthly_vpdmax,
         eightthreeback_ppt = Jul_trimonthly_ppt_tot,
         eightthreeback_tmean = Jul_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each 6 months back from Jul for Indiana
weatherINDFeb_Jul <- weatherIND_six_month_jul %>% filter(Census_six_month_Jul == "firstsixback") %>%
  rename(firstsixback_vpdmax = Jul_hexamonthly_vpdmax,
         firstsixback_ppt = Jul_hexamonthly_ppt_tot,
         firstsixback_tmean = Jul_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDAug_Jan <- weatherIND_six_month_jul %>% filter(Census_six_month_Jul == "secondsixback") %>%
  rename(secondsixback_vpdmax = Jul_hexamonthly_vpdmax,
         secondsixback_ppt = Jul_hexamonthly_ppt_tot,
         secondsixback_tmean = Jul_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDFeb_Jul_lastyear <- weatherIND_six_month_jul %>% filter(Census_six_month_Jul == "thirdsixback") %>%
  rename(thirdsixback_vpdmax = Jul_hexamonthly_vpdmax,
         thirdsixback_ppt = Jul_hexamonthly_ppt_tot,
         thirdsixback_tmean = Jul_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

weatherINDAug_Jan_lastyear <- weatherIND_six_month_jul %>% filter(Census_six_month_Jul == "fourthsixback") %>%
  rename(fourthsixback_vpdmax = Jul_hexamonthly_vpdmax,
         fourthsixback_ppt = Jul_hexamonthly_ppt_tot,
         fourthsixback_tmean = Jul_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Jul) %>%
  mutate(censusmonth = "Jul")

#making data frames with only necessary data for each one month back from Sep for Indiana
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

weatherINDSepSep_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "thirteenthoneback") %>%
  rename(thirteenthoneback_vpdmax = Sep_monthly_vpdmax,
         thirteenthoneback_ppt = Sep_monthly_ppt_tot,
         thirteenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepAug_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "fourteenthoneback") %>%
  rename(fourteenthoneback_vpdmax = Sep_monthly_vpdmax,
         fourteenthoneback_ppt = Sep_monthly_ppt_tot,
         fourteenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepJul_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "fifteenthoneback") %>%
  rename(fifteenthoneback_vpdmax = Sep_monthly_vpdmax,
         fifteenthoneback_ppt = Sep_monthly_ppt_tot,
         fifteenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepJun_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "sixteenthoneback") %>%
  rename(sixteenthoneback_vpdmax = Sep_monthly_vpdmax,
         sixteenthoneback_ppt = Sep_monthly_ppt_tot,
         sixteenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepMay_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "seventeenthoneback") %>%
  rename(seventeenthoneback_vpdmax = Sep_monthly_vpdmax,
         seventeenthoneback_ppt = Sep_monthly_ppt_tot,
         seventeenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepApr_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "eighteenthoneback") %>%
  rename(eighteenthoneback_vpdmax = Sep_monthly_vpdmax,
         eighteenthoneback_ppt = Sep_monthly_ppt_tot,
         eighteenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepMar_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "nineteenthoneback") %>%
  rename(nineteenthoneback_vpdmax = Sep_monthly_vpdmax,
         nineteenthoneback_ppt = Sep_monthly_ppt_tot,
         nineteenthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepFeb_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "twentiethoneback") %>%
  rename(twentiethoneback_vpdmax = Sep_monthly_vpdmax,
         twentiethoneback_ppt = Sep_monthly_ppt_tot,
         twentiethoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepJan_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "twentyfirstoneback") %>%
  rename(twentyfirstoneback_vpdmax = Sep_monthly_vpdmax,
         twentyfirstoneback_ppt = Sep_monthly_ppt_tot,
         twentyfirstoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepDec_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "twentysecondoneback") %>%
  rename(twentysecondoneback_vpdmax = Sep_monthly_vpdmax,
         twentysecondoneback_ppt = Sep_monthly_ppt_tot,
         twentysecondoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepNov_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "twentythirdoneback") %>%
  rename(twentythirdoneback_vpdmax = Sep_monthly_vpdmax,
         twentythirdoneback_ppt = Sep_monthly_ppt_tot,
         twentythirdoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDSepOct_lastyear <- weatherIND_one_month_sep %>% filter(Census_one_month_Sep == "twentyfourthoneback") %>%
  rename(twentyfourthoneback_vpdmax = Sep_monthly_vpdmax,
         twentyfourthoneback_ppt = Sep_monthly_ppt_tot,
         twentyfourthoneback_tmean = Sep_monthly_tmean_mean,
         censusmonth = Census_one_month_Sep) %>%
  mutate(censusmonth = "Sep")

#making data frames with only necessary data for each 3 months back from Sep for Indiana
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

weatherINDJan_Mar <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "thirdthreeback")%>%
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

weatherINDJul_Sep_lastyear <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "fifththreeback") %>%
  rename(fifththreeback_vpdmax = Sep_trimonthly_vpdmax,
         fifththreeback_ppt = Sep_trimonthly_ppt_tot,
         fifththreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDApr_Jun_lastyear <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "sixththreeback") %>%
  rename(sixththreeback_vpdmax = Sep_trimonthly_vpdmax,
         sixththreeback_ppt = Sep_trimonthly_ppt_tot,
         sixththreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDJan_Mar_lastyear <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "sevenththreeback") %>%
  rename(sevenththreeback_vpdmax = Sep_trimonthly_vpdmax,
         sevenththreeback_ppt = Sep_trimonthly_ppt_tot,
         sevenththreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDOct_Dec_lastyear <- weatherIND_three_month_sep %>% filter(Census_three_month_Sep == "eightthreeback") %>%
  rename(eightthreeback_vpdmax = Sep_trimonthly_vpdmax,
         eightthreeback_ppt = Sep_trimonthly_ppt_tot,
         eightthreeback_tmean = Sep_trimonthly_tmean_mean,
         censusmonth = Census_three_month_Sep) %>%
  mutate(censusmonth = "Sep")

#making data frames with only necessary data for each 6 months back from Sep for Indiana
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

weatherINDApr_Sep_lastyear <- weatherIND_six_month_sep %>% filter(Census_six_month_Sep == "thirdsixback") %>%
  rename(thirdsixback_vpdmax = Sep_hexamonthly_vpdmax,
         thirdsixback_ppt = Sep_hexamonthly_ppt_tot,
         thirdsixback_tmean = Sep_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_Sep) %>%
  mutate(censusmonth = "Sep")

weatherINDOct_Mar_lastyear <- weatherIND_six_month_sep %>% filter(Census_six_month_Sep == "fourthsixback") %>%
  rename(fourthsixback_vpdmax = Sep_hexamonthly_vpdmax,
         fourthsixback_ppt = Sep_hexamonthly_ppt_tot,
         fourthsixback_tmean = Sep_hexamonthly_tmean_mean,
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

weatherTEXMayMay_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "thirteenthoneback") %>%
  rename(thirteenthoneback_vpdmax = May_monthly_vpdmax,
         thirteenthoneback_ppt = May_monthly_ppt_tot,
         thirteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayApr_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "fourteenthoneback") %>%
  rename(fourteenthoneback_vpdmax = May_monthly_vpdmax,
         fourteenthoneback_ppt = May_monthly_ppt_tot,
         fourteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayMar_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "fifteenthoneback") %>%
  rename(fifteenthoneback_vpdmax = May_monthly_vpdmax,
         fifteenthoneback_ppt = May_monthly_ppt_tot,
         fifteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayFeb_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "sixteenthoneback") %>%
  rename(sixteenthoneback_vpdmax = May_monthly_vpdmax,
         sixteenthoneback_ppt = May_monthly_ppt_tot,
         sixteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayJan_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "seventeenthoneback") %>%
  rename(seventeenthoneback_vpdmax = May_monthly_vpdmax,
         seventeenthoneback_ppt = May_monthly_ppt_tot,
         seventeenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayDec_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "eighteenthoneback") %>%
  rename(eighteenthoneback_vpdmax = May_monthly_vpdmax,
         eighteenthoneback_ppt = May_monthly_ppt_tot,
         eighteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayNov_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "nineteenthoneback") %>%
  rename(nineteenthoneback_vpdmax = May_monthly_vpdmax,
         nineteenthoneback_ppt = May_monthly_ppt_tot,
         nineteenthoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayOct_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "twentiethoneback") %>%
  rename(twentiethoneback_vpdmax = May_monthly_vpdmax,
         twentiethoneback_ppt = May_monthly_ppt_tot,
         twentiethoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMaySep_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "twentyfirstoneback") %>%
  rename(twentyfirstoneback_vpdmax = May_monthly_vpdmax,
         twentyfirstoneback_ppt = May_monthly_ppt_tot,
         twentyfirstoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayAug_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "twentysecondoneback") %>%
  rename(twentysecondoneback_vpdmax = May_monthly_vpdmax,
         twentysecondoneback_ppt = May_monthly_ppt_tot,
         twentysecondoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayJul_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "twentythirdoneback") %>%
  rename(twentythirdoneback_vpdmax = May_monthly_vpdmax,
         twentythirdoneback_ppt = May_monthly_ppt_tot,
         twentythirdoneback_tmean = May_monthly_tmean_mean,
         censusmonth = Census_one_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXMayJun_lastyear <- weatherTEX_one_month_may %>% filter(Census_one_month_May == "twentyfourthoneback") %>%
  rename(twentyfourthoneback_vpdmax = May_monthly_vpdmax,
         twentyfourthoneback_ppt = May_monthly_ppt_tot,
         twentyfourthoneback_tmean = May_monthly_tmean_mean,
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

weatherTEXMar_May_lastyear <- weatherTEX_three_month_may %>% filter(Census_three_month_May == "fifththreeback") %>%
  rename(fifththreeback_vpdmax = May_trimonthly_vpdmax,
         fifththreeback_ppt = May_trimonthly_ppt_tot,
         fifththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXDec_Feb_lastyear <- weatherTEX_three_month_may %>% filter(Census_three_month_May == "sixththreeback") %>%
  rename(sixththreeback_vpdmax = May_trimonthly_vpdmax,
         sixththreeback_ppt = May_trimonthly_ppt_tot,
         sixththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXSep_Novlastyear <- weatherTEX_three_month_may %>% filter(Census_three_month_May == "sevenththreeback") %>%
  rename(sevenththreeback_vpdmax = May_trimonthly_vpdmax,
         sevenththreeback_ppt = May_trimonthly_ppt_tot,
         sevenththreeback_tmean = May_trimonthly_tmean_mean,
         censusmonth = Census_three_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXJun_Auglastyear <- weatherTEX_three_month_may %>% filter(Census_three_month_May == "eightthreeback") %>%
  rename(eightthreeback_vpdmax = May_trimonthly_vpdmax,
         eightthreeback_ppt = May_trimonthly_ppt_tot,
         eightthreeback_tmean = May_trimonthly_tmean_mean,
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

weatherTEXDec_May <- weatherTEX_six_month_may %>% filter(Census_six_month_May == "thirdsixback") %>%
  rename(thirdsixback_vpdmax = May_hexamonthly_vpdmax,
         thirdsixback_ppt = May_hexamonthly_ppt_tot,
         thirdsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

weatherTEXJun_Nov <- weatherTEX_six_month_may %>% filter(Census_six_month_May == "fourthsixback") %>%
  rename(fourthsixback_vpdmax = May_hexamonthly_vpdmax,
         fourthsixback_ppt = May_hexamonthly_ppt_tot,
         fourthsixback_tmean = May_hexamonthly_tmean_mean,
         censusmonth = Census_six_month_May) %>%
  mutate(censusmonth = "May")

#Combining demographic data with the weather data
gras <- read.csv("data/ltreb_allspp_2007_2025.csv")

#Grouping species with census timing
grasMayCensus <- gras %>% filter(species == c("FESU","POAL","POSY"))
grasMayCensusTEX <- gras %>% filter(species == "POAU")
grasJulCensus <- gras %>% filter(species == c("ELVI","ELRI","LOAR"))
grasSepCensus <- gras %>% filter(species == "AGPE")


