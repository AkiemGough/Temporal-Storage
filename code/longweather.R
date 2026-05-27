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

#fixing date
weatherIND$Date <- as.Date(weatherIND$Date, format = "%m/%d/%Y")
year(weatherIND$Date) <- year(weatherIND$Date) + 2000

weatherTEX$Date <- as.Date(weatherTEX$Date, format = "%m/%d/%Y")
year(weatherTEX$Date) <- year(weatherTEX$Date) + 2000

#extracting month and year from date
weatherIND <- weatherIND %>%
  mutate(
    weather_year = year(Date),
    weather_month = month(Date)
  )

weatherTEX <- weatherTEX %>%
  mutate(
    weather_year = year(Date),
    weather_month = month(Date)
  )

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

# Generate a schedule of all Census periods from 2007 to 2025 GEMINI
census_schedule <- expand.grid(
  CensusYear = 2007:2025,
  CensusMonth = c(5, 7, 9) # May, July, September
) %>%
  mutate(
    # Create a label to identify the type of census group
    CensusGroup = case_when(
      CensusMonth == 5 ~ "May",
      CensusMonth == 7 ~ "Jul",
      CensusMonth == 9 ~ "Sep"))

# FOR INDIANA Join the datasets and map out the lookback structure GEMINI
structured_data <- census_schedule %>%
  # Cross join pairs every census deadline with every weather record
  cross_join(weatherIND) %>%
  mutate(
    # Calculate total month difference between the Census Deadline and the Weather Row
    diff_months = (CensusYear * 12 + CensusMonth) 
    - (weather_year * 12 + weather_month) + 1
  ) %>%
  # Keep ONLY weather data that falls within your 24-month window
  filter(diff_months >= 1 & diff_months <= 24) 

# FOR TEXAS Join the datasets and map out the lookback structure GEMINI
structured_data <- census_schedule %>%
  # Cross join pairs every census deadline with every weather record
  cross_join(weatherTEX) %>%
  mutate(
    # Calculate total month difference between the Census Deadline and the Weather Row
    diff_months = (CensusYear * 12 + CensusMonth) 
    - (weather_year * 12 + weather_month) + 1
  ) %>%
  # Keep ONLY weather data that falls within your 24-month window
  filter(diff_months >= 1 & diff_months <= 24) 

# FOR INDIANA Map the text labels dynamically based on the lookback window
weatherIND <- structured_data %>%
  mutate(
    Census_one_month = one_month_back_labels[diff_months],
    Census_three_month = three_months_back_labels[(diff_months - 1) %/% 3 + 1],
    Census_six_month = six_months_back_labels[(diff_months - 1) %/% 6 + 1]
  ) %>%
  # Clean up temporary difference columns
  select(-diff_months)

# FOR TEXAS Map the text labels dynamically based on the lookback window
weatherTEX <- structured_data %>%
  mutate(
    Census_one_month = one_month_back_labels[diff_months],
    Census_three_month = three_months_back_labels[(diff_months - 1) %/% 3 + 1],
    Census_six_month = six_months_back_labels[(diff_months - 1) %/% 6 + 1]
  ) %>%
  # Clean up temporary difference columns
  select(-diff_months)

#generating summary stats for each one month block
weatherIND_one_month <- weatherIND %>% 
  filter(!is.na(Census_one_month)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYear, CensusGroup, Census_one_month) %>%
  summarize(
    monthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    monthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherTEX_one_month <- weatherTEX %>% 
  filter(!is.na(Census_one_month)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYear, CensusGroup, Census_one_month) %>%
  summarize(
    monthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    monthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    monthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

#generating summary stats for each three month block
weatherIND_three_month <- weatherIND %>% 
  filter(!is.na(Census_three_month)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYear, CensusGroup, Census_three_month) %>%
  summarize(
    trimonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    trimonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherTEX_three_month <- weatherTEX %>% 
  filter(!is.na(Census_three_month)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYear, CensusGroup, Census_three_month) %>%
  summarize(
    trimonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    trimonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    trimonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

#generating summary stats for each six month block
weatherIND_six_month <- weatherIND %>% 
  filter(!is.na(Census_six_month)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYear, CensusGroup, Census_six_month) %>%
  summarize(
    hexamonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    hexamonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

weatherTEX_six_month <- weatherTEX %>% 
  filter(!is.na(Census_six_month)) %>% # SUGGESTED BY GEMINI Excludes data older than 24 months
  group_by(CensusYear, CensusGroup, Census_six_month) %>%
  summarize(
    hexamonthly_vpdmax     = mean(`vpdmax (hPa)`, na.rm = TRUE), 
    hexamonthly_ppt_tot    = sum(`ppt (inches)`, na.rm = TRUE),
    hexamonthly_tmean_mean = mean(`tmean (degrees F)`, na.rm = TRUE),
    .groups = "drop")

#making data frames with only necessary data for each one month back from May for Indiana
weatherINDMayMay <- weatherIND_one_month %>% 
  filter(Census_one_month == "firstoneback" & CensusGroup == "May") %>% 
  rename(firstoneback_vpdmax = monthly_vpdmax,
         firstoneback_ppt = monthly_ppt_tot,
         firstoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayApr <- weatherIND_one_month %>%
  filter(Census_one_month == "secondoneback" & CensusGroup == "May") %>%
  rename(secondoneback_vpdmax = monthly_vpdmax,
         secondoneback_ppt = monthly_ppt_tot,
         secondoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayMar <- weatherIND_one_month %>% 
  filter(Census_one_month == "thirdoneback" & CensusGroup == "May")%>%
  rename(thirdoneback_vpdmax = monthly_vpdmax,
         thirdoneback_ppt = monthly_ppt_tot,
         thirdoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayFeb <- weatherIND_one_month %>%
  filter(Census_one_month == "fourthoneback" & CensusGroup == "May") %>%
  rename(fourthoneback_vpdmax = monthly_vpdmax,
         fourthoneback_ppt = monthly_ppt_tot,
         fourthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayJan <- weatherIND_one_month %>%
  filter(Census_one_month == "fifthoneback" & CensusGroup == "May") %>%
  rename(fifthoneback_vpdmax = monthly_vpdmax,
         fifthoneback_ppt = monthly_ppt_tot,
         fifthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayDec <- weatherIND_one_month %>%
  filter(Census_one_month == "sixthoneback" & CensusGroup == "May") %>%
  rename(sixthoneback_vpdmax = monthly_vpdmax,
         sixthoneback_ppt = monthly_ppt_tot,
         sixthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayNov <- weatherIND_one_month %>%
  filter(Census_one_month == "seventhoneback" & CensusGroup == "May") %>%
  rename(seventhoneback_vpdmax = monthly_vpdmax,
         seventhoneback_ppt = monthly_ppt_tot,
         seventhoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayOct <- weatherIND_one_month %>% 
  filter(Census_one_month == "eighthoneback" & CensusGroup == "May") %>%
  rename(eighthoneback_vpdmax = monthly_vpdmax,
         eighthoneback_ppt = monthly_ppt_tot,
         eighthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMaySep <- weatherIND_one_month %>% 
  filter(Census_one_month == "ninthoneback" & CensusGroup == "May") %>%
  rename(ninthoneback_vpdmax = monthly_vpdmax,
         ninthoneback_ppt = monthly_ppt_tot,
         ninthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayAug <- weatherIND_one_month %>%
  filter(Census_one_month == "tenthoneback" & CensusGroup == "May") %>%
  rename(tenthoneback_vpdmax = monthly_vpdmax,
         tenthoneback_ppt = monthly_ppt_tot,
         tenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayJul <- weatherIND_one_month %>% 
  filter(Census_one_month == "eleventhoneback" & CensusGroup == "May") %>%
  rename(eleventhoneback_vpdmax = monthly_vpdmax,
         eleventhoneback_ppt = monthly_ppt_tot,
         eleventhoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayJun <- weatherIND_one_month %>%
  filter(Census_one_month == "twelvthoneback" & CensusGroup == "May") %>%
  rename(twelvthoneback_vpdmax = monthly_vpdmax,
         twelvthoneback_ppt = monthly_ppt_tot,
         twelvthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayMay_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "thirteenthoneback" & CensusGroup == "May") %>%
  rename(thirteenthoneback_vpdmax = monthly_vpdmax,
         thirteenthoneback_ppt = monthly_ppt_tot,
         thirteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayApr_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "fourteenthoneback" & CensusGroup == "May") %>%
  rename(fourteenthoneback_vpdmax = monthly_vpdmax,
         fourteenthoneback_ppt = monthly_ppt_tot,
         fourteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayMar_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "fifteenthoneback" & CensusGroup == "May") %>%
  rename(fifteenthoneback_vpdmax = monthly_vpdmax,
         fifteenthoneback_ppt = monthly_ppt_tot,
         fifteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayFeb_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "sixteenthoneback" & CensusGroup == "May") %>%
  rename(sixteenthoneback_vpdmax = monthly_vpdmax,
         sixteenthoneback_ppt = monthly_ppt_tot,
         sixteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayJan_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "seventeenthoneback" & CensusGroup == "May") %>%
  rename(seventeenthoneback_vpdmax = monthly_vpdmax,
         seventeenthoneback_ppt = monthly_ppt_tot,
         seventeenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayDec_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "eighteenthoneback" & CensusGroup == "May") %>%
  rename(eighteenthoneback_vpdmax = monthly_vpdmax,
         eighteenthoneback_ppt = monthly_ppt_tot,
         eighteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayNov_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "nineteenthoneback" & CensusGroup == "May") %>%
  rename(nineteenthoneback_vpdmax = monthly_vpdmax,
         nineteenthoneback_ppt = monthly_ppt_tot,
         nineteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayOct_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "twentiethoneback" & CensusGroup == "May") %>%
  rename(twentiethoneback_vpdmax = monthly_vpdmax,
         twentiethoneback_ppt = monthly_ppt_tot,
         twentiethoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMaySep_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "twentyfirstoneback" & CensusGroup == "May") %>%
  rename(twentyfirstoneback_vpdmax = monthly_vpdmax,
         twentyfirstoneback_ppt = monthly_ppt_tot,
         twentyfirstoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayAug_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "twentysecondoneback" & CensusGroup == "May") %>%
  rename(twentysecondoneback_vpdmax = monthly_vpdmax,
         twentysecondoneback_ppt = monthly_ppt_tot,
         twentysecondoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayJul_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "twentythirdoneback" & CensusGroup == "May") %>%
  rename(twentythirdoneback_vpdmax = monthly_vpdmax,
         twentythirdoneback_ppt = monthly_ppt_tot,
         twentythirdoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDMayJun_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "twentyfourthoneback" & CensusGroup == "May") %>%
  rename(twentyfourthoneback_vpdmax = monthly_vpdmax,
         twentyfourthoneback_ppt = monthly_ppt_tot,
         twentyfourthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

#making data frames with only necessary data for each 3 months back from May for Indiana
weatherINDMar_May <- weatherIND_three_month %>% 
  filter(Census_three_month == "firstthreeback" & CensusGroup == "May") %>%
  rename(firstthreeback_vpdmax = trimonthly_vpdmax,
         firstthreeback_ppt = trimonthly_ppt_tot,
         firstthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDDec_Feb <- weatherIND_three_month %>% 
  filter(Census_three_month == "secondthreeback" & CensusGroup == "May") %>%
  rename(secondthreeback_vpdmax = trimonthly_vpdmax,
         secondthreeback_ppt = trimonthly_ppt_tot,
         secondthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDSep_Nov <- weatherIND_three_month %>% 
  filter(Census_three_month == "thirdthreeback" & CensusGroup == "May") %>%
  rename(thirdthreeback_vpdmax = trimonthly_vpdmax,
         thirdthreeback_ppt = trimonthly_ppt_tot,
         thirdthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDJun_Aug <- weatherIND_three_month %>% 
  filter(Census_three_month == "fourththreeback" & CensusGroup == "May") %>%
  rename(fourththreeback_vpdmax = trimonthly_vpdmax,
         fourththreeback_ppt = trimonthly_ppt_tot,
         fourththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDMar_May_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "fifththreeback" & CensusGroup == "May") %>%
  rename(fifththreeback_vpdmax = trimonthly_vpdmax,
         fifththreeback_ppt = trimonthly_ppt_tot,
         fifththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDDec_Feb_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "sixththreeback" & CensusGroup == "May") %>%
  rename(sixththreeback_vpdmax = trimonthly_vpdmax,
         sixththreeback_ppt = trimonthly_ppt_tot,
         sixththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDSep_Nov_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "sevenththreeback" & CensusGroup == "May") %>%
  rename(sevenththreeback_vpdmax = trimonthly_vpdmax,
         sevenththreeback_ppt = trimonthly_ppt_tot,
         sevenththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDJun_Aug_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "eighththreeback" & CensusGroup == "May") %>%
  rename(eighththreeback_vpdmax = trimonthly_vpdmax,
         eighththreeback_ppt = trimonthly_ppt_tot,
         eighththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

#making data frames with only necessary data for each 6 months back from May for Indiana
weatherINDDec_May <- weatherIND_six_month %>%
  filter(Census_six_month == "firstsixback" & CensusGroup == "May") %>%
  rename(firstsixback_vpdmax = hexamonthly_vpdmax,
         firstsixback_ppt = hexamonthly_ppt_tot,
         firstsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDJun_Nov <- weatherIND_six_month %>% 
  filter(Census_six_month == "secondsixback" & CensusGroup == "May") %>%
  rename(secondsixback_vpdmax = hexamonthly_vpdmax,
         secondsixback_ppt = hexamonthly_ppt_tot,
         secondsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDDec_May_lastyear <- weatherIND_six_month %>%
  filter(Census_six_month == "thirdsixback" & CensusGroup == "May") %>%
  rename(thirdsixback_vpdmax = hexamonthly_vpdmax,
         thirdsixback_ppt = hexamonthly_ppt_tot,
         thirdsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDJun_Nov_lastyear <- weatherIND_six_month %>% 
  filter(Census_six_month == "fourthsixback" & CensusGroup == "May") %>%
  rename(fourthsixback_vpdmax = hexamonthly_vpdmax,
         fourthsixback_ppt = hexamonthly_ppt_tot,
         fourthsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

#making data frames with only necessary data for each one month back from Jul for Indiana
weatherINDJulJul <- weatherIND_one_month %>%
  filter(Census_one_month == "firstoneback" & CensusGroup == "Jul") %>%
  rename(firstoneback_vpdmax = monthly_vpdmax,
         firstoneback_ppt = monthly_ppt_tot,
         firstoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulJun <- weatherIND_one_month %>% 
  filter(Census_one_month == "secondoneback" & CensusGroup == "Jul") %>%
  rename(secondoneback_vpdmax = monthly_vpdmax,
         secondoneback_ppt = monthly_ppt_tot,
         secondoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulMay <- weatherIND_one_month %>% 
  filter(Census_one_month == "thirdoneback" & CensusGroup == "Jul")%>%
  rename(thirdoneback_vpdmax = monthly_vpdmax,
         thirdoneback_ppt = monthly_ppt_tot,
         thirdoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulApr <- weatherIND_one_month %>% 
  filter(Census_one_month == "fourthoneback" & CensusGroup == "Jul") %>%
  rename(fourthoneback_vpdmax = monthly_vpdmax,
         fourthoneback_ppt = monthly_ppt_tot,
         fourthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulMar <- weatherIND_one_month %>% 
  filter(Census_one_month == "fifthoneback" & CensusGroup == "Jul") %>%
  rename(fifthoneback_vpdmax = monthly_vpdmax,
         fifthoneback_ppt = monthly_ppt_tot,
         fifthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulFeb <- weatherIND_one_month %>%
  filter(Census_one_month == "sixthoneback" & CensusGroup == "Jul") %>%
  rename(sixthoneback_vpdmax = monthly_vpdmax,
         sixthoneback_ppt = monthly_ppt_tot,
         sixthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulJan <- weatherIND_one_month %>% 
  filter(Census_one_month == "seventhoneback" & CensusGroup == "Jul") %>%
  rename(seventhoneback_vpdmax = monthly_vpdmax,
         seventhoneback_ppt = monthly_ppt_tot,
         seventhoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulDec <- weatherIND_one_month %>% 
  filter(Census_one_month == "eighthoneback" & CensusGroup == "Jul") %>%
  rename(eighthoneback_vpdmax = monthly_vpdmax,
         eighthoneback_ppt = monthly_ppt_tot,
         eighthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulNov <- weatherIND_one_month %>%
  filter(Census_one_month == "ninthoneback" & CensusGroup == "Jul") %>%
  rename(ninthoneback_vpdmax = monthly_vpdmax,
         ninthoneback_ppt = monthly_ppt_tot,
         ninthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulOct <- weatherIND_one_month %>% 
  filter(Census_one_month == "tenthoneback" & CensusGroup == "Jul") %>%
  rename(tenthoneback_vpdmax = monthly_vpdmax,
         tenthoneback_ppt = monthly_ppt_tot,
         tenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulSep <- weatherIND_one_month %>%
  filter(Census_one_month == "eleventhoneback" & CensusGroup == "Jul") %>%
  rename(eleventhoneback_vpdmax = monthly_vpdmax,
         eleventhoneback_ppt = monthly_ppt_tot,
         eleventhoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulAug <- weatherIND_one_month %>% 
  filter(Census_one_month == "twelvthoneback" & CensusGroup == "Jul") %>%
  rename(twelvthoneback_vpdmax = monthly_vpdmax,
         twelvthoneback_ppt = monthly_ppt_tot,
         twelvthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulJul_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "thirteenthoneback" & CensusGroup == "Jul") %>%
  rename(thirteenthoneback_vpdmax = monthly_vpdmax,
         thirteenthoneback_ppt = monthly_ppt_tot,
         thirteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulJun_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "fourteenthoneback" & CensusGroup == "Jul") %>%
  rename(fourteenthoneback_vpdmax = monthly_vpdmax,
         fourteenthoneback_ppt = monthly_ppt_tot,
         fourteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulMay_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "fifteenthoneback" & CensusGroup == "Jul") %>%
  rename(fifteenthoneback_vpdmax = monthly_vpdmax,
         fifteenthoneback_ppt = monthly_ppt_tot,
         fifteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulApr_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "sixteenthoneback" & CensusGroup == "Jul") %>%
  rename(sixteenthoneback_vpdmax = monthly_vpdmax,
         sixteenthoneback_ppt = monthly_ppt_tot,
         sixteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulMar_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "seventeenthoneback" & CensusGroup == "Jul") %>%
  rename(seventeenthoneback_vpdmax = monthly_vpdmax,
         seventeenthoneback_ppt = monthly_ppt_tot,
         seventeenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulFeb_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "eighteenthoneback" & CensusGroup == "Jul") %>%
  rename(eighteenthoneback_vpdmax = monthly_vpdmax,
         eighteenthoneback_ppt = monthly_ppt_tot,
         eighteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulJan_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "nineteenthoneback" & CensusGroup == "Jul") %>%
  rename(nineteenthoneback_vpdmax = monthly_vpdmax,
         nineteenthoneback_ppt = monthly_ppt_tot,
         nineteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulDec_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "twentiethoneback" & CensusGroup == "Jul") %>%
  rename(twentiethoneback_vpdmax = monthly_vpdmax,
         twentiethoneback_ppt = monthly_ppt_tot,
         twentiethoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulNov_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "twentyfirstoneback" & CensusGroup == "Jul") %>%
  rename(twentyfirstoneback_vpdmax = monthly_vpdmax,
         twentyfirstoneback_ppt = monthly_ppt_tot,
         twentyfirstoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulOct_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "twentysecondoneback" & CensusGroup == "Jul") %>%
  rename(twentysecondoneback_vpdmax = monthly_vpdmax,
         twentysecondoneback_ppt = monthly_ppt_tot,
         twentysecondoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulSep_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "twentythirdoneback" & CensusGroup == "Jul") %>%
  rename(twentythirdoneback_vpdmax = monthly_vpdmax,
         twentythirdoneback_ppt = monthly_ppt_tot,
         twentythirdoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDJulAug_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "twentyfourthoneback" & CensusGroup == "Jul") %>%
  rename(twentyfourthoneback_vpdmax = monthly_vpdmax,
         twentyfourthoneback_ppt = monthly_ppt_tot,
         twentyfourthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

#making data frames with only necessary data for each 3 months back from Jul for Indiana
weatherINDMay_Jul <- weatherIND_three_month %>% 
  filter(Census_three_month == "firstthreeback" & CensusGroup == "Jul") %>%
  rename(firstthreeback_vpdmax = trimonthly_vpdmax,
         firstthreeback_ppt = trimonthly_ppt_tot,
         firstthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDFeb_Apr <- weatherIND_three_month %>%
  filter(Census_three_month == "secondthreeback" & CensusGroup == "Jul") %>%
  rename(secondthreeback_vpdmax = trimonthly_vpdmax,
         secondthreeback_ppt = trimonthly_ppt_tot,
         secondthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDNov_Jan <- weatherIND_three_month %>%
  filter(Census_three_month == "thirdthreeback" & CensusGroup == "Jul") %>%
  rename(thirdthreeback_vpdmax = trimonthly_vpdmax,
         thirdthreeback_ppt = trimonthly_ppt_tot,
         thirdthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDAug_Oct <- weatherIND_three_month %>% 
  filter(Census_three_month == "fourththreeback" & CensusGroup == "Jul") %>%
  rename(fourththreeback_vpdmax = trimonthly_vpdmax,
         fourththreeback_ppt = trimonthly_ppt_tot,
         fourththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDMay_Jul_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "fifththreeback" & CensusGroup == "Jul") %>%
  rename(fifththreeback_vpdmax = trimonthly_vpdmax,
         fifththreeback_ppt = trimonthly_ppt_tot,
         fifththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDFeb_Apr_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "sixththreeback" & CensusGroup == "Jul") %>%
  rename(sixththreeback_vpdmax = trimonthly_vpdmax,
         sixththreeback_ppt = trimonthly_ppt_tot,
         sixththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDNov_Jan_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "sevenththreeback" & CensusGroup == "Jul") %>%
  rename(sevenththreeback_vpdmax = trimonthly_vpdmax,
         sevenththreeback_ppt = trimonthly_ppt_tot,
         sevenththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDAug_Oct_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "eighththreeback" & CensusGroup == "Jul") %>%
  rename(eighththreeback_vpdmax = trimonthly_vpdmax,
         eighththreeback_ppt = trimonthly_ppt_tot,
         eighththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

#making data frames with only necessary data for each 6 months back from Jul for Indiana
weatherINDFeb_Jul <- weatherIND_six_month %>%
  filter(Census_six_month == "firstsixback" & CensusGroup == "Jul") %>%
  rename(firstsixback_vpdmax = hexamonthly_vpdmax,
         firstsixback_ppt = hexamonthly_ppt_tot,
         firstsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDAug_Jan <- weatherIND_six_month %>% 
  filter(Census_six_month == "secondsixback" & CensusGroup == "Jul") %>%
  rename(secondsixback_vpdmax = hexamonthly_vpdmax,
         secondsixback_ppt = hexamonthly_ppt_tot,
         secondsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDFeb_Jul_lastyear <- weatherIND_six_month %>%
  filter(Census_six_month == "thirdsixback" & CensusGroup == "Jul") %>%
  rename(thirdsixback_vpdmax = hexamonthly_vpdmax,
         thirdsixback_ppt = hexamonthly_ppt_tot,
         thirdsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDAug_Jan_lastyear <- weatherIND_six_month %>% 
  filter(Census_six_month == "fourthsixback" & CensusGroup == "Jul") %>%
  rename(fourthsixback_vpdmax = hexamonthly_vpdmax,
         fourthsixback_ppt = hexamonthly_ppt_tot,
         fourthsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

#making data frames with only necessary data for each one month back from Sep for Indiana
weatherINDSepSep <- weatherIND_one_month %>%
  filter(Census_one_month == "firstoneback" & CensusGroup == "Sep") %>%
  rename(firstoneback_vpdmax = monthly_vpdmax,
         firstoneback_ppt = monthly_ppt_tot,
         firstoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepAug <- weatherIND_one_month %>%
  filter(Census_one_month == "secondoneback" & CensusGroup == "Sep") %>%
  rename(secondoneback_vpdmax = monthly_vpdmax,
         secondoneback_ppt = monthly_ppt_tot,
         secondoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepJul <- weatherIND_one_month %>% 
  filter(Census_one_month == "thirdoneback" & CensusGroup == "Sep") %>%
  rename(thirdoneback_vpdmax = monthly_vpdmax,
         thirdoneback_ppt = monthly_ppt_tot,
         thirdoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepJun <- weatherIND_one_month %>% 
  filter(Census_one_month == "fourthoneback" & CensusGroup == "Sep") %>%
  rename(fourthoneback_vpdmax = monthly_vpdmax,
         fourthoneback_ppt = monthly_ppt_tot,
         fourthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepMay <- weatherIND_one_month %>%
  filter(Census_one_month == "fifthoneback" & CensusGroup == "Sep") %>%
  rename(fifthoneback_vpdmax = monthly_vpdmax,
         fifthoneback_ppt = monthly_ppt_tot,
         fifthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepApr <- weatherIND_one_month %>% 
  filter(Census_one_month == "sixthoneback" & CensusGroup == "Sep") %>%
  rename(sixthoneback_vpdmax = monthly_vpdmax,
         sixthoneback_ppt = monthly_ppt_tot,
         sixthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepMar <- weatherIND_one_month %>%
  filter(Census_one_month == "seventhoneback" & CensusGroup == "Sep") %>%
  rename(seventhoneback_vpdmax = monthly_vpdmax,
         seventhoneback_ppt = monthly_ppt_tot,
         seventhoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepFeb <- weatherIND_one_month %>% 
  filter(Census_one_month == "eighthoneback" & CensusGroup == "Sep") %>%
  rename(eighthoneback_vpdmax = monthly_vpdmax,
         eighthoneback_ppt = monthly_ppt_tot,
         eighthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepJan <- weatherIND_one_month %>%
  filter(Census_one_month == "ninthoneback" & CensusGroup == "Sep") %>%
  rename(ninthoneback_vpdmax = monthly_vpdmax,
         ninthoneback_ppt = monthly_ppt_tot,
         ninthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepDec <- weatherIND_one_month %>%
  filter(Census_one_month == "tenthoneback" & CensusGroup == "Sep") %>%
  rename(tenthoneback_vpdmax = monthly_vpdmax,
         tenthoneback_ppt = monthly_ppt_tot,
         tenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepNov <- weatherIND_one_month %>%
  filter(Census_one_month == "eleventhoneback" & CensusGroup == "Sep") %>%
  rename(eleventhoneback_vpdmax = monthly_vpdmax,
         eleventhoneback_ppt = monthly_ppt_tot,
         eleventhoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepOct <- weatherIND_one_month %>% 
  filter(Census_one_month == "twelvthoneback" & CensusGroup == "Sep") %>%
  rename(twelvthoneback_vpdmax = monthly_vpdmax,
         twelvthoneback_ppt = monthly_ppt_tot,
         twelvthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepSep_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "thirteenthoneback" & CensusGroup == "Sep") %>%
  rename(thirteenthoneback_vpdmax = monthly_vpdmax,
         thirteenthoneback_ppt = monthly_ppt_tot,
         thirteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepAug_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "fourteenthoneback" & CensusGroup == "Sep") %>%
  rename(fourteenthoneback_vpdmax = monthly_vpdmax,
         fourteenthoneback_ppt = monthly_ppt_tot,
         fourteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepJul_lastyear <- weatherIND_one_month %>%
  filter(Census_one_month == "fifteenthoneback" & CensusGroup == "Sep") %>%
  rename(fifteenthoneback_vpdmax = monthly_vpdmax,
         fifteenthoneback_ppt = monthly_ppt_tot,
         fifteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepJun_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "sixteenthoneback" & CensusGroup == "Sep") %>%
  rename(sixteenthoneback_vpdmax = monthly_vpdmax,
         sixteenthoneback_ppt = monthly_ppt_tot,
         sixteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepMay_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "seventeenthoneback" & CensusGroup == "Sep") %>%
  rename(seventeenthoneback_vpdmax = monthly_vpdmax,
         seventeenthoneback_ppt = monthly_ppt_tot,
         seventeenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepApr_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "eighteenthoneback" & CensusGroup == "Sep") %>%
  rename(eighteenthoneback_vpdmax = monthly_vpdmax,
         eighteenthoneback_ppt = monthly_ppt_tot,
         eighteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepMar_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "nineteenthoneback" & CensusGroup == "Sep") %>%
  rename(nineteenthoneback_vpdmax = monthly_vpdmax,
         nineteenthoneback_ppt = monthly_ppt_tot,
         nineteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepFeb_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "twentiethoneback" & CensusGroup == "Sep") %>%
  rename(twentiethoneback_vpdmax = monthly_vpdmax,
         twentiethoneback_ppt = monthly_ppt_tot,
         twentiethoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepJan_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "twentyfirstoneback" & CensusGroup == "Sep") %>%
  rename(twentyfirstoneback_vpdmax = monthly_vpdmax,
         twentyfirstoneback_ppt = monthly_ppt_tot,
         twentyfirstoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepDec_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "twentysecondoneback" & CensusGroup == "Sep") %>%
  rename(twentysecondoneback_vpdmax = monthly_vpdmax,
         twentysecondoneback_ppt = monthly_ppt_tot,
         twentysecondoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepNov_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "twentythirdoneback" & CensusGroup == "Sep") %>%
  rename(twentythirdoneback_vpdmax = monthly_vpdmax,
         twentythirdoneback_ppt = monthly_ppt_tot,
         twentythirdoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherINDSepOct_lastyear <- weatherIND_one_month %>% 
  filter(Census_one_month == "twentyfourthoneback" & CensusGroup == "Sep") %>%
  rename(twentyfourthoneback_vpdmax = monthly_vpdmax,
         twentyfourthoneback_ppt = monthly_ppt_tot,
         twentyfourthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

#making data frames with only necessary data for each 3 months back from Sep for Indiana
weatherINDJul_Sep <- weatherIND_three_month %>% 
  filter(Census_three_month == "firstthreeback" & CensusGroup == "Sep") %>%
  rename(firstthreeback_vpdmax = trimonthly_vpdmax,
         firstthreeback_ppt = trimonthly_ppt_tot,
         firstthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDApr_Jun <- weatherIND_three_month %>% 
  filter(Census_three_month == "secondthreeback" & CensusGroup == "Sep") %>%
  rename(secondthreeback_vpdmax = trimonthly_vpdmax,
         secondthreeback_ppt = trimonthly_ppt_tot,
         secondthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDJan_Mar <- weatherIND_three_month %>% 
  filter(Census_three_month == "thirdthreeback" & CensusGroup == "Sep") %>%
  rename(thirdthreeback_vpdmax = trimonthly_vpdmax,
         thirdthreeback_ppt = trimonthly_ppt_tot,
         thirdthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDOct_Dec <- weatherIND_three_month %>% 
  filter(Census_three_month == "fourththreeback" & CensusGroup == "Sep") %>%
  rename(fourththreeback_vpdmax = trimonthly_vpdmax,
         fourththreeback_ppt = trimonthly_ppt_tot,
         fourththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDJul_Sep_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "fifththreeback" & CensusGroup == "Sep") %>%
  rename(fifththreeback_vpdmax = trimonthly_vpdmax,
         fifththreeback_ppt = trimonthly_ppt_tot,
         fifththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDApr_Jun_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "sixththreeback" & CensusGroup == "Sep") %>%
  rename(sixththreeback_vpdmax = trimonthly_vpdmax,
         sixththreeback_ppt = trimonthly_ppt_tot,
         sixththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDJan_Mar_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "sevenththreeback" & CensusGroup == "Sep") %>%
  rename(sevenththreeback_vpdmax = trimonthly_vpdmax,
         sevenththreeback_ppt = trimonthly_ppt_tot,
         sevenththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherINDOct_Dec_lastyear <- weatherIND_three_month %>% 
  filter(Census_three_month == "eighththreeback" & CensusGroup == "Sep") %>%
  rename(eighththreeback_vpdmax = trimonthly_vpdmax,
         eighththreeback_ppt = trimonthly_ppt_tot,
         eighththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

#making data frames with only necessary data for each 6 months back from Sep for Indiana
weatherINDApr_Sep <- weatherIND_six_month %>% 
  filter(Census_six_month == "firstsixback" & CensusGroup == "Sep") %>%
  rename(firstsixback_vpdmax = hexamonthly_vpdmax,
         firstsixback_ppt = hexamonthly_ppt_tot,
         firstsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDOct_Mar <- weatherIND_six_month %>% 
  filter(Census_six_month == "secondsixback" & CensusGroup == "Sep") %>%
  rename(secondsixback_vpdmax = hexamonthly_vpdmax,
         secondsixback_ppt = hexamonthly_ppt_tot,
         secondsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDApr_Sep_lastyear <- weatherIND_six_month %>%
  filter(Census_six_month == "thirdsixback" & CensusGroup == "Sep") %>%
  rename(thirdsixback_vpdmax = hexamonthly_vpdmax,
         thirdsixback_ppt = hexamonthly_ppt_tot,
         thirdsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherINDOct_Mar_lastyear <- weatherIND_six_month %>%
  filter(Census_six_month == "fourthsixback" & CensusGroup == "Sep") %>%
  rename(fourthsixback_vpdmax = hexamonthly_vpdmax,
         fourthsixback_ppt = hexamonthly_ppt_tot,
         fourthsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

#making data frames with only necessary data for each one month back from May for Texas
weatherTEXMayMay <- weatherTEX_one_month %>% 
  filter(Census_one_month == "firstoneback" & CensusGroup == "May") %>% 
  rename(firstoneback_vpdmax = monthly_vpdmax,
         firstoneback_ppt = monthly_ppt_tot,
         firstoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayApr <- weatherTEX_one_month %>%
  filter(Census_one_month == "secondoneback" & CensusGroup == "May") %>%
  rename(secondoneback_vpdmax = monthly_vpdmax,
         secondoneback_ppt = monthly_ppt_tot,
         secondoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayMar <- weatherTEX_one_month %>% 
  filter(Census_one_month == "thirdoneback" & CensusGroup == "May")%>%
  rename(thirdoneback_vpdmax = monthly_vpdmax,
         thirdoneback_ppt = monthly_ppt_tot,
         thirdoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayFeb <- weatherTEX_one_month %>%
  filter(Census_one_month == "fourthoneback" & CensusGroup == "May") %>%
  rename(fourthoneback_vpdmax = monthly_vpdmax,
         fourthoneback_ppt = monthly_ppt_tot,
         fourthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayJan <- weatherTEX_one_month %>%
  filter(Census_one_month == "fifthoneback" & CensusGroup == "May") %>%
  rename(fifthoneback_vpdmax = monthly_vpdmax,
         fifthoneback_ppt = monthly_ppt_tot,
         fifthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayDec <- weatherTEX_one_month %>%
  filter(Census_one_month == "sixthoneback" & CensusGroup == "May") %>%
  rename(sixthoneback_vpdmax = monthly_vpdmax,
         sixthoneback_ppt = monthly_ppt_tot,
         sixthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayNov <- weatherTEX_one_month %>%
  filter(Census_one_month == "seventhoneback" & CensusGroup == "May") %>%
  rename(seventhoneback_vpdmax = monthly_vpdmax,
         seventhoneback_ppt = monthly_ppt_tot,
         seventhoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayOct <- weatherTEX_one_month %>% 
  filter(Census_one_month == "eighthoneback" & CensusGroup == "May") %>%
  rename(eighthoneback_vpdmax = monthly_vpdmax,
         eighthoneback_ppt = monthly_ppt_tot,
         eighthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMaySep <- weatherTEX_one_month %>% 
  filter(Census_one_month == "ninthoneback" & CensusGroup == "May") %>%
  rename(ninthoneback_vpdmax = monthly_vpdmax,
         ninthoneback_ppt = monthly_ppt_tot,
         ninthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayAug <- weatherTEX_one_month %>%
  filter(Census_one_month == "tenthoneback" & CensusGroup == "May") %>%
  rename(tenthoneback_vpdmax = monthly_vpdmax,
         tenthoneback_ppt = monthly_ppt_tot,
         tenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayJul <- weatherTEX_one_month %>% 
  filter(Census_one_month == "eleventhoneback" & CensusGroup == "May") %>%
  rename(eleventhoneback_vpdmax = monthly_vpdmax,
         eleventhoneback_ppt = monthly_ppt_tot,
         eleventhoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayJun <- weatherTEX_one_month %>%
  filter(Census_one_month == "twelvthoneback" & CensusGroup == "May") %>%
  rename(twelvthoneback_vpdmax = monthly_vpdmax,
         twelvthoneback_ppt = monthly_ppt_tot,
         twelvthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayMay_lastyear <- weatherTEX_one_month %>% 
  filter(Census_one_month == "thirteenthoneback" & CensusGroup == "May") %>%
  rename(thirteenthoneback_vpdmax = monthly_vpdmax,
         thirteenthoneback_ppt = monthly_ppt_tot,
         thirteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayApr_lastyear <- weatherTEX_one_month %>% 
  filter(Census_one_month == "fourteenthoneback" & CensusGroup == "May") %>%
  rename(fourteenthoneback_vpdmax = monthly_vpdmax,
         fourteenthoneback_ppt = monthly_ppt_tot,
         fourteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayMar_lastyear <- weatherTEX_one_month %>% 
  filter(Census_one_month == "fifteenthoneback" & CensusGroup == "May") %>%
  rename(fifteenthoneback_vpdmax = monthly_vpdmax,
         fifteenthoneback_ppt = monthly_ppt_tot,
         fifteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayFeb_lastyear <- weatherTEX_one_month %>%
  filter(Census_one_month == "sixteenthoneback" & CensusGroup == "May") %>%
  rename(sixteenthoneback_vpdmax = monthly_vpdmax,
         sixteenthoneback_ppt = monthly_ppt_tot,
         sixteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayJan_lastyear <- weatherTEX_one_month %>%
  filter(Census_one_month == "seventeenthoneback" & CensusGroup == "May") %>%
  rename(seventeenthoneback_vpdmax = monthly_vpdmax,
         seventeenthoneback_ppt = monthly_ppt_tot,
         seventeenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayDec_lastyear <- weatherTEX_one_month %>% 
  filter(Census_one_month == "eighteenthoneback" & CensusGroup == "May") %>%
  rename(eighteenthoneback_vpdmax = monthly_vpdmax,
         eighteenthoneback_ppt = monthly_ppt_tot,
         eighteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayNov_lastyear <- weatherTEX_one_month %>%
  filter(Census_one_month == "nineteenthoneback" & CensusGroup == "May") %>%
  rename(nineteenthoneback_vpdmax = monthly_vpdmax,
         nineteenthoneback_ppt = monthly_ppt_tot,
         nineteenthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayOct_lastyear <- weatherTEX_one_month %>%
  filter(Census_one_month == "twentiethoneback" & CensusGroup == "May") %>%
  rename(twentiethoneback_vpdmax = monthly_vpdmax,
         twentiethoneback_ppt = monthly_ppt_tot,
         twentiethoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMaySep_lastyear <- weatherTEX_one_month %>% 
  filter(Census_one_month == "twentyfirstoneback" & CensusGroup == "May") %>%
  rename(twentyfirstoneback_vpdmax = monthly_vpdmax,
         twentyfirstoneback_ppt = monthly_ppt_tot,
         twentyfirstoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayAug_lastyear <- weatherTEX_one_month %>%
  filter(Census_one_month == "twentysecondoneback" & CensusGroup == "May") %>%
  rename(twentysecondoneback_vpdmax = monthly_vpdmax,
         twentysecondoneback_ppt = monthly_ppt_tot,
         twentysecondoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayJul_lastyear <- weatherTEX_one_month %>%
  filter(Census_one_month == "twentythirdoneback" & CensusGroup == "May") %>%
  rename(twentythirdoneback_vpdmax = monthly_vpdmax,
         twentythirdoneback_ppt = monthly_ppt_tot,
         twentythirdoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

weatherTEXMayJun_lastyear <- weatherTEX_one_month %>%
  filter(Census_one_month == "twentyfourthoneback" & CensusGroup == "May") %>%
  rename(twentyfourthoneback_vpdmax = monthly_vpdmax,
         twentyfourthoneback_ppt = monthly_ppt_tot,
         twentyfourthoneback_tmean = monthly_tmean_mean) %>% select(-Census_one_month)

#making data frames with only necessary data for each 3 months back from May for Texas
weatherTEXMar_May <- weatherTEX_three_month %>% 
  filter(Census_three_month == "firstthreeback" & CensusGroup == "May") %>%
  rename(firstthreeback_vpdmax = trimonthly_vpdmax,
         firstthreeback_ppt = trimonthly_ppt_tot,
         firstthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherTEXDec_Feb <- weatherTEX_three_month %>% 
  filter(Census_three_month == "secondthreeback" & CensusGroup == "May") %>%
  rename(secondthreeback_vpdmax = trimonthly_vpdmax,
         secondthreeback_ppt = trimonthly_ppt_tot,
         secondthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherTEXSep_Nov <- weatherTEX_three_month %>% 
  filter(Census_three_month == "thirdthreeback" & CensusGroup == "May") %>%
  rename(thirdthreeback_vpdmax = trimonthly_vpdmax,
         thirdthreeback_ppt = trimonthly_ppt_tot,
         thirdthreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherTEXJun_Aug <- weatherTEX_three_month %>% 
  filter(Census_three_month == "fourththreeback" & CensusGroup == "May") %>%
  rename(fourththreeback_vpdmax = trimonthly_vpdmax,
         fourththreeback_ppt = trimonthly_ppt_tot,
         fourththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherTEXMar_May_lastyear <- weatherTEX_three_month %>% 
  filter(Census_three_month == "fifththreeback" & CensusGroup == "May") %>%
  rename(fifththreeback_vpdmax = trimonthly_vpdmax,
         fifththreeback_ppt = trimonthly_ppt_tot,
         fifththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherTEXDec_Feb_lastyear <- weatherTEX_three_month %>% 
  filter(Census_three_month == "sixththreeback" & CensusGroup == "May") %>%
  rename(sixththreeback_vpdmax = trimonthly_vpdmax,
         sixththreeback_ppt = trimonthly_ppt_tot,
         sixththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherTEXSep_Nov_lastyear <- weatherTEX_three_month %>% 
  filter(Census_three_month == "sevenththreeback" & CensusGroup == "May") %>%
  rename(sevenththreeback_vpdmax = trimonthly_vpdmax,
         sevenththreeback_ppt = trimonthly_ppt_tot,
         sevenththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

weatherTEXJun_Aug_lastyear <- weatherTEX_three_month %>% 
  filter(Census_three_month == "eighththreeback" & CensusGroup == "May") %>%
  rename(eighththreeback_vpdmax = trimonthly_vpdmax,
         eighththreeback_ppt = trimonthly_ppt_tot,
         eighththreeback_tmean = trimonthly_tmean_mean) %>% select(-Census_three_month)

#making data frames with only necessary data for each 6 months back from May for Texas
weatherTEXDec_May <- weatherTEX_six_month %>%
  filter(Census_six_month == "firstsixback" & CensusGroup == "May") %>%
  rename(firstsixback_vpdmax = hexamonthly_vpdmax,
         firstsixback_ppt = hexamonthly_ppt_tot,
         firstsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherTEXJun_Nov <- weatherTEX_six_month %>% 
  filter(Census_six_month == "secondsixback" & CensusGroup == "May") %>%
  rename(secondsixback_vpdmax = hexamonthly_vpdmax,
         secondsixback_ppt = hexamonthly_ppt_tot,
         secondsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherTEXDec_May_lastyear <- weatherTEX_six_month %>%
  filter(Census_six_month == "thirdsixback" & CensusGroup == "May") %>%
  rename(thirdsixback_vpdmax = hexamonthly_vpdmax,
         thirdsixback_ppt = hexamonthly_ppt_tot,
         thirdsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)

weatherTEXJun_Nov_lastyear <- weatherTEX_six_month %>% 
  filter(Census_six_month == "fourthsixback" & CensusGroup == "May") %>%
  rename(fourthsixback_vpdmax = hexamonthly_vpdmax,
         fourthsixback_ppt = hexamonthly_ppt_tot,
         fourthsixback_tmean = hexamonthly_tmean_mean) %>% select(-Census_six_month)


#Combining demographic data with the weather data
gras <- read.csv("data/ltreb_allspp_2007_2025.csv")

#Grouping species with census timing
grasMayCensus <- gras %>% filter(species == c("FESU","POAL","POSY"))
grasMayCensusTEX <- gras %>% filter(species == "POAU")
grasJulCensus <- gras %>% filter(species == c("ELVI","ELRI","LOAR"))
grasSepCensus <- gras %>% filter(species == "AGPE")

#Combining corresponding weather and demographic data frames

#recombining weather data for May census Indiana
weatherINDAllMay <- left_join(x=weatherINDMayMay, y=weatherINDMayApr, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayMar, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayFeb, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJan, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayDec, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayNov, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayOct, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMaySep, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayAug, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJul, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJun, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayMay_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayApr_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayMar_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayFeb_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJan_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayDec_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayNov_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayOct_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMaySep_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayAug_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJul_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMayJun_lastyear, by= c("CensusGroup","CensusYear"))

weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMar_May, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDDec_Feb, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDSep_Nov, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDJun_Aug, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDMar_May_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDDec_Feb_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDSep_Nov_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDJun_Aug_lastyear, by= c("CensusGroup","CensusYear"))

weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDDec_May, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDJun_Nov, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDDec_May_lastyear, by= c("CensusGroup","CensusYear"))
weatherINDAllMay <- left_join(x=weatherINDAllMay, y=weatherINDJun_Nov_lastyear, by= c("CensusGroup","CensusYear"))

#combing weather and demographic data for species censused in May Indiana
CombinedMay <- left_join(x=grasMayCensus, y=weatherINDAllMay, by=c("year_t1" = "CensusYear"))

#recombining weather data for Jul census

weatherINDAllJul <- left_join(x=weatherINDJulJul, y=weatherINDJulJun, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulMay, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulApr, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulMar, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulFeb, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulJan, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulDec, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulNov, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulOct, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulSep, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulAug, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDJulJul, y=weatherINDJulJul_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDJulJul, y=weatherINDJulJun_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulMay_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulApr_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulMar_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulFeb_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulJan_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulDec_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulNov_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulOct_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulSep_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDJulAug_lastyear, by=c("CensusGroup","CensusYear"))

weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDMay_Jul, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDFeb_Apr, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDNov_Jan, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDAug_Oct, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDMay_Jul_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDFeb_Apr_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDNov_Jan_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDAug_Oct_lastyear, by=c("CensusGroup","CensusYear"))

weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDFeb_Jul, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDAug_Jan, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDFeb_Jul_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllJul <- left_join(x=weatherINDAllJul, y=weatherINDAug_Jan_lastyear, by=c("CensusGroup","CensusYear"))

#combing weather and demographic data for species censused in Jul
CombinedJul <- left_join(x=grasJulCensus, y=weatherINDAllJul, by=c("year_t1" = "CensusYear"))

#recombining weather data for Sep census
weatherINDAllSep <- left_join(x=weatherINDSepSep, y=weatherINDSepAug, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJul, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJun, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepMay, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepApr, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepMar, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepFeb, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJan, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepDec, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepNov, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepOct, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDSepSep, y=weatherINDSepSep_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDSepSep, y=weatherINDSepAug_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJul_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJun_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepMay_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepApr_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepMar_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepFeb_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepJan_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepDec_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepNov_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDSepOct_lastyear, by=c("CensusGroup","CensusYear"))

weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDJul_Sep, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDApr_Jun, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDJan_Mar, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDOct_Dec, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDJul_Sep_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDApr_Jun_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDJan_Mar_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDOct_Dec_lastyear, by=c("CensusGroup","CensusYear"))

weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDApr_Sep, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDOct_Mar, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDApr_Sep_lastyear, by=c("CensusGroup","CensusYear"))
weatherINDAllSep <- left_join(x=weatherINDAllSep, y=weatherINDOct_Mar_lastyear, by=c("CensusGroup","CensusYear"))

#combing weather and demographic data for species censused in Sep
CombinedSep <- left_join(x=grasSepCensus, y=weatherINDAllSep, by=c("year_t1" = "CensusYear"))

#recombining weather data for May census Texas
weatherTEXAllMay <- left_join(x=weatherTEXMayMay, y=weatherTEXMayApr, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayMar, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayFeb, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJan, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayDec, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayNov, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayOct, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMaySep, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayAug, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJul, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJun, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayMay_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayApr_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayMar_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayFeb_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJan_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayDec_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayNov_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayOct_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMaySep_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayAug_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJul_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMayJun_lastyear, by= c("CensusGroup","CensusYear"))

weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMar_May, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXDec_Feb, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXSep_Nov, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXJun_Aug, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXMar_May_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXDec_Feb_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXSep_Nov_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXJun_Aug_lastyear, by= c("CensusGroup","CensusYear"))

weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXDec_May, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXJun_Nov, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXDec_May_lastyear, by= c("CensusGroup","CensusYear"))
weatherTEXAllMay <- left_join(x=weatherTEXAllMay, y=weatherTEXJun_Nov_lastyear, by= c("CensusGroup","CensusYear"))

#combing weather and demographic data for species censused in May Texas
CombinedMayTEX <- left_join(x=grasMayCensusTEX, y=weatherTEXAllMay, by=c("year_t1" = "CensusYear"))


#Combining all species and censusmonths of data
CombinedDataSegments <- bind_rows(CombinedMay, CombinedJul, CombinedSep, CombinedMayTEX)

write.csv(CombinedDataSegments, "data/CombinedDataSegments")

