library(devtools)
library(remotes)
install_github("ropensci/prism")
library(prism)
library(dplyr)
library(tidyr)
library(terra) #AKIEM - doesn't seem to work
library(lubridate)

prism_set_dl_dir("C:/Users/ag285/OneDrive - Rice University/Desktop/Akiem G/R Research/Temporal-Storage/ppt_2007_2025")

?get_prism_dailys
  
get_prism_dailys(
  type = "ppt",
  minDate = "2007-05-01", 
  maxDate = "2025-11-30", 
  resolution = "4km",
  keepZip = FALSE
) 


#AKIEM - I changed the below to have just the 2 Indiana and Texas sites
SiteCords <- data.frame(SiteState = c("INDIANA", "TEXAS"),
                        latitude = c(39.2359, 31.5033),
                        longitude = c(-86.2181,-94.6784))

#AKIEM - Don't understand whats happening below. Is Site_ppt a file you created? whats in it? what does it do?
root <- "C:/Users/bm162/OneDrive - Rice University/Desktop/Site_ppt"

# Find all the .bil files inside the day folders
bil_files24 <- list.files(root, pattern = "\\.bil$", recursive = TRUE, full.names = TRUE)

ppt24 <- rast(bil_files24)  # SpatRaster with 365 layers

# extract date from folder name
date_chr24 <- sub(".*_(\\d{8})\\.bil$", "\\1", basename(bil_files24))
dates24 <- as.Date(date_chr24, "%Y%m%d")

#AKIEM - why are we working with raster data?
# add dates to raster
time(ppt24)  <- dates24
names(ppt24) <- format(dates24)

pts24 <- vect(SiteCords,
              geom = c("longitude", "latitude"),
              crs = "EPSG:4326")

# make sure projection matches the PRISM raster
pts24 <- project(pts24, ppt24)

#extract daily precipitation for each point
vals24 <- extract(ppt24, pts24)
head(vals24)

#long table of all results

ppt_long24 <- vals24 %>%
  # add the SiteName column using the ID
  mutate(SiteName = pts24$SiteName[ID]) %>%
  # pivot all the date columns to long format
  pivot_longer(
    cols = -c(ID, SiteName),   # keep ID + SiteName, gather the rest
    names_to  = "date",
    values_to = "ppt_mm"
  ) %>%
  # turn "2024-01-01" strings into Date objects
  mutate(date = as.Date(date)) %>%
  # bring in lat/long from your SiteCords table
  left_join(SiteCords, by = "SiteName")

#_____________________________________________________________________________#
#VERY USEFUL
# extracting month from dates and creating census years
ppt_may <- ppt %>%
  mutate(month = month(date), 
         CensusYear = ifelse(month >= 5, (year(date) + 1), year(date)))

ppt_july <- ppt %>%
  mutate(month = month(date), 
         CensusYear = ifelse(month >= 7, (year(date) + 1), year(date)))

ppt_sep <- ppt %>%
  mutate(month = month(date), 
         CensusYear = ifelse(month >= 9, (year(date) + 1), year(date)))
#_____________________________________________________________________________#


ppt_long24 <- ppt_long24 %>%
mutate(Season = ifelse((month >= 5) & (month <= 10), "Warm", "Cool"))

ppt_long24 <- ppt_long24 %>%
  group_by(
    SiteName, 
    CensusYear) %>%
  mutate(
    CoolSeason_totalppt = sum(ppt_mm[Season == "Cool"],  na.rm = TRUE),
    CoolSeason_tppt_sd  = sd(ppt_mm[Season == "Cool"],   na.rm = TRUE),
    WarmSeason_totalppt = sum(ppt_mm[Season == "Warm"],  na.rm = TRUE),
    WarmSeason_tppt_sd  = sd(ppt_mm[Season == "Warm"],   na.rm = TRUE),
    CensusYear_totalppt = sum(ppt_mm,                    na.rm = TRUE),
    CensusYear_tppt_sd  = sd(ppt_mm,                     na.rm = TRUE)
  ) %>%
  select(-ID)


##----------Repeat for mean temp


setwd("C:/Users/bm162/OneDrive - Rice University/Desktop/Site_tmean")
prism_set_dl_dir("C:/Users/bm162/OneDrive - Rice University/Desktop/Site_tmean")

get_prism_dailys(
  type = "tmean", 
  minDate = "2023-05-01", 
  maxDate = "2025-05-15", 
  resolution = "4km",
  keepZip = FALSE
)

root <- "C:/Users/bm162/OneDrive - Rice University/Desktop/Site_tmean"

bil_tmean24 <- list.files(root, pattern = "\\.bil$", recursive = TRUE, full.names = TRUE)

tmean24 <- rast(bil_tmean24)  # SpatRaster with 365 layers

# extract date from folder name
date_chr24 <- sub(".*_(\\d{8})\\.bil$", "\\1", basename(bil_tmean24))
dates24 <- as.Date(date_chr24, "%Y%m%d")

# add dates to raster
time(tmean24)  <- dates24
names(tmean24) <- format(dates24)

pts24_tmean <- vect(SiteCords,
                    geom = c("longitude", "latitude"),
                    crs = "EPSG:4326")

# make sure projection matches the PRISM raster
pts24_tmean <- project(pts24_tmean, tmean24)

#extract daily precipitation for each point
vals24_tmean <- extract(tmean24, pts24_tmean)
head(vals24_tmean)

#long table of all results

tmean_long24 <- vals24_tmean %>%
  # add the SiteName column using the ID
  mutate(SiteName = pts24_tmean$SiteName[ID]) %>%
  # pivot all the date columns to long format
  pivot_longer(
    cols = -c(ID, SiteName),   # keep ID + SiteName, gather the rest
    names_to  = "date",
    values_to = "tmean"
  ) %>%
  # turn "2024-01-01" strings into Date objects
  mutate(date = as.Date(date)) %>%
  # bring in lat/long from your SiteCords table
  left_join(SiteCords, by = "SiteName")

tmean_long24 <- tmean_long24 %>%
  mutate(
    month = month(date),
    CensusYear = ifelse(month >= 5, (year(date) + 1), year(date)) %>%
      mutate(Season = ifelse((month >= 5) & (month <= 10), "Warm", "Cool"))
  )

tmean_long24 <- tmean_long24 %>%
  group_by(
    SiteName, 
    CensusYear) %>%
  mutate(
    CoolSeason_tmean    = mean(tmean[Season == "Cool"], na.rm = TRUE),
    CoolSeason_tmeansd  = sd(tmean[Season == "Cool"],   na.rm = TRUE),
    WarmSeason_tmean    = mean(tmean[Season == "Warm"], na.rm = TRUE),
    WarmSeason_tmeansd  = sd(tmean[Season == "Warm"],   na.rm = TRUE),
    CensusYear_tmean    = mean(tmean,                   na.rm = TRUE),
    CensusYear_tmeansd  = sd(tmean,                     na.rm = TRUE)
  ) %>%
  select(-ID)

write.csv(ppt_long24,  "Site_PPT.csv",    row.names = FALSE)
write.csv(tmean_long24, "Site_TMEAN.csv", row.names = FALSE)


#__________________________________________________________________________#
library("googlesheets4")
library("googledrive")
library("tidyverse")
library("dplyr")
library(lme4)
library(AICcmodavg)
library(dplyr)
library(ggplot2)
library(scales)
library(DHARMa)
library(car)

file_id<-"11m_-0KYtLtp5J0EqGWZ8RzNM-JfsDB3B"
drive_download(as_id(file_id), path = "Data/endo_range_limits_experiment.xlsx", overwrite = TRUE)

plots24 <- readxl::read_excel(
  "Data//endo_range_limits_experiment.xlsx",
  sheet = "2024 Data Census",
  col_types = c("text", rep("guess", 10))) 

plots25 <- readxl::read_excel("Data//endo_range_limits_experiment.xlsx",sheet="2025 Data Census")
Tag_SiteID <- readxl::read_excel(
  "Data//endo_range_limits_experiment.xlsx",
  sheet    = "Initial Plant Data",
  col_types = "text"
)

Tag_SiteID <- Tag_SiteID %>%
  select(Site, Tag_ID, Population, Species, Plot)
Tag_SiteID <- Tag_SiteID %>%
  filter(!is.na(Tag_ID))
Tag_SiteID <- Tag_SiteID %>%
  mutate(
    Tag_ID = sub("\\.0$", "", as.character(Tag_ID)),
    Tag_ID = str_replace(Tag_ID, "^0+", ""))

plots25 <-  plots25 %>%
  mutate(Tag_ID = as.character(plots25$Tag_ID),
         Tag_ID = str_replace(Tag_ID, "^0+", ""))
TagInfo25 <- plots25 %>%
  select(Tag_ID,Site)

plots24 <- plots24 %>%
  mutate(Tag_ID = sub("\\.0$", "", as.character(Tag_ID)),
         Tag_ID = str_replace(Tag_ID, "^0+", ""))
plots24 <- plots24 %>%
  left_join(Tag_SiteID, by = "Tag_ID") %>%
  left_join(TagInfo25,  by = "Tag_ID") 

plots24 <- plots24 %>%
  mutate(Site = coalesce(Site.x, Site.y)) %>%
  select(-Site.x, -Site.y)

plots24 <- plots24 %>%
  mutate(
    extracted_number = str_extract(Notes, "\\d+"),
    Stroma = if_else(
      Stroma >= 1 & !is.na(extracted_number),   
      as.numeric(extracted_number),
      Stroma                                    
    )
  ) %>%
  select(-extracted_number)


plots24_25 <- plots24 %>%
  bind_rows(plots25)

plots24_25 <- plots24_25 %>%
  mutate(Year = str_extract(Date, "\\d{4}")) %>%
  left_join(Tag_SiteID, by = "Tag_ID") %>%
  mutate(Site = coalesce(Site.x, Site.y)) %>%
  select(-Site.x, -Site.y) %>%
  mutate(Population = coalesce(Population.x, Population.y)) %>%
  select(-Population.x, -Population.y) %>%
  mutate(Species = coalesce(Species.x, Species.y)) %>%
  select(-Species.x, -Species.y) %>%
  mutate(Plot = coalesce(Plot.x, Plot.y)) %>%
  select(-Plot.x, -Plot.y) 


plots24_25 <- plots24_25 %>%
  mutate(
    total_inf = case_when(
      Year == "2024" ~ coalesce(attachedInf_24, 0) + coalesce(brokenInf_24, 0),
      Year == "2025" ~ coalesce(attachedInf_25, 0) + coalesce(brokenInf_25, 0),
      TRUE           ~ NA_real_
    ),
    Stroma = case_when(
      is.na(Stroma) & total_inf > 0 ~ 0,
      TRUE ~ Stroma),
    Trans_Per = case_when(
      total_inf > 0 ~ Stroma / total_inf,  
      TRUE ~ NA_real_
        )
      )
#Sanity check to make sure the only rows with no Stroma value also had no inf    
StromaNA <- plots24_25 %>%
  filter(is.na(Stroma))


#Cleaning the climate data 
ClimateData <- read.csv("Site_PPT.csv")
TempData    <- read.csv("Site_TMEAN.csv")
ClimateData <- ClimateData %>%
  left_join(
    TempData,
    by = c("SiteName", "date", "latitude", "longitude", "CensusYear", "month"),
    relationship = "one-to-one"
  ) %>%
  mutate(Season = coalesce(Season.x, Season.y)) %>%
  select(-Season.x, -Season.y)

ClimateData <- ClimateData %>% 
  mutate(Year = as.character(CensusYear)) %>%
  select(-longitude, -latitude,-date, -ppt_mm, -tmean, -month, -Season,
         -CensusYear) %>% 
  distinct() %>%
  select(SiteName, Year, everything())

CombinedData <- plots24_25 %>%
  left_join(ClimateData, by = c("Site" = "SiteName", "Year" = "Year"))
CombinedData <- CombinedData %>%
  select(-Tiller_24, -Spikelet_A, -Spikelet_B, -Spikelet_C, -tiller_Herb, -Species, 
         -Tiller_25, -Samples, -Date)

Stroma_cand <- list()
Stroma_cand[[1]] <- glmer(Trans_Per~1 + (1|Site) + (1|Plot) + (Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[2]] <- glmer(Trans_Per~CoolSeason_totalppt + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[3]] <- glmer(Trans_Per~CoolSeason_tmean + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[4]] <- glmer(Trans_Per~CoolSeason_ppt_sd + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[5]] <- glmer(Trans_Per~CoolSeason_tmeansd + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[6]] <- glmer(Trans_Per~WarmSeason_totalppt + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[7]] <- glmer(Trans_Per~WarmSeason_tmean + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[8]] <- glmer(Trans_Per~WarmSeason_ppt_sd + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[9]] <- glmer(Trans_Per~WarmSeason_tmeansd + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[10]] <- glmer(Trans_Per~CensusYear_totalppt + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[11]] <- glmer(Trans_Per~CensusYear_tmean + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[12]]<- glmer(Trans_Per~CensusYear_ppt_sd + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)
Stroma_cand[[13]]<- glmer(Trans_Per~CensusYear_tmeansd + (1|Site) + (1|Plot) + (1|Tag_ID), family = "binomial", weights = (total_inf), data=CombinedData)

aictab(Stroma_cand)



VIFCheck    <-glmer(Trans_Per~CoolSeason_totalppt + CoolSeason_tmean + (1|Site) + (1|Plot) , family = "binomial", weights = (total_inf), data=CombinedData)
**VIFCheck2 <-glmer(Trans_Per~WarmSeason_totalppt + WarmSeason_tmean + (1|Site) + (1|Plot) , family = "binomial", weights = (total_inf), data=CombinedData)
VIFCheck3   <-glmer(Trans_Per~CensusYear_totalppt + CensusYear_tmean + (1|Site) + (1|Plot) , family = "binomial", weights = (total_inf), data=CombinedData)
vif(VIFCheck)

