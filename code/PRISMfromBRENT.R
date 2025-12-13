library(devtools)
library(remotes)
install_github("ropensci/prism")
library(prism)
library(dplyr)
library(tidyr)
library(terra) #AKIEM - doesn't seem to work
library(lubridate)

setwd("/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage")
prism_set_dl_dir("/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage")

get_prism_dailys(
  type = "ppt", 
  minDate = "2007-05-01", 
  maxDate = "2025-11-30", 
  resolution = "4km",
  keepZip = FALSE
) #AKIEM - where above, do we assign the location? Is it gathering data across the whole globe


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

ppt_long24 <- ppt_long24 %>%
  mutate(
    month = month(date),
    CensusYear = ifelse(month >= 5, (year(date) + 1), year(date)))

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