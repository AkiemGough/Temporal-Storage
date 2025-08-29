## load packages
library(googledrive)
library(readxl)
library(tidyverse)
##set wriking directory
setwd("~/Documents/GitHub/Temporal-Storage")

ltrebman <- read.csv("data/ltreb_allspp_qaqc.csv",stringsAsFactors = T)

## to be sorted out
##it seems I don't have the permission to grab this data file from the google drive
string23 <- "1QhWF-7SbvjAAhsfbXMNdA1IWEwSf1kBe"
drive_download(as_id(string23), path = "data/LTREB_data_2024_recruits_inc.xlsx", overwrite=TRUE) 
##

##read in 2025 data
poaals25 <- read_excel("data/LTREB_data_2025_recruits_inc.xlsx", sheet= "POAL with recruits")
poasyl25 <- read_excel("data/LTREB_data_2025_recruits_inc.xlsx", sheet= "POSY with recruits")
elyvir25 <- read_excel("data/LTREB_data_2025_recruits_inc.xlsx", sheet= "ELVI with recruits")
elyvil25 <- read_excel("data/LTREB_data_2025_recruits_inc.xlsx", sheet= "ELRI with recruits")
fessub25 <- read_excel("data/LTREB_data_2025_recruits_inc.xlsx", sheet= "FESU with recruits")
## agrper25 data has not be yet collected

##read in 2024 data
poaals24 <- read_excel("data/LTREB_data_2024_recruits_inc.xlsx", sheet= "POAL with recruits")
poasyl24 <- read_excel("data/LTREB_data_2024_recruits_inc.xlsx", sheet= "POSY with recruits")
elyvir24 <- read_excel("data/LTREB_data_2024_recruits_inc.xlsx", sheet= "ELVI with recruits")
elyvil24 <- read_excel("data/LTREB_data_2024_recruits_inc.xlsx", sheet= "ELRI with recruits")
agrper24 <- read_excel("data/LTREB_data_2024_recruits_inc.xlsx", sheet= "AGPE with recruits")
fessub24 <- read_excel("data/LTREB_data_2024_recruits_inc.xlsx", sheet= "FESU with recruits")

##read in 2023 data
poaals23 <- read_excel("data/LTREB_data_2023_recruits_inc.xlsx", sheet= "POAL")
poasyl23 <- read_excel("data/LTREB_data_2023_recruits_inc.xlsx", sheet= "POSY")
elyvir23 <- read_excel("data/LTREB_data_2023_recruits_inc.xlsx", sheet= "ELVI")
elyvil23 <- read_excel("data/LTREB_data_2023_recruits_inc.xlsx", sheet= "ELRI")
agrper23 <- read_excel("data/LTREB_data_2023_recruits_inc.xlsx", sheet= "AGPE")
fessub23 <- read_excel("data/LTREB_data_2023_recruits_inc.xlsx", sheet= "FESU")


unique(ltrebman$year_t1)
str(ltrebman)

ltrebman %>% filter(species == "POAL" & year_t == 2021) %>% View


(POAL_22_23 <- ltrebman [ltrebman$species=="POAL" & 
                          ltrebman$year_t == 2021 & ltrebman$surv_t1==1 ,])
POAL_22_23$year_t <- 2022
POAL_22_23$size_t <- POAL_22_23$size_t1
POAL_22_23$flw_count_t <- POAL_22_23$flw_count_t1
POAL_22_23$mean_spike_t <- POAL_22_23$mean_spike_t1

POAL_22_23$year_t1 <- 2023
#see I was thinking of doing this: 
POAL_22_23$size_t1 <- poaals23$size_tillers
#but that would mess up what we did in line 50 no? and would have a different column length
#the size data might not match with the correct IDs and birth years and such

View(POAL_22_23)
