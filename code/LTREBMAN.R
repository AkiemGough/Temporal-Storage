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


unique(ltrebman$year_t)
str(ltrebman)

ltrebman %>% filter(species == "POAL" & year_t == 2021) %>% View


(POAL_22_23 <- ltrebman [ltrebman$species=="POAL" & 
                          ltrebman$year_t == 2021 & ltrebman$surv_t1==1 ,])
POAL_22_23$year_t <- 2022
POAL_22_23$age <- POAL_22_23$year_t - POAL_22_23$birth
POAL_22_23$size_t <- POAL_22_23$size_t1
POAL_22_23$flw_count_t <- POAL_22_23$flw_count_t1
POAL_22_23$mean_spike_t <- POAL_22_23$mean_spike_t1
POAL_22_23$year_t1 <- 2023

##Can't we just do lines 49-54 after the code in line 62-65?

poaals23 %>% rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T))


## need to populate 2023 data for surv, size, flowering, spikelets
POAL_22_23new <- full_join(x=POAL_22_23,
          y=poaals23 %>% rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
          select(survival,size_tillers,flowering_tillers,mean_spike,id,species,plot,origin,birth_year,distance_A,distance_B),
          by=c("id","species", "plot"))

## write over the surv, size, flow, spike for 2023
names(POAL_22_23new)
POAL_22_23new$birth <- POAL_22_23new$birth_year
POAL_22_23new$birth <- POAL_22_23new$birth_year
POAL_22_23new$year_t <- 2022
POAL_22_23new$year_t1 <- 2023
POAL_22_23new$age <- POAL_22_23new$year_t - POAL_22_23$birth
POAL_22_23new$size_t <- POAL_22_23new$size_t1
POAL_22_23new$flw_count_t <- POAL_22_23new$flw_count_t1
POAL_22_23new$mean_spike_t <- POAL_22_23new$mean_spike_t1
POAL_22_23new$surv_t1 <- POAL_22_23new$survival
POAL_22_23new$size_t1 <- POAL_22_23new$size_tillers
POAL_22_23new$flw_count_t1 <- POAL_22_23new$flowering_tillers
POAL_22_23new$dist_a <- POAL_22_23new$distance_A
POAL_22_23new$dist_b <- POAL_22_23new$distance_B

View(POAL_22_23new)

##what do we do with original and orig since they have different types of data
##what do we do about endo_01 and endo_status_from_check

