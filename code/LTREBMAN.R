## load packages
library(tidyverse)
library(dplyr)

#read in pre-2023 data
ltreb_allspp_qaqc <- read.csv("data/ltreb_allspp_qaqc.csv",stringsAsFactors = T)

#create small table with endo status
plot_endo <- ltreb_allspp_qaqc %>% select(species,plot,endo_01) %>% unique()

## to be sorted out
##it seems I don't have the permission to grab this data file from the google drive
#string23 <- "1QhWF-7SbvjAAhsfbXMNdA1IWEwSf1kBe"
#drive_download(as_id(string23), path = "data/LTREB_data_2024_recruits_inc.xlsx", overwrite=TRUE) 
##

#### ADDING 2023 DATA________________________________________________________##

##read in 2023 data
poaals23data <- read.csv("data/2023/LTREB_data_2023_recruits_inc.xlsx - POAL.csv")
poasyl23data <- read.csv("data/2023/LTREB_data_2023_recruits_inc.xlsx - POSY.csv")
elyvir23data <- read.csv("data/2023/LTREB_data_2023_recruits_inc.xlsx - ELVI.csv")
elyvil23data <- read.csv("data/2023/LTREB_data_2023_recruits_inc.xlsx - ELRI.csv")
agrper23data <- read.csv("data/2023/LTREB_data_2023_recruits_inc.xlsx - AGPE.csv")
fessub23data <- read.csv("data/2023/LTREB_data_2023_recruits_inc.xlsx - FESU.csv")

##all the elyvir,elyvil,agrper,fessub have birth_year as chr 
##instead of int or num except agreper24 & fessub25

###Creating POAL 2022-2023 transition year___________________________________##

##creating and populating an endo_01 column in 2023 data
poaals23 <- left_join(x=poaals23data, y=plot_endo, by=c("species", "plot")) %>% 
  ##populating original column for new rows especially in 2023 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2023 data (columns and rows) to 2021-2022 table in order to create 2022-2023 data
POAL_22_23 <- full_join(x=ltreb_allspp_qaqc %>% filter(species=="POAL" & year_t == 2021 & surv_t1==1),
          y=poaals23,
          by=c("id","species", "plot","endo_01", "original")) 

## write over the relevant data for 2023
#names(POAL_22_23)
#str(POAL_22_23)

POAL_22_23$birth <- POAL_22_23$birth_year
POAL_22_23$year_t <- 2022
POAL_22_23$year_t1 <- 2023
POAL_22_23$age <- POAL_22_23$year_t - POAL_22_23$birth
POAL_22_23$size_t <- POAL_22_23$size_t1
POAL_22_23$flw_count_t <- POAL_22_23$flw_count_t1
POAL_22_23$mean_spike_t <- POAL_22_23$mean_spike_t1
POAL_22_23$surv_t1 <- POAL_22_23$survival
POAL_22_23$size_t1 <- POAL_22_23$size_tillers
POAL_22_23$flw_count_t1 <- POAL_22_23$flowering_tillers
POAL_22_23$mean_spike_t1 <- POAL_22_23$mean_spike
POAL_22_23$dist_a <- POAL_22_23$distance_A
POAL_22_23$dist_b <- POAL_22_23$distance_B 

##removing excess columns
#names(POAL_22_23)

POAL_22_23 <- POAL_22_23[ ,c("X","species","plot","endo_01","id","original","endo_status_from_check",
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(POAL_22_23)

###Creating POSY 2022-2023 transition year___________________________________##

##creating and populating an endo_01 column in 2023 data
poasyl23 <- left_join(x=poasyl23data, y=plot_endo, by=c("species", "plot")) %>% 
  ##populating original column for new rows especially in 2023 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)


## adding 2023 data (columns and rows) to 2021-2022 table in order to create 2022-2023 data
POSY_22_23 <- full_join(x=ltreb_allspp_qaqc %>% filter(species=="POSY" & year_t == 2021 & surv_t1==1),
                        y=poasyl23,
                        by=c("id","species", "plot","endo_01", "original")) 

## write over the relevant data for 2023
#names(POSY_22_23)
#str(POSY_22_23)

POSY_22_23$birth <- POSY_22_23$birth_year
POSY_22_23$year_t <- 2022
POSY_22_23$year_t1 <- 2023
POSY_22_23$age <- POSY_22_23$year_t - POSY_22_23$birth
POSY_22_23$size_t <- POSY_22_23$size_t1
POSY_22_23$flw_count_t <- POSY_22_23$flw_count_t1
POSY_22_23$mean_spike_t <- POSY_22_23$mean_spike_t1
POSY_22_23$surv_t1 <- POSY_22_23$survival
POSY_22_23$size_t1 <- POSY_22_23$size_tillers
POSY_22_23$flw_count_t1 <- POSY_22_23$flowering_tillers
POSY_22_23$mean_spike_t1 <- POSY_22_23$mean_spike
POSY_22_23$dist_a <- POSY_22_23$distance_A
POSY_22_23$dist_b <- POSY_22_23$distance_B

##removing excess columns
names(POSY_22_23)

POSY_22_23 <- POSY_22_23[,c("X","species","plot","endo_01","id","original","endo_status_from_check",
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE]  
#View(POSY_22_23)

###Creating elyvir23 2022-2023 transition year___________________________________##

##creating and populating an endo_01 column in 2023 data
elyvir23 <- left_join(x=elyvir23data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2023 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2023 data (columns and rows) to 2021-2022 table in order to create 2022-2023 data
ELVI_22_23 <- full_join(x=ltreb_allspp_qaqc %>% filter(species=="ELVI" & year_t == 2021 & surv_t1==1),
                        y=elyvir23,
                        by=c("id","species", "plot","endo_01", "original")) 

## write over the relevant data for 2023
#names(ELVI_22_23)

ELVI_22_23$birth <- ELVI_22_23$birth_year
ELVI_22_23$year_t <- 2022
ELVI_22_23$year_t1 <- 2023
ELVI_22_23$age <- ELVI_22_23$year_t - ELVI_22_23$birth
ELVI_22_23$size_t <- ELVI_22_23$size_t1
ELVI_22_23$flw_count_t <- ELVI_22_23$flw_count_t1
ELVI_22_23$mean_spike_t <- ELVI_22_23$mean_spike_t1
ELVI_22_23$surv_t1 <- ELVI_22_23$survival
ELVI_22_23$size_t1 <- ELVI_22_23$size_tillers
ELVI_22_23$flw_count_t1 <- ELVI_22_23$flowering_tillers
ELVI_22_23$mean_spike_t1 <- ELVI_22_23$mean_spike
ELVI_22_23$dist_a <- ELVI_22_23$distance_A
ELVI_22_23$dist_b <- ELVI_22_23$distance_B

##removing excess columns
#names(ELVI_22_23)

ELVI_22_23 <- ELVI_22_23[,c("X","species","plot","endo_01","id","original","endo_status_from_check",
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE]
#View(ELVI_22_23)

###Creating elyvil23 2022-2023 transition year___________________________________##

##creating and populating an endo_01 column in 2023 data
elyvil23 <- left_join(x=elyvil23data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2023 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2023 data (columns and rows) to 2021-2022 table in order to create 2022-2023 data
ELRI_22_23 <- full_join(x=ltreb_allspp_qaqc %>% filter(species=="ELRI" & year_t == 2021 & surv_t1==1),
                        y=elyvil23,
                        by=c("id","species", "plot","endo_01", "original")) 

## write over the relevant data for 2023
#str(ELRI_22_23)

ELRI_22_23$birth <- ELRI_22_23$birth_year
ELRI_22_23$year_t <- 2022
ELRI_22_23$year_t1 <- 2023
ELRI_22_23$age <- ELRI_22_23$year_t - ELRI_22_23$birth
ELRI_22_23$size_t <- ELRI_22_23$size_t1
ELRI_22_23$flw_count_t <- ELRI_22_23$flw_count_t1
ELRI_22_23$mean_spike_t <- ELRI_22_23$mean_spike_t1
ELRI_22_23$surv_t1 <- ELRI_22_23$survival
ELRI_22_23$size_t1 <- ELRI_22_23$size_tillers
ELRI_22_23$flw_count_t1 <- ELRI_22_23$flowering_tillers
ELRI_22_23$mean_spike_t1 <- ELRI_22_23$mean_spike
ELRI_22_23$dist_a <- ELRI_22_23$distance_A
ELRI_22_23$dist_b <- ELRI_22_23$distance_B

##removing excess columns
#names(ELRI_22_23)

ELRI_22_23 <- ELRI_22_23[,c("X","species","plot","endo_01","id","original","endo_status_from_check",
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(ELRI_22_23)

###Creating agrper23 2022-2023 transition year___________________________________##

##creating and populating an endo_01 column in 2023 data
agrper23 <- left_join(x=agrper23data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2023 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2023 data (columns and rows) to 2021-2022 table in order to create 2022-2023 data
AGPE_22_23 <- full_join(x=ltreb_allspp_qaqc %>% filter(species=="AGPE" & year_t == 2021 & surv_t1==1),
                        y=agrper23,
                        by=c("id","species", "plot","endo_01", "original")) 

## write over the relevant data for 2023
#names(AGPE_22_23)

AGPE_22_23$birth <- AGPE_22_23$birth_year
AGPE_22_23$year_t <- 2022
AGPE_22_23$year_t1 <- 2023
AGPE_22_23$age <- AGPE_22_23$year_t - AGPE_22_23$birth
AGPE_22_23$size_t <- AGPE_22_23$size_t1
AGPE_22_23$flw_count_t <- AGPE_22_23$flw_count_t1
AGPE_22_23$mean_spike_t <- AGPE_22_23$mean_spike_t1
AGPE_22_23$surv_t1 <- AGPE_22_23$survival
AGPE_22_23$size_t1 <- AGPE_22_23$size_tillers
AGPE_22_23$flw_count_t1 <- AGPE_22_23$flowering_tillers
AGPE_22_23$mean_spike_t1 <- AGPE_22_23$mean_spike
AGPE_22_23$dist_a <- AGPE_22_23$distance_A
AGPE_22_23$dist_b <- AGPE_22_23$distance_B

##removing excess columns
#names(AGPE_22_23)

AGPE_22_23 <- AGPE_22_23[,c("X","species","plot","endo_01","id","original","endo_status_from_check",
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(AGPE_22_23)

###Creating fessub23 2022-2023 transition year___________________________________##

##creating and populating an endo_01 column in 2023 data
fessub23 <- left_join(x=fessub23data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2023 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2023 data (columns and rows) to 2021-2022 table in order to create 2022-2023 data
FESU_22_23 <- full_join(x=ltreb_allspp_qaqc %>% filter(species=="FESU" & year_t == 2021 & surv_t1==1),
                        y=fessub23,
                        by=c("id","species", "plot","endo_01", "original")) 

## write over the relevant data for 2023
#names(FESU_22_23)

FESU_22_23$birth <- FESU_22_23$birth_year
FESU_22_23$year_t <- 2022
FESU_22_23$year_t1 <- 2023
FESU_22_23$age <- FESU_22_23$year_t - FESU_22_23$birth
FESU_22_23$size_t <- FESU_22_23$size_t1
FESU_22_23$flw_count_t <- FESU_22_23$flw_count_t1
FESU_22_23$mean_spike_t <- FESU_22_23$mean_spike_t1
FESU_22_23$surv_t1 <- FESU_22_23$survival
FESU_22_23$size_t1 <- FESU_22_23$size_tillers
FESU_22_23$flw_count_t1 <- FESU_22_23$flowering_tillers
FESU_22_23$mean_spike_t1 <- FESU_22_23$mean_spike
FESU_22_23$dist_a <- FESU_22_23$distance_A
FESU_22_23$dist_b <- FESU_22_23$distance_B 

unique(FESU_22_23$distance_B)
##removing excess columns
#names(FESU_22_23)

FESU_22_23 <- FESU_22_23[,c("X","species","plot","endo_01","id","original","endo_status_from_check",
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE]
#View(FESU_22_23)



#### ADDING 2024 DATA________________________________________________________##

##read in 2024 data
poaals24data <- read.csv("data/2024/LTREB_data_2024_recruits_inc.xlsx - POAL.csv")
poasyl24data <- read.csv("data/2024/LTREB_data_2024_recruits_inc.xlsx - POSY.csv")
elyvir24data <- read.csv("data/2024/LTREB_data_2024_recruits_inc.xlsx - ELVI.csv")
elyvil24data <- read.csv("data/2024/LTREB_data_2024_recruits_inc.xlsx - ELRI.csv")
agrper24data <- read.csv("data/2024/LTREB_data_2024_recruits_inc.xlsx - AGPE.csv")
fessub24data <- read.csv("data/2024/LTREB_data_2024_recruits_inc.xlsx - FESU.csv")


##all the elyvir,elyvil,agrper,fessub have birth_year as chr 
##instead of int or num except agreper24 & fessub25

###Creating POAL 2023-2024 transition year___________________________________##

##creating and populating an endo_01 column in 2024 data
poaals24 <- left_join(x=poaals24data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2024 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2024 data (columns and rows) to 2022-2023 table in order to create 2023-2024 data
POAL_23_24 <- full_join(x=POAL_22_23 %>% filter(surv_t1==1),
                        y=poaals24,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 


##populating original column for new rows especially in 2024 data
#poaals24 <- poaals24 %>% mutate (original = if_else(origin == "R",0,1), .after= origin)

## write over the relevant data for 2024
#names(POAL_23_24)

POAL_23_24$birth <- POAL_23_24$birth_year
POAL_23_24$year_t <- 2023
POAL_23_24$year_t1 <- 2024
POAL_23_24$age <- POAL_23_24$year_t - POAL_23_24$birth
POAL_23_24$size_t <- POAL_23_24$size_t1
POAL_23_24$flw_count_t <- POAL_23_24$flw_count_t1
POAL_23_24$mean_spike_t <- POAL_23_24$mean_spike_t1
POAL_23_24$surv_t1 <- POAL_23_24$survival
POAL_23_24$size_t1 <- POAL_23_24$size_tillers
POAL_23_24$flw_count_t1 <- POAL_23_24$flowering_tillers
POAL_23_24$mean_spike_t1 <- POAL_23_24$mean_spike
POAL_23_24$dist_a <- POAL_23_24$distance_A
POAL_23_24$dist_b <- POAL_23_24$distance_B

##removing excess columns
#names(POAL_23_24)

POAL_23_24 <- POAL_23_24[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                             "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(POAL_23_24)

###Creating POSY 2023-2024 transition year___________________________________##

##creating and populating an endo_01 column in 2024 data
poasyl24 <- left_join(x=poasyl24data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2024 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2024 data (columns and rows) to 2022-2023 table in order to create 2023-2024 data
POSY_23_24 <- full_join(x=POSY_22_23 %>% filter(surv_t1==1),
                        y=poasyl24,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2024
#names(POSY_23_24)

POSY_23_24$birth <- POSY_23_24$birth_year
POSY_23_24$year_t <- 2023
POSY_23_24$year_t1 <- 2024
POSY_23_24$age <- POSY_23_24$year_t - POSY_23_24$birth
POSY_23_24$size_t <- POSY_23_24$size_t1
POSY_23_24$flw_count_t <- POSY_23_24$flw_count_t1
POSY_23_24$mean_spike_t <- POSY_23_24$mean_spike_t1
POSY_23_24$surv_t1 <- POSY_23_24$survival
POSY_23_24$size_t1 <- POSY_23_24$size_tillers
POSY_23_24$flw_count_t1 <- POSY_23_24$flowering_tillers
POSY_23_24$mean_spike_t1 <- POSY_23_24$mean_spike
POSY_23_24$dist_a <- POSY_23_24$distance_A
POSY_23_24$dist_b <- POSY_23_24$distance_B

##removing excess columns
#names(POSY_23_24)

POSY_23_24 <- POSY_23_24[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                             "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(POSY_23_24)

###Creating elyvir24 2023-2024 transition year___________________________________##
#View(ELVI_23_24)
##creating and populating an endo_01 column in 2024 data
elyvir24 <- left_join(x=elyvir24data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2024 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2024 data (columns and rows) to 2022-2023 table in order to create 2023-2024 data
ELVI_23_24 <- full_join(x=ELVI_22_23 %>% filter(surv_t1==1),
                        y=elyvir24,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2024
#names(ELVI_23_24)

ELVI_23_24$birth <- ELVI_23_24$birth_year
ELVI_23_24$year_t <- 2023
ELVI_23_24$year_t1 <- 2024
ELVI_23_24$age <- ELVI_23_24$year_t - ELVI_23_24$birth
ELVI_23_24$size_t <- ELVI_23_24$size_t1
ELVI_23_24$flw_count_t <- ELVI_23_24$flw_count_t1
ELVI_23_24$mean_spike_t <- ELVI_23_24$mean_spike_t1
ELVI_23_24$surv_t1 <- ELVI_23_24$survival
ELVI_23_24$size_t1 <- ELVI_23_24$size_tillers
ELVI_23_24$flw_count_t1 <- ELVI_23_24$flowering_tillers
ELVI_23_24$mean_spike_t1 <- ELVI_23_24$mean_spike
ELVI_23_24$dist_a <- ELVI_23_24$distance_A
ELVI_23_24$dist_b <- ELVI_23_24$distance_B

##removing excess columns
#names(ELVI_23_24)

ELVI_23_24 <- ELVI_23_24[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                             "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(ELVI_23_24)

###Creating elyvil24 2023-2024 transition year___________________________________##

##creating and populating an endo_01 column in 2024 data
elyvil24 <- left_join(x=elyvil24data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2024 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2024 data (columns and rows) to 2022-2023 table in order to create 2023-2024 data
ELRI_23_24 <- full_join(x=ELRI_22_23 %>% filter(surv_t1==1),
                        y=elyvil24,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2024
#names(ELRI_23_24)

ELRI_23_24$birth <- ELRI_23_24$birth_year
ELRI_23_24$year_t <- 2023
ELRI_23_24$year_t1 <- 2024
ELRI_23_24$age <- ELRI_23_24$year_t - ELRI_23_24$birth
ELRI_23_24$size_t <- ELRI_23_24$size_t1
ELRI_23_24$flw_count_t <- ELRI_23_24$flw_count_t1
ELRI_23_24$mean_spike_t <- ELRI_23_24$mean_spike_t1
ELRI_23_24$surv_t1 <- ELRI_23_24$survival
ELRI_23_24$size_t1 <- ELRI_23_24$size_tillers
ELRI_23_24$flw_count_t1 <- ELRI_23_24$flowering_tillers
ELRI_23_24$mean_spike_t1 <- ELRI_23_24$mean_spike
ELRI_23_24$dist_a <- ELRI_23_24$distance_A
ELRI_23_24$dist_b <- ELRI_23_24$distance_B

##removing excess columns
#names(ELRI_23_24)

ELRI_23_24 <- ELRI_23_24[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                             "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(ELRI_23_24)

###Creating agrper24 2023-2024 transition year___________________________________##

##creating and populating an endo_01 column in 2024 data
agrper24 <- left_join(x=agrper24data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2024 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2024 data (columns and rows) to 2022-2023 table in order to create 2023-2024 data
AGPE_23_24 <- full_join(x=AGPE_22_23 %>% filter(surv_t1==1),
                        y=agrper24,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2024
#names(AGPE_23_24)

AGPE_23_24$birth <- AGPE_23_24$birth_year
AGPE_23_24$year_t <- 2023
AGPE_23_24$year_t1 <- 2024
AGPE_23_24$age <- AGPE_23_24$year_t - AGPE_23_24$birth
AGPE_23_24$size_t <- AGPE_23_24$size_t1
AGPE_23_24$flw_count_t <- AGPE_23_24$flw_count_t1
AGPE_23_24$mean_spike_t <- AGPE_23_24$mean_spike_t1
AGPE_23_24$surv_t1 <- AGPE_23_24$survival
AGPE_23_24$size_t1 <- AGPE_23_24$size_tillers
AGPE_23_24$flw_count_t1 <- AGPE_23_24$flowering_tillers
AGPE_23_24$mean_spike_t1 <- AGPE_23_24$mean_spike
AGPE_23_24$dist_a <- AGPE_23_24$distance_A
AGPE_23_24$dist_b <- AGPE_23_24$distance_B

##removing excess columns
#names(AGPE_23_24)

AGPE_23_24 <- AGPE_23_24[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                             "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(AGPE_23_24)

###Creating fessub24 2023-2024 transition year___________________________________##

##creating and populating an endo_01 column in 2024 data
fessub24 <- left_join(x=fessub24data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2024 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2024 data (columns and rows) to 2022-2023 table in order to create 2023-2024 data
FESU_23_24 <- full_join(x=FESU_22_23 %>% filter(surv_t1==1),
                        y=fessub24,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2024
#names(FESU_23_24)

FESU_23_24$birth <- FESU_23_24$birth_year
FESU_23_24$year_t <- 2023
FESU_23_24$year_t1 <- 2024
FESU_23_24$age <- FESU_23_24$year_t - FESU_23_24$birth
FESU_23_24$size_t <- FESU_23_24$size_t1
FESU_23_24$flw_count_t <- FESU_23_24$flw_count_t1
FESU_23_24$mean_spike_t <- FESU_23_24$mean_spike_t1
FESU_23_24$surv_t1 <- FESU_23_24$survival
FESU_23_24$size_t1 <- FESU_23_24$size_tillers
FESU_23_24$flw_count_t1 <- FESU_23_24$flowering_tillers
FESU_23_24$mean_spike_t1 <- FESU_23_24$mean_spike
FESU_23_24$dist_a <- FESU_23_24$distance_A
FESU_23_24$dist_b <- FESU_23_24$distance_B

##removing excess columns
#names(FESU_23_24)

FESU_23_24 <- FESU_23_24[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                          
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                     
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(FESU_23_24)


#### ADDING 2025 DATA________________________________________________________##

##read in 2025 data
#no data for POAL because plots went extinct in 2024
poasyl25data <- read.csv("data/2025/LTREB_data_2025_recruits_inc.xlsx - POSY.csv")
elyvir25data <- read.csv("data/2025/LTREB_data_2025_recruits_inc.xlsx - ELVI.csv")
elyvil25data <- read.csv("data/2025/LTREB_data_2025_recruits_inc.xlsx - ELRI.csv")
agrper25data <- read.csv("data/2025/LTREB_data_2025_recruits_inc.xlsx - AGPE.csv")
fessub25data <- read.csv("data/2025/LTREB_data_2025_recruits_inc.xlsx - FESU.csv")


##all the elyvir,elyvil,agrper,fessub have birth_year as chr 
##instead of int or num except agreper24 & fessub25


###POAL 2025 has no data___________________________________##

###Creating POSY 2024-2025 transition year___________________________________##

##creating and populating an endo_01 column in 2025 data
poasyl25 <- left_join(x=poasyl25data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2025 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2025 data (columns and rows) to 2023-2024 table in order to create 2024-2025 data
POSY_24_25 <- full_join(x=POSY_23_24 %>% filter(surv_t1==1),
                        y=poasyl25,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2025
#names(POSY_24_25)

POSY_24_25$birth <- POSY_24_25$birth_year
POSY_24_25$year_t <- 2024
POSY_24_25$year_t1 <- 2025
POSY_24_25$age <- POSY_24_25$year_t - POSY_24_25$birth
POSY_24_25$size_t <- POSY_24_25$size_t1
POSY_24_25$flw_count_t <- POSY_24_25$flw_count_t1
POSY_24_25$mean_spike_t <- POSY_24_25$mean_spike_t1
POSY_24_25$surv_t1 <- POSY_24_25$survival
POSY_24_25$size_t1 <- POSY_24_25$size_tillers
POSY_24_25$flw_count_t1 <- POSY_24_25$flowering_tillers
POSY_24_25$mean_spike_t1 <- POSY_24_25$mean_spike
POSY_24_25$dist_a <- POSY_24_25$distance_A
POSY_24_25$dist_b <- POSY_24_25$distance_B

##removing excess columns
#names(POSY_24_25)

POSY_24_25 <- POSY_24_25[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1", 
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(POSY_24_25)

###Creating elyvir25 2024-2025 transition year___________________________________##

##creating and populating an endo_01 column in 2025 data
elyvir25 <- left_join(x=elyvir25data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2025 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2025 data (columns and rows) to 2023-2024 table in order to create 2024-2025 data
ELVI_24_25 <- full_join(x=ELVI_23_24 %>% filter(surv_t1==1),
                        y=elyvir25,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2025
#names(ELVI_24_25)

ELVI_24_25$birth <- ELVI_24_25$birth_year
ELVI_24_25$year_t <- 2024
ELVI_24_25$year_t1 <- 2025
ELVI_24_25$age <- ELVI_24_25$year_t - ELVI_24_25$birth
ELVI_24_25$size_t <- ELVI_24_25$size_t1
ELVI_24_25$flw_count_t <- ELVI_24_25$flw_count_t1
ELVI_24_25$mean_spike_t <- ELVI_24_25$mean_spike_t1
ELVI_24_25$surv_t1 <- ELVI_24_25$survival
ELVI_24_25$size_t1 <- ELVI_24_25$size_tillers
ELVI_24_25$flw_count_t1 <- ELVI_24_25$flowering_tillers
ELVI_24_25$mean_spike_t1 <- ELVI_24_25$mean_spike
ELVI_24_25$dist_a <- ELVI_24_25$distance_A
ELVI_24_25$dist_b <- ELVI_24_25$distance_B

##removing excess columns
#names(ELVI_24_25)

ELVI_24_25 <- ELVI_24_25[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                             "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(ELVI_24_25)

###Creating elyvil25 2024-2025 transition year___________________________________##

##creating and populating an endo_01 column in 2025 data
elyvil25 <- left_join(x=elyvil25data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2025 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2025 data (columns and rows) to 2023-2024 table in order to create 2024-2025 data
ELRI_24_25 <- full_join(x=ELRI_23_24 %>% filter(surv_t1==1),
                        y=elyvil25,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2025
#names(ELRI_24_25)

ELRI_24_25$birth <- ELRI_24_25$birth_year
ELRI_24_25$year_t <- 2024
ELRI_24_25$year_t1 <- 2025
ELRI_24_25$age <- ELRI_24_25$year_t - ELRI_24_25$birth
ELRI_24_25$size_t <- ELRI_24_25$size_t1
ELRI_24_25$flw_count_t <- ELRI_24_25$flw_count_t1
ELRI_24_25$mean_spike_t <- ELRI_24_25$mean_spike_t1
ELRI_24_25$surv_t1 <- ELRI_24_25$survival
ELRI_24_25$size_t1 <- ELRI_24_25$size_tillers
ELRI_24_25$flw_count_t1 <- ELRI_24_25$flowering_tillers
ELRI_24_25$mean_spike_t1 <- ELRI_24_25$mean_spike
ELRI_24_25$dist_a <- ELRI_24_25$distance_A
ELRI_24_25$dist_b <- ELRI_24_25$distance_B

##removing excess columns
#names(ELRI_24_25)

ELRI_24_25 <- ELRI_24_25[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                             "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(ELRI_24_25)

###Creating agrper25 2024-2025 transition year___________________________________##

##creating and populating an endo_01 column in 2025 data
agrper25 <- left_join(x=agrper25data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2025 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2025 data (columns and rows) to 2023-2024 table in order to create 2024-2025 data
AGPE_24_25 <- full_join(x=AGPE_23_24 %>% filter(surv_t1==1),
                        y=agrper25,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2025
#names(AGPE_24_25)

AGPE_24_25$birth <- AGPE_24_25$birth_year
AGPE_24_25$year_t <- 2024
AGPE_24_25$year_t1 <- 2025
AGPE_24_25$age <- AGPE_24_25$year_t - AGPE_24_25$birth
AGPE_24_25$size_t <- AGPE_24_25$size_t1
AGPE_24_25$flw_count_t <- AGPE_24_25$flw_count_t1
AGPE_24_25$mean_spike_t <- AGPE_24_25$mean_spike_t1
AGPE_24_25$surv_t1 <- AGPE_24_25$survival
AGPE_24_25$size_t1 <- AGPE_24_25$size_tillers
AGPE_24_25$flw_count_t1 <- AGPE_24_25$flowering_tillers
AGPE_24_25$mean_spike_t1 <- AGPE_24_25$mean_spike
AGPE_24_25$dist_a <- AGPE_24_25$distance_A
AGPE_24_25$dist_b <- AGPE_24_25$distance_B

##removing excess columns
#names(AGPE_24_25)

AGPE_24_25 <- AGPE_24_25[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                             "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",                             "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 
#View(AGPE_24_25)

###Creating fessub25 2024-2025 transition year___________________________________##

##creating and populating an endo_01 column in 2025 data
fessub25 <- left_join(x=fessub25data, y=plot_endo, by=c("species", "plot"))%>% 
  ##populating original column for new rows especially in 2025 data
  mutate (original = if_else(origin == "R",0,1), .after= origin) %>% 
  rowwise() %>% mutate(mean_spike=mean(c_across(c(spikelets_A,spikelets_B,spikelets_C)),na.rm=T)) %>%
  select(survival,size_tillers,flowering_tillers,mean_spike,id,origin,species,plot,birth_year,distance_A,distance_B,endo_01,original)

## adding 2025 data (columns and rows) to 2023-2024 table in order to create 2024-2025 data
FESU_24_25 <- full_join(x=FESU_23_24 %>% filter(surv_t1==1),
                        y=fessub25,
                        by=c("id","species", "plot","endo_01", "origin", "original")) 

## write over the relevant data for 2025
#names(FESU_24_25)

FESU_24_25$birth <- FESU_24_25$birth_year
FESU_24_25$year_t <- 2024
FESU_24_25$year_t1 <- 2025
FESU_24_25$age <- FESU_24_25$year_t - FESU_24_25$birth
FESU_24_25$size_t <- FESU_24_25$size_t1
FESU_24_25$flw_count_t <- FESU_24_25$flw_count_t1
FESU_24_25$mean_spike_t <- FESU_24_25$mean_spike_t1
FESU_24_25$surv_t1 <- FESU_24_25$survival
FESU_24_25$size_t1 <- FESU_24_25$size_tillers
FESU_24_25$flw_count_t1 <- FESU_24_25$flowering_tillers
FESU_24_25$mean_spike_t1 <- FESU_24_25$mean_spike
FESU_24_25$dist_a <- FESU_24_25$distance_A
FESU_24_25$dist_b <- FESU_24_25$distance_B

##removing excess columns
#names(FESU_24_25)

FESU_24_25 <- FESU_24_25[,c("X","species","plot","endo_01","id","original","endo_status_from_check",                     
                            "birth","year_t","age","size_t","flw_count_t","mean_spike_t","year_t1",  
                            "surv_t1","size_t1","flw_count_t1","mean_spike_t1","dist_a","dist_b","origin"),drop=FALSE] 

bind_rows(ltreb_allspp_qaqc,
          POAL_22_23,POSY_22_23,ELVI_22_23,ELRI_22_23,AGPE_22_23,FESU_22_23,
          POAL_23_24,POSY_23_24,ELVI_23_24,ELRI_23_24,AGPE_23_24,FESU_23_24,
          POSY_24_25,ELVI_24_25,ELRI_24_25,AGPE_24_25,FESU_24_25) %>% write.csv("ltreb_allspp_2007_2025.csv")

##The index column (X) kind of got duplicated
