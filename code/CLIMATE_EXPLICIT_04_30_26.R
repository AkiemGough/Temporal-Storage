#--------------------#
#title: "Climate explicit models of precipitation, temperature, VPDmax and endophyte effects on plant demography"
#author: "Akiem_Gough"
#date: "2026-04-30"
#--------------------#

#Important packages
library(tidyverse)
library(tidybayes)
library(lme4)
library(scales)
library(rstan)
library(bayesplot)
options(mc.cores = parallel::detectCores())
library(dplyr)

#reading in the data file with demographic and climate data
grasclim <-read.csv("data/CombinedDataSegments")

##cleaning up data frame
#removing rows with untrusted data
grasclim %>% filter(size_t>0) -> grasclim

##Making integers for species
grasclim$spec <- as.integer (case_when(grasclim$species == "POSY" ~ 1,
                                       grasclim$species == "ELRI" ~ 2,
                                       grasclim$species == "ELVI" ~ 3,
                                       grasclim$species == "FESU" ~ 4,
                                       grasclim$species == "LOAR" ~ 5,
                                       grasclim$species == "POAL" ~ 6,
                                       grasclim$species == "POAU" ~ 7,
                                       grasclim$species == "AGPE" ~ 8))

##centering size and climate variables
grasclim$log_tillers_centered <- log(grasclim$size_t) - mean(log(grasclim$size_t),na.rm=T)
#grasclim$ppt_tot_scaled <- as.numeric (scale(grasclim$ppt_tot))

#renaming origin_01 
grasclim$original <- grasclim$origin_01

#Generating new data frame for Population Growth Rate

pop_growth_df <- grasclim %>%
  # 1. Group by the variables that define a "population"
  group_by(species, plot, endo_01, year_t, spec, 
           firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
           fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
           firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
           fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
           firstthreeback_vpdmax,secondthreeback_vpdmax,thirdthreeback_vpdmax,fourththreeback_vpdmax,
           fifththreeback_vpdmax,sixththreeback_vpdmax,sevenththreeback_vpdmax,eighththreeback_vpdmax) %>%
  # 2. Count the number of individuals (N) in each group/year
  summarise(N_t = n(), .groups = "drop") %>%
  # 3. Arrange by year to ensure the math follows the timeline
  arrange(species, plot, endo_01, year_t) %>%
  # 4. Group again to calculate growth within each specific plot
  group_by(species, plot, endo_01) %>%
  mutate(
    N_t_plus_1 = lead(N_t),
    lambda = N_t_plus_1 / N_t,
    r = log(lambda)) %>%
  filter(!is.na(r)) # Remove the last year for each group (no t+1 data)




##MODEL FPT: PROBABILITY OF FLOWERING AS RESPONSE TO PREICIPITATION AND TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t,endo_01,spec,log_tillers_centered,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
         fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
         plot,year_t,original) %>% 
  drop_na() -> all_flow_exp

# Safety conversion to continuous sequential factor integers 
all_flow_exp$plot    <- as.integer(as.factor(all_flow_exp$plot))
all_flow_exp$spec    <- as.integer(as.factor(all_flow_exp$spec))
all_flow_exp$year_t  <- as.integer(as.factor(all_flow_exp$year_t))

all_flow_dat_exp<-list(n_obs=nrow(all_flow_exp),
                       y=as.integer(all_flow_exp$flw_count_t > 0),
                       n_yrs = length(unique(all_flow_exp$year_t)),
                       n_plots = length(unique(all_flow_exp$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_flow_exp$spec)),
                       endo_01= as.integer(all_flow_exp$endo_01),
                       size=as.numeric(all_flow_exp$log_tillers_centered),
                       K = 8,
                       precip = as.matrix(scale(all_flow_exp[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                  "thirdthreeback_ppt","fourththreeback_ppt",
                                                                  "fifththreeback_ppt","sixththreeback_ppt",
                                                                  "sevenththreeback_ppt","eighththreeback_ppt")])),
                       temper = as.matrix(scale(all_flow_exp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                   "thirdthreeback_tmean","fourththreeback_tmean",
                                                                   "fifththreeback_tmean","sixththreeback_tmean",
                                                                   "sevenththreeback_tmean","eighththreeback_tmean")])),
                       year_index = all_flow_exp$year_t,
                       plot=all_flow_exp$plot,
                       species=all_flow_exp$spec,
                       original= as.integer(all_flow_exp$original))

all_flow_model_exp = stan_model(file="code/explicit_binomial_SAM.stan")
all_flow_sampling_exp <- sampling(all_flow_model_exp,
                                  data = all_flow_dat_exp,
                                  chains = 3, 
                                  iter = 10000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_prec","beta_temp","sigma_year",
                                         "sigma_plot", "w_prec", "w_temp", "y_rep"),
                                  include = TRUE)


saveRDS(all_flow_sampling_exp,"all_flow_sampling_exp.rds")
all_flow_sampling_exp<-readRDS("all_flow_sampling_exp.rds")

mcmc_intervals(all_flow_sampling_exp,regex_pars = "beta_prec")
mcmc_intervals(all_flow_sampling_exp,regex_pars = "beta_temp")

##trace plots of beta clim and w
mcmc_trace(all_flow_sampling_exp,regex_pars = "beta_0")
mcmc_trace(all_flow_sampling_exp,regex_pars = "beta_prec") #looks weird for 2 of them
mcmc_trace(all_flow_sampling_exp,regex_pars = "beta_temp") #one of them looks weird
mcmc_trace(all_flow_sampling_exp,regex_pars = "w_prec")
mcmc_trace(all_flow_sampling_exp,regex_pars = "w_temp")

##posterior predictive check
y_rep<-extract(all_flow_sampling_exp,pars="y_rep")
ppc_dens_overlay(all_flow_dat_exp$y,y_rep$y_rep[1:500,])

summary(all_flow_sampling_exp)

#extracting parameters
params_all_f_exp<-rstan::extract(all_flow_sampling_exp,pars=c('beta_0','beta_prec','beta_temp','w_prec', 'w_temp'))
dim(params_all_f_exp$beta_0)
dim(params_all_f_exp$beta_prec)
dim(params_all_f_exp$beta_temp)
dim(params_all_f_exp$w_prec)
dim(params_all_f_exp$w_temp)

##PLOTTING PREC EFFECTS

#take a random subset of posterior draws for the slopes / beta_prec / precipitation effects
all_betaprec_postf<-params_all_f_exp$beta_prec[sample(dim(params_all_f_exp$beta_prec)[1],size=1000),,]
dim(all_betaprec_postf)

long_df_all_betaprecf <- as.data.frame.table(all_betaprec_postf,
                                                 responseName = "estimate")
str(long_df_all_betaprecf)

# Convert to long data frame
long_df_all_betaprecf <- as.data.frame.table(all_betaprec_postf,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaprecf$spec <- case_when(long_df_all_betaprecf$species == 8 ~ "AGPE",
                                            long_df_all_betaprecf$species == 2 ~ "ELRI",
                                            long_df_all_betaprecf$species == 3 ~ "ELVI",
                                            long_df_all_betaprecf$species == 4 ~ "FESU",
                                            long_df_all_betaprecf$species == 5 ~ "LOAR",
                                            long_df_all_betaprecf$species == 6 ~ "POAL",
                                            long_df_all_betaprecf$species == 7 ~ "POAU",
                                            long_df_all_betaprecf$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_fp <- long_df_all_betaprecf %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data_fp, aes(x = slope_val, y = spec, color = endo_label)) +
  # Draw the 50% and 95% intervals + median dot (mimicking mcmc_intervals)
  stat_pointinterval(.width = c(0.5, 0.95), 
                     position = position_dodge(width = 0.4)) +
  
  # Add a vertical reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Set the specific colors you requested
  scale_color_manual(values = c("E-" = "deeppink", 
                                "E+" = "cornflowerblue")) +
  
  # Clean up formatting
  labs(
    x = "Posterior Climate Coefficient (precipitation)",
    y = "Species",
    color = "Endophyte Status",
    title = "Precipitation Effects on Probability of Flowering"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#it appears all species' flowering are effected by precipitation but none have interactions between prec and endo
#except for POSY and ELRI maybe

##PLOTTING TEMP EFFECTS

#take a random subset of posterior draws for the slopes / beta_temp / temperature effects
all_betatemp_postf<-params_all_f_exp$beta_temp[sample(dim(params_all_f_exp$beta_temp)[1],size=1000),,]
dim(all_betatemp_postf)

long_df_all_betatempf <- as.data.frame.table(all_betatemp_postf,
                                             responseName = "estimate")
str(long_df_all_betatempf)

# Convert to long data frame
long_df_all_betatempf <- as.data.frame.table(all_betatemp_postf,
                                             responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betatempf$spec <- case_when(long_df_all_betatempf$species == 8 ~ "AGPE",
                                        long_df_all_betatempf$species == 2 ~ "ELRI",
                                        long_df_all_betatempf$species == 3 ~ "ELVI",
                                        long_df_all_betatempf$species == 4 ~ "FESU",
                                        long_df_all_betatempf$species == 5 ~ "LOAR",
                                        long_df_all_betatempf$species == 6 ~ "POAL",
                                        long_df_all_betatempf$species == 7 ~ "POAU",
                                        long_df_all_betatempf$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_ft <- long_df_all_betatempf %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data_ft, aes(x = slope_val, y = spec, color = endo_label)) +
  # Draw the 50% and 95% intervals + median dot (mimicking mcmc_intervals)
  stat_pointinterval(.width = c(0.5, 0.95), 
                     position = position_dodge(width = 0.4)) +
  
  # Add a vertical reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Set the specific colors you requested
  scale_color_manual(values = c("E-" = "deeppink", 
                                "E+" = "cornflowerblue")) +
  
  # Clean up formatting
  labs(
    x = "Posterior Climate Coefficient (temperature)",
    y = "Species",
    color = "Endophyte Status",
    title = "Temperature Effects on Probability of Flowering"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#it appears AGPE flowering is strongly affected by temperature, and E+ and E- respond differently for this species
#POSY and FESU are also affected by temperature and have a bit of interaction between endo and temp
#LOAR experiences some effect from temperature but no interaction, and the other species experience neither

##PLOTTING PRECIPITATION TIME LAGS / Ws
#take a random subset of posterior draws for the w_prec
all_w_postf_ppt<-params_all_f_exp$w_prec[sample(dim(params_all_f_exp$w_prec)[1],size=1000),,]
dim(all_w_postf_ppt)

long_df_all_wf_ppt <- as.data.frame.table(all_w_postf_ppt,
                                          responseName = "estimate")
str(long_df_all_wf_ppt)

# Convert to long data frame
long_df_all_wf_ppt <- as.data.frame.table(all_w_postf_ppt,
                                          responseName = "weight") %>%
  rename(draw = iterations, species = Var2, threemonth = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_wf_ppt$spec <- case_when(long_df_all_wf_ppt$species == 8 ~ "AGPE",
                                     long_df_all_wf_ppt$species == 2 ~ "ELRI",
                                     long_df_all_wf_ppt$species == 3 ~ "ELVI",
                                     long_df_all_wf_ppt$species == 4 ~ "FESU",
                                     long_df_all_wf_ppt$species == 5 ~ "LOAR",
                                     long_df_all_wf_ppt$species == 6 ~ "POAL",
                                     long_df_all_wf_ppt$species == 7 ~ "POAU",
                                     long_df_all_wf_ppt$species == 1 ~ "POSY")

long_df_all_wf_ppt$monthsprior <- case_when(long_df_all_wf_ppt$threemonth == "A" ~ "1-3",
                                            long_df_all_wf_ppt$threemonth == "B" ~ "4-6",
                                            long_df_all_wf_ppt$threemonth == "C" ~ "7-9",
                                            long_df_all_wf_ppt$threemonth == "D" ~ "10-12",
                                            long_df_all_wf_ppt$threemonth == "E" ~ "13-15",
                                            long_df_all_wf_ppt$threemonth == "F" ~ "16-18",
                                            long_df_all_wf_ppt$threemonth == "G" ~ "19-21",
                                            long_df_all_wf_ppt$threemonth == "H" ~ "22-24")

long_df_all_wf_ppt$monthsprior <- factor(long_df_all_wf_ppt$monthsprior,
                                         levels=c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-24"))

summary_df_all_wf_ppt <- long_df_all_wf_ppt %>%
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wf_ppt)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

ggplot(data = summary_df_all_wf_ppt, aes(x = monthsprior, y = median_weight, color = spec, group=spec)) +
  geom_point() + geom_line() +
  scale_color_manual(
    values = c(
      "AGPE" = "olivedrab",
      "ELRI" = "goldenrod",
      "ELVI" = "darkorange",
      "FESU" = "tomato",
      "LOAR" = "deeppink",
      "POAL" = "purple",
      "POAU" = "slateblue",
      "POSY" = "cornflowerblue")) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Precipitation in Each Time Scale on Probability of Flowering")

#most of the species experience multiple important time lapses of precipitation across 2 years

##PLOTTING TEMPERATURE TIME LAGS / Ws
#take a random subset of posterior draws for the w_temp
all_w_postf_temp<-params_all_f_exp$w_temp[sample(dim(params_all_f_exp$w_temp)[1],size=1000),,]
dim(all_w_postf_temp)

long_df_all_wf_temp <- as.data.frame.table(all_w_postf_temp,
                                          responseName = "estimate")
str(long_df_all_wf_temp)

# Convert to long data frame
long_df_all_wf_temp <- as.data.frame.table(all_w_postf_temp,
                                          responseName = "weight") %>%
  rename(draw = iterations, species = Var2, threemonth = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_wf_temp$spec <- case_when(long_df_all_wf_temp$species == 8 ~ "AGPE",
                                     long_df_all_wf_temp$species == 2 ~ "ELRI",
                                     long_df_all_wf_temp$species == 3 ~ "ELVI",
                                     long_df_all_wf_temp$species == 4 ~ "FESU",
                                     long_df_all_wf_temp$species == 5 ~ "LOAR",
                                     long_df_all_wf_temp$species == 6 ~ "POAL",
                                     long_df_all_wf_temp$species == 7 ~ "POAU",
                                     long_df_all_wf_temp$species == 1 ~ "POSY")

long_df_all_wf_temp$monthsprior <- case_when(long_df_all_wf_temp$threemonth == "A" ~ "1-3",
                                            long_df_all_wf_temp$threemonth == "B" ~ "4-6",
                                            long_df_all_wf_temp$threemonth == "C" ~ "7-9",
                                            long_df_all_wf_temp$threemonth == "D" ~ "10-12",
                                            long_df_all_wf_temp$threemonth == "E" ~ "13-15",
                                            long_df_all_wf_temp$threemonth == "F" ~ "16-18",
                                            long_df_all_wf_temp$threemonth == "G" ~ "19-21",
                                            long_df_all_wf_temp$threemonth == "H" ~ "22-24")

long_df_all_wf_temp$monthsprior <- factor(long_df_all_wf_temp$monthsprior,
                                         levels=c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-24"))

summary_df_all_wf_temp <- long_df_all_wf_temp %>%
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wf_temp)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

ggplot(data = summary_df_all_wf_temp, aes(x = monthsprior, y = median_weight, color = spec, group=spec)) +
  geom_point() + geom_line() +
  scale_color_manual(
    values = c(
      "AGPE" = "olivedrab",
      "ELRI" = "goldenrod",
      "ELVI" = "darkorange",
      "FESU" = "tomato",
      "LOAR" = "deeppink",
      "POAL" = "purple",
      "POAU" = "slateblue",
      "POSY" = "cornflowerblue")) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Temperature in Each Time Scale on Probability of Flowering")

#temperature effects for most species seems explainable by cumulative temperature over 2 years
#4 for 4 of the species tho, further back time lags of temperature are most impactful

##MODEL SPT: PROBABILITY OF SURVIVAL AS RESPONSE TO PREICIPITATION AND TEMPERATURE

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(surv_t1,endo_01,spec,log_tillers_centered,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
         fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
         plot,year_t,original) %>% 
  drop_na() -> all_surv_exp

# Safety conversion to continuous sequential factor integers 
all_surv_exp$plot    <- as.integer(as.factor(all_surv_exp$plot))
all_surv_exp$spec    <- as.integer(as.factor(all_surv_exp$spec))
all_surv_exp$year_t  <- as.integer(as.factor(all_surv_exp$year_t))

all_surv_dat_exp<-list(n_obs=nrow(all_surv_exp),
                       y=as.integer(all_surv_exp$surv_t1),
                       n_yrs = length(unique(all_surv_exp$year_t)),
                       n_plots = length(unique(all_surv_exp$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_surv_exp$spec)),
                       endo_01= as.integer(all_surv_exp$endo_01),
                       size=as.numeric(all_surv_exp$log_tillers_centered),
                       K = 8,
                       precip = as.matrix(scale(all_surv_exp[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                 "thirdthreeback_ppt","fourththreeback_ppt",
                                                                 "fifththreeback_ppt","sixththreeback_ppt",
                                                                 "sevenththreeback_ppt","eighththreeback_ppt")])),
                       temper = as.matrix(scale(all_surv_exp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                 "thirdthreeback_tmean","fourththreeback_tmean",
                                                                 "fifththreeback_tmean","sixththreeback_tmean",
                                                                 "sevenththreeback_tmean","eighththreeback_tmean")])),
                       year_index = all_surv_exp$year_t,
                       plot=all_surv_exp$plot,
                       species=all_surv_exp$spec,
                       original= as.integer(all_surv_exp$original))

all_surv_model_exp = stan_model(file="code/explicit_binomial_SAM.stan")
all_surv_sampling_exp <- sampling(all_surv_model_exp,
                                  data = all_surv_dat_exp,
                                  chains = 3, 
                                  iter = 10000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_prec","beta_temp","sigma_year",
                                         "sigma_plot", "w_prec","w_temp","y_rep"),
                                  include = TRUE)

saveRDS(all_surv_sampling_exp,"all_surv_sampling_exp.rds")
all_surv_sampling_exp<-readRDS("all_surv_sampling_exp.rds")

saveRDS(all_surv_sampling_exp1,"all_surv_sampling_exp.rds")#newest priors
all_surv_sampling_exp<-readRDS("all_surv_sampling_exp1.rds")#newest priors

mcmc_intervals(all_surv_sampling_exp,regex_pars = "beta_prec")
mcmc_intervals(all_surv_sampling_exp,regex_pars = "beta_temp")
#note to self - make the E+ E- pairs close to each other and distinguished by color

##trace plots of beta clim and w
mcmc_trace(all_surv_sampling_exp,regex_pars = "beta_0") #a few look kinda weird
mcmc_trace(all_surv_sampling_exp,regex_pars = "beta_prec") #looks bad
mcmc_trace(all_surv_sampling_exp,regex_pars = "beta_temp") # looks bad
mcmc_trace(all_surv_sampling_exp,regex_pars = "w_prec")
mcmc_trace(all_surv_sampling_exp,regex_pars = "w_temp")

##posterior predictive check
y_rep<-extract(all_surv_sampling_exp,pars="y_rep")
ppc_dens_overlay(all_surv_dat_exp$y,y_rep$y_rep[1:500,])

summary(all_surv_sampling_exp)

#extracting parameters
params_all_s_exp<-rstan::extract(all_surv_sampling_exp,pars=c('beta_0','beta_prec','beta_temp','w_prec','w_temp'))
dim(params_all_s_exp$beta_0)
dim(params_all_s_exp$beta_prec)
dim(params_all_s_exp$beta_temp)
dim(params_all_s_exp$w_prec)
dim(params_all_s_exp$w_temp)


##PLOTTING PREC EFFECTS

#take a random subset of posterior draws for the slopes / beta_prec / precipitation effects
all_betaprec_posts<-params_all_s_exp$beta_prec[sample(dim(params_all_s_exp$beta_prec)[1],size=1000),,]
dim(all_betaprec_posts)

long_df_all_betaprecs <- as.data.frame.table(all_betaprec_posts,
                                             responseName = "estimate")
str(long_df_all_betaprecs)

# Convert to long data frame
long_df_all_betaprecs <- as.data.frame.table(all_betaprec_posts,
                                             responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaprecs$spec <- case_when(long_df_all_betaprecs$species == 8 ~ "AGPE",
                                        long_df_all_betaprecs$species == 2 ~ "ELRI",
                                        long_df_all_betaprecs$species == 3 ~ "ELVI",
                                        long_df_all_betaprecs$species == 4 ~ "FESU",
                                        long_df_all_betaprecs$species == 5 ~ "LOAR",
                                        long_df_all_betaprecs$species == 6 ~ "POAL",
                                        long_df_all_betaprecs$species == 7 ~ "POAU",
                                        long_df_all_betaprecs$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_sp <- long_df_all_betaprecs %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data_sp, aes(x = slope_val, y = spec, color = endo_label)) +
  # Draw the 50% and 95% intervals + median dot (mimicking mcmc_intervals)
  stat_pointinterval(.width = c(0.5, 0.95), 
                     position = position_dodge(width = 0.4)) +
  
  # Add a vertical reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Set the specific colors you requested
  scale_color_manual(values = c("E-" = "deeppink", 
                                "E+" = "cornflowerblue")) +
  
  # Clean up formatting
  labs(
    x = "Posterior Climate Coefficient (precipitation)",
    y = "Species",
    color = "Endophyte Status",
    title = "Precipitation Effects on Probability of Survival"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#it aappears precipitation only has a bit of effect on POSY, LOAR, ELVI and AGPE survival
#only AGPE has strong evidence and none of the species show an interaction between endo and prec effects

##PLOTTING TEMP EFFECTS

#take a random subset of posterior draws for the slopes / beta_temp / temperature effects
all_betatemp_posts<-params_all_s_exp$beta_temp[sample(dim(params_all_s_exp$beta_temp)[1],size=1000),,]
dim(all_betatemp_posts)

long_df_all_betatemps <- as.data.frame.table(all_betatemp_posts,
                                             responseName = "estimate")
str(long_df_all_betatemps)

# Convert to long data frame
long_df_all_betatemps <- as.data.frame.table(all_betatemp_posts,
                                             responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betatemps$spec <- case_when(long_df_all_betatemps$species == 8 ~ "AGPE",
                                        long_df_all_betatemps$species == 2 ~ "ELRI",
                                        long_df_all_betatemps$species == 3 ~ "ELVI",
                                        long_df_all_betatemps$species == 4 ~ "FESU",
                                        long_df_all_betatemps$species == 5 ~ "LOAR",
                                        long_df_all_betatemps$species == 6 ~ "POAL",
                                        long_df_all_betatemps$species == 7 ~ "POAU",
                                        long_df_all_betatemps$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_st <- long_df_all_betatemps %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data_st, aes(x = slope_val, y = spec, color = endo_label)) +
  # Draw the 50% and 95% intervals + median dot (mimicking mcmc_intervals)
  stat_pointinterval(.width = c(0.5, 0.95), 
                     position = position_dodge(width = 0.4)) +
  
  # Add a vertical reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Set the specific colors you requested
  scale_color_manual(values = c("E-" = "deeppink", 
                                "E+" = "cornflowerblue")) +
  
  # Clean up formatting
  labs(
    x = "Posterior Climate Coefficient (temperature)",
    y = "Species",
    color = "Endophyte Status",
    title = "Temperature Effects on Probability of Survival"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#It appears AGPE is especially affected by temperature but not by endo and temp interaction 
#it is hard to tell with the other species

##PLOTTING PRECIPITATION TIME LAGS / Ws
#take a random subset of posterior draws for the w_prec
all_w_posts_ppt<-params_all_s_exp$w_prec[sample(dim(params_all_s_exp$w_prec)[1],size=1000),,]
dim(all_w_posts_ppt)

long_df_all_ws_ppt <- as.data.frame.table(all_w_posts_ppt,
                                          responseName = "estimate")
str(long_df_all_ws_ppt)

# Convert to long data frame
long_df_all_ws_ppt <- as.data.frame.table(all_w_posts_ppt,
                                          responseName = "weight") %>%
  rename(draw = iterations, species = Var2, threemonth = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_ws_ppt$spec <- case_when(long_df_all_ws_ppt$species == 8 ~ "AGPE",
                                     long_df_all_ws_ppt$species == 2 ~ "ELRI",
                                     long_df_all_ws_ppt$species == 3 ~ "ELVI",
                                     long_df_all_ws_ppt$species == 4 ~ "FESU",
                                     long_df_all_ws_ppt$species == 5 ~ "LOAR",
                                     long_df_all_ws_ppt$species == 6 ~ "POAL",
                                     long_df_all_ws_ppt$species == 7 ~ "POAU",
                                     long_df_all_ws_ppt$species == 1 ~ "POSY")

long_df_all_ws_ppt$monthsprior <- case_when(long_df_all_ws_ppt$threemonth == "A" ~ "1-3",
                                            long_df_all_ws_ppt$threemonth == "B" ~ "4-6",
                                            long_df_all_ws_ppt$threemonth == "C" ~ "7-9",
                                            long_df_all_ws_ppt$threemonth == "D" ~ "10-12",
                                            long_df_all_ws_ppt$threemonth == "E" ~ "13-15",
                                            long_df_all_ws_ppt$threemonth == "F" ~ "16-18",
                                            long_df_all_ws_ppt$threemonth == "G" ~ "19-21",
                                            long_df_all_ws_ppt$threemonth == "H" ~ "22-24")

long_df_all_ws_ppt$monthsprior <- factor(long_df_all_ws_ppt$monthsprior,
                                         levels=c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-24"))

summary_df_all_ws_ppt <- long_df_all_ws_ppt %>%
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wf_ppt)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

ggplot(data = summary_df_all_ws_ppt, aes(x = monthsprior, y = median_weight, color = spec, group=spec)) +
  geom_point() + geom_line() +
  scale_color_manual(
    values = c(
      "AGPE" = "olivedrab",
      "ELRI" = "goldenrod",
      "ELVI" = "darkorange",
      "FESU" = "tomato",
      "LOAR" = "deeppink",
      "POAL" = "purple",
      "POAU" = "slateblue",
      "POSY" = "cornflowerblue")) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Precipitation in Each Time Scale on Probability of Survival")

#most of the species are most impacted by either recent or very far back precipitation or both

##PLOTTING TEMPERATURE TIME LAGS / Ws
#take a random subset of posterior draws for the w_temp
all_w_posts_temp<-params_all_s_exp$w_temp[sample(dim(params_all_s_exp$w_temp)[1],size=1000),,]
dim(all_w_posts_temp)

long_df_all_ws_temp <- as.data.frame.table(all_w_posts_temp,
                                           responseName = "estimate")
str(long_df_all_ws_temp)

# Convert to long data frame
long_df_all_ws_temp <- as.data.frame.table(all_w_posts_temp,
                                           responseName = "weight") %>%
  rename(draw = iterations, species = Var2, threemonth = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_ws_temp$spec <- case_when(long_df_all_ws_temp$species == 8 ~ "AGPE",
                                      long_df_all_ws_temp$species == 2 ~ "ELRI",
                                      long_df_all_ws_temp$species == 3 ~ "ELVI",
                                      long_df_all_ws_temp$species == 4 ~ "FESU",
                                      long_df_all_ws_temp$species == 5 ~ "LOAR",
                                      long_df_all_ws_temp$species == 6 ~ "POAL",
                                      long_df_all_ws_temp$species == 7 ~ "POAU",
                                      long_df_all_ws_temp$species == 1 ~ "POSY")

long_df_all_ws_temp$monthsprior <- case_when(long_df_all_ws_temp$threemonth == "A" ~ "1-3",
                                             long_df_all_ws_temp$threemonth == "B" ~ "4-6",
                                             long_df_all_ws_temp$threemonth == "C" ~ "7-9",
                                             long_df_all_ws_temp$threemonth == "D" ~ "10-12",
                                             long_df_all_ws_temp$threemonth == "E" ~ "13-15",
                                             long_df_all_ws_temp$threemonth == "F" ~ "16-18",
                                             long_df_all_ws_temp$threemonth == "G" ~ "19-21",
                                             long_df_all_ws_temp$threemonth == "H" ~ "22-24")

long_df_all_ws_temp$monthsprior <- factor(long_df_all_ws_temp$monthsprior,
                                          levels=c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-24"))

summary_df_all_ws_temp <- long_df_all_ws_temp %>%
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_ws_temp)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

ggplot(data = summary_df_all_ws_temp, aes(x = monthsprior, y = median_weight, color = spec, group=spec)) +
  geom_point() + geom_line() +
  scale_color_manual(
    values = c(
      "AGPE" = "olivedrab",
      "ELRI" = "goldenrod",
      "ELVI" = "darkorange",
      "FESU" = "tomato",
      "LOAR" = "deeppink",
      "POAL" = "purple",
      "POAU" = "slateblue",
      "POSY" = "cornflowerblue")) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Temperature in Each Time Scale on Probability of Survival")

#not sure how to summarize this 



##MODEL IPT: INFLORESCENE COUNT AS RESPONSE TO PREICIPITATION AND TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t1,endo_01,spec,log_tillers_centered,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
         fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
         plot,year_t,original) %>% 
  drop_na() -> all_infl_exp

# Safety conversion to continuous sequential factor integers 
all_infl_exp$plot    <- as.integer(as.factor(all_infl_exp$plot))
all_infl_exp$spec    <- as.integer(as.factor(all_infl_exp$spec))
all_infl_exp$year_t  <- as.integer(as.factor(all_infl_exp$year_t))

all_infl_dat_exp<-list(n_obs=nrow(all_infl_exp),
                       y=as.integer(all_infl_exp$flw_count_t1),
                       n_yrs = length(unique(all_infl_exp$year_t)),
                       n_plots = length(unique(all_infl_exp$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_infl_exp$spec)),
                       endo_01= as.integer(all_infl_exp$endo_01),
                       size=as.numeric(all_infl_exp$log_tillers_centered),
                       K = 8,
                       precip = as.matrix(scale(all_infl_exp[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                 "thirdthreeback_ppt","fourththreeback_ppt",
                                                                 "fifththreeback_ppt","sixththreeback_ppt",
                                                                 "sevenththreeback_ppt","eighththreeback_ppt")])),
                       temper = as.matrix(scale(all_infl_exp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                 "thirdthreeback_tmean","fourththreeback_tmean",
                                                                 "fifththreeback_tmean","sixththreeback_tmean",
                                                                 "sevenththreeback_tmean","eighththreeback_tmean")])),
                       year_index = all_infl_exp$year_t,
                       plot=all_infl_exp$plot,
                       species=all_infl_exp$spec,
                       original= as.integer(all_infl_exp$original))

all_infl_model_exp = stan_model(file="code/explicit_negativebinomial_SAM_prior1.stan")
all_infl_sampling_exp1 <- sampling(all_infl_model_exp,
                                  data = all_infl_dat_exp,
                                  chains = 3, 
                                  iter = 10000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_prec","beta_temp","sigma_year",
                                         "sigma_plot", "w_prec","w_temp","y_rep"),
                                  include = TRUE)

saveRDS(all_infl_sampling_exp,"all_infl_sampling_exp.rds")
all_infl_sampling_exp<-readRDS("all_infl_sampling_exp.rds")

saveRDS(all_infl_sampling_exp1,"all_infl_sampling_exp1.rds")
all_infl_sampling_exp<-readRDS("all_infl_sampling_exp1.rds")

#WARNINGS: Warning messages:
#1: The largest R-hat is 2.13, indicating chains have not mixed.
#Running the chains for more iterations may help. See
#https://mc-stan.org/misc/warnings.html#r-hat 
#2: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#Running the chains for more iterations may help. See
#https://mc-stan.org/misc/warnings.html#bulk-ess 
#3: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#Running the chains for more iterations may help. See
#https://mc-stan.org/misc/warnings.html#tail-ess 

##trace plots of beta clim and w (priors 0,2 for beta0 and 0,0.2 betaprec & betatemp)
mcmc_trace(all_infl_sampling_exp,regex_pars = "beta_0")
mcmc_trace(all_infl_sampling_exp,regex_pars = "beta_prec") # 2 looks weird
mcmc_trace(all_infl_sampling_exp,regex_pars = "beta_temp") 
mcmc_trace(all_infl_sampling_exp,regex_pars = "w_prec")
mcmc_trace(all_infl_sampling_exp,regex_pars = "w_temp")

mcmc_trace(all_infl_sampling_exp,pars = "beta_prec[6,2]")
mcmc_trace(all_infl_sampling_exp,pars = "w_temp[5,2]")

##trace plots of beta clim and w (priors 0,1 for beta0, betaprec & betatemp)
mcmc_trace(all_infl_sampling_exp,regex_pars = "beta_0") #1 or 2 looks weird
mcmc_trace(all_infl_sampling_exp,regex_pars = "beta_prec") #all of them look weird
mcmc_trace(all_infl_sampling_exp,regex_pars = "beta_temp") # 5 or 6 looks weird
mcmc_trace(all_infl_sampling_exp,regex_pars = "w_prec")
mcmc_trace(all_infl_sampling_exp,regex_pars = "w_temp")

mcmc_trace(all_infl_sampling_exp,pars = "w_temp[5,2]")



mcmc_intervals(all_infl_sampling_exp,regex_pars = "beta_prec")
mcmc_intervals(all_infl_sampling_exp,regex_pars = "beta_temp")

##posterior predictive check
y_rep<-extract(all_infl_sampling_exp,pars="y_rep")
ppc_dens_overlay(all_infl_dat_exp$y,y_rep$y_rep[1:500,])+xlim(0,5)

summary(all_infl_sampling_exp)

#extracting parameters
params_all_i_exp<-rstan::extract(all_infl_sampling_exp,pars=c('beta_0','beta_prec','beta_temp','w_prec','w_temp'))
dim(params_all_i_exp$beta_0)
dim(params_all_i_exp$beta_prec)
dim(params_all_i_exp$beta_temp)
dim(params_all_i_exp$w_prec)
dim(params_all_i_exp$w_temp)




##PLOTTING PREC EFFECTS

#take a random subset of posterior draws for the slopes / beta_prec / precipitation effects
all_betaprec_posti<-params_all_i_exp$beta_prec[sample(dim(params_all_i_exp$beta_prec)[1],size=1000),,]
dim(all_betaprec_posti)

long_df_all_betapreci <- as.data.frame.table(all_betaprec_posti,
                                             responseName = "estimate")
str(long_df_all_betapreci)

# Convert to long data frame
long_df_all_betapreci <- as.data.frame.table(all_betaprec_posti,
                                             responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betapreci$spec <- case_when(long_df_all_betapreci$species == 8 ~ "AGPE",
                                        long_df_all_betapreci$species == 2 ~ "ELRI",
                                        long_df_all_betapreci$species == 3 ~ "ELVI",
                                        long_df_all_betapreci$species == 4 ~ "FESU",
                                        long_df_all_betapreci$species == 5 ~ "LOAR",
                                        long_df_all_betapreci$species == 6 ~ "POAL",
                                        long_df_all_betapreci$species == 7 ~ "POAU",
                                        long_df_all_betapreci$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_ip <- long_df_all_betapreci %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data_ip, aes(x = slope_val, y = spec, color = endo_label)) +
  # Draw the 50% and 95% intervals + median dot (mimicking mcmc_intervals)
  stat_pointinterval(.width = c(0.5, 0.95), 
                     position = position_dodge(width = 0.4)) +
  
  # Add a vertical reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Set the specific colors you requested
  scale_color_manual(values = c("E-" = "deeppink", 
                                "E+" = "cornflowerblue")) +
  
  # Clean up formatting
  labs(
    x = "Posterior Climate Coefficient (precipitation)",
    y = "Species",
    color = "Endophyte Status",
    title = "Precipitation Effects on Infloresecence Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#it appears precipitation is impactful to POSY, POAU, POAL, AGPE and maybe LOAR inflorescence count
#the POA species also show an interaction effect between endo and prec 
#that is precipitation affects E+ and E- inflorescence count a differently for POSY, POAU & POAL

##PLOTTING TEMP EFFECTS

#take a random subset of posterior draws for the slopes / beta_temp / temperature effects
all_betatemp_posti<-params_all_i_exp$beta_temp[sample(dim(params_all_i_exp$beta_temp)[1],size=1000),,]
dim(all_betatemp_posti)

long_df_all_betatempi <- as.data.frame.table(all_betatemp_posti,
                                             responseName = "estimate")
str(long_df_all_betatempi)

# Convert to long data frame
long_df_all_betatempi <- as.data.frame.table(all_betatemp_posti,
                                             responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betatempi$spec <- case_when(long_df_all_betatempi$species == 8 ~ "AGPE",
                                        long_df_all_betatempi$species == 2 ~ "ELRI",
                                        long_df_all_betatempi$species == 3 ~ "ELVI",
                                        long_df_all_betatempi$species == 4 ~ "FESU",
                                        long_df_all_betatempi$species == 5 ~ "LOAR",
                                        long_df_all_betatempi$species == 6 ~ "POAL",
                                        long_df_all_betatempi$species == 7 ~ "POAU",
                                        long_df_all_betatempi$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_it <- long_df_all_betatempi %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data_it, aes(x = slope_val, y = spec, color = endo_label)) +
  # Draw the 50% and 95% intervals + median dot (mimicking mcmc_intervals)
  stat_pointinterval(.width = c(0.5, 0.95), 
                     position = position_dodge(width = 0.4)) +
  
  # Add a vertical reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Set the specific colors you requested
  scale_color_manual(values = c("E-" = "deeppink", 
                                "E+" = "cornflowerblue")) +
  
  # Clean up formatting
  labs(
    x = "Posterior Climate Coefficient (temperature)",
    y = "Species",
    color = "Endophyte Status",
    title = "Temperature Effects on Inflorescence Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#it appears temperature is impactful to only AGPE inflorescence count, maybe POSY and POAU
#these species also so a bit of interaction effect between endo and temp
#that is temperature affects E+ and E- inflorescence count a bit differently for AGPE,POSY and POAU 

##PLOTTING PRECIPITATION TIME LAGS / Ws
#take a random subset of posterior draws for the w_prec
all_w_posti_ppt<-params_all_i_exp$w_prec[sample(dim(params_all_i_exp$w_prec)[1],size=1000),,]
dim(all_w_posti_ppt)

long_df_all_wi_ppt <- as.data.frame.table(all_w_posti_ppt,
                                          responseName = "estimate")
str(long_df_all_wi_ppt)

# Convert to long data frame
long_df_all_wi_ppt <- as.data.frame.table(all_w_posti_ppt,
                                          responseName = "weight") %>%
  rename(draw = iterations, species = Var2, threemonth = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_wi_ppt$spec <- case_when(long_df_all_wi_ppt$species == 8 ~ "AGPE",
                                     long_df_all_wi_ppt$species == 2 ~ "ELRI",
                                     long_df_all_wi_ppt$species == 3 ~ "ELVI",
                                     long_df_all_wi_ppt$species == 4 ~ "FESU",
                                     long_df_all_wi_ppt$species == 5 ~ "LOAR",
                                     long_df_all_wi_ppt$species == 6 ~ "POAL",
                                     long_df_all_wi_ppt$species == 7 ~ "POAU",
                                     long_df_all_wi_ppt$species == 1 ~ "POSY")

long_df_all_wi_ppt$monthsprior <- case_when(long_df_all_wi_ppt$threemonth == "A" ~ "1-3",
                                            long_df_all_wi_ppt$threemonth == "B" ~ "4-6",
                                            long_df_all_wi_ppt$threemonth == "C" ~ "7-9",
                                            long_df_all_wi_ppt$threemonth == "D" ~ "10-12",
                                            long_df_all_wi_ppt$threemonth == "E" ~ "13-15",
                                            long_df_all_wi_ppt$threemonth == "F" ~ "16-18",
                                            long_df_all_wi_ppt$threemonth == "G" ~ "19-21",
                                            long_df_all_wi_ppt$threemonth == "H" ~ "22-24")

long_df_all_wi_ppt$monthsprior <- factor(long_df_all_wi_ppt$monthsprior,
                                         levels=c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-24"))

summary_df_all_wi_ppt <- long_df_all_wi_ppt %>%
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wi_ppt)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

ggplot(data = summary_df_all_wi_ppt, aes(x = monthsprior, y = median_weight, color = spec, group=spec)) +
  geom_point() + geom_line() +
  scale_color_manual(
    values = c(
      "AGPE" = "olivedrab",
      "ELRI" = "goldenrod",
      "ELVI" = "darkorange",
      "FESU" = "tomato",
      "LOAR" = "deeppink",
      "POAL" = "purple",
      "POAU" = "slateblue",
      "POSY" = "cornflowerblue")) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Precipitation in Each Time Scale on Inflorescence Count")

#it appears for 5 species, further back time lags contribute most to the effects of precipitation
#for the other species precipitation effects were suffficiently accounted for by cumulative precipitation
#over a 2 year period

##PLOTTING TEMPERATURE TIME LAGS / Ws
#take a random subset of posterior draws for the w_temp
all_w_posti_temp<-params_all_i_exp$w_temp[sample(dim(params_all_i_exp$w_temp)[1],size=1000),,]
dim(all_w_posti_temp)

long_df_all_wi_temp <- as.data.frame.table(all_w_posti_temp,
                                           responseName = "estimate")
str(long_df_all_wi_temp)

# Convert to long data frame
long_df_all_wi_temp <- as.data.frame.table(all_w_posti_temp,
                                           responseName = "weight") %>%
  rename(draw = iterations, species = Var2, threemonth = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_wi_temp$spec <- case_when(long_df_all_wi_temp$species == 8 ~ "AGPE",
                                      long_df_all_wi_temp$species == 2 ~ "ELRI",
                                      long_df_all_wi_temp$species == 3 ~ "ELVI",
                                      long_df_all_wi_temp$species == 4 ~ "FESU",
                                      long_df_all_wi_temp$species == 5 ~ "LOAR",
                                      long_df_all_wi_temp$species == 6 ~ "POAL",
                                      long_df_all_wi_temp$species == 7 ~ "POAU",
                                      long_df_all_wi_temp$species == 1 ~ "POSY")

long_df_all_wi_temp$monthsprior <- case_when(long_df_all_wi_temp$threemonth == "A" ~ "1-3",
                                             long_df_all_wi_temp$threemonth == "B" ~ "4-6",
                                             long_df_all_wi_temp$threemonth == "C" ~ "7-9",
                                             long_df_all_wi_temp$threemonth == "D" ~ "10-12",
                                             long_df_all_wi_temp$threemonth == "E" ~ "13-15",
                                             long_df_all_wi_temp$threemonth == "F" ~ "16-18",
                                             long_df_all_wi_temp$threemonth == "G" ~ "19-21",
                                             long_df_all_wi_temp$threemonth == "H" ~ "22-24")

long_df_all_wi_temp$monthsprior <- factor(long_df_all_wi_temp$monthsprior,
                                          levels=c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-24"))

summary_df_all_wi_temp <- long_df_all_wi_temp %>%
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wi_temp)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

ggplot(data = summary_df_all_wi_temp, aes(x = monthsprior, y = median_weight, color = spec, group=spec)) +
  geom_point() + geom_line() +
  scale_color_manual(
    values = c(
      "AGPE" = "olivedrab",
      "ELRI" = "goldenrod",
      "ELVI" = "darkorange",
      "FESU" = "tomato",
      "LOAR" = "deeppink",
      "POAL" = "purple",
      "POAU" = "slateblue",
      "POSY" = "cornflowerblue")) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Temperature in Each Time Scale on Inflorescence Count")

#it appears for POSY, the most recent temperatures contributes most to temperature effects
#while for AGPE and POAL, further back time lags, past a year, contributed more
#for the other species temperature effects were sufficiently accounted for by cumulative temperature
#over a 2 year period

##`MODEL G`PT: GROWTH RATE AS RESPONSE TO PREICIPITATION and TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
pop_growth_df %>% 
  select(r,endo_01,spec,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
         fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
         plot,year_t) %>% 
  drop_na() -> all_grow_exp

# Safety conversion to continuous sequential factor integers 
all_grow_exp$plot    <- as.integer(as.factor(all_grow_exp$plot))
all_grow_exp$spec    <- as.integer(as.factor(all_grow_exp$spec))
all_grow_exp$year_t  <- as.integer(as.factor(all_grow_exp$year_t))

all_grow_dat_exp<-list(n_obs=nrow(all_grow_exp),
                       y=all_grow_exp$r,
                       n_yrs = length(unique(all_grow_exp$year_t)),
                       n_plots = length(unique(all_grow_exp$plot)),
                       n_endo = 2,
                       n_spp = max(all_grow_exp$spec),
                       endo_01= all_grow_exp$endo_01,
                       K = 8,
                       precip = as.matrix(scale(all_grow_exp[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                 "thirdthreeback_ppt","fourththreeback_ppt",
                                                                 "fifththreeback_ppt","sixththreeback_ppt",
                                                                 "sevenththreeback_ppt","eighththreeback_ppt")])),
                       temper = as.matrix(scale(all_grow_exp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                 "thirdthreeback_tmean","fourththreeback_tmean",
                                                                 "fifththreeback_tmean","sixththreeback_tmean",
                                                                 "sevenththreeback_tmean","eighththreeback_tmean")])),
                       year_index = as.integer(as.factor(all_grow_exp$year_t)),
                       plot=all_grow_exp$plot,
                       species=all_grow_exp$spec)

all_grow_model_exp = stan_model(file="code/explicit_studentT_SAM.stan")
all_grow_sampling_exp <- sampling(all_grow_model_exp,
                                  data = all_grow_dat_exp,
                                  chains = 3, 
                                  iter = 10000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_prec","beta_temp","sigma_year",
                                         "sigma_plot", "w_prec","w_temp","y_rep"),
                                  include = TRUE)

saveRDS(all_grow_sampling_exp,"all_grow_sampling_exp.rds")
all_grow_sampling_exp<-readRDS("all_grow_sampling_exp.rds")

##trace plots of beta clim and w
mcmc_trace(all_grow_sampling_exp,regex_pars = "beta_0")
mcmc_trace(all_grow_sampling_exp,regex_pars = "beta_prec")
mcmc_trace(all_grow_sampling_exp,regex_pars = "beta_temp")
mcmc_trace(all_grow_sampling_exp,regex_pars = "w_prec")
mcmc_trace(all_grow_sampling_exp,regex_pars = "w_temp")

mcmc_intervals(all_grow_sampling_exp,regex_pars = "beta_prec")
mcmc_intervals(all_grow_sampling_exp,regex_pars = "beta_temp")
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_grow_sampling_exp,pars="y_rep")
ppc_dens_overlay(all_grow_dat_exp$y,y_rep$y_rep[1:500,])

summary(all_grow_sampling_exp)

#extracting parameters
params_all_g_exp<-rstan::extract(all_grow_sampling_exp,pars=c('beta_0','beta_prec','beta_temp','w_prec','w_temp'))
dim(params_all_g_exp$beta_0)
dim(params_all_g_exp$beta_prec)
dim(params_all_g_exp$beta_temp)
dim(params_all_g_exp$w_prec)
dim(params_all_g_exp$w_temp)

##PLOTTING PRECIPITATION EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaprec_postg<-params_all_g_exp$beta_prec[sample(dim(params_all_g_exp$beta_prec)[1],size=1000),,]
dim(all_betaprec_postg)

long_df_all_betaprecg <- as.data.frame.table(all_betaprec_postg,
                                                 responseName = "estimate")
str(long_df_all_betaprecg)

# Convert to long data frame
long_df_all_betaprecg <- as.data.frame.table(all_betaprec_postg,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaprecg$spec <- case_when(long_df_all_betaprecg$species == 8 ~ "AGPE",
                                            long_df_all_betaprecg$species == 2 ~ "ELRI",
                                            long_df_all_betaprecg$species == 3 ~ "ELVI",
                                            long_df_all_betaprecg$species == 4 ~ "FESU",
                                            long_df_all_betaprecg$species == 5 ~ "LOAR",
                                            long_df_all_betaprecg$species == 6 ~ "POAL",
                                            long_df_all_betaprecg$species == 7 ~ "POAU",
                                            long_df_all_betaprecg$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_gp <- long_df_all_betaprecg %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data_gp, aes(x = slope_val, y = spec, color = endo_label)) +
  # Draw the 50% and 95% intervals + median dot (mimicking mcmc_intervals)
  stat_pointinterval(.width = c(0.5, 0.95), 
                     position = position_dodge(width = 0.4)) +
  
  # Add a vertical reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Set the specific colors you requested
  scale_color_manual(values = c("E-" = "deeppink", 
                                "E+" = "cornflowerblue")) +
  
  # Clean up formatting
  labs(
    x = "Posterior Climate Coefficient (precipitation)",
    y = "Species",
    color = "Endophyte Status",
    title = "Precipitation Effects on Growth by Species and Endophyte Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

##PLOTTING TEMPERATURE EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betatemp_postg<-params_all_g_exp$beta_temp[sample(dim(params_all_g_exp$beta_temp)[1],size=1000),,]
dim(all_betatemp_postg)

long_df_all_betatempg <- as.data.frame.table(all_betatemp_postg,
                                                 responseName = "estimate")
str(long_df_all_betatempg)

# Convert to long data frame
long_df_all_betatempg <- as.data.frame.table(all_betatemp_postg,
                                             responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betatempg$spec <- case_when(long_df_all_betatempg$species == 8 ~ "AGPE",
                                       long_df_all_betatempg$species == 2 ~ "ELRI",
                                       long_df_all_betatempg$species == 3 ~ "ELVI",
                                       long_df_all_betatempg$species == 4 ~ "FESU",
                                       long_df_all_betatempg$species == 5 ~ "LOAR",
                                       long_df_all_betatempg$species == 6 ~ "POAL",
                                       long_df_all_betatempg$species == 7 ~ "POAU",
                                       long_df_all_betatempg$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_gt <- long_df_all_betatempg %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data_gt, aes(x = slope_val, y = spec, color = endo_label)) +
  # Draw the 50% and 95% intervals + median dot (mimicking mcmc_intervals)
  stat_pointinterval(.width = c(0.5, 0.95), 
                     position = position_dodge(width = 0.4)) +
  
  # Add a vertical reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Set the specific colors you requested
  scale_color_manual(values = c("E-" = "deeppink", 
                                "E+" = "cornflowerblue")) +
  
  # Clean up formatting
  labs(
    x = "Posterior Climate Coefficient (temperature)",
    y = "Species",
    color = "Endophyte Status",
    title = "Temperature Effects on Growth by Species and Endophyte Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

##PLOTTING PRECIPITATION TIME LAGS / Ws
#take a random subset of posterior draws for the w 
all_w_postg_ppt<-params_all_g_exp$w_prec[sample(dim(params_all_g_exp$w_prec)[1],size=1000),,]
dim(all_w_postg_ppt)

long_df_all_wg_ppt <- as.data.frame.table(all_w_postg_ppt,
                                                 responseName = "estimate")
str(long_df_all_wg_ppt)

# Convert to long data frame
long_df_all_wg_ppt <- as.data.frame.table(all_w_postg_ppt,
                                                 responseName = "weight") %>%
  rename(draw = iterations, species = Var2, threemonth = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_wg_ppt$spec <- case_when(long_df_all_wg_ppt$species == 8 ~ "AGPE",
                                            long_df_all_wg_ppt$species == 2 ~ "ELRI",
                                            long_df_all_wg_ppt$species == 3 ~ "ELVI",
                                            long_df_all_wg_ppt$species == 4 ~ "FESU",
                                            long_df_all_wg_ppt$species == 5 ~ "LOAR",
                                            long_df_all_wg_ppt$species == 6 ~ "POAL",
                                            long_df_all_wg_ppt$species == 7 ~ "POAU",
                                            long_df_all_wg_ppt$species == 1 ~ "POSY")

long_df_all_wg_ppt$monthsprior <- case_when(long_df_all_wg_ppt$threemonth == "A" ~ "1-3",
                                            long_df_all_wg_ppt$threemonth == "B" ~ "4-6",
                                            long_df_all_wg_ppt$threemonth == "C" ~ "7-9",
                                            long_df_all_wg_ppt$threemonth == "D" ~ "10-12",
                                            long_df_all_wg_ppt$threemonth == "E" ~ "13-15",
                                            long_df_all_wg_ppt$threemonth == "F" ~ "16-18",
                                            long_df_all_wg_ppt$threemonth == "G" ~ "19-21",
                                            long_df_all_wg_ppt$threemonth == "H" ~ "22-24")

long_df_all_wg_ppt$monthsprior <- factor(long_df_all_wg_ppt$monthsprior,
                                         levels=c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-24"))

summary_df_all_wg_ppt <- long_df_all_wg_ppt %>%
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wg_ppt)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

ggplot(data = summary_df_all_wg_ppt, aes(x = monthsprior, y = median_weight, color = spec, group=spec)) +
  geom_point() + geom_line() +
  scale_color_manual(
    values = c(
      "AGPE" = "olivedrab",
      "ELRI" = "goldenrod",
      "ELVI" = "darkorange",
      "FESU" = "tomato",
      "LOAR" = "deeppink",
      "POAL" = "purple",
      "POAU" = "slateblue",
      "POSY" = "cornflowerblue")) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Temperature in Each Time Scale on Growth Rate")

##PLOTTING TEMPERATURE TIME LAGS / Ws
#take a random subset of posterior draws for the w 
all_w_postg_temp<-params_all_g_exp$w_temp[sample(dim(params_all_g_exp$w_temp)[1],size=1000),,]
dim(all_w_postg_temp)

long_df_all_wg_temp <- as.data.frame.table(all_w_postg_temp,
                                           responseName = "estimate")
str(long_df_all_wg_temp)

# Convert to long data frame
long_df_all_wg_temp <- as.data.frame.table(all_w_postg_temp,
                                           responseName = "weight") %>%
  rename(draw = iterations, species = Var2, threemonth = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_wg_temp$spec <- case_when(long_df_all_wg_temp$species == 8 ~ "AGPE",
                                      long_df_all_wg_temp$species == 2 ~ "ELRI",
                                      long_df_all_wg_temp$species == 3 ~ "ELVI",
                                      long_df_all_wg_temp$species == 4 ~ "FESU",
                                      long_df_all_wg_temp$species == 5 ~ "LOAR",
                                      long_df_all_wg_temp$species == 6 ~ "POAL",
                                      long_df_all_wg_temp$species == 7 ~ "POAU",
                                      long_df_all_wg_temp$species == 1 ~ "POSY")

long_df_all_wg_temp$monthsprior <- case_when(long_df_all_wg_temp$threemonth == "A" ~ "1-3",
                                             long_df_all_wg_temp$threemonth == "B" ~ "4-6",
                                             long_df_all_wg_temp$threemonth == "C" ~ "7-9",
                                             long_df_all_wg_temp$threemonth == "D" ~ "10-12",
                                             long_df_all_wg_temp$threemonth == "E" ~ "13-15",
                                             long_df_all_wg_temp$threemonth == "F" ~ "16-18",
                                             long_df_all_wg_temp$threemonth == "G" ~ "19-21",
                                             long_df_all_wg_temp$threemonth == "H" ~ "22-24")

long_df_all_wg_temp$monthsprior <- factor(long_df_all_wg_temp$monthsprior,
                                          levels=c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-24"))

summary_df_all_wg_temp <- long_df_all_wg_temp %>%
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wg_temp)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

ggplot(data = summary_df_all_wg_temp, aes(x = monthsprior, y = median_weight, color = spec, group=spec)) +
  geom_point() + geom_line() +
  scale_color_manual(
    values = c(
      "AGPE" = "olivedrab",
      "ELRI" = "goldenrod",
      "ELVI" = "darkorange",
      "FESU" = "tomato",
      "LOAR" = "deeppink",
      "POAL" = "purple",
      "POAU" = "slateblue",
      "POSY" = "cornflowerblue")) +
  labs(x = "months prior", 
       y = "weight",
       title = "Comparison of Weights of Temperature in Each Time Scale")


