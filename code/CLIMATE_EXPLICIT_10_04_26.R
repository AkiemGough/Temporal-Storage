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
library(rstanarm)
library(loo)

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


#MODEL SELECTION PROCESS FOR SURVIVAL (PRECIPITATION VS TEMPERATURE)

##prep data, dropping NAs
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

#precipiation modeling
all_surv_dat_prec<-list(n_obs=nrow(all_surv_exp),
                        y=as.integer(all_surv_exp$surv_t1),
                        n_yrs = length(unique(all_surv_exp$year_t)),
                        n_plots = length(unique(all_surv_exp$plot)),
                        n_endo = 2,
                        n_spp = length(unique(all_surv_exp$spec)),
                        endo_01= as.integer(all_surv_exp$endo_01),
                        size=as.numeric(all_surv_exp$log_tillers_centered),
                        K = 8,
                        climate = as.matrix(scale(all_surv_exp[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                   "thirdthreeback_ppt","fourththreeback_ppt",
                                                                   "fifththreeback_ppt","sixththreeback_ppt",
                                                                   "sevenththreeback_ppt","eighththreeback_ppt")])),
                        year_index = all_surv_exp$year_t,
                        plot=all_surv_exp$plot,
                        species=all_surv_exp$spec,
                        original= as.integer(all_surv_exp$original))

all_surv_model_prec = stan_model(file="code/climate_binomial.stan")
all_surv_sampling_prec <- sampling(all_surv_model_prec,
                                   data = all_surv_dat_prec,
                                   chains = 3, 
                                   iter = 5000, #reduced interations to make the sampling lighter
                                   warmup  = 1000,
                                   pars=c("beta_0","beta_clim","w_clim","log_lik"), #reduced amount of selected parameters to make the sampling lighter
                                   include = TRUE)

#computing PSIS-LOO for precipitation model
looprec <- loo(all_surv_sampling_prec, save_psis = TRUE)

#temperature modeling
all_surv_dat_temp<-list(n_obs=nrow(all_surv_exp),
                        y=as.integer(all_surv_exp$surv_t1),
                        n_yrs = length(unique(all_surv_exp$year_t)),
                        n_plots = length(unique(all_surv_exp$plot)),
                        n_endo = 2,
                        n_spp = length(unique(all_surv_exp$spec)),
                        endo_01= as.integer(all_surv_exp$endo_01),
                        size=as.numeric(all_surv_exp$log_tillers_centered),
                        K = 8,
                        climate = as.matrix(scale(all_surv_exp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                   "thirdthreeback_tmean","fourththreeback_tmean",
                                                                   "fifththreeback_tmean","sixththreeback_tmean",
                                                                   "sevenththreeback_tmean","eighththreeback_tmean")])),
                        year_index = all_surv_exp$year_t,
                        plot=all_surv_exp$plot,
                        species=all_surv_exp$spec,
                        original= as.integer(all_surv_exp$original))

all_surv_model_temp = stan_model(file="code/climate_binomial.stan")
all_surv_sampling_temp <- sampling(all_surv_model_temp,
                                   data = all_surv_dat_temp,
                                   chains = 3, 
                                   iter = 5000,#reduced iterations to make the sampling lighter
                                   warmup  = 1000,
                                   pars=c("beta_0","beta_clim","w_clim","log_lik"),#reduced amount of selected parameters to make the sampling lighter
                                   include = TRUE)

#computing PSIS-LOO for temperature model
lootemp <- loo(all_surv_sampling_temp, save_psis = TRUE)

#comparing models
loo_compare(looprec, lootemp)
#this model comparison determined that the precipitation model
#has better predictive performance than than the temperature model
#The difference in ELPD is >3 times but <4 times larger 
#than the estimated standard error of the difference

#so now use the precipitation model


##MODEL SPT: PROBABILITY OF SURVIVAL AS RESPONSE TO PREICIPITATION

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(surv_t1,endo_01,spec,log_tillers_centered,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         plot,year_t,original) %>% 
  drop_na() -> all_surv_prec

# Safety conversion to continuous sequential factor integers 
all_surv_prec$plot    <- as.integer(as.factor(all_surv_prec$plot))
all_surv_prec$spec    <- as.integer(as.factor(all_surv_prec$spec))
all_surv_prec$year_t  <- as.integer(as.factor(all_surv_prec$year_t))

all_surv_dat_prec<-list(n_obs=nrow(all_surv_prec),
                       y=as.integer(all_surv_prec$surv_t1),
                       n_yrs = length(unique(all_surv_prec$year_t)),
                       n_plots = length(unique(all_surv_prec$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_surv_prec$spec)),
                       endo_01= as.integer(all_surv_prec$endo_01),
                       size=as.numeric(all_surv_prec$log_tillers_centered),
                       K = 8,
                       climate = as.matrix(scale(all_surv_prec[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                 "thirdthreeback_ppt","fourththreeback_ppt",
                                                                 "fifththreeback_ppt","sixththreeback_ppt",
                                                                 "sevenththreeback_ppt","eighththreeback_ppt")])),
                       year_index = all_surv_prec$year_t,
                       plot=all_surv_prec$plot,
                       species=all_surv_prec$spec,
                       original= as.integer(all_surv_prec$original))

all_surv_model_prec = stan_model(file="code/climate_binomial.stan")
all_surv_sampling_prec <- sampling(all_surv_model_prec,
                                  data = all_surv_dat_prec,
                                  chains = 3, 
                                  iter = 10000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_clim","sigma_year",
                                         "sigma_plot", "w_clim","y_rep"),
                                  include = TRUE)

saveRDS(all_surv_sampling_prec,"all_surv_sampling_prec.rds")
all_surv_sampling_prec<-readRDS("all_surv_sampling_prec.rds")


mcmc_intervals(all_surv_sampling_prec,regex_pars = "beta_clim")
#note to self - make the E+ E- pairs close to each other and distinguished by color

##trace plots of beta clim and w
mcmc_trace(all_surv_sampling_prec,regex_pars = "beta_0") #looks mostly good
mcmc_trace(all_surv_sampling_prec,regex_pars = "beta_clim") #looks mostly good, 6,1 & 6,2 look a little weird
mcmc_trace(all_surv_sampling_prec,regex_pars = "w_clim")

##posterior predictive check
y_rep<-extract(all_surv_sampling_prec,pars="y_rep")
ppc_dens_overlay(all_surv_dat_prec$y,y_rep$y_rep[1:500,]) #looks good

#summary(all_surv_sampling_prec)

#extracting parameters
params_all_s_prec<-rstan::extract(all_surv_sampling_prec,pars=c('beta_0','beta_clim','w_clim'))
dim(params_all_s_prec$beta_0)
dim(params_all_s_prec$beta_clim)
dim(params_all_s_prec$w_clim)



##PLOTTING PREC EFFECTS

#take a random subset of posterior draws for the slopes / beta_clim / precipitation effects
all_betaclim_posts<-params_all_s_prec$beta_clim[sample(dim(params_all_s_prec$beta_clim)[1],size=1000),,]
dim(all_betaclim_posts)

long_df_all_betaclims <- as.data.frame.table(all_betaclim_posts,
                                             responseName = "estimate")
str(long_df_all_betaclims)

# Convert to long data frame
long_df_all_betaclims <- as.data.frame.table(all_betaclim_posts,
                                             responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclims$spec <- case_when(long_df_all_betaclims$species == 8 ~ "AGPE",
                                        long_df_all_betaclims$species == 2 ~ "ELRI",
                                        long_df_all_betaclims$species == 3 ~ "ELVI",
                                        long_df_all_betaclims$species == 4 ~ "FESU",
                                        long_df_all_betaclims$species == 5 ~ "LOAR",
                                        long_df_all_betaclims$species == 6 ~ "POAL",
                                        long_df_all_betaclims$species == 7 ~ "POAU",
                                        long_df_all_betaclims$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_sp <- long_df_all_betaclims %>%
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
    x = "Effect of precipitation on survival",
    y = "Species",
    color = "Endophyte Status",
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#it aappears precipitation only has a bit of effect on POSY, LOAR, ELVI and AGPE survival
#only AGPE has strong evidence and none of the species show an interaction between endo and prec effects



##PLOTTING PRECIPITATION TIME LAGS / Ws
#take a random subset of posterior draws for the w_clim
all_w_posts_ppt<-params_all_s_prec$w_clim[sample(dim(params_all_s_prec$w_clim)[1],size=1000),,]
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

summary_df_all_ws_ppt %>% arrange(draw,spec,monthsprior)

summary_df_all_ws_ppt <- long_df_all_ws_ppt %>%
  group_by(draw,spec) %>% 
  mutate(
    cum_weight = cumsum(weight),
    .groups = "drop") %>% 
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    cum_median_weight = median(cum_weight),
    cum_lower_weight = quantile(cum_weight, 0.05),
    cum_upper_weight = quantile(cum_weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wf_ppt)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")
ggplot(data = summary_df_all_ws_ppt, aes(x = monthsprior, y = cum_median_weight, color = spec, fill=spec, group=spec)) +
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
  scale_fill_manual(
    values = c("AGPE" = "olivedrab",
               "ELRI" = "goldenrod",
               "ELVI" = "darkorange",
               "FESU" = "tomato",
               "LOAR" = "deeppink",
               "POAL" = "purple",
               "POAU" = "slateblue",
               "POSY" = "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = cum_lower_weight, ymax = cum_upper_weight), alpha = 0.125, color = NA) +
  geom_hline(yintercept = 0.8, linetype = 2, color = "azure4") +
  labs(x = "months prior", 
       y = "weight",
       title = "Cumulative Weights of Precipitation in Each Time Scale on Probability of Survival")


ggplot(data = summary_df_all_ws_ppt, aes(x = monthsprior, y = median_weight, color = spec, fill=spec, group=spec)) +
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
  scale_fill_manual(
    values = c("AGPE" = "olivedrab",
               "ELRI" = "goldenrod",
               "ELVI" = "darkorange",
               "FESU" = "tomato",
               "LOAR" = "deeppink",
               "POAL" = "purple",
               "POAU" = "slateblue",
               "POSY" = "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower_weight, ymax = upper_weight), alpha = 0.125, color = NA) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Precipitation in Each Time Scale on Probability of Survival")

#most of the species are most impacted by either recent or very far back precipitation or both






#MODEL SELECETION PROCESS FOR INFLORESENCE COUNT MODELS (PRECIPITATION VS TEMPERATURE)

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

all_infl_dat_prec<-list(n_obs=nrow(all_infl_exp),
                        y=as.integer(all_infl_exp$flw_count_t1),
                        n_yrs = length(unique(all_infl_exp$year_t)),
                        n_plots = length(unique(all_infl_exp$plot)),
                        n_endo = 2,
                        n_spp = length(unique(all_infl_exp$spec)),
                        endo_01= as.integer(all_infl_exp$endo_01),
                        size=as.numeric(all_infl_exp$log_tillers_centered),
                        K = 8,
                        climate = as.matrix(scale(all_infl_exp[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                   "thirdthreeback_ppt","fourththreeback_ppt",
                                                                   "fifththreeback_ppt","sixththreeback_ppt",
                                                                   "sevenththreeback_ppt","eighththreeback_ppt")])),
                        year_index = all_infl_exp$year_t,
                        plot=all_infl_exp$plot,
                        species=all_infl_exp$spec,
                        original= as.integer(all_infl_exp$original))

all_infl_model_prec = stan_model(file="code/climate_negbinom.stan")
all_infl_sampling_prec <- sampling(all_infl_model_prec,
                                   data = all_infl_dat_prec,
                                   chains = 3, 
                                   iter = 5000,#reduced iterations to make the sampling lighter
                                   warmup  = 1000,
                                   pars=c("beta_0","beta_clim","w_clim","log_lik"),#reduced amount of selected parameters to make the sampling lighter
                                   include = TRUE)

#computing PSIS-LOO for precipitation model
looprecinfl <- loo(all_infl_sampling_prec, save_psis = TRUE)

#temperature modeling
all_infl_dat_temp<-list(n_obs=nrow(all_infl_exp),
                        y=as.integer(all_infl_exp$flw_count_t1),
                        n_yrs = length(unique(all_infl_exp$year_t)),
                        n_plots = length(unique(all_infl_exp$plot)),
                        n_endo = 2,
                        n_spp = length(unique(all_infl_exp$spec)),
                        endo_01= as.integer(all_infl_exp$endo_01),
                        size=as.numeric(all_infl_exp$log_tillers_centered),
                        K = 8,
                        climate = as.matrix(scale(all_infl_exp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                   "thirdthreeback_tmean","fourththreeback_tmean",
                                                                   "fifththreeback_tmean","sixththreeback_tmean",
                                                                   "sevenththreeback_tmean","eighththreeback_tmean")])),
                        year_index = all_infl_exp$year_t,
                        plot=all_infl_exp$plot,
                        species=all_infl_exp$spec,
                        original= as.integer(all_infl_exp$original))

all_infl_model_temp = stan_model(file="code/climate_negbinom.stan")
all_infl_sampling_temp <- sampling(all_infl_model_temp,
                                   data = all_infl_dat_temp,
                                   chains = 3, 
                                   iter = 5000,#reduced iterations to make the sampling lighter
                                   warmup  = 1000,
                                   pars=c("beta_0","beta_clim","w_clim","log_lik"),#reduced amount of selected parameters to make the sampling lighter
                                   include = TRUE)

#computing PSIS-LOO for precipitation model
lootempinfl <- loo(all_infl_sampling_temp, save_psis = TRUE)

#comparing models
loo_compare(looprecinfl, lootempinfl)
#this model comparison determined that the precipitation model
#has better predictive performance than than the temperature model
#The difference in ELPD is >4 times but <5 times larger 
#than the estimated standard error of the difference

#so now use precipitation model



##MODEL IPT: INFLORESCENE COUNT AS RESPONSE TO PREICIPITATION AND TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t1,endo_01,spec,log_tillers_centered,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         plot,year_t,original) %>% 
  drop_na() -> all_infl_prec

# Safety conversion to continuous sequential factor integers 
all_infl_prec$plot    <- as.integer(as.factor(all_infl_prec$plot))
all_infl_prec$spec    <- as.integer(as.factor(all_infl_prec$spec))
all_infl_prec$year_t  <- as.integer(as.factor(all_infl_prec$year_t))

all_infl_dat_prec<-list(n_obs=nrow(all_infl_prec),
                       y=as.integer(all_infl_prec$flw_count_t1),
                       n_yrs = length(unique(all_infl_prec$year_t)),
                       n_plots = length(unique(all_infl_prec$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_infl_prec$spec)),
                       endo_01= as.integer(all_infl_prec$endo_01),
                       size=as.numeric(all_infl_prec$log_tillers_centered),
                       K = 8,
                       climate = as.matrix(scale(all_infl_prec[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                 "thirdthreeback_ppt","fourththreeback_ppt",
                                                                 "fifththreeback_ppt","sixththreeback_ppt",
                                                                 "sevenththreeback_ppt","eighththreeback_ppt")])),
                       year_index = all_infl_prec$year_t,
                       plot=all_infl_prec$plot,
                       species=all_infl_prec$spec,
                       original= as.integer(all_infl_prec$original))

all_infl_model_prec = stan_model(file="code/climate_negbinom.stan")
all_infl_sampling_prec <- sampling(all_infl_model_prec,
                                  data = all_infl_dat_prec,
                                  chains = 3, 
                                  iter = 10000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_clim","sigma_year",
                                         "sigma_plot", "w_clim","y_rep"),
                                  include = TRUE)

saveRDS(all_infl_sampling_prec,"all_infl_sampling_prec.rds")
all_infl_sampling_prec<-readRDS("all_infl_sampling_prec.rds")

##trace plots of beta clim and w (priors 0,1 for beta0 & betaclim)
mcmc_trace(all_infl_sampling_prec,regex_pars = "beta_0") #looks good
mcmc_trace(all_infl_sampling_prec,regex_pars = "beta_clim") #looks good, 4,2 looks a little weird
mcmc_trace(all_infl_sampling_prec,regex_pars = "w_clim")

mcmc_trace(all_infl_sampling_prec,pars = "w_clim[5,2]")

mcmc_intervals(all_infl_sampling_prec,regex_pars = "beta_clim")

##posterior predictive check
y_rep<-extract(all_infl_sampling_prec,pars="y_rep") #looks good
ppc_dens_overlay(all_infl_dat_prec$y,y_rep$y_rep[1:500,])+xlim(0,5)

summary(all_infl_sampling_prec)

#extracting parameters
params_all_i_prec<-rstan::extract(all_infl_sampling_prec,pars=c('beta_0','beta_clim','w_clim'))
dim(params_all_i_prec$beta_0)
dim(params_all_i_prec$beta_clim)
dim(params_all_i_prec$w_clim)


##PLOTTING PREC EFFECTS

#take a random subset of posterior draws for the slopes / beta_clim / precipitation effects
all_betaclim_posti<-params_all_i_prec$beta_clim[sample(dim(params_all_i_prec$beta_clim)[1],size=1000),,]
dim(all_betaclim_posti)

long_df_all_betaclimi <- as.data.frame.table(all_betaclim_posti,
                                             responseName = "estimate")
str(long_df_all_betaclimi)

# Convert to long data frame
long_df_all_betaclimi <- as.data.frame.table(all_betaclim_posti,
                                             responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimi$spec <- case_when(long_df_all_betaclimi$species == 8 ~ "AGPE",
                                        long_df_all_betaclimi$species == 2 ~ "ELRI",
                                        long_df_all_betaclimi$species == 3 ~ "ELVI",
                                        long_df_all_betaclimi$species == 4 ~ "FESU",
                                        long_df_all_betaclimi$species == 5 ~ "LOAR",
                                        long_df_all_betaclimi$species == 6 ~ "POAL",
                                        long_df_all_betaclimi$species == 7 ~ "POAU",
                                        long_df_all_betaclimi$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data_ip <- long_df_all_betaclimi %>%
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
    x = "Efffect of precipitation on inflorescence count",
    y = "Species",
    color = "Endophyte Status",
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

##PLOTTING PRECIPITATION TIME LAGS / Ws
#take a random subset of posterior draws for the w_clim
all_w_posti_ppt<-params_all_i_prec$w_clim[sample(dim(params_all_i_prec$w_clim)[1],size=1000),,]
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


summary_df_all_wi_ppt %>% arrange(draw,spec,monthsprior)

summary_df_all_wi_ppt <- long_df_all_wi_ppt %>%
  group_by(draw,spec) %>% 
  mutate(
    cum_weight = cumsum(weight),
    .groups = "drop") %>% 
  group_by(spec,monthsprior) %>% 
  summarize(
    median_weight = median(weight),
    lower_weight = quantile(weight, 0.05),
    upper_weight = quantile(weight, 0.95),
    cum_median_weight = median(cum_weight),
    cum_lower_weight = quantile(cum_weight, 0.05),
    cum_upper_weight = quantile(cum_weight, 0.95),
    .groups = "drop")

#ggplot(summary_df_all_wi_ppt)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")
ggplot(data = summary_df_all_wi_ppt, aes(x = monthsprior, y = cum_median_weight, color = spec, fill=spec, group=spec)) +
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
  scale_fill_manual(
    values = c("AGPE" = "olivedrab",
               "ELRI" = "goldenrod",
               "ELVI" = "darkorange",
               "FESU" = "tomato",
               "LOAR" = "deeppink",
               "POAL" = "purple",
               "POAU" = "slateblue",
               "POSY" = "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = cum_lower_weight, ymax = cum_upper_weight), alpha = 0.15, color = NA) +
  geom_hline(yintercept = 0.8, linetype = 2, color = "azure4") +
  labs(x = "months prior", 
       y = "weight",
       title = "Cumulative Weights of Precipitation in Each Time Scale on Inflorescence Count")

ggplot(data = summary_df_all_wi_ppt, aes(x = monthsprior, y = median_weight, color = spec, fill=spec, group=spec)) +
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
  scale_fill_manual(
    values = c("AGPE" = "olivedrab",
               "ELRI" = "goldenrod",
               "ELVI" = "darkorange",
               "FESU" = "tomato",
               "LOAR" = "deeppink",
               "POAL" = "purple",
               "POAU" = "slateblue",
               "POSY" = "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower_weight, ymax = upper_weight), alpha = 0.125, color = NA) +
  labs(x = "months prior", 
       y = "weight",
       title = "Weights of Precipitation in Each Time Scale on Inflorescence Count")




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

