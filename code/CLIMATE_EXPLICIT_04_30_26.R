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




#####PRECIPITATION MODELS___________________________________________________________



##MODEL FP: PROBAILITY OF FLOWERING AS RESPONSE TO PREICIPITATION___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t,endo_01,spec,log_tillers_centered,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         plot,year_t,original) %>% 
  drop_na() -> all_flow_ppt

# Safety conversion to continuous sequential factor integers 
all_flow_ppt$plot    <- as.integer(as.factor(all_flow_ppt$plot))
all_flow_ppt$spec    <- as.integer(as.factor(all_flow_ppt$spec))
all_flow_ppt$year_t  <- as.integer(as.factor(all_flow_ppt$year_t))

all_flow_dat_ppt<-list(n_obs=nrow(all_flow_ppt),
                       y=as.integer(all_flow_ppt$flw_count_t > 0),
                       n_yrs = length(unique(all_flow_ppt$year_t)),
                       n_plots = length(unique(all_flow_ppt$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_flow_ppt$spec)),
                       endo_01= as.integer(all_flow_ppt$endo_01),
                       size=as.numeric(all_flow_ppt$log_tillers_centered),
                       K = 8,
                       climate = as.matrix(scale(all_flow_ppt[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                  "thirdthreeback_ppt","fourththreeback_ppt",
                                                                  "fifththreeback_ppt","sixththreeback_ppt",
                                                                  "sevenththreeback_ppt","eighththreeback_ppt")])),
                       year_index = all_flow_ppt$year_t,
                       plot=all_flow_ppt$plot,
                       species=all_flow_ppt$spec,
                       original= as.integer(all_flow_ppt$original))

all_flow_model_ppt = stan_model(file="code/climatedemoSAMnoncen.stan")
all_flow_sampling_ppt <- sampling(all_flow_model_ppt,
                                  data = all_flow_dat_ppt,
                                  chains = 3, 
                                  iter = 8000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_clim","sigma_year",
                                         "sigma_plot", "w", "y_rep"),
                                  include = TRUE)


saveRDS(all_flow_sampling_ppt,"all_flow_sampling_ppt.rds")
all_flow_sampling_ppt<-readRDS("all_flow_sampling_ppt.rds")

saveRDS(all_flow_sampling_ppt,"all_flow_sampling_ppt2.rds")
all_flow_sampling_ppt2<-readRDS("all_flow_sampling_ppt2.rds")

mcmc_intervals(all_flow_sampling_ppt,regex_pars = "beta_clim",cols=c("red","blue"))
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_flow_sampling_ppt,pars="y_rep")
ppc_dens_overlay(all_flow_dat_ppt$y,y_rep$y_rep[1:500,])

summary(all_flow_sampling_ppt)

#extracting parameters
params_all_f_ppt<-rstan::extract(all_flow_sampling_ppt,pars=c('beta_0','beta_clim','w'))
dim(params_all_f_ppt$beta_0)
dim(params_all_f_ppt$beta_clim)
dim(params_all_f_ppt$w)

##trace plots of beta clim and w
mcmc_trace(all_flow_sampling_ppt,regex_pars = "beta_0")
mcmc_trace(all_flow_sampling_ppt,regex_pars = "beta_clim")
mcmc_trace(all_flow_sampling_ppt,regex_pars = "w")

##PLOTTING EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postf_ppt<-params_all_f_ppt$beta_clim[sample(dim(params_all_f_ppt$beta_clim)[1],size=1000),,]
dim(all_betaclim_postf_ppt)

long_df_all_betaclimf_ppt <- as.data.frame.table(all_betaclim_postf_ppt,
                                                 responseName = "estimate")
str(long_df_all_betaclimf_ppt)

# Convert to long data frame
long_df_all_betaclimf_ppt <- as.data.frame.table(all_betaclim_postf_ppt,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimf_ppt$spec <- case_when(long_df_all_betaclimf_ppt$species == 8 ~ "AGPE",
                                            long_df_all_betaclimf_ppt$species == 2 ~ "ELRI",
                                            long_df_all_betaclimf_ppt$species == 3 ~ "ELVI",
                                            long_df_all_betaclimf_ppt$species == 4 ~ "FESU",
                                            long_df_all_betaclimf_ppt$species == 5 ~ "LOAR",
                                            long_df_all_betaclimf_ppt$species == 6 ~ "POAL",
                                            long_df_all_betaclimf_ppt$species == 7 ~ "POAU",
                                            long_df_all_betaclimf_ppt$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data <- long_df_all_betaclimf_ppt %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data, aes(x = slope_val, y = spec, color = endo_label)) +
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

##PLOTTING Ws
#take a random subset of posterior draws for the w 
all_w_postf_ppt<-params_all_f_ppt$w[sample(dim(params_all_f_ppt$w)[1],size=1000),,]
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



##MODEL SP: SURVIVAL AS RESPONSE TO PREICIPITATION___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(surv_t1,endo_01,spec,log_tillers_centered,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         plot,year_t,original) %>% 
  drop_na() -> all_surv_ppt

# Safety conversion to continuous sequential factor integers 
all_surv_ppt$plot    <- as.integer(as.factor(all_surv_ppt$plot))
all_surv_ppt$spec    <- as.integer(as.factor(all_surv_ppt$spec))
all_surv_ppt$year_t  <- as.integer(as.factor(all_surv_ppt$year_t))

all_surv_dat_ppt<-list(n_obs=nrow(all_surv_ppt),
                       y=as.integer(all_surv_ppt$surv_t1),
                       n_yrs = length(unique(all_surv_ppt$year_t)),
                       n_plots = length(unique(all_surv_ppt$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_surv_ppt$spec)),
                       endo_01= as.integer(all_surv_ppt$endo_01),
                       size=as.numeric(all_surv_ppt$log_tillers_centered),
                       K = 8,
                       climate = as.matrix(scale(all_surv_ppt[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                  "thirdthreeback_ppt","fourththreeback_ppt",
                                                                  "fifththreeback_ppt","sixththreeback_ppt",
                                                                  "sevenththreeback_ppt","eighththreeback_ppt")])),
                       year_index = all_surv_ppt$year_t,
                       plot=all_surv_ppt$plot,
                       species=all_surv_ppt$spec,
                       original= as.integer(all_surv_ppt$original))

all_surv_model_ppt = stan_model(file="code/climatedemoSAM.stan")
all_surv_sampling_ppt <- sampling(all_surv_model_ppt,
                                  data = all_surv_dat_ppt,
                                  chains = 3, 
                                  iter = 8000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_clim","sigma_year",
                                         "sigma_plot", "w", "y_rep"),
                                  include = TRUE)


saveRDS(all_surv_sampling_ppt,"all_surv_sampling_ppt.rds")
all_surv_sampling_ppt<-readRDS("all_surv_sampling_ppt.rds")

mcmc_intervals(all_surv_sampling_ppt,regex_pars = "beta_clim",cols=c("red","blue"))
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_surv_sampling_ppt,pars="y_rep")
ppc_dens_overlay(all_surv_dat_ppt$y,y_rep$y_rep[1:500,])

summary(all_surv_sampling_ppt)

#extracting parameters
params_all_s_ppt<-rstan::extract(all_surv_sampling_ppt,pars=c('beta_0','beta_clim','w'))
dim(params_all_s_ppt$beta_0)
dim(params_all_s_ppt$beta_clim)
dim(params_all_s_ppt$w)

##trace plots of beta clim and w
mcmc_trace(all_surv_sampling_ppt,regex_pars = "beta_0")
mcmc_trace(all_surv_sampling_ppt,regex_pars = "beta_clim")
mcmc_trace(all_surv_sampling_ppt,regex_pars = "w")

##PLOTTING EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_posts_ppt<-params_all_s_ppt$beta_clim[sample(dim(params_all_s_ppt$beta_clim)[1],size=1000),,]
dim(all_betaclim_posts_ppt)

long_df_all_betaclims_ppt <- as.data.frame.table(all_betaclim_posts_ppt,
                                                 responseName = "estimate")
str(long_df_all_betaclims_ppt)

# Convert to long data frame
long_df_all_betaclims_ppt <- as.data.frame.table(all_betaclim_posts_ppt,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclims_ppt$spec <- case_when(long_df_all_betaclims_ppt$species == 8 ~ "AGPE",
                                            long_df_all_betaclims_ppt$species == 2 ~ "ELRI",
                                            long_df_all_betaclims_ppt$species == 3 ~ "ELVI",
                                            long_df_all_betaclims_ppt$species == 4 ~ "FESU",
                                            long_df_all_betaclims_ppt$species == 5 ~ "LOAR",
                                            long_df_all_betaclims_ppt$species == 6 ~ "POAL",
                                            long_df_all_betaclims_ppt$species == 7 ~ "POAU",
                                            long_df_all_betaclims_ppt$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data <- long_df_all_betaclims_ppt %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data, aes(x = slope_val, y = spec, color = endo_label)) +
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
    title = "Precipitation Effects on Survival"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

##PLOTTING Ws
#take a random subset of posterior draws for the w 
all_w_posts_ppt<-params_all_s_ppt$w[sample(dim(params_all_s_ppt$w)[1],size=1000),,]
dim(all_w_posts_ppt)

long_df_all_ws_ppt <- as.data.frame.table(all_w_posts_ppt,
                                          responseName = "estimate")
str(long_df_all_wf_ppt)

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
       title = "Weights of Precipitation in Each Time Scale on Survival")



##MODEL IP: INFLORESCENE COUNT AS RESPONSE TO PREICIPITATION___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t1,endo_01,spec,log_tillers_centered,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         plot,year_t,original) %>% 
  drop_na() -> all_infl_ppt

# Safety conversion to continuous sequential factor integers 
all_infl_ppt$plot    <- as.integer(as.factor(all_infl_ppt$plot))
all_infl_ppt$spec    <- as.integer(as.factor(all_infl_ppt$spec))
all_infl_ppt$year_t  <- as.integer(as.factor(all_infl_ppt$year_t))

all_infl_dat_ppt<-list(n_obs=nrow(all_infl_ppt),
                       y=as.integer(all_infl_ppt$flw_count_t1),
                       n_yrs = length(unique(all_infl_ppt$year_t)),
                       n_plots = length(unique(all_infl_ppt$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_infl_ppt$spec)),
                       endo_01= as.integer(all_infl_ppt$endo_01),
                       size=as.numeric(all_infl_ppt$log_tillers_centered),
                       K = 8,
                       climate = as.matrix(scale(all_infl_ppt[, c("firstthreeback_ppt","secondthreeback_ppt",
                                                                  "thirdthreeback_ppt","fourththreeback_ppt",
                                                                  "fifththreeback_ppt","sixththreeback_ppt",
                                                                  "sevenththreeback_ppt","eighththreeback_ppt")])),
                       year_index = all_infl_ppt$year_t,
                       plot=all_infl_ppt$plot,
                       species=all_infl_ppt$spec,
                       original= as.integer(all_infl_ppt$original))

all_infl_model_ppt = stan_model(file="code/climatedemoinfloSAM.stan")
all_infl_sampling_ppt<-sampling(all_infl_model_ppt,
                                data=all_infl_dat_ppt,
                                chains = 3, 
                                iter = 8000, 
                                warmup  = 1000,
                                pars=c("beta_0","beta_clim","sigma_year",
                                       "sigma_plot", "w", "y_rep"),
                                include = TRUE)

saveRDS(all_infl_sampling_ppt,"all_infl_sampling_ppt.rds")
all_infl_sampling_ppt<-readRDS("all_infl_sampling_ppt.rds")


mcmc_intervals(all_infl_sampling_ppt,regex_pars = "beta_clim",cols=c("red","blue"))
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_infl_sampling_ppt,pars="y_rep")
ppc_dens_overlay(all_infl_dat_ppt$y,y_rep$y_rep[1:500,])+xlim(0,5)

summary(all_infl_sampling_ppt)

#extracting parameters
params_all_i_ppt<-rstan::extract(all_infl_sampling_ppt,pars=c('beta_0','beta_clim','w'))
dim(params_all_i_ppt$beta_0)
dim(params_all_i_ppt$beta_clim)
dim(params_all_i_ppt$w)

##trace plots of beta clim and w
mcmc_trace(all_infl_sampling_ppt,regex_pars = "beta_0")
mcmc_trace(all_infl_sampling_ppt,regex_pars = "beta_clim")
mcmc_trace(all_infl_sampling_ppt,regex_pars = "w")

##PLOTTING EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_posti_ppt<-params_all_i_ppt$beta_clim[sample(dim(params_all_i_ppt$beta_clim)[1],size=1000),,]
dim(all_betaclim_posti_ppt)

long_df_all_betaclimi_ppt <- as.data.frame.table(all_betaclim_posti_ppt,
                                                 responseName = "estimate")
str(long_df_all_betaclimi_ppt)

# Convert to long data frame
long_df_all_betaclimi_ppt <- as.data.frame.table(all_betaclim_posti_ppt,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimi_ppt$spec <- case_when(long_df_all_betaclimi_ppt$species == 8 ~ "AGPE",
                                            long_df_all_betaclimi_ppt$species == 2 ~ "ELRI",
                                            long_df_all_betaclimi_ppt$species == 3 ~ "ELVI",
                                            long_df_all_betaclimi_ppt$species == 4 ~ "FESU",
                                            long_df_all_betaclimi_ppt$species == 5 ~ "LOAR",
                                            long_df_all_betaclimi_ppt$species == 6 ~ "POAL",
                                            long_df_all_betaclimi_ppt$species == 7 ~ "POAU",
                                            long_df_all_betaclimi_ppt$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data <- long_df_all_betaclimi_ppt %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data, aes(x = slope_val, y = spec, color = endo_label)) +
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
    title = "Precipitation Effects on Inflorescence Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

##PLOTTING Ws
#take a random subset of posterior draws for the w 
all_w_posti_ppt<-params_all_i_ppt$w[sample(dim(params_all_i_ppt$w)[1],size=1000),,]
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

#ggplot(summary_df_all_wf_ppt)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

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



##MODEL GP: GROWTH RATE AS RESPONSE TO PREICIPITATION___________________

##prep data for total precipitation, dropping NAs
pop_growth_df %>% 
  select(r,endo_01,spec,
         firstthreeback_ppt,secondthreeback_ppt,thirdthreeback_ppt,fourththreeback_ppt,
         fifththreeback_ppt,sixththreeback_ppt,sevenththreeback_ppt,eighththreeback_ppt,
         plot,year_t) %>% 
  drop_na() -> all_grow_ppt

all_grow_dat_ppt <- list(n_obs = nrow(all_grow_ppt),
                         y = all_grow_ppt$r,
                         n_yrs = length(unique(all_grow_ppt$year_t)),
                         n_plots = max(all_grow_ppt$plot),
                         n_endo = 2,
                         n_spp = max(all_grow_ppt$spec),
                         endo_01 = all_grow_ppt$endo_01,
                         K = 8,
                         climate = scale(all_grow_ppt[,c("firstthreeback_ppt","secondthreeback_ppt",
                                                         "thirdthreeback_ppt","fourththreeback_ppt",
                                                         "fifththreeback_ppt","sixththreeback_ppt",
                                                         "sevenththreeback_ppt","eighththreeback_ppt")]),
                         year_index = as.integer(as.factor(all_grow_ppt$year_t)),
                         plot = all_grow_ppt$plot,
                         species = all_grow_ppt$spec)

all_grow_model_ppt = stan_model(file="code/climatedemogrowthSAM.stan")
all_grow_sampling_ppt <- sampling(all_grow_model_ppt,
                                  data = all_grow_dat_ppt,
                                  chains = 3, 
                                  iter = 8000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_clim","sigma_year",
                                         "sigma_plot","sigma", "w", "y_rep"),
                                  include = TRUE)

saveRDS(all_grow_sampling_ppt,"all_grow_sampling_ppt.rds")
all_grow_sampling_ppt<-readRDS("all_grow_sampling_ppt.rds")

mcmc_intervals(all_grow_sampling_ppt,regex_pars = "beta_clim",cols=c("red","blue"))
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_grow_sampling_ppt,pars="y_rep")
ppc_dens_overlay(all_grow_dat_ppt$y,y_rep$y_rep[1:500,])

summary(all_grow_sampling_ppt)

#extracting parameters
params_all_g_ppt<-rstan::extract(all_grow_sampling_ppt,pars=c('beta_0','beta_clim','w'))
dim(params_all_g_ppt$beta_0)
dim(params_all_g_ppt$beta_clim)
dim(params_all_g_ppt$w)

##trace plots of beta clim and w
mcmc_trace(all_grow_sampling_ppt,regex_pars = "beta_0")
mcmc_trace(all_grow_sampling_ppt,regex_pars = "beta_clim")
mcmc_trace(all_grow_sampling_ppt,regex_pars = "w")

##PLOTTING EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postg_ppt<-params_all_g_ppt$beta_clim[sample(dim(params_all_g_ppt$beta_clim)[1],size=1000),,]
dim(all_betaclim_postg_ppt)

long_df_all_betaclimg_ppt <- as.data.frame.table(all_betaclim_postg_ppt,
                                                 responseName = "estimate")
str(long_df_all_betaclimg_ppt)

# Convert to long data frame
long_df_all_betaclimg_ppt <- as.data.frame.table(all_betaclim_postg_ppt,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimg_ppt$spec <- case_when(long_df_all_betaclimg_ppt$species == 8 ~ "AGPE",
                                            long_df_all_betaclimg_ppt$species == 2 ~ "ELRI",
                                            long_df_all_betaclimg_ppt$species == 3 ~ "ELVI",
                                            long_df_all_betaclimg_ppt$species == 4 ~ "FESU",
                                            long_df_all_betaclimg_ppt$species == 5 ~ "LOAR",
                                            long_df_all_betaclimg_ppt$species == 6 ~ "POAL",
                                            long_df_all_betaclimg_ppt$species == 7 ~ "POAU",
                                            long_df_all_betaclimg_ppt$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data <- long_df_all_betaclimg_ppt %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data, aes(x = slope_val, y = spec, color = endo_label)) +
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

##PLOTTING Ws
#take a random subset of posterior draws for the w 
all_w_postg_ppt<-params_all_g_ppt$w[sample(dim(params_all_g_ppt$w)[1],size=1000),,]
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
       title = "Comparison of Weights of Precipitation in Each Time Scale")




#####TEMPERATURE MODELS___________________________________________________________



##MODEL FT: PROBAILITY OF FLOWERING AS RESPONSE TO TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t,endo_01,spec,log_tillers_centered,
         firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
         fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
         plot,year_t,original) %>% 
  drop_na() -> all_flow_temp

# Safety conversion to continuous sequential factor integers 
all_flow_temp$plot    <- as.integer(as.factor(all_flow_temp$plot))
all_flow_temp$spec    <- as.integer(as.factor(all_flow_temp$spec))
all_flow_temp$year_t  <- as.integer(as.factor(all_flow_temp$year_t))

all_flow_dat_temp<-list(n_obs=nrow(all_flow_temp),
                       y=as.integer(all_flow_temp$flw_count_t > 0),
                       n_yrs = length(unique(all_flow_temp$year_t)),
                       n_plots = length(unique(all_flow_temp$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_flow_temp$spec)),
                       endo_01= as.integer(all_flow_temp$endo_01),
                       size=as.numeric(all_flow_temp$log_tillers_centered),
                       K = 8,
                       climate = as.matrix(scale(all_flow_temp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                  "thirdthreeback_tmean","fourththreeback_tmean",
                                                                  "fifththreeback_tmean","sixththreeback_tmean",
                                                                  "sevenththreeback_tmean","eighththreeback_tmean")])),
                       year_index = all_flow_temp$year_t,
                       plot=all_flow_temp$plot,
                       species=all_flow_temp$spec,
                       original= as.integer(all_flow_temp$original))

all_flow_model_temp = stan_model(file="code/climatedemoSAM.stan")
all_flow_sampling_temp <- sampling(all_flow_model_temp,
                                  data = all_flow_dat_temp,
                                  chains = 3, 
                                  iter = 8000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_clim","sigma_year",
                                         "sigma_plot", "w", "y_rep"),
                                  include = TRUE)


saveRDS(all_flow_sampling_temp,"all_flow_sampling_temp.rds")
all_flow_sampling_temp<-readRDS("all_flow_sampling_temp.rds")

mcmc_intervals(all_flow_sampling_temp,regex_pars = "beta_clim",cols=c("red","blue"))
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_flow_sampling_temp,pars="y_rep")
ppc_dens_overlay(all_flow_dat_temp$y,y_rep$y_rep[1:500,])

summary(all_flow_sampling_temp)

#extracting parameters
params_all_f_temp<-rstan::extract(all_flow_sampling_temp,pars=c('beta_0','beta_clim','w'))
dim(params_all_f_temp$beta_0)
dim(params_all_f_temp$beta_clim)
dim(params_all_f_temp$w)

##trace plots of beta clim and w
mcmc_trace(all_flow_sampling_temp,regex_pars = "beta_0")
mcmc_trace(all_flow_sampling_temp,regex_pars = "beta_clim")
mcmc_trace(all_flow_sampling_temp,regex_pars = "w")

##PLOTTING EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postf_temp<-params_all_f_temp$beta_clim[sample(dim(params_all_f_temp$beta_clim)[1],size=1000),,]
dim(all_betaclim_postf_temp)

long_df_all_betaclimf_temp <- as.data.frame.table(all_betaclim_postf_temp,
                                                 responseName = "estimate")
str(long_df_all_betaclimf_temp)

# Convert to long data frame
long_df_all_betaclimf_temp <- as.data.frame.table(all_betaclim_postf_temp,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimf_temp$spec <- case_when(long_df_all_betaclimf_temp$species == 8 ~ "AGPE",
                                            long_df_all_betaclimf_temp$species == 2 ~ "ELRI",
                                            long_df_all_betaclimf_temp$species == 3 ~ "ELVI",
                                            long_df_all_betaclimf_temp$species == 4 ~ "FESU",
                                            long_df_all_betaclimf_temp$species == 5 ~ "LOAR",
                                            long_df_all_betaclimf_temp$species == 6 ~ "POAL",
                                            long_df_all_betaclimf_temp$species == 7 ~ "POAU",
                                            long_df_all_betaclimf_temp$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data <- long_df_all_betaclimf_temp %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data, aes(x = slope_val, y = spec, color = endo_label)) +
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
    title = "Temperature Effects on Probability of Flowering"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

##PLOTTING Ws
#take a random subset of posterior draws for the w 
all_w_postf_temp<-params_all_f_temp$w[sample(dim(params_all_f_temp$w)[1],size=1000),,]
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



##MODEL ST: SURVIVAL AS RESPONSE TO TEMPERATURE___________________


##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(surv_t1,endo_01,spec,log_tillers_centered,
         firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
         fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
         plot,year_t,original) %>% 
  drop_na() -> all_surv_temp

# Safety conversion to continuous sequential factor integers 
all_surv_temp$plot    <- as.integer(as.factor(all_surv_temp$plot))
all_surv_temp$spec    <- as.integer(as.factor(all_surv_temp$spec))
all_surv_temp$year_t  <- as.integer(as.factor(all_surv_temp$year_t))

all_surv_dat_temp<-list(n_obs=nrow(all_surv_temp),
                       y=as.integer(all_surv_temp$surv_t1),
                       n_yrs = length(unique(all_surv_temp$year_t)),
                       n_plots = length(unique(all_surv_temp$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_surv_temp$spec)),
                       endo_01= as.integer(all_surv_temp$endo_01),
                       size=as.numeric(all_surv_temp$log_tillers_centered),
                       K = 8,
                       climate = as.matrix(scale(all_surv_temp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                  "thirdthreeback_tmean","fourththreeback_tmean",
                                                                  "fifththreeback_tmean","sixththreeback_tmean",
                                                                  "sevenththreeback_tmean","eighththreeback_tmean")])),
                       year_index = all_surv_temp$year_t,
                       plot=all_surv_temp$plot,
                       species=all_surv_temp$spec,
                       original= as.integer(all_surv_temp$original))

all_surv_model_temp = stan_model(file="code/climatedemoSAMnoncen.stan")
all_surv_sampling_temp <- sampling(all_surv_model_temp,
                                  data = all_surv_dat_temp,
                                  chains = 3, 
                                  iter = 8000, 
                                  warmup  = 1000,
                                  pars=c("beta_0","beta_clim","sigma_year",
                                         "sigma_plot", "w", "y_rep"),
                                  include = TRUE)


saveRDS(all_surv_sampling_temp,"all_surv_sampling_temp.rds")
all_surv_sampling_temp<-readRDS("all_surv_sampling_temp.rds")

mcmc_intervals(all_surv_sampling_temp,regex_pars = "beta_clim",cols=c("red","blue"))
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_surv_sampling_temp,pars="y_rep")
ppc_dens_overlay(all_surv_dat_temp$y,y_rep$y_rep[1:500,])

summary(all_surv_sampling_temp)

#extracting parameters
params_all_s_temp<-rstan::extract(all_surv_sampling_temp,pars=c('beta_0','beta_clim','w'))
dim(params_all_s_temp$beta_0)
dim(params_all_s_temp$beta_clim)
dim(params_all_s_temp$w)

##trace plots of beta clim and w
mcmc_trace(all_surv_sampling_temp,regex_pars = "beta_0")
mcmc_trace(all_surv_sampling_temp,regex_pars = "beta_clim")
mcmc_trace(all_surv_sampling_temp,regex_pars = "w")

##PLOTTING EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_posts_temp<-params_all_s_temp$beta_clim[sample(dim(params_all_s_temp$beta_clim)[1],size=1000),,]
dim(all_betaclim_posts_temp)

long_df_all_betaclims_temp <- as.data.frame.table(all_betaclim_posts_temp,
                                                 responseName = "estimate")
str(long_df_all_betaclims_temp)

# Convert to long data frame
long_df_all_betaclims_temp <- as.data.frame.table(all_betaclim_posts_temp,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclims_temp$spec <- case_when(long_df_all_betaclims_temp$species == 8 ~ "AGPE",
                                            long_df_all_betaclims_temp$species == 2 ~ "ELRI",
                                            long_df_all_betaclims_temp$species == 3 ~ "ELVI",
                                            long_df_all_betaclims_temp$species == 4 ~ "FESU",
                                            long_df_all_betaclims_temp$species == 5 ~ "LOAR",
                                            long_df_all_betaclims_temp$species == 6 ~ "POAL",
                                            long_df_all_betaclims_temp$species == 7 ~ "POAU",
                                            long_df_all_betaclims_temp$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data <- long_df_all_betaclims_temp %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data, aes(x = slope_val, y = spec, color = endo_label)) +
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
    title = "Temperature Effects on Survival"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

##PLOTTING Ws
#take a random subset of posterior draws for the w 
all_w_posts_temp<-params_all_s_temp$w[sample(dim(params_all_s_temp$w)[1],size=1000),,]
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
       title = "Weights of Temperature in Each Time Scale on Survival")



##MODEL IT: INFLORESCENCE COUNT AS RESPONSE TO TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t1,endo_01,spec,log_tillers_centered,
         firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
         fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
         plot,year_t,original) %>% 
  drop_na() -> all_infl_temp

# Safety conversion to continuous sequential factor integers 
all_infl_temp$plot    <- as.integer(as.factor(all_infl_temp$plot))
all_infl_temp$spec    <- as.integer(as.factor(all_infl_temp$spec))
all_infl_temp$year_t  <- as.integer(as.factor(all_infl_temp$year_t))

all_infl_dat_temp<-list(n_obs=nrow(all_infl_temp),
                       y=as.integer(all_infl_temp$flw_count_t1),
                       n_yrs = length(unique(all_infl_temp$year_t)),
                       n_plots = length(unique(all_infl_temp$plot)),
                       n_endo = 2,
                       n_spp = length(unique(all_infl_temp$spec)),
                       endo_01= as.integer(all_infl_temp$endo_01),
                       size=as.numeric(all_infl_temp$log_tillers_centered),
                       K = 8,
                       climate = as.matrix(scale(all_infl_temp[, c("firstthreeback_tmean","secondthreeback_tmean",
                                                                  "thirdthreeback_tmean","fourththreeback_tmean",
                                                                  "fifththreeback_tmean","sixththreeback_tmean",
                                                                  "sevenththreeback_tmean","eighththreeback_tmean")])),
                       year_index = all_infl_temp$year_t,
                       plot=all_infl_temp$plot,
                       species=all_infl_temp$spec,
                       original= as.integer(all_infl_temp$original))

all_infl_model_temp = stan_model(file="code/climatedemoinfloSAM.stan")
all_infl_sampling_temp<-sampling(all_infl_model_temp,
                                 data=all_infl_dat_temp,
                                 chains = 3, 
                                 iter = 8000, 
                                 warmup  = 1000,
                                 pars=c("beta_0","beta_clim","sigma_year",
                                       "sigma_plot", "w", "y_rep"),
                                 include = TRUE)

saveRDS(all_infl_sampling_temp,"all_infl_sampling_temp.rds")
all_infl_sampling_temp<-readRDS("all_infl_sampling_temp.rds")


mcmc_intervals(all_infl_sampling_temp,regex_pars = "beta_clim",cols=c("red","blue"))
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_infl_sampling_temp,pars="y_rep")
ppc_dens_overlay(all_infl_dat_temp$y,y_rep$y_rep[1:500,])+xlim(0,5)

summary(all_infl_sampling_temp)

#extracting parameters
params_all_i_temp<-rstan::extract(all_infl_sampling_temp,pars=c('beta_0','beta_clim','w'))
dim(params_all_i_temp$beta_0)
dim(params_all_i_temp$beta_clim)
dim(params_all_i_temp$w)

##trace plots of beta clim and w
mcmc_trace(all_infl_sampling_temp,regex_pars = "beta_0")
mcmc_trace(all_infl_sampling_temp,regex_pars = "beta_clim")
mcmc_trace(all_infl_sampling_temp,regex_pars = "w")

##PLOTTING EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_posti_temp<-params_all_i_temp$beta_clim[sample(dim(params_all_i_temp$beta_clim)[1],size=1000),,]
dim(all_betaclim_posti_temp)

long_df_all_betaclimi_temp <- as.data.frame.table(all_betaclim_posti_temp,
                                                 responseName = "estimate")
str(long_df_all_betaclimi_temp)

# Convert to long data frame
long_df_all_betaclimi_temp <- as.data.frame.table(all_betaclim_posti_temp,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimi_temp$spec <- case_when(long_df_all_betaclimi_temp$species == 8 ~ "AGPE",
                                            long_df_all_betaclimi_temp$species == 2 ~ "ELRI",
                                            long_df_all_betaclimi_temp$species == 3 ~ "ELVI",
                                            long_df_all_betaclimi_temp$species == 4 ~ "FESU",
                                            long_df_all_betaclimi_temp$species == 5 ~ "LOAR",
                                            long_df_all_betaclimi_temp$species == 6 ~ "POAL",
                                            long_df_all_betaclimi_temp$species == 7 ~ "POAU",
                                            long_df_all_betaclimi_temp$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data <- long_df_all_betaclimi_temp %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data, aes(x = slope_val, y = spec, color = endo_label)) +
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

##PLOTTING Ws
#take a random subset of posterior draws for the w 
all_w_posti_temp<-params_all_i_temp$w[sample(dim(params_all_i_temp$w)[1],size=1000),,]
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

#ggplot(summary_df_all_wf_ppt)+ geom_point(aes(x=threemonth,y=median_weight))+ facet_grid("spec")

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



##MODEL GT: GROWTH RATE AS RESPONSE TO TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
pop_growth_df %>% 
  select(r,endo_01,spec,,
         firstthreeback_tmean,secondthreeback_tmean,thirdthreeback_tmean,fourththreeback_tmean,
         fifththreeback_tmean,sixththreeback_tmean,sevenththreeback_tmean,eighththreeback_tmean,
         plot,year_t) %>% 
  drop_na() -> all_grow_temp

all_grow_dat_temp <- list(n_obs = nrow(all_grow_temp),
                          y = all_grow_temp$r,
                          n_yrs = length(unique(all_grow_temp$year_t)),
                          n_plots = max(all_grow_temp$plot),
                          n_endo = 2,
                          n_spp = max(all_grow_temp$spec),
                          endo_01 = all_grow_temp$endo_01,
                          K = 8,
                          climate = scale(all_grow_temp[,c("firstthreeback_tmean","secondthreeback_tmean",
                                                          "thirdthreeback_tmean","fourththreeback_tmean",
                                                          "fifththreeback_tmean","sixththreeback_tmean",
                                                          "sevenththreeback_tmean","eighththreeback_tmean")]),
                          year_index = as.integer(as.factor(all_grow_temp$year_t)),
                          plot = all_grow_temp$plot,
                          species = all_grow_temp$spec)

all_grow_model_temp = stan_model(file="code/climatedemogrowthSAM.stan")
all_grow_sampling_temp <- sampling(all_grow_model_temp,
                                   data = all_grow_dat_temp,
                                   chains = 3, 
                                   iter = 8000, 
                                   warmup  = 1000,
                                   pars=c("beta_0","beta_clim","sigma_year",
                                          "sigma_plot","sigma", "w", "y_rep"),
                                   include = TRUE)

saveRDS(all_grow_sampling_temp,"all_grow_sampling_temp.rds")
all_grow_sampling_temp<-readRDS("all_grow_sampling_temp.rds")

mcmc_intervals(all_grow_sampling_temp,regex_pars = "beta_clim",cols=c("red","blue"))
#note to self - make the E+ E- pairs close to each other and distinguished by color

##posterior predictive check
y_rep<-extract(all_grow_sampling_temp,pars="y_rep")
ppc_dens_overlay(all_grow_dat_temp$y,y_rep$y_rep[1:500,])

summary(all_grow_sampling_temp)

#extracting parameters
params_all_g_temp<-rstan::extract(all_grow_sampling_temp,pars=c('beta_0','beta_clim','w'))
dim(params_all_g_temp$beta_0)
dim(params_all_g_temp$beta_clim)
dim(params_all_g_temp$w)

##trace plots of beta clim and w
mcmc_trace(all_grow_sampling_temp,regex_pars = "beta_0")
mcmc_trace(all_grow_sampling_temp,regex_pars = "beta_clim")
mcmc_trace(all_grow_sampling_temp,regex_pars = "w")

##PLOTTING EFFECTS

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postg_temp<-params_all_g_temp$beta_clim[sample(dim(params_all_g_temp$beta_clim)[1],size=1000),,]
dim(all_betaclim_postg_temp)

long_df_all_betaclimg_temp <- as.data.frame.table(all_betaclim_postg_temp,
                                                 responseName = "estimate")
str(long_df_all_betaclimg_temp)

# Convert to long data frame
long_df_all_betaclimg_temp <- as.data.frame.table(all_betaclim_postg_temp,
                                                 responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimg_temp$spec <- case_when(long_df_all_betaclimg_temp$species == 8 ~ "AGPE",
                                            long_df_all_betaclimg_temp$species == 2 ~ "ELRI",
                                            long_df_all_betaclimg_temp$species == 3 ~ "ELVI",
                                            long_df_all_betaclimg_temp$species == 4 ~ "FESU",
                                            long_df_all_betaclimg_temp$species == 5 ~ "LOAR",
                                            long_df_all_betaclimg_temp$species == 6 ~ "POAL",
                                            long_df_all_betaclimg_temp$species == 7 ~ "POAU",
                                            long_df_all_betaclimg_temp$species == 1 ~ "POSY")

# 1. Process your extracted long dataframe
plot_data <- long_df_all_betaclimg_temp %>%
  mutate(
    # Ensure endo is treated properly (Var3 becomes a factor by default in as.data.frame.table)
    # Map index 1 to Negative (0) and index 2 to Positive (1)
    endo_index = as.integer(endo), 
    endo_label = ifelse(endo_index == 1, "E-", "E+"),
  )

# 2. Build the structured interval plot
ggplot(plot_data, aes(x = slope_val, y = spec, color = endo_label)) +
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


##PLOTTING Ws
#take a random subset of posterior draws for the w 
all_w_postg_temp<-params_all_g_temp$w[sample(dim(params_all_g_temp$w)[1],size=1000),,]
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



