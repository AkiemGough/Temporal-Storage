#--------------------#
#title: "Climate explicit models of precipitation, temperature, VPDmax and endophyte effects on plant demography"
#author: "Akiem_Gough"
#date: "2026-04-30"
#--------------------#

#Important packages
library(tidyverse)
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
gras <- gras[!(gras$id=="79 1164 4"),] 

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
grasclim$ppt_tot_scaled <- as.numeric (scale(grasclim$ppt_tot))

#Generating new data frame for Population Growth Rate

pop_growth_df <- grasclim %>%
  # 1. Group by the variables that define a "population"
  group_by(species, plot, endo_01, year_t, spec, ppt_tot, ppt_tot_scaled, tmean_mean, vpdmax) %>%
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
  select(flw_count_t,endo_01,spec,log_tillers_centered,ppt_tot_scaled,plot,year_t,original) %>% 
  drop_na() -> all_flow_ppt

all_flow_dat_ppt<-list(n_obs=nrow(all_flow_ppt),
                       y=all_flow_ppt$flw_count_t>0,
                       n_yrs = length(unique(all_flow_ppt$year_t))+1,
                       n_plots = max(all_flow_ppt$plot),
                       n_endo = 2,
                       n_spp = length(unique(all_flow_ppt$spec)),
                       endo_01=all_flow_ppt$endo_01,
                       size=all_flow_ppt$log_tillers_centered,
                       year_index=all_flow_ppt$year_t-2006,
                       climate=all_flow_ppt$ppt_tot_scaled,
                       plot=all_flow_ppt$plot,
                       species=all_flow_ppt$spec,
                       original=all_flow_ppt$original)

all_flow_model_ppt = stan_model(file="code/climatedemo.stan")
all_flow_sampling_ppt<-sampling(all_flow_model_ppt,
     data=all_flow_dat_ppt,
     chains = 3,
    iter = 5000,
    warmup = 1000)

#saveRDS(all_flow_sampling_ppt,"all_flow_sampling_ppt.rds")
#all_flow_sampling_ppt<-readRDS("all_flow_sampling_ppt.rds")

summary(all_flow_sampling_ppt)

#extracting parameters
params_all_f_ppt<-rstan::extract(all_flow_sampling_ppt,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_f_ppt$beta_0)
dim(params_all_f_ppt$beta_clim)
dim(params_all_f_ppt$beta_size_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0
all_beta0_postf_ppt<-params_all_f_ppt$beta_0[sample(dim(params_all_f_ppt$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_postf_ppt)

long_df_all_beta0f_ppt <- as.data.frame.table(all_beta0_postf_ppt,
                                              responseName = "estimate")
str(long_df_all_beta0f_ppt)

# Convert to long data frame
long_df_all_beta0f_ppt <- as.data.frame.table(all_beta0_postf_ppt,
                                              responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0f_ppt$spec <- case_when(long_df_all_beta0f_ppt$species == 8 ~ "AGPE",
                                         long_df_all_beta0f_ppt$species == 2 ~ "ELRI",
                                         long_df_all_beta0f_ppt$species == 3 ~ "ELVI",
                                         long_df_all_beta0f_ppt$species == 4 ~ "FESU",
                                         long_df_all_beta0f_ppt$species == 5 ~ "LOAR",
                                         long_df_all_beta0f_ppt$species == 6 ~ "POAL",
                                         long_df_all_beta0f_ppt$species == 7 ~ "POAU",
                                         long_df_all_beta0f_ppt$species == 1 ~ "POSY")

summary_df_all_beta0f_ppt <- long_df_all_beta0f_ppt %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postf_ppt<-params_all_f_ppt$beta_clim[sample(dim(params_all_f_ppt$beta_clim)[1],size=1000),,]
dim(all_betaclim_postf_ppt)

long_df_all_betaclimf_ppt <- as.data.frame.table(all_betaclim_postf_ppt,
                                                 responseName = "estimate")
str(long_df_all_betaclimf_ppt)

# Convert to long data frame for endo effect
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


summary_df_all_betaclimf_ppt <- long_df_all_betaclimf_ppt %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")


#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data <- expand.grid(
  ppt_tot_scaled = seq(min(all_flow_ppt$ppt_tot_scaled, na.rm=T), 
                       max(all_flow_ppt$ppt_tot_scaled, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0f_ppt$spec),
  endo = unique(long_df_all_beta0f_ppt$endo)
)

plot_data <- plot_data %>%
  left_join(summary_df_all_beta0f_ppt, by = c("spec", "endo")) %>% # Brings in 'median' (intercept)
  left_join(summary_df_all_betaclimf_ppt, by = c("spec", "endo")) %>% # Brings in 'median_slope'
  mutate(
    # plogis(intercept + slope * x)
    prob_flowering = plogis(median + (median_slope * ppt_tot_scaled)),
    # Calculate ribbon bounds
    prob_lower = plogis(lower + (lower_slope * ppt_tot_scaled)),
    prob_upper = plogis(upper + (upper_slope * ppt_tot_scaled))
  )

all_flow_ppt$spec <- case_when(
  all_flow_ppt$spec == 8 ~ "AGPE",
  all_flow_ppt$spec == 2 ~ "ELRI",
  all_flow_ppt$spec == 3 ~ "ELVI",
  all_flow_ppt$spec == 4 ~ "FESU",
  all_flow_ppt$spec == 5 ~ "LOAR",
  all_flow_ppt$spec == 6 ~ "POAL",
  all_flow_ppt$spec == 7 ~ "POAU",
  all_flow_ppt$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data, 
              aes(x = ppt_tot_scaled, ymin = prob_lower, ymax = prob_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data, 
            aes(x = ppt_tot_scaled, y = prob_flowering, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_flow_ppt, 
             aes(x = ppt_tot_scaled, y = as.numeric(flw_count_t > 1)), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "Precipitation (Scaled)", 
       y = "Probability of Flowering",
       title = "Flowering Probability with 90% Credible Intervals") +
  theme_minimal()



##MODEL SP: SURVIVAL AS RESPONSE TO PREICIPITATION___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(surv_t1,endo_01,spec,log_tillers_centered,ppt_tot_scaled,plot,year_t,original) %>% 
  drop_na() -> all_surv_ppt

all_surv_dat_ppt<-list(n_obs=nrow(all_surv_ppt),
                       y=all_surv_ppt$surv_t1,
                       n_yrs = length(unique(all_surv_ppt$year_t))+1,
                       n_plots = max(all_surv_ppt$plot),
                       n_endo = 2,
                       n_spp = length(unique(all_surv_ppt$spec)),
                       endo_01=all_surv_ppt$endo_01,
                       size=all_surv_ppt$log_tillers_centered,
                       year_index=all_surv_ppt$year_t-2006,
                       climate=all_surv_ppt$ppt_tot_scaled,
                       plot=all_surv_ppt$plot,
                       species=all_surv_ppt$spec,
                       original=all_surv_ppt$original)

all_surv_model_ppt = stan_model(file="code/climatedemo.stan")
all_surv_sampling_ppt<-sampling(all_surv_model_ppt,
                                data=all_surv_dat_ppt,
                                chains = 3,
                                iter = 5000, 
                                warmup = 1000)

#saveRDS(all_surv_sampling_ppt,"all_surv_sampling_ppt.rds")
#all_surv_sampling_ppt<-readRDS("all_surv_sampling_ppt.rds")

summary(all_surv_sampling_ppt)

#extracting parameters
params_all_s_ppt<-rstan::extract(all_surv_sampling_ppt,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_s_ppt$beta_0)
dim(params_all_s_ppt$beta_clim)
dim(params_all_s_ppt$beta_size_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0
all_beta0_posts_ppt<-params_all_s_ppt$beta_0[sample(dim(params_all_s_ppt$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_posts_ppt)

long_df_all_beta0s_ppt <- as.data.frame.table(all_beta0_posts_ppt,
                                              responseName = "estimate")
str(long_df_all_beta0s_ppt)

# Convert to long data frame for endo effect
long_df_all_beta0s_ppt <- as.data.frame.table(all_beta0_posts_ppt,
                                              responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0s_ppt$spec <- case_when(long_df_all_beta0s_ppt$species == 8 ~ "AGPE",
                                         long_df_all_beta0s_ppt$species == 2 ~ "ELRI",
                                         long_df_all_beta0s_ppt$species == 3 ~ "ELVI",
                                         long_df_all_beta0s_ppt$species == 4 ~ "FESU",
                                         long_df_all_beta0s_ppt$species == 5 ~ "LOAR",
                                         long_df_all_beta0s_ppt$species == 6 ~ "POAL",
                                         long_df_all_beta0s_ppt$species == 7 ~ "POAU",
                                         long_df_all_beta0s_ppt$species == 1 ~ "POSY")

summary_df_all_beta0s_ppt <- long_df_all_beta0s_ppt %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_posts_ppt<-params_all_s_ppt$beta_clim[sample(dim(params_all_s_ppt$beta_clim)[1],size=1000),,]
dim(all_betaclim_posts_ppt)

long_df_all_betaclims_ppt <- as.data.frame.table(all_betaclim_posts_ppt,
                                                 responseName = "estimate")
str(long_df_all_betaclims_ppt)

# Convert to long data frame for endo effect
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

summary_df_all_betaclims_ppt <- long_df_all_betaclims_ppt %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")

#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data <- expand.grid(
  ppt_tot_scaled = seq(min(all_surv_ppt$ppt_tot_scaled, na.rm=T), 
                       max(all_surv_ppt$ppt_tot_scaled, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0s_ppt$spec),
  endo = unique(long_df_all_beta0s_ppt$endo)
)

plot_data <- plot_data %>%
  left_join(summary_df_all_beta0s_ppt, by = c("spec", "endo")) %>% # Brings in 'median' (intercept)
  left_join(summary_df_all_betaclims_ppt, by = c("spec", "endo")) %>% # Brings in 'median_slope'
  mutate(
    # plogis(intercept + slope * x)
    prob_survival = plogis(median + (median_slope * ppt_tot_scaled)),
    # Calculate ribbon bounds
    prob_lower = plogis(lower + (lower_slope * ppt_tot_scaled)),
    prob_upper = plogis(upper + (upper_slope * ppt_tot_scaled))
  )

all_surv_ppt$spec <- case_when(
  all_surv_ppt$spec == 8 ~ "AGPE",
  all_surv_ppt$spec == 2 ~ "ELRI",
  all_surv_ppt$spec == 3 ~ "ELVI",
  all_surv_ppt$spec == 4 ~ "FESU",
  all_surv_ppt$spec == 5 ~ "LOAR",
  all_surv_ppt$spec == 6 ~ "POAL",
  all_surv_ppt$spec == 7 ~ "POAU",
  all_surv_ppt$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data, 
              aes(x = ppt_tot_scaled, ymin = prob_lower, ymax = prob_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data, 
            aes(x = ppt_tot_scaled, y = prob_survival, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_surv_ppt, 
             aes(x = ppt_tot_scaled, y = surv_t1), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "Precipitation (Scaled)", 
       y = "Probability of Survival",
       title = "Survival with 90% Credible Intervals") +
  theme_minimal()

##MODEL GP: GROWTH RATE AS RESPONSE TO PREICIPITATION___________________

##prep data for total precipitation, dropping NAs
pop_growth_df %>% 
  select(r,endo_01,spec,ppt_tot_scaled,plot,year_t) %>% 
  drop_na() -> all_grow_ppt

all_grow_dat_ppt <- list(n_obs = nrow(all_grow_ppt),
                         y = all_grow_ppt$r,
                         n_yrs = length(unique(all_grow_ppt$year_t))+1,
                         n_plots = max(all_grow_ppt$plot),
                         n_endo = 2,
                         n_spp = max(all_grow_ppt$spec),
                         endo_01 = all_grow_ppt$endo_01,
                         year_index = all_grow_ppt$year_t-2006,
                         climate = all_grow_ppt$ppt_tot_scaled,
                         plot = all_grow_ppt$plot,
                         species = all_grow_ppt$spec)

all_grow_model_ppt = stan_model(file="code/climatedemogrowth.stan")
all_grow_sampling_ppt <- sampling(all_grow_model_ppt,
                                  data = all_grow_dat_ppt,
                                  chains = 3, 
                                  iter = 5000, 
                                  warmup  = 1000,
                                  include = TRUE)

#saveRDS(all_grow_sampling_ppt,"all_grow_sampling_ppt.rds")
#all_grow_sampling_ppt<-readRDS("all_grow_sampling_ppt.rds")

summary(all_grow_sampling_ppt)

#extracting parameters
params_all_g_ppt<-rstan::extract(all_grow_sampling_ppt,pars=c('beta_0','beta_clim'))
dim(params_all_g_ppt$beta_0)
dim(params_all_g_ppt$beta_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0
all_beta0_postg_ppt<-params_all_g_ppt$beta_0[sample(dim(params_all_g_ppt$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_postg_ppt)

long_df_all_beta0g_ppt <- as.data.frame.table(all_beta0_postg_ppt,
                                              responseName = "estimate")
str(long_df_all_beta0g_ppt)

# Convert to long data frame
long_df_all_beta0g_ppt <- as.data.frame.table(all_beta0_postg_ppt,
                                              responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0g_ppt$spec <- case_when(long_df_all_beta0g_ppt$species == 8 ~ "AGPE",
                                         long_df_all_beta0g_ppt$species == 2 ~ "ELRI",
                                         long_df_all_beta0g_ppt$species == 3 ~ "ELVI",
                                         long_df_all_beta0g_ppt$species == 4 ~ "FESU",
                                         long_df_all_beta0g_ppt$species == 5 ~ "LOAR",
                                         long_df_all_beta0g_ppt$species == 6 ~ "POAL",
                                         long_df_all_beta0g_ppt$species == 7 ~ "POAU",
                                         long_df_all_beta0g_ppt$species == 1 ~ "POSY")


summary_df_all_beta0g_ppt <- long_df_all_beta0g_ppt %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

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

summary_df_all_betaclimg_ppt <- long_df_all_betaclimg_ppt %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")

#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data_pptg <- expand.grid(
  ppt_tot_scaled = seq(min(all_grow_ppt$ppt_tot_scaled, na.rm=T), 
                       max(all_grow_ppt$ppt_tot_scaled, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0g_ppt$spec),
  endo = unique(long_df_all_beta0g_ppt$endo)
)

plot_data_pptg <- plot_data_pptg %>%
  left_join(summary_df_all_beta0g_ppt, by = c("spec", "endo")) %>% 
  left_join(summary_df_all_betaclimg_ppt, by = c("spec", "endo")) %>% 
  mutate(
    # the linear formula
    predicted_growth = median + (median_slope * ppt_tot_scaled),
    
    # Calculate ribbon bounds using the linear formula
    growth_lower = lower + (lower_slope * ppt_tot_scaled),
    growth_upper = upper + (upper_slope * ppt_tot_scaled)
  )

all_grow_ppt$spec <- case_when(
  all_grow_ppt$spec == 8 ~ "AGPE",
  all_grow_ppt$spec == 2 ~ "ELRI",
  all_grow_ppt$spec == 3 ~ "ELVI",
  all_grow_ppt$spec == 4 ~ "FESU",
  all_grow_ppt$spec == 5 ~ "LOAR",
  all_grow_ppt$spec == 6 ~ "POAL",
  all_grow_ppt$spec == 7 ~ "POAU",
  all_grow_ppt$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data_pptg, 
              aes(x = ppt_tot_scaled, ymin = growth_lower, ymax = growth_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data_pptg, 
            aes(x = ppt_tot_scaled, y = predicted_growth, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_grow_ppt, 
             aes(x = ppt_tot_scaled, y = as.numeric(r)), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "Precipitation (Scaled)", 
       y = "Population Growth",
       title = "Population Growth Rate with 90% Credible Intervals") +
  theme_minimal()




#####TEMPERATURE MODELS___________________________________________________________



##MODEL FT: PROBAILITY OF FLOWERING AS RESPONSE TO TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t,endo_01,spec,log_tillers_centered,tmean_mean,plot,year_t,original) %>% 
  drop_na() -> all_flow_temp

all_flow_dat_temp<-list(n_obs=nrow(all_flow_temp),
                        y=all_flow_temp$flw_count_t>0,
                        n_yrs = length(unique(all_flow_temp$year_t))+1,
                        n_plots = max(all_flow_temp$plot),
                        n_endo = 2,
                        n_spp = length(unique(all_flow_temp$spec)),
                        endo_01=all_flow_temp$endo_01,
                        size=all_flow_temp$log_tillers_centered,
                        year_index=all_flow_temp$year_t-2006,
                        climate=all_flow_temp$tmean_mean,
                        plot=all_flow_temp$plot,
                        species=all_flow_temp$spec,
                        original=all_flow_temp$original)

all_flow_model_temp = stan_model(file="code/climatedemo.stan")
all_flow_sampling_temp<-sampling(all_flow_model_temp,
                                 data=all_flow_dat_temp,
                                 chains = 3,
                                 iter = 5000,
                                 warmup = 1000)

#saveRDS(all_flow_sampling_temp,"all_flow_sampling_temp.rds")
#all_flow_sampling_temp<-readRDS("all_flow_sampling_temp.rds")

summary(all_flow_sampling_temp)

#extracting parameters
params_all_f_temp<-rstan::extract(all_flow_sampling_temp,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_f_temp$beta_0)
dim(params_all_f_temp$beta_clim)
dim(params_all_f_temp$beta_size_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0
all_beta0_postf_temp<-params_all_f_temp$beta_0[sample(dim(params_all_f_temp$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_postf_temp)

long_df_all_beta0f_temp <- as.data.frame.table(all_beta0_postf_temp,
                                               responseName = "estimate")
str(long_df_all_beta0f_temp)

# Convert to long data frame for endo effect
long_df_all_beta0f_temp <- as.data.frame.table(all_beta0_postf_temp,
                                               responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0f_temp$spec <- case_when(long_df_all_beta0f_temp$species == 8 ~ "AGPE",
                                          long_df_all_beta0f_temp$species == 2 ~ "ELRI",
                                          long_df_all_beta0f_temp$species == 3 ~ "ELVI",
                                          long_df_all_beta0f_temp$species == 4 ~ "FESU",
                                          long_df_all_beta0f_temp$species == 5 ~ "LOAR",
                                          long_df_all_beta0f_temp$species == 6 ~ "POAL",
                                          long_df_all_beta0f_temp$species == 7 ~ "POAU",
                                          long_df_all_beta0f_temp$species == 1 ~ "POSY")

summary_df_all_beta0f_temp <- long_df_all_beta0f_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postf_temp<-params_all_f_temp$beta_clim[sample(dim(params_all_f_temp$beta_clim)[1],size=1000),,]
dim(all_betaclim_postf_temp)

long_df_all_betaclimf_temp <- as.data.frame.table(all_betaclim_postf_temp,
                                                  responseName = "estimate")
str(long_df_all_betaclimf_temp)

# Convert to long data frame for endo effect
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

summary_df_all_betaclimf_temp <- long_df_all_betaclimf_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")

#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data_tempf <- expand.grid(
  tmean_mean = seq(min(all_flow_temp$tmean_mean, na.rm=T), 
                   max(all_flow_temp$tmean_mean, na.rm=T), 
                   length.out = 100),
  spec = unique(long_df_all_beta0f_temp$spec),
  endo = unique(long_df_all_beta0f_temp$endo)
)

plot_data_tempf <- plot_data_tempf %>%
  left_join(summary_df_all_beta0f_temp, by = c("spec", "endo")) %>% # Brings in 'median' (intercept)
  left_join(summary_df_all_betaclimf_temp, by = c("spec", "endo")) %>% # Brings in 'median_slope'
  mutate(
    # plogis(intercept + slope * x)
    prob_flowering = plogis(median + (median_slope * tmean_mean)),
    # Calculate ribbon bounds
    prob_lower = plogis(lower + (lower_slope * tmean_mean)),
    prob_upper = plogis(upper + (upper_slope * tmean_mean))
  )

all_flow_temp$spec <- case_when(
  all_flow_temp$spec == 8 ~ "AGPE",
  all_flow_temp$spec == 2 ~ "ELRI",
  all_flow_temp$spec == 3 ~ "ELVI",
  all_flow_temp$spec == 4 ~ "FESU",
  all_flow_temp$spec == 5 ~ "LOAR",
  all_flow_temp$spec == 6 ~ "POAL",
  all_flow_temp$spec == 7 ~ "POAU",
  all_flow_temp$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data_tempf, 
              aes(x = tmean_mean, ymin = prob_lower, ymax = prob_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data_tempf, 
            aes(x = tmean_mean, y = prob_flowering, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_flow_temp, 
             aes(x = tmean_mean, y = as.numeric(flw_count_t > 1)), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "Temperature", 
       y = "Probability of Flowering",
       title = "Flowering Probability with 90% Credible Intervals") +
  theme_minimal()



##MODEL ST: SURVIVAL AS RESPONSE TO TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(surv_t1,endo_01,spec,log_tillers_centered,tmean_mean,plot,year_t,original) %>% 
  drop_na() -> all_surv_temp

all_surv_dat_temp<-list(n_obs=nrow(all_surv_temp),
                        y=all_surv_temp$surv_t1,
                        n_yrs = length(unique(all_surv_temp$year_t))+1,
                        n_plots = max(all_surv_temp$plot),
                        n_endo = 2,
                        n_spp = length(unique(all_surv_temp$spec)),
                        endo_01=all_surv_temp$endo_01,
                        size=all_surv_temp$log_tillers_centered,
                        year_index=all_surv_temp$year_t-2006,
                        climate=all_surv_temp$tmean_mean,
                        plot=all_surv_temp$plot,
                        species=all_surv_temp$spec,
                        original=all_surv_temp$original)

all_surv_model_temp = stan_model(file="code/climatedemo.stan")
all_surv_sampling_temp<-sampling(all_surv_model_temp,
                                 data=all_surv_dat_temp,
                                 chains = 3,
                                 iter = 5000, 
                                 warmup = 1000)

#saveRDS(all_surv_sampling_temp,"all_surv_sampling_temp.rds")
#all_surv_sampling_temp<-readRDS("all_surv_sampling_temp.rds")

summary(all_surv_sampling_temp)

#extracting parameters
params_all_s_temp<-rstan::extract(all_surv_sampling_temp,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_s_temp$beta_0)
dim(params_all_s_temp$beta_clim)
dim(params_all_s_temp$beta_size_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0 (endophyte estimates?)
all_beta0_posts_temp<-params_all_s_temp$beta_0[sample(dim(params_all_s_temp$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_posts_temp)

long_df_all_beta0s_temp <- as.data.frame.table(all_beta0_posts_temp,
                                               responseName = "estimate")
str(long_df_all_beta0s_temp)

# Convert to long data frame for endo effect
long_df_all_beta0s_temp <- as.data.frame.table(all_beta0_posts_temp,
                                               responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0s_temp$spec <- case_when(long_df_all_beta0s_temp$species == 8 ~ "AGPE",
                                          long_df_all_beta0s_temp$species == 2 ~ "ELRI",
                                          long_df_all_beta0s_temp$species == 3 ~ "ELVI",
                                          long_df_all_beta0s_temp$species == 4 ~ "FESU",
                                          long_df_all_beta0s_temp$species == 5 ~ "LOAR",
                                          long_df_all_beta0s_temp$species == 6 ~ "POAL",
                                          long_df_all_beta0s_temp$species == 7 ~ "POAU",
                                          long_df_all_beta0s_temp$species == 1 ~ "POSY")

summary_df_all_beta0s_temp <- long_df_all_beta0s_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_posts_temp<-params_all_s_temp$beta_clim[sample(dim(params_all_s_temp$beta_clim)[1],size=1000),,]
dim(all_betaclim_posts_temp)

long_df_all_betaclims_temp <- as.data.frame.table(all_betaclim_posts_temp,
                                                  responseName = "estimate")
str(long_df_all_betaclims_temp)

# Convert to long data frame for endo effect
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

summary_df_all_betaclims_temp <- long_df_all_betaclims_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")

#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data_temps <- expand.grid(
  tmean_mean = seq(min(all_surv_temp$tmean_mean, na.rm=T), 
                   max(all_surv_temp$tmean_mean, na.rm=T), 
                   length.out = 100),
  spec = unique(long_df_all_beta0s_temp$spec),
  endo = unique(long_df_all_beta0s_temp$endo)
)

plot_data_temps <- plot_data_temps %>%
  left_join(summary_df_all_beta0s_temp, by = c("spec", "endo")) %>% # Brings in 'median' (intercept)
  left_join(summary_df_all_betaclims_temp, by = c("spec", "endo")) %>% # Brings in 'median_slope'
  mutate(
    # plogis(intercept + slope * x)
    prob_survival = plogis(median + (median_slope * tmean_mean)),
    # Calculate ribbon bounds
    prob_lower = plogis(lower + (lower_slope * tmean_mean)),
    prob_upper = plogis(upper + (upper_slope * tmean_mean))
  )

all_surv_temp$spec <- case_when(
  all_surv_temp$spec == 8 ~ "AGPE",
  all_surv_temp$spec == 2 ~ "ELRI",
  all_surv_temp$spec == 3 ~ "ELVI",
  all_surv_temp$spec == 4 ~ "FESU",
  all_surv_temp$spec == 5 ~ "LOAR",
  all_surv_temp$spec == 6 ~ "POAL",
  all_surv_temp$spec == 7 ~ "POAU",
  all_surv_temp$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data_temps, 
              aes(x = tmean_mean, ymin = prob_lower, ymax = prob_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data_temps, 
            aes(x = tmean_mean, y = prob_survival, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_surv_temp, 
             aes(x = tmean_mean, y = surv_t1), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "Precipitation (Scaled)", 
       y = "Probability of Survival",
       title = "Survival with 90% Credible Intervals") +
  theme_minimal()



##MODEL GT: GROWTH RATE AS RESPONSE TO TEMPERATURE___________________

##prep data for total precipitation, dropping NAs
pop_growth_df %>% 
  select(r,endo_01,spec,tmean_mean,plot,year_t) %>% 
  drop_na() -> all_grow_temp

all_grow_dat_temp <- list(n_obs = nrow(all_grow_temp),
                          y = all_grow_temp$r,
                          n_yrs = length(unique(all_grow_temp$year_t))+1,
                          n_plots = max(all_grow_temp$plot),
                          n_endo = 2,
                          n_spp = max(all_grow_temp$spec),
                          endo_01 = all_grow_temp$endo_01,
                          year_index = all_grow_temp$year_t-2006,
                          climate = all_grow_temp$tmean_mean,
                          plot = all_grow_temp$plot,
                          species = all_grow_temp$spec)

all_grow_model_temp = stan_model(file="code/climatedemogrowth.stan")
all_grow_sampling_temp <- sampling(all_grow_model_temp,
                                   data = all_grow_dat_temp,
                                   chains = 3, 
                                   iter = 5000, 
                                   warmup  = 1000,
                                   include = TRUE)

#saveRDS(all_grow_sampling_temp,"all_grow_sampling_temp.rds")
#all_grow_sampling_temp<-readRDS("all_grow_sampling_temp.rds")

summary(all_grow_sampling_temp)

#extracting parameters
params_all_g_temp<-rstan::extract(all_grow_sampling_temp,pars=c('beta_0','beta_clim'))
dim(params_all_g_temp$beta_0)
dim(params_all_g_temp$beta_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0 (endophyte estimates?)
all_beta0_postg_temp<-params_all_g_temp$beta_0[sample(dim(params_all_g_temp$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_postg_temp)

long_df_all_beta0g_temp <- as.data.frame.table(all_beta0_postg_temp,
                                               responseName = "estimate")
str(long_df_all_beta0g_temp)

# Convert to long data frame for endo effect
long_df_all_beta0g_temp <- as.data.frame.table(all_beta0_postg_temp,
                                               responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0g_temp$spec <- case_when(long_df_all_beta0g_temp$species == 8 ~ "AGPE",
                                          long_df_all_beta0g_temp$species == 2 ~ "ELRI",
                                          long_df_all_beta0g_temp$species == 3 ~ "ELVI",
                                          long_df_all_beta0g_temp$species == 4 ~ "FESU",
                                          long_df_all_beta0g_temp$species == 5 ~ "LOAR",
                                          long_df_all_beta0g_temp$species == 6 ~ "POAL",
                                          long_df_all_beta0g_temp$species == 7 ~ "POAU",
                                          long_df_all_beta0g_temp$species == 1 ~ "POSY")

summary_df_all_beta0g_temp <- long_df_all_beta0g_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postg_temp<-params_all_g_temp$beta_clim[sample(dim(params_all_g_temp$beta_clim)[1],size=1000),,]
dim(all_betaclim_postg_temp)

long_df_all_betaclimg_temp <- as.data.frame.table(all_betaclim_postg_temp,
                                                  responseName = "estimate")
str(long_df_all_betaclimg_temp)

# Convert to long data frame for endo effect
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

summary_df_all_betaclimg_temp <- long_df_all_betaclimg_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")

#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data_tempg <- expand.grid(
  tmean_mean = seq(min(all_grow_temp$tmean_mean, na.rm=T), 
                   max(all_grow_temp$tmean_mean, na.rm=T), 
                   length.out = 100),
  spec = unique(long_df_all_beta0g_temp$spec),
  endo = unique(long_df_all_beta0g_temp$endo)
)

plot_data_tempg <- plot_data_tempg %>%
  left_join(summary_df_all_beta0g_temp, by = c("spec", "endo")) %>% 
  left_join(summary_df_all_betaclimg_temp, by = c("spec", "endo")) %>% 
  mutate(
    # the linear formula
    predicted_growth = median + (median_slope * tmean_mean),
    
    # Calculate ribbon bounds using the linear formula
    growth_lower = lower + (lower_slope * tmean_mean),
    growth_upper = upper + (upper_slope * tmean_mean)
  )

all_grow_temp$spec <- case_when(
  all_grow_temp$spec == 8 ~ "AGPE",
  all_grow_temp$spec == 2 ~ "ELRI",
  all_grow_temp$spec == 3 ~ "ELVI",
  all_grow_temp$spec == 4 ~ "FESU",
  all_grow_temp$spec == 5 ~ "LOAR",
  all_grow_temp$spec == 6 ~ "POAL",
  all_grow_temp$spec == 7 ~ "POAu",
  all_grow_temp$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data_tempg, 
              aes(x = tmean_mean, ymin = growth_lower, ymax = growth_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data_tempg, 
            aes(x = tmean_mean, y = predicted_growth, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_grow_temp, 
             aes(x = tmean_mean, y = as.numeric(r)), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "Precipitation (Scaled)", 
       y = "Population Growth",
       title = "Population Growth Rate with 90% Credible Intervals") +
  theme_minimal()




#####VPDMAX MODELS___________________________________________________________



##MODEL FT: PROBAILITY OF FLOWERING AS RESPONSE TO VPDMAX___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(flw_count_t,endo_01,spec,log_tillers_centered,vpdmax,plot,year_t,original) %>% 
  drop_na() -> all_flow_vpdx

all_flow_dat_vpdx<-list(n_obs=nrow(all_flow_vpdx),
                        y=all_flow_vpdx$flw_count_t>0,
                        n_yrs = length(unique(all_flow_vpdx$year_t))+1,
                        n_plots = max(all_flow_vpdx$plot),
                        n_endo = 2,
                        n_spp = length(unique(all_flow_vpdx$spec)),
                        endo_01=all_flow_vpdx$endo_01,
                        size=all_flow_vpdx$log_tillers_centered,
                        year_index=all_flow_vpdx$year_t-2006,
                        climate=all_flow_vpdx$vpdmax,
                        plot=all_flow_vpdx$plot,
                        species=all_flow_vpdx$spec,
                        original=all_flow_vpdx$original)

all_flow_model_vpdx = stan_model(file="code/climatedemo.stan")
all_flow_sampling_vpdx<-sampling(all_flow_model_vpdx,
                                 data=all_flow_dat_vpdx,
                                 chains = 3,
                                 iter = 5000,
                                 warmup = 1000)

#saveRDS(all_flow_sampling_vpdx,"all_flow_sampling_vpdx.rds")
#all_flow_sampling_vpdx<-readRDS("all_flow_sampling_vpdx.rds")

summary(all_flow_sampling_vpdx)

#extracting parameters
params_all_f_vpdx<-rstan::extract(all_flow_sampling_vpdx,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_f_vpdx$beta_0)
dim(params_all_f_vpdx$beta_clim)
dim(params_all_f_vpdx$beta_size_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0
all_beta0_postf_vpdx<-params_all_f_vpdx$beta_0[sample(dim(params_all_f_vpdx$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_postf_vpdx)

long_df_all_beta0f_vpdx <- as.data.frame.table(all_beta0_postf_vpdx,
                                               responseName = "estimate")
str(long_df_all_beta0f_vpdx)

# Convert to long data frame for endo effect
long_df_all_beta0f_vpdx <- as.data.frame.table(all_beta0_postf_vpdx,
                                               responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0f_vpdx$spec <- case_when(long_df_all_beta0f_vpdx$species == 8 ~ "AGPE",
                                          long_df_all_beta0f_vpdx$species == 2 ~ "ELRI",
                                          long_df_all_beta0f_vpdx$species == 3 ~ "ELVI",
                                          long_df_all_beta0f_vpdx$species == 4 ~ "FESU",
                                          long_df_all_beta0f_vpdx$species == 5 ~ "LOAR",
                                          long_df_all_beta0f_vpdx$species == 6 ~ "POAL",
                                          long_df_all_beta0f_vpdx$species == 7 ~ "POAU",
                                          long_df_all_beta0f_vpdx$species == 1 ~ "POSY")

summary_df_all_beta0f_vpdx <- long_df_all_beta0f_vpdx %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postf_vpdx<-params_all_f_vpdx$beta_clim[sample(dim(params_all_f_vpdx$beta_clim)[1],size=1000),,]
dim(all_betaclim_postf_vpdx)

long_df_all_betaclimf_vpdx <- as.data.frame.table(all_betaclim_postf_vpdx,
                                                  responseName = "estimate")
str(long_df_all_betaclimf_vpdx)

# Convert to long data frame for endo effect
long_df_all_betaclimf_vpdx <- as.data.frame.table(all_betaclim_postf_vpdx,
                                                  responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimf_vpdx$spec <- case_when(long_df_all_betaclimf_vpdx$species == 8 ~ "AGPE",
                                             long_df_all_betaclimf_vpdx$species == 2 ~ "ELRI",
                                             long_df_all_betaclimf_vpdx$species == 3 ~ "ELVI",
                                             long_df_all_betaclimf_vpdx$species == 4 ~ "FESU",
                                             long_df_all_betaclimf_vpdx$species == 5 ~ "LOAR",
                                             long_df_all_betaclimf_vpdx$species == 6 ~ "POAL",
                                             long_df_all_betaclimf_vpdx$species == 7 ~ "POAU",
                                             long_df_all_betaclimf_vpdx$species == 1 ~ "POSY")

summary_df_all_betaclimf_vpdx <- long_df_all_betaclimf_vpdx %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")

#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data_vpdxf <- expand.grid(
  vpdmax = seq(min(all_flow_vpdx$vpdmax, na.rm=T), 
                   max(all_flow_vpdx$vpdmax, na.rm=T), 
                   length.out = 100),
  spec = unique(long_df_all_beta0f_vpdx$spec),
  endo = unique(long_df_all_beta0f_vpdx$endo)
)

plot_data_vpdxf <- plot_data_vpdxf %>%
  left_join(summary_df_all_beta0f_vpdx, by = c("spec", "endo")) %>% # Brings in 'median' (intercept)
  left_join(summary_df_all_betaclimf_vpdx, by = c("spec", "endo")) %>% # Brings in 'median_slope'
  mutate(
    # plogis(intercept + slope * x)
    prob_flowering = plogis(median + (median_slope * vpdmax)),
    # Calculate ribbon bounds
    prob_lower = plogis(lower + (lower_slope * vpdmax)),
    prob_upper = plogis(upper + (upper_slope * vpdmax))
  )

all_flow_vpdx$spec <- case_when(
  all_flow_vpdx$spec == 8 ~ "AGPE",
  all_flow_vpdx$spec == 2 ~ "ELRI",
  all_flow_vpdx$spec == 3 ~ "ELVI",
  all_flow_vpdx$spec == 4 ~ "FESU",
  all_flow_vpdx$spec == 5 ~ "LOAR",
  all_flow_vpdx$spec == 6 ~ "POAL",
  all_flow_vpdx$spec == 7 ~ "POAU",
  all_flow_vpdx$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data_vpdxf, 
              aes(x = vpdmax, ymin = prob_lower, ymax = prob_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data_vpdxf, 
            aes(x = vpdmax, y = prob_flowering, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_flow_vpdx, 
             aes(x = vpdmax, y = as.numeric(flw_count_t > 1)), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "VPDmax", 
       y = "Probability of Flowering",
       title = "Flowering Probability with 90% Credible Intervals") +
  theme_minimal()



##MODEL ST: SURVIVAL AS RESPONSE TO VPDMAX___________________

##prep data for total precipitation, dropping NAs
grasclim %>% 
  select(surv_t1,endo_01,spec,log_tillers_centered,vpdmax,plot,year_t,original) %>% 
  drop_na() -> all_surv_vpdx

all_surv_dat_vpdx<-list(n_obs=nrow(all_surv_vpdx),
                        y=all_surv_vpdx$surv_t1,
                        n_yrs = length(unique(all_surv_vpdx$year_t))+1,
                        n_plots = max(all_surv_vpdx$plot),
                        n_endo = 2,
                        n_spp = length(unique(all_surv_vpdx$spec)),
                        endo_01=all_surv_vpdx$endo_01,
                        size=all_surv_vpdx$log_tillers_centered,
                        year_index=all_surv_vpdx$year_t-2006,
                        climate=all_surv_vpdx$vpdmax,
                        plot=all_surv_vpdx$plot,
                        species=all_surv_vpdx$spec,
                        original=all_surv_vpdx$original)

all_surv_model_vpdx = stan_model(file="code/climatedemo.stan")
all_surv_sampling_vpdx<-sampling(all_surv_model_vpdx,
                                 data=all_surv_dat_vpdx,
                                 chains = 3,
                                 iter = 5000, 
                                 warmup = 1000)

#saveRDS(all_surv_sampling_vpdx,"all_surv_sampling_vpdx.rds")
#all_surv_sampling_vpdx<-readRDS("all_surv_sampling_vpdx.rds")

summary(all_surv_sampling_vpdx)

#extracting parameters
params_all_s_vpdx<-rstan::extract(all_surv_sampling_vpdx,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_s_vpdx$beta_0)
dim(params_all_s_vpdx$beta_clim)
dim(params_all_s_vpdx$beta_size_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0 (endophyte estimates?)
all_beta0_posts_vpdx<-params_all_s_vpdx$beta_0[sample(dim(params_all_s_vpdx$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_posts_vpdx)

long_df_all_beta0s_vpdx <- as.data.frame.table(all_beta0_posts_vpdx,
                                               responseName = "estimate")
str(long_df_all_beta0s_vpdx)

# Convert to long data frame for endo effect
long_df_all_beta0s_vpdx <- as.data.frame.table(all_beta0_posts_vpdx,
                                               responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0s_vpdx$spec <- case_when(long_df_all_beta0s_vpdx$species == 8 ~ "AGPE",
                                          long_df_all_beta0s_vpdx$species == 2 ~ "ELRI",
                                          long_df_all_beta0s_vpdx$species == 3 ~ "ELVI",
                                          long_df_all_beta0s_vpdx$species == 4 ~ "FESU",
                                          long_df_all_beta0s_vpdx$species == 5 ~ "LOAR",
                                          long_df_all_beta0s_vpdx$species == 6 ~ "POAL",
                                          long_df_all_beta0s_vpdx$species == 7 ~ "POAU",
                                          long_df_all_beta0s_vpdx$species == 1 ~ "POSY")

summary_df_all_beta0s_vpdx <- long_df_all_beta0s_vpdx %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_posts_vpdx<-params_all_s_vpdx$beta_clim[sample(dim(params_all_s_vpdx$beta_clim)[1],size=1000),,]
dim(all_betaclim_posts_vpdx)

long_df_all_betaclims_vpdx <- as.data.frame.table(all_betaclim_posts_vpdx,
                                                  responseName = "estimate")
str(long_df_all_betaclims_vpdx)

# Convert to long data frame for endo effect
long_df_all_betaclims_vpdx <- as.data.frame.table(all_betaclim_posts_vpdx,
                                                  responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclims_vpdx$spec <- case_when(long_df_all_betaclims_vpdx$species == 8 ~ "AGPE",
                                             long_df_all_betaclims_vpdx$species == 2 ~ "ELRI",
                                             long_df_all_betaclims_vpdx$species == 3 ~ "ELVI",
                                             long_df_all_betaclims_vpdx$species == 4 ~ "FESU",
                                             long_df_all_betaclims_vpdx$species == 5 ~ "LOAR",
                                             long_df_all_betaclims_vpdx$species == 6 ~ "POAL",
                                             long_df_all_betaclims_vpdx$species == 7 ~ "POAU",
                                             long_df_all_betaclims_vpdx$species == 1 ~ "POSY")

summary_df_all_betaclims_vpdx <- long_df_all_betaclims_vpdx %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")

#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data_vpdxs <- expand.grid(
  vpdmax = seq(min(all_surv_vpdx$vpdmax, na.rm=T), 
                   max(all_surv_vpdx$vpdmax, na.rm=T), 
                   length.out = 100),
  spec = unique(long_df_all_beta0s_vpdx$spec),
  endo = unique(long_df_all_beta0s_vpdx$endo)
)

plot_data_vpdxs <- plot_data_vpdxs %>%
  left_join(summary_df_all_beta0s_vpdx, by = c("spec", "endo")) %>% # Brings in 'median' (intercept)
  left_join(summary_df_all_betaclims_vpdx, by = c("spec", "endo")) %>% # Brings in 'median_slope'
  mutate(
    # plogis(intercept + slope * x)
    prob_survival = plogis(median + (median_slope * vpdmax)),
    # Calculate ribbon bounds
    prob_lower = plogis(lower + (lower_slope * vpdmax)),
    prob_upper = plogis(upper + (upper_slope * vpdmax))
  )

all_surv_vpdx$spec <- case_when(
  all_surv_vpdx$spec == 8 ~ "AGPE",
  all_surv_vpdx$spec == 2 ~ "ELRI",
  all_surv_vpdx$spec == 3 ~ "ELVI",
  all_surv_vpdx$spec == 4 ~ "FESU",
  all_surv_vpdx$spec == 5 ~ "LOAR",
  all_surv_vpdx$spec == 6 ~ "POAL",
  all_surv_vpdx$spec == 7 ~ "POAU",
  all_surv_vpdx$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data_vpdxs, 
              aes(x = vpdmax, ymin = prob_lower, ymax = prob_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data_vpdxs, 
            aes(x = vpdmax, y = prob_survival, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_surv_vpdx, 
             aes(x = vpdmax, y = surv_t1), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "VPDmax", 
       y = "Probability of Survival",
       title = "Survival with 90% Credible Intervals") +
  theme_minimal()



##MODEL GT: GROWTH RATE AS RESPONSE TO VPDMAX___________________

##prep data for total precipitation, dropping NAs
pop_growth_df %>% 
  select(r,endo_01,spec,vpdmax,plot,year_t) %>% 
  drop_na() -> all_grow_vpdx

all_grow_dat_vpdx <- list(n_obs = nrow(all_grow_vpdx),
                          y = all_grow_vpdx$r,
                          n_yrs = length(unique(all_grow_vpdx$year_t))+1,
                          n_plots = max(all_grow_vpdx$plot),
                          n_endo = 2,
                          n_spp = max(all_grow_vpdx$spec),
                          endo_01 = all_grow_vpdx$endo_01,
                          year_index = all_grow_vpdx$year_t-2006,
                          climate = all_grow_vpdx$vpdmax,
                          plot = all_grow_vpdx$plot,
                          species = all_grow_vpdx$spec)

all_grow_model_vpdx = stan_model(file="code/climatedemogrowth.stan")
all_grow_sampling_vpdx <- sampling(all_grow_model_vpdx,
                                   data = all_grow_dat_vpdx,
                                   chains = 3, 
                                   iter = 5000, 
                                   warmup  = 1000,
                                   include = TRUE)

#saveRDS(all_grow_sampling_vpdx,"all_grow_sampling_vpdx.rds")
#all_grow_sampling_vpdx<-readRDS("all_grow_sampling_vpdx.rds")

summary(all_grow_sampling_vpdx)

#extracting parameters
params_all_g_vpdx<-rstan::extract(all_grow_sampling_vpdx,pars=c('beta_0','beta_clim'))
dim(params_all_g_vpdx$beta_0)
dim(params_all_g_vpdx$beta_clim)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0 (endophyte estimates?)
all_beta0_postg_vpdx<-params_all_g_vpdx$beta_0[sample(dim(params_all_g_vpdx$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_postg_vpdx)

long_df_all_beta0g_vpdx <- as.data.frame.table(all_beta0_postg_vpdx,
                                               responseName = "estimate")
str(long_df_all_beta0g_vpdx)

# Convert to long data frame for endo effect
long_df_all_beta0g_vpdx <- as.data.frame.table(all_beta0_postg_vpdx,
                                               responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, estimate = estimate) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_beta0g_vpdx$spec <- case_when(long_df_all_beta0g_vpdx$species == 8 ~ "AGPE",
                                          long_df_all_beta0g_vpdx$species == 2 ~ "ELRI",
                                          long_df_all_beta0g_vpdx$species == 3 ~ "ELVI",
                                          long_df_all_beta0g_vpdx$species == 4 ~ "FESU",
                                          long_df_all_beta0g_vpdx$species == 5 ~ "LOAR",
                                          long_df_all_beta0g_vpdx$species == 6 ~ "POAL",
                                          long_df_all_beta0g_vpdx$species == 7 ~ "POAU",
                                          long_df_all_beta0g_vpdx$species == 1 ~ "POSY")

summary_df_all_beta0g_vpdx <- long_df_all_beta0g_vpdx %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#take a random subset of posterior draws for the slope / beta_clim / climate effect (AS SUGGESTED BY GEMINI)
all_betaclim_postg_vpdx<-params_all_g_vpdx$beta_clim[sample(dim(params_all_g_vpdx$beta_clim)[1],size=1000),,]
dim(all_betaclim_postg_vpdx)

long_df_all_betaclimg_vpdx <- as.data.frame.table(all_betaclim_postg_vpdx,
                                                  responseName = "estimate")
str(long_df_all_betaclimg_vpdx)

# Convert to long data frame for endo effect
long_df_all_betaclimg_vpdx <- as.data.frame.table(all_betaclim_postg_vpdx,
                                                  responseName = "slope_val") %>%
  rename(draw = iterations, species = Var2, endo = Var3) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_betaclimg_vpdx$spec <- case_when(long_df_all_betaclimg_vpdx$species == 8 ~ "AGPE",
                                             long_df_all_betaclimg_vpdx$species == 2 ~ "ELRI",
                                             long_df_all_betaclimg_vpdx$species == 3 ~ "ELVI",
                                             long_df_all_betaclimg_vpdx$species == 4 ~ "FESU",
                                             long_df_all_betaclimg_vpdx$species == 5 ~ "LOAR",
                                             long_df_all_betaclimg_vpdx$species == 6 ~ "POAL",
                                             long_df_all_betaclimg_vpdx$species == 7 ~ "POAU",
                                             long_df_all_betaclimg_vpdx$species == 1 ~ "POSY")

summary_df_all_betaclimg_vpdx <- long_df_all_betaclimg_vpdx %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")

#creating a "grid" of all combinations to be predicted (AS SUGGESTED BY GEMINI)
plot_data_vpdxg <- expand.grid(
  vpdmax = seq(min(all_grow_vpdx$vpdmax, na.rm=T), 
                   max(all_grow_vpdx$vpdmax, na.rm=T), 
                   length.out = 100),
  spec = unique(long_df_all_beta0g_vpdx$spec),
  endo = unique(long_df_all_beta0g_vpdx$endo)
)

plot_data_vpdxg <- plot_data_vpdxg %>%
  left_join(summary_df_all_beta0g_vpdx, by = c("spec", "endo")) %>% 
  left_join(summary_df_all_betaclimg_vpdx, by = c("spec", "endo")) %>% 
  mutate(
    # the linear formula
    predicted_growth = median + (median_slope * vpdmax),
    
    # Calculate ribbon bounds using the linear formula
    growth_lower = lower + (lower_slope * vpdmax),
    growth_upper = upper + (upper_slope * vpdmax)
  )

all_grow_vpdx$spec <- case_when(
  all_grow_vpdx$spec == 8 ~ "AGPE",
  all_grow_vpdx$spec == 2 ~ "ELRI",
  all_grow_vpdx$spec == 3 ~ "ELVI",
  all_grow_vpdx$spec == 4 ~ "FESU",
  all_grow_vpdx$spec == 5 ~ "LOAR",
  all_grow_vpdx$spec == 6 ~ "POAL",
  all_grow_vpdx$spec == 7 ~ "POAu",
  all_grow_vpdx$spec == 1 ~ "POSY"
)

#The actual plot
ggplot() +
  # 1. The Ribbon (Uncertainty)
  geom_ribbon(data = plot_data_vpdxg, 
              aes(x = vpdmax, ymin = growth_lower, ymax = growth_upper, fill = endo), 
              alpha = 0.2) + 
  # 2. The Prediction Lines
  geom_line(data = plot_data_vpdxg, 
            aes(x = vpdmax, y = predicted_growth, color = endo), 
            linewidth = 1) +
  # 3. The Raw Points
  geom_point(data = all_grow_vpdx, 
             aes(x = vpdmax, y = as.numeric(r)), 
             alpha = 0.2, color = "purple") + 
  facet_wrap(~spec) +
  
  # Make sure to set scale_fill_manual to match your colors!
  scale_color_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  scale_fill_manual(values = c("A" = "deeppink1", "B" = "cornflowerblue")) + 
  
  labs(x = "VPDmax", 
       y = "Population Growth",
       title = "Population Growth Rate with 90% Credible Intervals") +
  theme_minimal()
