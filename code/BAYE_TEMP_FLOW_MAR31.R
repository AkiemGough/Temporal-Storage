#Purpose: Vizualizing the difference between E+ and E- plants in how their vital rates with year
#In a bayesian framework

#Important packages
library(tidyverse)
library(lme4)
library(scales)
library(rstan)
library(bayesplot)
options(mc.cores = parallel::detectCores())

###ALL CLIMATE EXPLICIT______________________________________________________________________

#reading in the data file with demographic and climate data
grasclim <-read.csv("data/CombinedDataRefined")

##removing untrusted data
grasclim <- grasclim[!(grasclim$id=="79 1164 4"),] 
grasclim <- grasclim[!(grasclim$species=="POAU"),]

##Making integers for species
grasclim$spec <- as.integer (case_when(grasclim$species == "POSY" ~ 1,
                                       grasclim$species == "ELRI" ~ 2,
                                       grasclim$species == "ELVI" ~ 3,
                                       grasclim$species == "FESU" ~ 4,
                                       grasclim$species == "LOAR" ~ 5,
                                       grasclim$species == "POAL" ~ 6,
                                       grasclim$species == "AGPE" ~ 7))

##centering size and climate variables

grasclim$log_tillers_centered <- log(grasclim$size_t) - mean(log(grasclim$size_t),na.rm=T)

##ALL FLOWERING PRECIPITATION___________________

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
#all_flow_sampling_temp<-sampling(all_flow_model_temp,
#     data=all_flow_dat_temp,
#     chains = 3,
#    iter = 5000,
#    warmup = 1000)

#saveRDS(all_flow_sampling_temp,"all_flow_sampling_temp.rds")
all_flow_sampling_temp<-readRDS("all_flow_sampling_temp.rds")
summary(all_flow_sampling_temp)

#extracting parameters
params_all_f_temp<-rstan::extract(all_flow_sampling_temp,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_f_temp$beta_0)
dim(params_all_f_temp$beta_clim)
dim(params_all_f_temp$beta_size_clim)

##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0 (endophyte estimates?)
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

long_df_all_beta0f_temp$spec <- case_when(long_df_all_beta0f_temp$species == 7 ~ "AGPE",
                                         long_df_all_beta0f_temp$species == 2 ~ "ELRI",
                                         long_df_all_beta0f_temp$species == 3 ~ "ELVI",
                                         long_df_all_beta0f_temp$species == 4 ~ "FESU",
                                         long_df_all_beta0f_temp$species == 5 ~ "LOAR",
                                         long_df_all_beta0f_temp$species == 6 ~ "POAL",
                                         long_df_all_beta0f_temp$species == 1 ~ "POSY")


summary_df_all_beta0f_temp <- long_df_all_beta0f_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")


#AS SUGGESTED BY GEMINI
#take a random subset of posterior draws for the slope, beta_clim, climate effect
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

long_df_all_betaclimf_temp$spec <- case_when(long_df_all_betaclimf_temp$species == 7 ~ "AGPE",
                                            long_df_all_betaclimf_temp$species == 2 ~ "ELRI",
                                            long_df_all_betaclimf_temp$species == 3 ~ "ELVI",
                                            long_df_all_betaclimf_temp$species == 4 ~ "FESU",
                                            long_df_all_betaclimf_temp$species == 5 ~ "LOAR",
                                            long_df_all_betaclimf_temp$species == 6 ~ "POAL",
                                            long_df_all_betaclimf_temp$species == 1 ~ "POSY")


summary_df_all_betaclimf_temp <- long_df_all_betaclimf_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")


#FROM GEMINI
#Create a "grid" of all combinations you want to predict
plot_data_tempf <- expand.grid(
  tmean_mean = seq(min(all_flow_temp$tmean_mean, na.rm=T), 
                       max(all_flow_temp$tmean_mean, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0f_temp$spec),
  endo = unique(long_df_all_beta0f_temp$endo)
)

# 2. You need to apply your model coefficients to this grid.
# This usually looks something like: Prob = plogis(intercept + slope * temp)
# Since you have posterior draws, you'd calculate this for the medians:
# (Note: Replace 'beta_slope' with your actual slope parameter name)

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
  all_flow_temp$spec == 7 ~ "AGPE",
  all_flow_temp$spec == 2 ~ "ELRI",
  all_flow_temp$spec == 3 ~ "ELVI",
  all_flow_temp$spec == 4 ~ "FESU",
  all_flow_temp$spec == 5 ~ "LOAR",
  all_flow_temp$spec == 6 ~ "POAL",
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
  
  labs(x = "Precipitation (Scaled)", 
       y = "Probability of Flowering",
       title = "Flowering Probability with 90% Credible Intervals") +
  theme_minimal()


##ALL SURVIVAL PRECIPITATION___________________

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
#all_surv_sampling_temp<-sampling(all_surv_model_temp,
                                #data=all_surv_dat_temp,
                                #chains = 3,
                                #iter = 5000, 
                                #warmup = 1000)

#saveRDS(all_surv_sampling_temp,"all_surv_sampling_temp.rds")
all_surv_sampling_temp<-readRDS("all_surv_sampling_temp.rds")
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

long_df_all_beta0s_temp$spec <- case_when(long_df_all_beta0s_temp$species == 7 ~ "AGPE",
                                         long_df_all_beta0s_temp$species == 2 ~ "ELRI",
                                         long_df_all_beta0s_temp$species == 3 ~ "ELVI",
                                         long_df_all_beta0s_temp$species == 4 ~ "FESU",
                                         long_df_all_beta0s_temp$species == 5 ~ "LOAR",
                                         long_df_all_beta0s_temp$species == 6 ~ "POAL",
                                         long_df_all_beta0s_temp$species == 1 ~ "POSY")


summary_df_all_beta0s_temp <- long_df_all_beta0s_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")


#AS SUGGESTED BY GEMINI
#take a random subset of posterior draws for the slope, beta_clim, climate effect
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

long_df_all_betaclims_temp$spec <- case_when(long_df_all_betaclims_temp$species == 7 ~ "AGPE",
                                            long_df_all_betaclims_temp$species == 2 ~ "ELRI",
                                            long_df_all_betaclims_temp$species == 3 ~ "ELVI",
                                            long_df_all_betaclims_temp$species == 4 ~ "FESU",
                                            long_df_all_betaclims_temp$species == 5 ~ "LOAR",
                                            long_df_all_betaclims_temp$species == 6 ~ "POAL",
                                            long_df_all_betaclims_temp$species == 1 ~ "POSY")


summary_df_all_betaclims_temp <- long_df_all_betaclims_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")


#FROM GEMINI
#Create a "grid" of all combinations you want to predict
plot_data_temps <- expand.grid(
  tmean_mean = seq(min(all_surv_temp$tmean_mean, na.rm=T), 
                       max(all_surv_temp$tmean_mean, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0s_temp$spec),
  endo = unique(long_df_all_beta0s_temp$endo)
)

# 2. You need to apply your model coefficients to this grid.
# This usually looks something like: Prob = plogis(intercept + slope * temp)
# Since you have posterior draws, you'd calculate this for the medians:
# (Note: Replace 'beta_slope' with your actual slope parameter name)

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
  all_surv_temp$spec == 7 ~ "AGPE",
  all_surv_temp$spec == 2 ~ "ELRI",
  all_surv_temp$spec == 3 ~ "ELVI",
  all_surv_temp$spec == 4 ~ "FESU",
  all_surv_temp$spec == 5 ~ "LOAR",
  all_surv_temp$spec == 6 ~ "POAL",
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



#POP GROWTH RATE FROM GEMINI
library(dplyr)

pop_growth_df <- grasclim %>%
  # 1. Group by the variables that define a "population"
  group_by(species, plot, endo_01, year_t, temp_tot, tmean_mean, spec, tmean_mean, original) %>%
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


##ALL GROWTH PRECIPITATION___________________

##prep data for total precipitation, dropping NAs
pop_growth_df %>% 
  select(r,endo_01,spec,tmean_mean,plot,year_t,original) %>% 
  drop_na() -> all_grow_temp

#FROM Gemini
# 2. Build the list
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
                         species = all_grow_temp$spec,
                         original = all_grow_temp$original)

all_grow_model_temp = stan_model(file="code/climatedemogrowth.stan")
# 3. Sampling with 'init = 0' and 'verbose = TRUE'

all_grow_sampling_temp <- sampling(all_grow_model_temp,
                                  data = all_grow_dat_temp,
                                  chains = 3, 
                                  iter = 2000, # Increased slightly for better coverage
                                  warmup  = 1000,
                                  include = TRUE)


#saveRDS(all_grow_sampling_temp,"all_grow_sampling_temp.rds")
all_grow_sampling_temp<-readRDS("all_grow_sampling_temp.rds")
#summary(all_grow_sampling_temp)

#extracting parameters
params_all_g_temp<-rstan::extract(all_grow_sampling_temp,pars=c('beta_0','beta_clim'))
dim(params_all_g_temp$beta_0)
dim(params_all_g_temp$beta_clim)



##KENJI's WAY PLEASE REVISIT
##making size x variables for graphs
temp_tot_dummy_g<-seq(from=min(all_grow_temp$tmean_mean,na.rm=T),to=max(all_grow_temp$tmean_mean,na.rm=T),by=0.1)
temp_tot_dummy_scaled_g<-as.numeric (scale(temp_tot_dummy))

#creating logistic function 
logistic<-function(x){1/(1+exp(-x))}

#defining a predictor function
predict_c_g <- function(fit, climate, endo, species){
  params<-rstan::extract(fit,pars=c('beta_0','beta_clim'))
  beta_0 <- params$beta_0[, species, endo + 1]
  beta_clim <- params$beta_clim[, species, endo + 1]
  
  beta_0 + beta_clim*climate
}
##KENJI'S WAY ENDS HERE


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

long_df_all_beta0g_temp$spec <- case_when(long_df_all_beta0g_temp$species == 7 ~ "AGPE",
                                         long_df_all_beta0g_temp$species == 2 ~ "ELRI",
                                         long_df_all_beta0g_temp$species == 3 ~ "ELVI",
                                         long_df_all_beta0g_temp$species == 4 ~ "FESU",
                                         long_df_all_beta0g_temp$species == 5 ~ "LOAR",
                                         long_df_all_beta0g_temp$species == 6 ~ "POAL",
                                         long_df_all_beta0g_temp$species == 1 ~ "POSY")


summary_df_all_beta0g_temp <- long_df_all_beta0g_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#AS SUGGESTED BY GEMINI
#take a random subset of posterior draws for the slope, beta_clim, climate effect
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

long_df_all_betaclimg_temp$spec <- case_when(long_df_all_betaclimg_temp$species == 7 ~ "AGPE",
                                            long_df_all_betaclimg_temp$species == 2 ~ "ELRI",
                                            long_df_all_betaclimg_temp$species == 3 ~ "ELVI",
                                            long_df_all_betaclimg_temp$species == 4 ~ "FESU",
                                            long_df_all_betaclimg_temp$species == 5 ~ "LOAR",
                                            long_df_all_betaclimg_temp$species == 6 ~ "POAL",
                                            long_df_all_betaclimg_temp$species == 1 ~ "POSY")


summary_df_all_betaclimg_temp <- long_df_all_betaclimg_temp %>%
  group_by(spec,endo) %>% 
  summarize(
    median_slope = median(slope_val),
    lower_slope = quantile(slope_val, 0.05),
    upper_slope = quantile(slope_val, 0.95),
    .groups = "drop")


#FROM GEMINI
#Create a "grid" of all combinations you want to predict
plot_data_tempg <- expand.grid(
  tmean_mean = seq(min(all_grow_temp$tmean_mean, na.rm=T), 
                       max(all_grow_temp$tmean_mean, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0g_temp$spec),
  endo = unique(long_df_all_beta0g_temp$endo)
)

# 2. You need to apply your model coefficients to this grid.
# This usually looks something like: Prob = plogis(intercept + slope * temp)
# Since you have posterior draws, you'd calculate this for the medians:
# (Note: Replace 'beta_slope' with your actual slope parameter name)

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
  all_grow_temp$spec == 7 ~ "AGPE",
  all_grow_temp$spec == 2 ~ "ELRI",
  all_grow_temp$spec == 3 ~ "ELVI",
  all_grow_temp$spec == 4 ~ "FESU",
  all_grow_temp$spec == 5 ~ "LOAR",
  all_grow_temp$spec == 6 ~ "POAL",
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


