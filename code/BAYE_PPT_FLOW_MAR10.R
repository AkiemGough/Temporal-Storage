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

##ALL FLOWERING PRECIPITATION___________________

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
#all_flow_sampling_ppt<-sampling(all_flow_model_ppt,
#     data=all_flow_dat_ppt,
#     chains = 3,
#    iter = 5000,
#    warmup = 1000)

#saveRDS(all_flow_sampling_ppt,"all_flow_sampling_ppt.rds")
all_flow_sampling_ppt<-readRDS("all_flow_sampling_ppt.rds")
#summary(all_flow_sampling_ppt)

#extracting parameters
params_all_f_ppt<-rstan::extract(all_flow_sampling_ppt,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_f_ppt$beta_0)
dim(params_all_f_ppt$beta_clim)
dim(params_all_f_ppt$beta_size_clim)

##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0 (endophyte estimates?)
all_beta0_postf_ppt<-params_all_f_ppt$beta_0[sample(dim(params_all_f_ppt$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_postf_ppt)

long_df_all_beta0f_ppt <- as.data.frame.table(all_beta0_postf_ppt,
                                              responseName = "estimate")
str(long_df_all_beta0f_ppt)

# Convert to long data frame for endo effect
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


#AS SUGGESTED BY GEMINI
#take a random subset of posterior draws for the slope, beta_clim, climate effect
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


#FROM GEMINI
#Create a "grid" of all combinations you want to predict
plot_data <- expand.grid(
  ppt_tot_scaled = seq(min(all_flow_ppt$ppt_tot_scaled, na.rm=T), 
                       max(all_flow_ppt$ppt_tot_scaled, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0f_ppt$spec),
  endo = unique(long_df_all_beta0f_ppt$endo)
)

# 2. You need to apply your model coefficients to this grid.
# This usually looks something like: Prob = plogis(intercept + slope * ppt)
# Since you have posterior draws, you'd calculate this for the medians:
# (Note: Replace 'beta_slope' with your actual slope parameter name)

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


##ALL SURVIVAL PRECIPITATION___________________

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
#all_surv_sampling_ppt<-sampling(all_surv_model_ppt,
                                #data=all_surv_dat_ppt,
                                #chains = 3,
                                #iter = 5000, 
                                #warmup = 1000)

#saveRDS(all_surv_sampling_ppt,"all_surv_sampling_ppt.rds")
all_surv_sampling_ppt<-readRDS("all_surv_sampling_ppt.rds")
#summary(all_surv_sampling_ppt)

#extracting parameters
params_all_s_ppt<-rstan::extract(all_surv_sampling_ppt,pars=c('beta_0','beta_clim','beta_size_clim'))
dim(params_all_s_ppt$beta_0)
dim(params_all_s_ppt$beta_clim)
dim(params_all_s_ppt$beta_size_clim)

##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0 (endophyte estimates?)
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


#AS SUGGESTED BY GEMINI
#take a random subset of posterior draws for the slope, beta_clim, climate effect
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


#FROM GEMINI
#Create a "grid" of all combinations you want to predict
plot_data <- expand.grid(
  ppt_tot_scaled = seq(min(all_surv_ppt$ppt_tot_scaled, na.rm=T), 
                       max(all_surv_ppt$ppt_tot_scaled, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0s_ppt$spec),
  endo = unique(long_df_all_beta0s_ppt$endo)
)

# 2. You need to apply your model coefficients to this grid.
# This usually looks something like: Prob = plogis(intercept + slope * ppt)
# Since you have posterior draws, you'd calculate this for the medians:
# (Note: Replace 'beta_slope' with your actual slope parameter name)

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


#POP GROWTH RATE FROM GEMINI
library(dplyr)

pop_growth_df <- grasclim %>%
  # 1. Group by the variables that define a "population"
  group_by(species, plot, endo_01, year_t, ppt_tot, tmean_mean, spec, ppt_tot_scaled, original) %>%
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
  select(r,endo_01,spec,ppt_tot_scaled,plot,year_t,original) %>% 
  drop_na() -> all_grow_ppt

#FROM Gemini
# 2. Build the list
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
                         species = all_grow_ppt$spec,
                         original = all_grow_ppt$original)

all_grow_model_ppt = stan_model(file="code/climatedemogrowth.stan")
# 3. Sampling with 'init = 0' and 'verbose = TRUE'

all_grow_sampling_ppt <- sampling(all_grow_model_ppt,
                                  data = all_grow_dat_ppt,
                                  chains = 3, 
                                  iter = 2000, # Increased slightly for better coverage
                                  warmup  = 1000,
                                  include = TRUE)


#saveRDS(all_grow_sampling_ppt,"all_grow_sampling_ppt.rds")
all_grow_sampling_ppt<-readRDS("all_grow_sampling_ppt.rds")
#summary(all_grow_sampling_ppt)

#extracting parameters
params_all_g_ppt<-rstan::extract(all_grow_sampling_ppt,pars=c('beta_0','beta_clim'))
dim(params_all_g_ppt$beta_0)
dim(params_all_g_ppt$beta_clim)



##KENJI's WAY PLEASE REVISIT
##making size x variables for graphs
ppt_tot_dummy_g<-seq(from=min(all_grow_ppt$ppt_tot_scaled,na.rm=T),to=max(all_grow_ppt$ppt_tot_scaled,na.rm=T),by=0.1)
ppt_tot_dummy_scaled_g<-as.numeric (scale(ppt_tot_dummy))

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
all_beta0_postg_ppt<-params_all_g_ppt$beta_0[sample(dim(params_all_g_ppt$beta_0)[1],size=1000,replace=F),,]
dim(all_beta0_postg_ppt)

long_df_all_beta0g_ppt <- as.data.frame.table(all_beta0_postg_ppt,
                                              responseName = "estimate")
str(long_df_all_beta0g_ppt)

# Convert to long data frame for endo effect
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

#AS SUGGESTED BY GEMINI
#take a random subset of posterior draws for the slope, beta_clim, climate effect
all_betaclim_postg_ppt<-params_all_g_ppt$beta_clim[sample(dim(params_all_g_ppt$beta_clim)[1],size=1000),,]
dim(all_betaclim_postg_ppt)

long_df_all_betaclimg_ppt <- as.data.frame.table(all_betaclim_postg_ppt,
                                                 responseName = "estimate")
str(long_df_all_betaclimg_ppt)

# Convert to long data frame for endo effect
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


#FROM GEMINI
#Create a "grid" of all combinations you want to predict
plot_data_pptg <- expand.grid(
  ppt_tot_scaled = seq(min(all_grow_ppt$ppt_tot_scaled, na.rm=T), 
                       max(all_grow_ppt$ppt_tot_scaled, na.rm=T), 
                       length.out = 100),
  spec = unique(long_df_all_beta0g_ppt$spec),
  endo = unique(long_df_all_beta0g_ppt$endo)
)

# 2. You need to apply your model coefficients to this grid.
# This usually looks something like: Prob = plogis(intercept + slope * ppt)
# Since you have posterior draws, you'd calculate this for the medians:
# (Note: Replace 'beta_slope' with your actual slope parameter name)

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







library(tidybayes)


hist(all_grow_ppt$lambda_size$lambda_size)
plot(density(all_grow_ppt$lambda_size))
#seems not normally distributed, most values are around 0

any(all_grow_ppt$lambda_size<0)
#The data is gamma distributed? given that it is continuous, non-negative 
#with small values clustered

