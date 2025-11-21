library(tidyverse)
library(lme4)
library(rstan)
library(googlesheets4)
library(bayesplot)
options(mc.cores = parallel::detectCores())

#setwd("G:/.shortcut-targets-by-id/1o66WMnQligL0_-ahkQMfFOklR3X-6vOA/Akiem PhD Research/Data & Analysis")
setwd("~/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/Data & Analysis")
compcoe <- read.csv("Data/ltreb_allspp_qaqc.csv",stringsAsFactors = T)

##POA ALSODES

poaals <- compcoe %>% filter(species == "POAL")
poaals$log_tillers_centered <- log(poaals$size_t) - mean(log(poaals$size_t),na.rm=T)

##bayesian version of same model for Flowering
##prep data, dropping NAs
poaals %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> poaals_flow

poal_flow_dat<-list(n_obs=nrow(poaals_flow),
               y=poaals_flow$flw_count_t>0,
               n_yrs = length(unique(poaals_flow$year_t)),
               n_plots = max(poaals_flow$plot),
               n_endo = 2,
               endo_01=poaals_flow$endo_01,
               size=poaals_flow$log_tillers_centered,
               year_index=poaals_flow$year_t-2006,
               plot=poaals_flow$plot
               )
flow_model = stan_model(file="R Studio Codes/flowering.stan")
poal_flow_sampling<-sampling(flow_model,
                        data=poal_flow_dat,
                        chains = 3,
                        iter = 5000,
                        warmup = 1000)
mcmc_trace(poal_flow_sampling,par=c('endo_effect[5]'))
mcmc_dens(poal_flow_sampling,par=c('beta_size'))

params_poal_f<-rstan::extract(poal_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_poal_f$beta_0)
#take a random subset of posterior draws
beta0_post_poal_f<-params_poal_f$beta_0[sample(12000,size=500,replace=F),,]
dim(beta0_post_poal_f)

# Convert to long data frame
long_df_poal_f <- as.data.frame.table(beta0_post_poal_f,
                                         responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_poal_f <- long_df_poal_f %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_poal_f, aes(x = year, y = median, color = group, fill = group)) +
  scale_color_manual(values = c("goldenrod1", "cornflowerblue")) +
  scale_fill_manual(values = c("goldenrod1", "cornflowerblue")) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Flowering)",
    fill = "Group",
    color = "Group",
    title = "POAL: Variation of E+ and E- flowering probability with year"
  ) +
  theme_minimal()

## same but for survival -- note we can use the exact same model.
## even though it's called "flowering", I will give it survival data
poaals %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> poaals_surv
poal_surv_dat<-list(n_obs=nrow(poaals_surv),
               y=poaals_surv$surv_t1,
               n_yrs = length(unique(poaals_surv$year_t)),
               n_plots = max(poaals_surv$plot),
               n_endo = 2,
               endo_01=poaals_surv$endo_01,
               size=poaals_surv$log_tillers_centered,
               year_index=poaals_surv$year_t-2006,
               plot=poaals_surv$plot)
poal_surv_sampling<-sampling(flow_model,
                        data=poal_surv_dat,
                        chains = 3,
                        iter = 8000,
                        warmup = 2000)
mcmc_trace(poal_surv_sampling,pars='tau_plot[111]')
params_poal_s<-rstan::extract(poal_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_poal_s$beta_0)
#take a random subset of posterior draws
beta0_post_poal_s<-params_poal_s$beta_0[sample(18000,size=500,replace=F),,]
dim(beta0_post_poal_s)

# Convert to long data frame
long_df_poal_s <- as.data.frame.table(beta0_post_poal_s,
                               responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_poal_s <- long_df_poal_s %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_poal_s, aes(x = year, y = median, color = group, fill = group)) +
  scale_color_manual(values = c("goldenrod1", "cornflowerblue")) +
  scale_fill_manual(values = c("goldenrod1", "cornflowerblue")) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Survival)",
    fill = "Group",
    color = "Group",
    title = "POAL: Variation of E+ and E- survival with year"
  ) +
  theme_minimal()

##POA SYLVESTRIS

poasyl <- compcoe %>% filter(species == "POSY")
poasyl$log_tillers_centered <- log(poasyl$size_t) - mean(log(poasyl$size_t),na.rm=T)

##bayesian version of same model for Flowering
##prep data, dropping NAs
poasyl %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> poasyl_flow

posy_flow_dat<-list(n_obs=nrow(poasyl_flow),
                    y=poasyl_flow$flw_count_t>0,
                    n_yrs = length(unique(poasyl_flow$year_t)),
                    n_plots = max(poasyl_flow$plot),
                    n_endo = 2,
                    endo_01=poasyl_flow$endo_01,
                    size=poasyl_flow$log_tillers_centered,
                    year_index=poasyl_flow$year_t-2006,
                    plot=poasyl_flow$plot)
posy_flow_sampling<-sampling(flow_model,
                             data=posy_flow_dat,
                             chains = 3,
                             iter = 5000,
                             warmup = 1000)
mcmc_trace(posy_flow_sampling,par=c('endo_effect[5]'))
mcmc_dens(posy_flow_sampling,par=c('beta_size'))

params_posy_f<-rstan::extract(posy_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_posy_f$beta_0)
#take a random subset of posterior draws
beta0_post_posy_f<-params_posy_f$beta_0[sample(12000,size=500,replace=F),,]
dim(beta0_post_posy_f)

# Convert to long data frame
long_df_posy_f <- as.data.frame.table(beta0_post_posy_f,
                                      responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_posy_f <- long_df_posy_f %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_posy_f, aes(x = year, y = median, color = group, fill = group)) +
  scale_color_manual(values = c("goldenrod1", "cornflowerblue")) +
  scale_fill_manual(values = c("goldenrod1", "cornflowerblue")) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Flowering)",
    fill = "Group",
    color = "Group",
    title = "POSY: Variation of E+ and E- flowering probability with year"
  ) +
  theme_minimal()

## same but for survival -- note we can use the exact same model.
## even though it's called "flowering", I will give it survival data
poasyl %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> poasyl_surv
posy_surv_dat<-list(n_obs=nrow(poasyl_surv),
                    y=poasyl_surv$surv_t1,
                    n_yrs = length(unique(poasyl_surv$year_t)),
                    n_plots = max(poasyl_surv$plot),
                    n_endo = 2,
                    endo_01=poasyl_surv$endo_01,
                    size=poasyl_surv$log_tillers_centered,
                    year_index=poasyl_surv$year_t-2006,
                    plot=poasyl_surv$plot)
posy_surv_sampling<-sampling(flow_model,
                             data=posy_surv_dat,
                             chains = 3,
                             iter = 8000,
                             warmup = 2000)
mcmc_trace(posy_surv_sampling,pars='tau_plot[111]')
params_posy_s<-rstan::extract(posy_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_posy_s$beta_0)
#take a random subset of posterior draws
beta0_post_posy_s<-params_posy_s$beta_0[sample(18000,size=500,replace=F),,]
dim(beta0_post_posy_s)

# Convert to long data frame
long_df_posy_s <- as.data.frame.table(beta0_post_posy_s,
                                      responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_posy_s <- long_df_posy_s %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_posy_s, aes(x = year, y = median, color = group, fill = group)) +
  scale_color_manual(values = c("goldenrod1", "cornflowerblue")) +
  scale_fill_manual(values = c("goldenrod1", "cornflowerblue")) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Survival)",
    fill = "Group",
    color = "Group",
    title = "POSY: Variation of E+ and E- survival with year"
  ) +
  theme_minimal()

##Festuca subverticillata

fessub <- compcoe %>% filter(species == "FESU")
fessub$log_tillers_centered <- log(fessub$size_t) - mean(log(fessub$size_t),na.rm=T)

##bayesian version of same model for Flowering
##prep data, dropping NAs
fessub %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> fessub_flow

fesu_flow_dat<-list(n_obs=nrow(fessub_flow),
                    y=fessub_flow$flw_count_t>0,
                    n_yrs = length(unique(fessub_flow$year_t)),
                    n_plots = max(fessub_flow$plot),
                    n_endo = 2,
                    endo_01=fessub_flow$endo_01,
                    size=fessub_flow$log_tillers_centered,
                    year_index=fessub_flow$year_t-2006,
                    plot=fessub_flow$plot)
fesu_flow_sampling<-sampling(flow_model,
                             data=fesu_flow_dat,
                             chains = 3,
                             iter = 5000,
                             warmup = 1000)

params_fesu_f<-rstan::extract(fesu_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_fesu_f$beta_0)
#take a random subset of posterior draws
beta0_post_fesu_f<-params_fesu_f$beta_0[sample(12000,size=500,replace=F),,]
dim(beta0_post_fesu_f)

# Convert to long data frame
long_df_fesu_f <- as.data.frame.table(beta0_post_fesu_f,
                                      responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_fesu_f <- long_df_fesu_f %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_fesu_f, aes(x = year, y = median, color = group, fill = group)) +
  scale_color_manual(values = c("goldenrod1", "cornflowerblue")) +
  scale_fill_manual(values = c("goldenrod1", "cornflowerblue")) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Flowering)",
    fill = "Group",
    color = "Group",
    title = "FESU: Variation of E+ and E- flowering probability with year"
  ) +
  theme_minimal()

## same but for survival -- note we can use the exact same model.
## even though it's called "flowering", I will give it survival data
fessub %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> fessub_surv
fesu_surv_dat<-list(n_obs=nrow(fessub_surv),
                    y=fessub_surv$surv_t1,
                    n_yrs = length(unique(fessub_surv$year_t)),
                    n_plots = max(fessub_surv$plot),
                    n_endo = 2,
                    endo_01=fessub_surv$endo_01,
                    size=fessub_surv$log_tillers_centered,
                    year_index=fessub_surv$year_t-2006,
                    plot=fessub_surv$plot)
fesu_surv_sampling<-sampling(flow_model,
                             data=fesu_surv_dat,
                             chains = 3,
                             iter = 8000,
                             warmup = 2000)

params_fesu_s<-rstan::extract(fesu_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_fesu_s$beta_0)
#take a random subset of posterior draws
beta0_post_fesu_s<-params_fesu_s$beta_0[sample(18000,size=500,replace=F),,]
dim(beta0_post_fesu_s)

# Convert to long data frame
long_df_fesu_s <- as.data.frame.table(beta0_post_fesu_s,
                                      responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_fesu_s <- long_df_fesu_s %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_fesu_s, aes(x = year, y = median, color = group, fill = group)) +
  scale_color_manual(values = c("goldenrod1", "cornflowerblue")) +
  scale_fill_manual(values = c("goldenrod1", "cornflowerblue")) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Survival)",
    fill = "Group",
    color = "Group",
    title = "FESU: Variation of E+ and E- survival with year"
  ) +
  theme_minimal()
