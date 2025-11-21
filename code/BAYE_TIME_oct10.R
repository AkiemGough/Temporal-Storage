library(tidyverse)
library(lme4)
library(rstan)
library(googlesheets4)
library(bayesplot)
options(mc.cores = parallel::detectCores())

#setwd("G:/.shortcut-targets-by-id/1o66WMnQligL0_-ahkQMfFOklR3X-6vOA/Akiem PhD Research/Data & Analysis")
gras <- read.csv("ltreb_allspp_2007_2025.csv",stringsAsFactors = T)

##Making integers for species
gras$spec <- as.factor (case_when(gras$species == "AGPE" ~ 1,
                                  gras$species == "ELRI" ~ 2,
                                  gras$species == "ELVI" ~ 3,
                                  gras$species == "FESU" ~ 4,
                                  gras$species == "LOAR" ~ 5,
                                  gras$species == "POAL" ~ 6,
                                  gras$species == "POAU" ~ 7,
                                  gras$species == "POSY" ~ 8))

gras$log_tillers_centered <- log(gras$size_t) - mean(log(gras$size_t),na.rm=T)

##bayesian version of same model for Flowering
##prep data, dropping NAs
gras %>% 
  select(flw_count_t,endo_01,spec,log_tillers_centered,year_t,plot,original) %>% 
  drop_na() -> gras_mod

gras_mod_dat<-list(n_obs=nrow(gras_mod),
               y=gras_mod$flw_count_t>0,
               n_yrs = length(unique(gras_mod$year_t)),
               n_plots = max(gras_mod$plot),
               n_endo = 2,
               endo_01=gras_mod$endo_01,
               size=gras_mod$log_tillers_centered,
               year_index=gras_mod$year_t-2006,
               plot=gras_mod$plot,
               spec=gras_mod$spec
               )
flow_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/model2.stan")
gras_mod_sampling<-sampling(flow_model,
                        data=gras_mod_dat,
                        chains = 3,
                        iter = 5000,
                        warmup = 1000)
mcmc_trace(gras_mod_sampling,par=c('endo_effect[5]'))
mcmc_dens(gras_mod_sampling,par=c('beta_size'))

params_gras_f<-rstan::extract(gras_mod_sampling,pars=c('beta_0','endo_effect'))
dim(params_gras_f$beta_0)
#take a random subset of posterior draws
beta0_post_gras_f<-params_gras_f$beta_0[sample(12000,size=500,replace=F),,]
dim(beta0_post_gras_f)

# Convert to long data frame
long_df_gras_f <- as.data.frame.table(beta0_post_gras_f,
                                         responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_gras_f <- long_df_gras_f %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_gras_f, aes(x = year, y = median, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Flowering)",
    fill = "Group",
    color = "Group",
    title = "gras: Variation of E+ and E- flowering probability with year"
  ) +
  theme_minimal()


## same but for survival -- note we can use the exact same model.
## even though it's called "flowering", I will give it survival data
gras %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> gras_surv
gras_surv_dat<-list(n_obs=nrow(gras_surv),
               y=gras_surv$surv_t1,
               n_yrs = length(unique(gras_surv$year_t)),
               n_plots = max(gras_surv$plot),
               n_endo = 2,
               endo_01=gras_surv$endo_01,
               size=gras_surv$log_tillers_centered,
               year_index=gras_surv$year_t-2006,
               plot=gras_surv$plot)
gras_surv_sampling<-sampling(flow_model,
                        data=gras_surv_dat,
                        chains = 3,
                        iter = 8000,
                        warmup = 2000)
mcmc_trace(gras_surv_sampling,pars='tau_plot[111]')
params_gras_s<-rstan::extract(gras_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_gras_s$beta_0)
#take a random subset of posterior draws
beta0_post_gras_s<-params_gras_s$beta_0[sample(18000,size=500,replace=F),,]
dim(beta0_post_gras_s)

# Convert to long data frame
long_df_gras_s <- as.data.frame.table(beta0_post_gras_s,
                               responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_gras_s <- long_df_gras_s %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_gras_s, aes(x = year, y = median, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Survival)",
    fill = "Group",
    color = "Group",
    title = "gras: Variation of E+ and E- survival with year"
  ) +
  theme_minimal()
