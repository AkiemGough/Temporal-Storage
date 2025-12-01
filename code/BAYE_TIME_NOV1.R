#Purpose: Vizualizing the difference between E+ and E- plants in how their vital rates with year
#In a bayesian framework

#Important packages
library(tidyverse)
library(lme4)
library(rstan)
library(googlesheets4)
library(bayesplot)
options(mc.cores = parallel::detectCores())

#setwd("G:/.shortcut-targets-by-id/1o66WMnQligL0_-ahkQMfFOklR3X-6vOA/Akiem PhD Research/Data & Analysis")
setwd("/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage")
gras <- read.csv("/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/data/ltreb_allspp_2007_2025.csv")


###POA ALSODES_______________________________________________________________________

##Species filter
poaals <- gras %>% filter(species == "POAL")

poaals$log_tillers_centered <- log(poaals$size_t) - mean(log(poaals$size_t),na.rm=T)

##POAL FLOWERING___________________

##prep data, dropping NAs
poaals %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> poaals_flow

poal_flow_dat <- list(n_obs=nrow(poaals_flow),
                    y=poaals_flow$flw_count_t>0,
                    n_yrs = length(unique(poaals_flow$year_t)),
                    n_plots = max(poaals_flow$plot),
                    n_endo = 2,
                    endo_01=poaals_flow$endo_01,
                    size=poaals_flow$log_tillers_centered,
                    year_index=poaals_flow$year_t-2006,
                    plot=poaals_flow$plot)

poal_flow_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
poal_flow_sampling<-sampling(poal_flow_model,
                             data=poal_flow_dat,
                             chains = 3,
                             iter = 5000,
                             warmup = 1000)
#mcmc_trace(poal_flow_sampling,par=c('endo_effect[5]'))
#mcmc_dens(poal_flow_sampling,par=c('beta_size'))

#Something is being done
params_poal_f<-rstan::extract(poal_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_poal_f$beta_0)

#take a random subset of posterior draws
poal_endoeffect_postf<-params_poal_f$endo_effect[sample(12000,size=500,replace=F),]
dim(poal_endoeffect_postf)

# Convert to long data frame
long_df_poal_f <- as.data.frame.table(poal_endoeffect_postf,
                                 responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006)

summary_df_poal_f <- long_df_poal_f %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop")

#plot
ggplot(summary_df_poal_f, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
       title = "POAL: Difference of E+ and E- flowering with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()


#ggplot(summary_df_poal_f, aes(x = year, y = probgzero)) +
  #geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") + theme_minimal()

##POAL SURVIVAL___________________

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

poal_surv_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
poal_surv_sampling<-sampling(poal_surv_model,
                             data=poal_surv_dat,
                             chains = 3,
                             iter = 8000,
                             warmup = 2000)
#mcmc_trace(surv_sampling,pars='tau_plot[111]')

#Something is being done
params_poal_s<-rstan::extract(poal_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_poal_s$beta_0)

#take a random subset of posterior draws
poal_endoeffect_posts<-params_poal_s$endo_effect[sample(12000,size=500,replace=F),]
dim(poal_endoeffect_posts)

# Convert to long data frame
long_df_poal_s <- as.data.frame.table(poal_endoeffect_posts,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006)

summary_df_poal_s <- long_df_poal_s %>%
  group_by(year) %>%
  summarize(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95),
            probgzero = mean(value>0),
            .groups = "drop")

# plot
ggplot(summary_df_poal_s, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
  title = "POAL: Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_poal_s, aes(x = year, y = probgzero)) +
  #geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") +
 # theme_minimal()



###POA SYLVESTRIS_______________________________________________________________________

##Species filter
poasyl <- gras %>% filter(species == "POSY")

poasyl$log_tillers_centered <- log(poasyl$size_t) - mean(log(poasyl$size_t),na.rm=T)

##POSY FLOWERING___________________

##prep data, dropping NAs
poasyl %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> poasyl_flow

posy_flow_dat <- list(n_obs=nrow(poasyl_flow),
                      y=poasyl_flow$flw_count_t>0,
                      n_yrs = length(unique(poasyl_flow$year_t)),
                      n_plots = max(poasyl_flow$plot),
                      n_endo = 2,
                      endo_01=poasyl_flow$endo_01,
                      size=poasyl_flow$log_tillers_centered,
                      year_index=poasyl_flow$year_t-2006,
                      plot=poasyl_flow$plot)

posy_flow_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
posy_flow_sampling<-sampling(posy_flow_model,
                             data=posy_flow_dat,
                             chains = 3,
                             iter = 5000,
                             warmup = 1000)
#mcmc_trace(posy_flow_sampling,par=c('endo_effect[5]'))
#mcmc_dens(posy_flow_sampling,par=c('beta_size'))

#Something is being done
params_posy_f<-rstan::extract(posy_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_posy_f$beta_0)

#take a random subset of posterior draws
posy_endoeffect_postf<-params_posy_f$endo_effect[sample(12000,size=500,replace=F),]
dim(posy_endoeffect_postf)

# Convert to long data frame
long_df_posy_f <- as.data.frame.table(posy_endoeffect_postf,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(
    draw = as.integer(draw),
    year = as.integer(year)+2006)

summary_df_posy_f <- long_df_posy_f %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop")

#plot
ggplot(summary_df_posy_f, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
       title = "POSY: Difference of E+ and E- flowering prob with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_posy_f, aes(x = year, y = probgzero)) +
# geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") + theme_minimal()

##POSY SURVIVAL___________________

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

posy_surv_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
posy_surv_sampling<-sampling(posy_surv_model,
                             data=posy_surv_dat,
                             chains = 3,
                             iter = 8000,
                             warmup = 2000)
#mcmc_trace(surv_sampling,pars='tau_plot[111]')

#Something is being done
params_posy_s<-rstan::extract(posy_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_posy_s$beta_0)

#take a random subset of posterior draws
posy_endoeffect_posts<-params_posy_s$endo_effect[sample(12000,size=500,replace=F),]
dim(posy_endoeffect_posts)

# Convert to long data frame
long_df_posy_s <- as.data.frame.table(posy_endoeffect_posts,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006)

summary_df_posy_s <- long_df_posy_s %>%
  group_by(year) %>%
  summarize(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95),
            probgzero = mean(value>0),
            .groups = "drop")

# plot
ggplot(summary_df_posy_s, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
       title = "POSY: Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_posy_s, aes(x = year, y = probgzero)) +
#geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") +
# theme_minimal()



###AGROSTIS PERENNANS_______________________________________________________________________

##Species filter
agrper <- gras %>% filter(species == "AGPE")

agrper$log_tillers_centered <- log(agrper$size_t) - mean(log(agrper$size_t),na.rm=T)

##AGPE FLOWERING___________________

##prep data, dropping NAs
agrper %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> agrper_flow

agpe_flow_dat <- list(n_obs=nrow(agrper_flow),
                      y=agrper_flow$flw_count_t>0,
                      n_yrs = length(unique(agrper_flow$year_t)),
                      n_plots = max(agrper_flow$plot),
                      n_endo = 2,
                      endo_01=agrper_flow$endo_01,
                      size=agrper_flow$log_tillers_centered,
                      year_index=agrper_flow$year_t-2006,
                      plot=agrper_flow$plot)

agpe_flow_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
agpe_flow_sampling<-sampling(agpe_flow_model,
                             data=agpe_flow_dat,
                             chains = 1, #following prompt to check when only 1 chain
                             iter = 5000,
                             warmup = 1000)

#A BUNCH OF ERRORS SHOWED UP
#mcmc_trace(agpe_flow_sampling,par=c('endo_effect[5]'))
#mcmc_dens(agpe_flow_sampling,par=c('beta_size'))

#pulling the posterior draws 
params_agpe_f<-rstan::extract(agpe_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_agpe_f$beta_0)

#take a random subset of posterior draws
agpe_endoeffect_postf<-params_agpe_f$endo_effect[sample(12000,size=500,replace=F),]
dim(agpe_endoeffect_postf)

# Convert to long data frame
long_df_agpe_f <- as.data.frame.table(agpe_endoeffect_postf,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(
    draw = as.integer(draw),
    year = as.integer(year)+2006)

summary_df_agpe_f <- long_df_agpe_f %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop")

#plot
ggplot(summary_df_agpe_f, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year", y = "Endophyte effect",
    title = "AGPE: Difference of E+ and E- flowering prob with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_agpe_f, aes(x = year, y = probgzero)) +
# geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") + theme_minimal()

##AGPE SURVIVAL___________________

agrper %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> agrper_surv

agpe_surv_dat<-list(n_obs=nrow(agrper_surv),
                    y=agrper_surv$surv_t1,
                    n_yrs = length(unique(agrper_surv$year_t)),
                    n_plots = max(agrper_surv$plot),
                    n_endo = 2,
                    endo_01=agrper_surv$endo_01,
                    size=agrper_surv$log_tillers_centered,
                    year_index=agrper_surv$year_t-2006,
                    plot=agrper_surv$plot)

agpe_surv_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
agpe_surv_sampling<-sampling(agpe_surv_model,
                             data=agpe_surv_dat,
                             chains = 1,
                             iter = 8000,
                             warmup = 2000)

#A BUNCH OF ERRORS SHOWED UP
#mcmc_trace(surv_sampling,pars='tau_plot[111]')

#Something is being done
params_agpe_s<-rstan::extract(agpe_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_agpe_s$beta_0)

#take a random subset of posterior draws
agpe_endoeffect_posts<-params_agpe_s$endo_effect[sample(12000,size=500,replace=F),]
dim(agpe_endoeffect_posts)

# Convert to long data frame
long_df_agpe_s <- as.data.frame.table(agpe_endoeffect_posts,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006)

summary_df_agpe_s <- long_df_agpe_s %>%
  group_by(year) %>%
  summarize(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95),
            probgzero = mean(value>0),
            .groups = "drop")

# plot
ggplot(summary_df_agpe_s, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
       title = "AGPE: Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_agpe_s, aes(x = year, y = probgzero)) +
#geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") +
# theme_minimal()



###ELYMUS VIRGINICUS_______________________________________________________________________

##Species filter
elyvir <- gras %>% filter(species == "ELVI")

elyvir$log_tillers_centered <- log(elyvir$size_t) - mean(log(elyvir$size_t),na.rm=T)

##ELVI FLOWERING___________________

##prep data, dropping NAs
elyvir %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> elyvir_flow

elvi_flow_dat <- list(n_obs=nrow(elyvir_flow),
                      y=elyvir_flow$flw_count_t>0,
                      n_yrs = length(unique(elyvir_flow$year_t)),
                      n_plots = max(elyvir_flow$plot),
                      n_endo = 2,
                      endo_01=elyvir_flow$endo_01,
                      size=elyvir_flow$log_tillers_centered,
                      year_index=elyvir_flow$year_t-2006,
                      plot=elyvir_flow$plot)

elvi_flow_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
elvi_flow_sampling<-sampling(elvi_flow_model,
                             data=elvi_flow_dat,
                             chains = 3,
                             iter = 5000,
                             warmup = 1000)
#mcmc_trace(elvi_flow_sampling,par=c('endo_effect[5]'))
#mcmc_dens(elvi_flow_sampling,par=c('beta_size'))

#Something is being done
params_elvi_f<-rstan::extract(elvi_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_elvi_f$beta_0)

#take a random subset of posterior draws
elvi_endoeffect_postf<-params_elvi_f$endo_effect[sample(12000,size=500,replace=F),]
dim(elvi_endoeffect_postf)

# Convert to long data frame
long_df_elvi_f <- as.data.frame.table(elvi_endoeffect_postf,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(
    draw = as.integer(draw),
    year = as.integer(year)+2006)

summary_df_elvi_f <- long_df_elvi_f %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop")

#plot
ggplot(summary_df_elvi_f, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year", y = "Endophyte effect",
    title = "ELVI: Difference of E+ and E- flowering prob with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_elvi_f, aes(x = year, y = probgzero)) +
# geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") + theme_minimal()

##ELVI SURVIVAL___________________

elyvir %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> elyvir_surv

elvi_surv_dat<-list(n_obs=nrow(elyvir_surv),
                    y=elyvir_surv$surv_t1,
                    n_yrs = length(unique(elyvir_surv$year_t)),
                    n_plots = max(elyvir_surv$plot),
                    n_endo = 2,
                    endo_01=elyvir_surv$endo_01,
                    size=elyvir_surv$log_tillers_centered,
                    year_index=elyvir_surv$year_t-2006,
                    plot=elyvir_surv$plot)

elvi_surv_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
elvi_surv_sampling<-sampling(elvi_surv_model,
                             data=elvi_surv_dat,
                             chains = 3,
                             iter = 8000,
                             warmup = 2000)
#mcmc_trace(surv_sampling,pars='tau_plot[111]')

#Something is being done
params_elvi_s<-rstan::extract(elvi_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_elvi_s$beta_0)

#take a random subset of posterior draws
elvi_endoeffect_posts<-params_elvi_s$endo_effect[sample(12000,size=500,replace=F),]
dim(elvi_endoeffect_posts)

# Convert to long data frame
long_df_elvi_s <- as.data.frame.table(elvi_endoeffect_posts,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006)

summary_df_elvi_s <- long_df_elvi_s %>%
  group_by(year) %>%
  summarize(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95),
            probgzero = mean(value>0),
            .groups = "drop")

# plot
ggplot(summary_df_elvi_s, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
       title = "ELVI: Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_elvi_s, aes(x = year, y = probgzero)) +
#geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") +
# theme_minimal()




###ELYMUS VILLOSUS_______________________________________________________________________

##Species filter
elyvil <- gras %>% filter(species == "ELRI")

elyvil$log_tillers_centered <- log(elyvil$size_t) - mean(log(elyvil$size_t),na.rm=T)

##ELRI FLOWERING___________________

##prep data, dropping NAs
elyvil %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> elyvil_flow

elri_flow_dat <- list(n_obs=nrow(elyvil_flow),
                      y=elyvil_flow$flw_count_t>0,
                      n_yrs = length(unique(elyvil_flow$year_t)),
                      n_plots = max(elyvil_flow$plot),
                      n_endo = 2,
                      endo_01=elyvil_flow$endo_01,
                      size=elyvil_flow$log_tillers_centered,
                      year_index=elyvil_flow$year_t-2006,
                      plot=elyvil_flow$plot)

elri_flow_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
elri_flow_sampling<-sampling(elri_flow_model,
                             data=elri_flow_dat,
                             chains = 3,
                             iter = 5000,
                             warmup = 1000)
#mcmc_trace(elri_flow_sampling,par=c('endo_effect[5]'))
#mcmc_dens(elri_flow_sampling,par=c('beta_size'))

#Something is being done
params_elri_f<-rstan::extract(elri_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_elri_f$beta_0)

#take a random subset of posterior draws
elri_endoeffect_postf<-params_elri_f$endo_effect[sample(12000,size=500,replace=F),]
dim(elri_endoeffect_postf)

# Convert to long data frame
long_df_elri_f <- as.data.frame.table(elri_endoeffect_postf,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(
    draw = as.integer(draw),
    year = as.integer(year)+2006)

summary_df_elri_f <- long_df_elri_f %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop")

#plot
ggplot(summary_df_elri_f, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year", y = "Endophyte effect",
    title = "ELRI: Difference of E+ and E- flowering prob with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_elri_f, aes(x = year, y = probgzero)) +
# geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") + theme_minimal()

##ELRI SURVIVAL___________________

elyvil %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> elyvil_surv

elri_surv_dat<-list(n_obs=nrow(elyvil_surv),
                    y=elyvil_surv$surv_t1,
                    n_yrs = length(unique(elyvil_surv$year_t)),
                    n_plots = max(elyvil_surv$plot),
                    n_endo = 2,
                    endo_01=elyvil_surv$endo_01,
                    size=elyvil_surv$log_tillers_centered,
                    year_index=elyvil_surv$year_t-2006,
                    plot=elyvil_surv$plot)

elri_surv_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
elri_surv_sampling<-sampling(elri_surv_model,
                             data=elri_surv_dat,
                             chains = 3,
                             iter = 8000,
                             warmup = 2000)
#mcmc_trace(surv_sampling,pars='tau_plot[111]')

#Something is being done
params_elri_s<-rstan::extract(elri_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_elri_s$beta_0)

#take a random subset of posterior draws
elri_endoeffect_posts<-params_elri_s$endo_effect[sample(12000,size=500,replace=F),]
dim(elri_endoeffect_posts)

# Convert to long data frame
long_df_elri_s <- as.data.frame.table(elri_endoeffect_posts,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006)

summary_df_elri_s <- long_df_elri_s %>%
  group_by(year) %>%
  summarize(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95),
            probgzero = mean(value>0),
            .groups = "drop")

# plot
ggplot(summary_df_elri_s, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
       title = "ELRI: Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_elri_s, aes(x = year, y = probgzero)) +
#geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") +
# theme_minimal()



###FESTUCA SUBVERTICILLATA_______________________________________________________________________

##Species filter
fessub <- gras %>% filter(species == "FESU")

fessub$log_tillers_centered <- log(fessub$size_t) - mean(log(fessub$size_t),na.rm=T)

##FESU FLOWERING___________________

##prep data, dropping NAs
fessub %>% 
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> fessub_flow

fesu_flow_dat <- list(n_obs=nrow(fessub_flow),
                      y=fessub_flow$flw_count_t>0,
                      n_yrs = length(unique(fessub_flow$year_t)),
                      n_plots = max(fessub_flow$plot),
                      n_endo = 2,
                      endo_01=fessub_flow$endo_01,
                      size=fessub_flow$log_tillers_centered,
                      year_index=fessub_flow$year_t-2006,
                      plot=fessub_flow$plot)

fesu_flow_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
fesu_flow_sampling<-sampling(fesu_flow_model,
                             data=fesu_flow_dat,
                             chains = 3,
                             iter = 5000,
                             warmup = 1000)

#WARNING: 3: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#Running the chains for more iterations may help. See
#https://mc-stan.org/misc/warnings.html#bulk-ess 

#mcmc_trace(fesu_flow_sampling,par=c('endo_effect[5]'))
#mcmc_dens(fesu_flow_sampling,par=c('beta_size'))

#Something is being done
params_fesu_f<-rstan::extract(fesu_flow_sampling,pars=c('beta_0','endo_effect'))
dim(params_fesu_f$beta_0)

#take a random subset of posterior draws
fesu_endoeffect_postf<-params_fesu_f$endo_effect[sample(12000,size=500,replace=F),]
dim(fesu_endoeffect_postf)

# Convert to long data frame
long_df_fesu_f <- as.data.frame.table(fesu_endoeffect_postf,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(
    draw = as.integer(draw),
    year = as.integer(year)+2006)

summary_df_fesu_f <- long_df_fesu_f %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop")

#plot
ggplot(summary_df_fesu_f, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year", y = "Endophyte effect",
  title = "FESU: Difference of E+ and E- flowering prob with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_fesu_f, aes(x = year, y = probgzero)) +
# geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") + theme_minimal()

##FESU SURVIVAL___________________

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

fesu_surv_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
fesu_surv_sampling<-sampling(fesu_surv_model,
                             data=fesu_surv_dat,
                             chains = 3,
                             iter = 8000,
                             warmup = 2000)

#SAME WARNING AS WITH FLOWERING
#mcmc_trace(surv_sampling,pars='tau_plot[111]')

#Something is being done
params_fesu_s<-rstan::extract(fesu_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_fesu_s$beta_0)

#take a random subset of posterior draws
fesu_endoeffect_posts<-params_fesu_s$endo_effect[sample(12000,size=500,replace=F),]
dim(fesu_endoeffect_posts)

# Convert to long data frame
long_df_fesu_s <- as.data.frame.table(fesu_endoeffect_posts,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006)

summary_df_fesu_s <- long_df_fesu_s %>%
  group_by(year) %>%
  summarize(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95),
            probgzero = mean(value>0),
            .groups = "drop")

# plot
ggplot(summary_df_fesu_s, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
       title = "FESU: Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_fesu_s, aes(x = year, y = probgzero)) +
#geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") +
# theme_minimal()


##ONE FOR ALL

##Making integers for species
gras$spec <- as.integer (case_when(gras$species == "AGPE" ~ 8,
                                  gras$species == "ELRI" ~ 2,
                                  gras$species == "ELVI" ~ 3,
                                  gras$species == "FESU" ~ 4,
                                  gras$species == "LOAR" ~ 5,
                                  gras$species == "POAL" ~ 6,
                                  gras$species == "POAU" ~ 7,
                                  gras$species == "POSY" ~ 1))

##centering size
gras$log_tillers_centered <- log(gras$size_t) - mean(log(gras$size_t),na.rm=T)

##ALL FLOWERING___________________

##prep data, dropping NAs
gras %>% filter (spec!=8) %>%
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot,spec,original) %>% 
  drop_na() -> all_flow

all_flow_dat <- list(n_obs=nrow(all_flow),
                      y=all_flow$flw_count_t>0,
                      n_yrs = length(unique(all_flow$year_t)),
                      n_plots = max(all_flow$plot),
                      n_endo = 2,
                      n_spp = length(unique(all_flow$spec)),
                      endo_01=all_flow$endo_01,
                      size=all_flow$log_tillers_centered,
                      year_index=all_flow$year_t-2006,
                      plot=all_flow$plot,
                      species=all_flow$spec,
                      original=all_flow$original)

all_flow_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/flowering_mvn.stan")
all_flow_sampling<-sampling(all_flow_model,
                             data=all_flow_dat,
                             chains = 1,
                             iter = 5000,
                             warmup = 1000)
#mcmc_trace(all_flow_sampling,par=c('endo_effect[5]'))
#mcmc_dens(all_flow_sampling,par=c('beta_size'))

#Something is being done
params_all_f<-rstan::extract(all_flow_sampling,pars=c('beta_0','endo_effect')) #does species go in this line too?
dim(params_all_f$beta_0)

#take a random subset of posterior draws
all_endoeffect_postf<-params_all_f$endo_effect[sample(12000,size=500,replace=F),]
dim(all_endoeffect_postf)

# Convert to long data frame
long_df_all_f <- as.data.frame.table(all_endoeffect_postf,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006) #does species go here

summary_df_all_f <- long_df_all_f %>%
  group_by(year) %>% #should i group by species as well
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop")

#plot
ggplot(summary_df_all_f, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- flowering with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()


#ggplot(summary_df_all_f, aes(x = year, y = probgzero)) +
#geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") + theme_minimal()

##ALL SURVIVAL___________________

gras %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> all_surv

all_surv_dat<-list(n_obs=nrow(all_surv),
                    y=all_surv$surv_t1,
                    n_yrs = length(unique(all_surv$year_t)),
                    n_plots = max(all_surv$plot),
                    n_endo = 2,
                    n_spp = length(unique(all_flow$spec)), #made this up, don't know if it works
                    endo_01=all_surv$endo_01,
                    size=all_surv$log_tillers_centered,
                    year_index=all_surv$year_t-2006,
                    plot=all_surv$plot,
                    species=all_flow$spec)

all_surv_model = stan_model(file="/Users/akiemgough/Library/CloudStorage/GoogleDrive-ag285@rice.edu/My Drive/Akiem PhD Research/GitHub/Temporal-Storage/code/earlier_flowering.stan")
all_surv_sampling<-sampling(all_surv_model,
                             data=all_surv_dat,
                             chains = 3,
                             iter = 8000,
                             warmup = 2000)
#mcmc_trace(surv_sampling,pars='tau_plot[111]')

#Something is being done
params_all_s<-rstan::extract(all_surv_sampling,pars=c('beta_0','endo_effect'))
dim(params_all_s$beta_0)

#take a random subset of posterior draws
all_endoeffect_posts<-params_all_s$endo_effect[sample(12000,size=500,replace=F),]
dim(all_endoeffect_posts)

# Convert to long data frame
long_df_all_s <- as.data.frame.table(all_endoeffect_posts,
                                      responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006)

summary_df_all_s <- long_df_all_s %>%
  group_by(year) %>%
  summarize(median = median(value),
            lower = quantile(value, 0.05),
            upper = quantile(value, 0.95),
            probgzero = mean(value>0),
            .groups = "drop")

# plot
ggplot(summary_df_all_s, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()

#ggplot(summary_df_all_s, aes(x = year, y = probgzero)) +
#geom_line(linewidth = 1.2) + labs(x = "Year", y = "Pr(E+ > E-)") +
# theme_minimal()

