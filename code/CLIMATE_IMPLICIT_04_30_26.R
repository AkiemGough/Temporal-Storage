#--------------------#
#title: "Climate implicit models of effects on endophyte mediated plant demography"
#author: "Akiem_Gough"
#date: "2026-04-30"
#--------------------#

#Important packages
library(tidyverse)
library(lme4)
library(rstan)
library(googlesheets4)
library(bayesplot)
options(mc.cores = parallel::detectCores())
library(dplyr)

#reading in demographic data
gras <- read.csv("data/ltreb_allspp_2007_2025.csv")


##cleaning up data frame
#removing rows with untrusted data

gras %>% filter(size_t>0) -> gras

#finding what years of LOAR data are missing, not sure how to exclude them from figures
gras %>% filter(species == "LOAR") %>% distinct(year_t)

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

#renaming origin_01 
gras$original <- gras$origin_01

##MODEL FI: PROBAILITY OF FLOWERING AS RESPONSE, CLIMATE IMPLICIT___________________

##prepping data, dropping NAs
gras %>%
  select(flw_count_t,endo_01,log_tillers_centered,year_t,plot,spec,original) %>% 
  drop_na() -> all_flow

all_flow_dat <- list(n_obs=nrow(all_flow),
                     y= as.integer(all_flow$flw_count_t > 0),
                     n_yrs = length(unique(all_flow$year_t))+1,
                     n_plots = max(all_flow$plot),
                     n_endo = 2,
                     n_spp = length(unique(all_flow$spec)),
                     endo_01=all_flow$endo_01,
                     size=all_flow$log_tillers_centered,
                     year_index=all_flow$year_t-2006,
                     plot=all_flow$plot,
                     species=all_flow$spec,
                     original=all_flow$original)

all_flow_model = stan_model(file="code/implicit_binomial_mvn.stan")
all_flow_sampling<-sampling(all_flow_model,
                            data=all_flow_dat,
                            chains = 3,thin=5,
                            iter = 5000,
                            warmup = 1000,
                            pars=c("beta_0","beta_size","beta_size_endo",
                                   "meanflow","beta_orig","sigma_year",
                                   "sigma_plot","Omega","endo_effect","y_rep"),
                            save_warmup=F)

saveRDS(all_flow_sampling,"all_flow_sampling.rds")
all_flow_sampling<-readRDS("all_flow_sampling.rds")

mcmc_trace(all_flow_sampling,par=c('endo_effect[1,5]'))
mcmc_trace(all_flow_sampling,par=c('Omega[1,1,2]'))
mcmc_dens(all_flow_sampling,par=c('Omega[2,1,2]'))


##posterior predictive check
y_rep<-extract(all_flow_sampling,pars="y_rep")
ppc_dens_overlay(all_flow_dat$y,y_rep$y_rep[1:500,])

#extracting parameters
params_all_f<-rstan::extract(all_flow_sampling,pars=c('beta_0','endo_effect','Omega'))
dim(params_all_f$beta_0)
dim(params_all_f$Omega)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta0
all_beta0_postf<-params_all_f$beta_0[sample(dim(params_all_f$beta_0)[1],size=1000,replace=F),,,]
dim(all_beta0_postf)

long_df_all_beta0f <- as.data.frame.table(all_beta0_postf,
                                             responseName = "estimate")
str(long_df_all_beta0f)


# Convert to long data frame
long_df_all_beta0f <- as.data.frame.table(all_beta0_postf,
                                          responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, year = Var3, endo = Var4, estimate = estimate) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006, species=as.integer(species))

long_df_all_beta0f$spec <- case_when(long_df_all_beta0f$species == 8 ~ "AGPE",
                                     long_df_all_beta0f$species == 2 ~ "ELRI",
                                     long_df_all_beta0f$species == 3 ~ "ELVI",
                                     long_df_all_beta0f$species == 4 ~ "FESU",
                                     long_df_all_beta0f$species == 5 ~ "LOAR",
                                     long_df_all_beta0f$species == 6 ~ "POAL",
                                     long_df_all_beta0f$species == 7 ~ "POAU",
                                     long_df_all_beta0f$species == 1 ~ "POSY")

summary_df_all_beta0f <- long_df_all_beta0f %>%
  group_by(year,spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#plotting endo estimates stacked
ggplot(summary_df_all_beta0f, aes(x = year, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "probability of flowering",
       title = "Change in probability of E+ and E- flowering with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

#plotting endo estimates grids
ggplot(summary_df_all_beta0f, aes(x = year, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "probability of flowering",
       title = "Change in probability of E+ and E- flowering with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_wrap(~spec, scales = "free_y", nrow = 2, ncol = 4)


##PLOTTING CORRELATION COEFFICENTS
#take a random subset of posterior draws for correlation coefficients
all_corr_postf<-params_all_f$Omega[sample(dim(params_all_f$Omega)[1],size=1000,replace=F),,1,2]
dim(all_corr_postf)

## Convert to long data frame for correlation coefficients
long_df_all_corrf <- as.data.frame.table(all_corr_postf,
                                         responseName = "value") %>%
  rename(draw = Var1, species = Var2, corr = value) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_corrf$spec <- case_when(long_df_all_corrf$species == 8 ~ "AGPE",
                                    long_df_all_corrf$species == 2 ~ "ELRI",
                                    long_df_all_corrf$species == 3 ~ "ELVI",
                                    long_df_all_corrf$species == 4 ~ "FESU",
                                    long_df_all_corrf$species == 5 ~ "LOAR",
                                    long_df_all_corrf$species == 6 ~ "POAL",
                                    long_df_all_corrf$species == 7 ~ "POAU",
                                    long_df_all_corrf$species == 1 ~ "POSY")

#plotting correlation coefficients
summary_df_all_corrf <- long_df_all_corrf %>%
  group_by(spec) %>% 
  summarize(
    mean = mean(corr),
    median = median(corr),
    lower = quantile(corr, 0.05),
    upper = quantile(corr, 0.95),
    probgzero = mean(corr>0),
    .groups = "drop")

ggplot(long_df_all_corrf)+
  geom_histogram(aes (x=corr), fill="mediumpurple",binwidth = 0.02)+
  facet_grid("spec")+xlim(-0.5,1) +
  geom_vline(data = summary_df_all_corrf,
             aes(xintercept = mean),
             colour = "mediumpurple4",
             size=0.75,
             linetype = "dashed")

#finding the mean correlation coefficients
long_df_all_corrf %>% group_by(spec) %>% summarize(mean = mean(corr, na.rm = TRUE))


##PLOTTING ENDO EFFECT (DIFFERENCE)
#take a random subset of posterior draws for endo effect
all_endoeffect_postf<-params_all_f$endo_effect[sample(dim(params_all_f$endo_effect)[1],size=1000,replace=F),,]
dim(all_endoeffect_postf)

# Convert to long data frame for endo effect
long_df_all_f <- as.data.frame.table(all_endoeffect_postf,
                                     responseName = "value") %>%
  rename(draw = iterations, species = Var2, year = Var3, endo_effect = value) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006, species=as.integer(species))

long_df_all_f$spec <- case_when(long_df_all_f$species == 8 ~ "AGPE",
                                long_df_all_f$species == 2 ~ "ELRI",
                                long_df_all_f$species == 3 ~ "ELVI",
                                long_df_all_f$species == 4 ~ "FESU",
                                long_df_all_f$species == 5 ~ "LOAR",
                                long_df_all_f$species == 6 ~ "POAL",
                                long_df_all_f$species == 7 ~ "POAU",
                                long_df_all_f$species == 1 ~ "POSY")

summary_df_all_f <- long_df_all_f %>%
  group_by(year,spec) %>% 
  summarize(
    median = median(endo_effect),
    lower = quantile(endo_effect, 0.05),
    upper = quantile(endo_effect, 0.95),
    probgzero = mean(endo_effect>0),
    .groups = "drop")

#plotting endo effect stacked
ggplot(summary_df_all_f, aes(x = year, y = median)) +
  geom_line(linewidth = 0.5, col = "mediumpurple3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "mediumpurple", color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- flowering with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

#plotting endo effect grids
ggplot(summary_df_all_f, aes(x = year, y = median)) +
  geom_line(linewidth = 0.5, col = "mediumpurple3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "mediumpurple", color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- flowering with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_wrap(~spec, scales = "free_y", nrow = 2, ncol = 4)


##MODEL SI: SURVIVAL AS RESPONSE, CLIMATE IMPLICIT___________________
##prep data, dropping NAs
gras %>%
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot,original,spec) %>% 
  drop_na() -> all_surv

all_surv_dat <- list(n_obs=nrow(all_surv),
                     y=all_surv$surv_t1,
                     n_yrs = length(unique(all_surv$year_t))+1,
                     n_plots = max(all_surv$plot),
                     n_endo = 2,
                     n_spp = length(unique(all_surv$spec)),
                     endo_01=all_surv$endo_01,
                     size=all_surv$log_tillers_centered,
                     year_index=all_surv$year_t-2006,
                     plot=all_surv$plot,
                     species=all_surv$spec,
                     original=all_surv$original)

all_surv_model = stan_model(file = "code/implicit_binomial_mvn.stan")
all_surv_sampling<-sampling(all_surv_model,
                            data = all_surv_dat,
                            chains = 3,thin = 5,
                            iter = 5000,
                            warmup = 1000,
                            pars=c("beta_0","beta_size","beta_size_endo",
                                   "meanflow","beta_orig","sigma_year",
                                   "sigma_plot","Omega","endo_effect","y_rep"),
                            save_warmup=F)

saveRDS(all_surv_sampling,"all_surv_sampling.rds")
#all_surv_sampling<-readRDS("all_surv_sampling.rds")

mcmc_trace(all_surv_sampling,par=c('endo_effect[1,5]'))
mcmc_trace(all_surv_sampling,par=c('Omega[1,1,2]'))
mcmc_dens(all_surv_sampling,par=c('Omega[2,1,2]'))

##posterior predictive check
y_rep<-extract(all_surv_sampling,pars="y_rep")
ppc_dens_overlay(all_surv_dat$y,y_rep$y_rep[1:500,])

#mcmc_trace(surv_sampling,pars='tau_plot[111]')

#extracting parameters
params_all_s<-rstan::extract(all_surv_sampling,pars=c('beta_0','endo_effect','Omega'))
dim(params_all_s$beta_0)
dim(params_all_s$Omega)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta0
all_beta0_posts<-params_all_s$beta_0[sample(dim(params_all_s$beta_0)[1],size=1000,replace=F),,,]
dim(all_beta0_posts)

long_df_all_beta0s <- as.data.frame.table(all_beta0_posts,
                                          responseName = "estimate")
str(long_df_all_beta0s)

# Convert to long data frame
long_df_all_beta0s <- as.data.frame.table(all_beta0_posts,
                                          responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, year = Var3, endo = Var4, estimate = estimate) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006, species=as.integer(species))

long_df_all_beta0s$spec <- case_when(long_df_all_beta0s$species == 8 ~ "AGPE",
                                     long_df_all_beta0s$species == 2 ~ "ELRI",
                                     long_df_all_beta0s$species == 3 ~ "ELVI",
                                     long_df_all_beta0s$species == 4 ~ "FESU",
                                     long_df_all_beta0s$species == 5 ~ "LOAR",
                                     long_df_all_beta0s$species == 6 ~ "POAL",
                                     long_df_all_beta0s$species == 7 ~ "POAU",
                                     long_df_all_beta0s$species == 1 ~ "POSY")


summary_df_all_beta0s <- long_df_all_beta0s %>%
  group_by(year,spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#plotting endo effect stacked
ggplot(summary_df_all_beta0s, aes(x = year, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "probaility of surviving",
       title = "Change in probability of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

#plotting endo effect grids
ggplot(summary_df_all_beta0s, aes(x = year, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "probaility of surviving",
       title = "Change in probability of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_wrap(~spec, scales = "free_y", nrow = 2, ncol = 4)

##PLOTTING CORRELATION COEFFICENTS
#take a random subset of posterior draws for correlation coefficients
all_corr_posts<-params_all_s$Omega[sample(dim(params_all_s$Omega)[1],size=1000,replace=F),,1,2]
dim(all_corr_posts)

long_df_all_corrs <- as.data.frame.table(all_corr_posts,
                                          responseName = "estimate")
str(long_df_all_corrs)

## Convert to long data frame for correlation coefficients
long_df_all_corrs <- as.data.frame.table(all_corr_posts,
                                         responseName = "value") %>%
  rename(draw = Var1, species = Var2, corr = value) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_corrs$spec <- case_when(long_df_all_corrs$species == 8 ~ "AGPE",
                                    long_df_all_corrs$species == 2 ~ "ELRI",
                                    long_df_all_corrs$species == 3 ~ "ELVI",
                                    long_df_all_corrs$species == 4 ~ "FESU",
                                    long_df_all_corrs$species == 5 ~ "LOAR",
                                    long_df_all_corrs$species == 6 ~ "POAL",
                                    long_df_all_corrs$species == 7 ~ "POAU",
                                    long_df_all_corrs$species == 1 ~ "POSY")

#plotting correlation coefficients
summary_df_all_corrs <- long_df_all_corrs %>%
  group_by(spec) %>% 
  summarize(
    mean = mean(corr),
    median = median(corr),
    lower = quantile(corr, 0.05),
    upper = quantile(corr, 0.95),
    probgzero = mean(corr>0),
    .groups = "drop")

ggplot(long_df_all_corrs)+
  geom_histogram(aes (x=corr), fill="mediumpurple",binwidth = 0.02)+
  facet_grid("spec")+xlim(-0.5,1) +
  geom_vline(data = summary_df_all_corrs,
             aes(xintercept = mean),
             colour = "mediumpurple4",
             size=0.75,
             linetype = "dashed")

#finding the mean correlation coefficients
long_df_all_corrs %>% group_by(spec) %>% summarize(mean = mean(corr, na.rm = TRUE))


##PLOTTING ENDO EFFECT (DIFFERENCE)
#take a random subset of posterior draws for endo effect
all_endoeffect_posts<-params_all_s$endo_effect[sample(dim(params_all_s$endo_effect)[1],size=1000,replace=F),,]
dim(all_endoeffect_posts)

# Convert to long data frame for endo effect
long_df_all_s <- as.data.frame.table(all_endoeffect_posts,
                                     responseName = "value") %>%
  rename(draw = iterations, species = Var2, year = Var3, endo_effect = value) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006, species=as.integer(species))

long_df_all_s$spec <- case_when(long_df_all_s$species == 8 ~ "AGPE",
                                long_df_all_s$species == 2 ~ "ELRI",
                                long_df_all_s$species == 3 ~ "ELVI",
                                long_df_all_s$species == 4 ~ "FESU",
                                long_df_all_s$species == 5 ~ "LOAR",
                                long_df_all_s$species == 6 ~ "POAL",
                                long_df_all_s$species == 7 ~ "POAU",
                                long_df_all_s$species == 1 ~ "POSY")

summary_df_all_s <- long_df_all_s %>%
  group_by(year,spec) %>% 
  summarize(
    median = median(endo_effect),
    lower = quantile(endo_effect, 0.05),
    upper = quantile(endo_effect, 0.95),
    probgzero = mean(endo_effect>0),
    .groups = "drop")

#plotting endo effect stacked
ggplot(summary_df_all_s, aes(x = year, y = median)) +
  geom_line(linewidth = 0.5, col = "mediumpurple3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "mediumpurple", color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

#plotting endo effect grids
ggplot(summary_df_all_s, aes(x = year, y = median)) +
  geom_line(linewidth = 0.5, col = "mediumpurple3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "mediumpurple", color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- survival with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_wrap(~spec, scales = "free_y", nrow = 2, ncol = 4)



##MODEL II: INFLORESCENE COUNT AS RESPONSE, CLIMATE IMPLICIT___________________

##prepping data, dropping NAs
gras %>%
  select(flw_count_t1,endo_01,log_tillers_centered,year_t,plot,spec,original) %>% 
  drop_na() -> all_infl

# Safety conversion to continuous sequential factor integers 
all_infl$plot    <- as.integer(as.factor(all_infl$plot))
all_infl$spec    <- as.integer(as.factor(all_infl$spec))
all_infl$year_t  <- as.integer(as.factor(all_infl$year_t))

all_infl_dat <- list(
  n_obs       = nrow(all_infl),
  y           = as.integer(all_infl$flw_count_t1), 
  n_yrs       = length(unique(all_infl$year_t)),   
  n_plots     = length(unique(all_infl$plot)),
  n_endo      = 2,
  n_spp       = length(unique(all_infl$spec)),
  endo_01     = as.integer(all_infl$endo_01),
  size        = as.numeric(all_infl$log_tillers_centered),
  year_index  = all_infl$year_t,
  plot        = all_infl$plot,
  species     = all_infl$spec,
  original    = as.integer(all_infl$original)
)

all_infl_model = stan_model(file="code/implicit_negativebinomial_mvn.stan")
all_infl_sampling <- sampling(
  all_infl_model,
  data = all_infl_dat,
  chains = 3, 
  thin = 5,
  iter = 5000,
  warmup = 1000,
  # CRITICAL UPDATE: Updated to track "beta_0_vec" instead of "beta_0"
  pars = c("beta_0", "beta_size", "beta_size_endo",
           "meanflow", "beta_orig", "sigma_year",
           "sigma_plot", "Omega", "endo_effect","y_rep"),
  save_warmup = FALSE,
  # SAFETY OPTION: bound random initialization tightly around 0 to avoid large exponential blowups
  init = "0" 
)

saveRDS(all_infl_sampling,"all_infl_sampling.rds")
all_infl_sampling<-readRDS("all_infl_sampling.rds")

mcmc_trace(all_infl_sampling,par=c('endo_effect[1,5]'))
mcmc_trace(all_infl_sampling,par=c('Omega[1,1,2]'))
mcmc_dens(all_infl_sampling,par=c('Omega[2,1,2]'))


##posterior predictive check
y_rep<-extract(all_infl_sampling,pars="y_rep")
ppc_dens_overlay(all_infl_dat$y,y_rep$y_rep[1:500,])+xlim(0,5)
#try a negative binomial and if see if it fits better

#extracting parameters
params_all_i<-rstan::extract(all_infl_sampling,pars=c('beta_0','endo_effect','Omega'))
dim(params_all_i$beta_0)
dim(params_all_i$Omega)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta0
all_beta0_posti<-params_all_i$beta_0[sample(dim(params_all_i$beta_0)[1],size=1000,replace=F),,,]
dim(all_beta0_posti)

long_df_all_beta0i <- as.data.frame.table(all_beta0_posti,
                                               responseName = "estimate")
str(long_df_all_beta0i)
# Convert to long data frame 
long_df_all_beta0i <- as.data.frame.table(all_beta0_posti,
                                          responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, year = Var3, endo = Var4, estimate = estimate) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006, species=as.integer(species))

long_df_all_beta0i$spec <- case_when(long_df_all_beta0i$species == 8 ~ "AGPE",
                                     long_df_all_beta0i$species == 2 ~ "ELRI",
                                     long_df_all_beta0i$species == 3 ~ "ELVI",
                                     long_df_all_beta0i$species == 4 ~ "FESU",
                                     long_df_all_beta0i$species == 5 ~ "LOAR",
                                     long_df_all_beta0i$species == 6 ~ "POAL",
                                     long_df_all_beta0i$species == 7 ~ "POAU",
                                     long_df_all_beta0i$species == 1 ~ "POSY")


summary_df_all_beta0i <- long_df_all_beta0i %>%
  group_by(year,spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#plotting endo estimates stacked
ggplot(summary_df_all_beta0i, aes(x = year, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "inflorescence count",
       title = "Change in inflorescence count of E+ and E- with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

#plotting endo estimates grids
ggplot(summary_df_all_beta0i, aes(x = year, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "inflorescence count",
       title = "Change in inflorescence count of E+ and E- with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_wrap(~spec, scales = "free_y", nrow = 2, ncol = 4)

##PLOTTING CORRELATION COEFFICENTS
#take a random subset of posterior draws for correlation coefficients
all_corr_posti<-params_all_i$Omega[sample(dim(params_all_i$Omega)[1],size=1000,replace=F),,1,2]
dim(all_corr_posti)

## Convert to long data frame for correlation coefficients
long_df_all_corri <- as.data.frame.table(all_corr_posti,
                                         responseName = "value") %>%
  rename(draw = Var1, species = Var2, corr = value) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_corri$spec <- case_when(long_df_all_corri$species == 8 ~ "AGPE",
                                    long_df_all_corri$species == 2 ~ "ELRI",
                                    long_df_all_corri$species == 3 ~ "ELVI",
                                    long_df_all_corri$species == 4 ~ "FESU",
                                    long_df_all_corri$species == 5 ~ "LOAR",
                                    long_df_all_corri$species == 6 ~ "POAL",
                                    long_df_all_corri$species == 7 ~ "POAU",
                                    long_df_all_corri$species == 1 ~ "POSY")

#plotting correlation coefficients
summary_df_all_corri <- long_df_all_corri %>%
  group_by(spec) %>% 
  summarize(
    mean = mean(corr),
    median = median(corr),
    lower = quantile(corr, 0.05),
    upper = quantile(corr, 0.95),
    probgzero = mean(corr>0),
    .groups = "drop")

ggplot(long_df_all_corri)+
  geom_histogram(aes (x=corr), fill="mediumpurple",binwidth = 0.02)+
  facet_grid("spec")+xlim(-0.5,1) +
  geom_vline(data = summary_df_all_corri,
             aes(xintercept = mean),
             colour = "mediumpurple4",
             linewidth=0.75,
             linetype = "dashed")

#finding the mean correlation coefficients
long_df_all_corri %>% group_by(spec) %>% summarize(mean = mean(corr, na.rm = TRUE))


##PLOTTING ENDO EFFECT (DIFFERENCE)
#take a random subset of posterior draws for endo effect
all_endoeffect_posti<-params_all_i$endo_effect[sample(dim(params_all_i$endo_effect)[1],size=1000,replace=F),,]
dim(all_endoeffect_posti)

# Convert to long data frame for endo effect
long_df_all_i <- as.data.frame.table(all_endoeffect_posti,
                                     responseName = "value") %>%
  rename(draw = iterations, species = Var2, year = Var3, endo_effect = value) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006, species=as.integer(species))

long_df_all_i$spec <- case_when(long_df_all_i$species == 8 ~ "AGPE",
                                long_df_all_i$species == 2 ~ "ELRI",
                                long_df_all_i$species == 3 ~ "ELVI",
                                long_df_all_i$species == 4 ~ "FESU",
                                long_df_all_i$species == 5 ~ "LOAR",
                                long_df_all_i$species == 6 ~ "POAL",
                                long_df_all_i$species == 7 ~ "POAU",
                                long_df_all_i$species == 1 ~ "POSY")

summary_df_all_i <- long_df_all_i %>%
  group_by(year,spec) %>% 
  summarize(
    median = median(endo_effect),
    lower = quantile(endo_effect, 0.05),
    upper = quantile(endo_effect, 0.95),
    probgzero = mean(endo_effect>0),
    .groups = "drop")

#plotting endo effect stacked
ggplot(summary_df_all_i, aes(x = year, y = median)) +
  geom_line(linewidth = 0.5, col = "mediumpurple3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "mediumpurple", color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- inflorescence count with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

#plotting endo effect grids
ggplot(summary_df_all_i, aes(x = year, y = median)) +
  geom_line(linewidth = 0.5, col = "mediumpurple3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "mediumpurple", color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- inflorescence count with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_wrap(~spec, scales = "free_y", nrow = 2, ncol = 4)



##MODEL GI: POPULATION GROWTH RATE AS RESPONSE, CLIMATE IMPLICIT___________________

#Making a data frame for population growth rate (FROM GEMINI)
pop_growth_df_imp <- gras %>%
  # 1. Group by the variables that define a "population"
  group_by(species, plot, endo_01, year_t, spec) %>%
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

##prep data, dropping NAs
pop_growth_df_imp %>%
  select(r,endo_01,spec,plot,year_t) %>% 
  drop_na() -> all_grow

all_grow_dat <- list(n_obs=nrow(all_grow),
                     y=all_grow$r,
                     n_yrs = length(unique(all_grow$year_t)),
                     n_plots = max(all_grow$plot),
                     n_endo = 2,
                     n_spp = max(all_grow$spec),
                     endo_01=all_grow$endo_01,
                     year_index=all_grow$year_t-2006,
                     plot=all_grow$plot,
                     species=all_grow$spec)

all_grow_model = stan_model(file="code/demogrowth.stan")
all_grow_sampling <- sampling(all_grow_model,
                              data = all_grow_dat,
                              chains = 3, 
                              iter = 5000, 
                              #warmup  = 1000,
                              include = TRUE,
                              pars=c('beta_0','endo_effect','Omega','y_rep'))

#saveRDS(all_grow_sampling,"all_grow_sampling.rds")
all_grow_sampling<-readRDS("all_grow_sampling.rds")

##posterior predictive check
y_rep<-extract(all_grow_sampling,pars="y_rep")
ppc_dens_overlay(all_grow_dat$y,y_rep$y_rep[1:500,])

#extracting parameters
params_all_g<-rstan::extract(all_grow_sampling,pars=c('beta_0','endo_effect','Omega'))
dim(params_all_g$beta_0)
dim(params_all_g$endo_effect)
dim(params_all_g$Omega)


##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for beta_0
all_beta0_postg<-params_all_g$beta_0[sample(dim(params_all_g$beta_0)[1],size=1000,replace=F),,,]
dim(all_beta0_postg)

long_df_all_beta0g <- as.data.frame.table(all_beta0_postg,
                                          responseName = "estimate")
str(long_df_all_beta0g)

# Convert to long data frame 
long_df_all_beta0g <- as.data.frame.table(all_beta0_postg,
                                          responseName = "estimate") %>%
  rename(draw = iterations, species = Var2, endo = Var3, year = Var4, estimate = estimate) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006, species=as.integer(species))

long_df_all_beta0g$spec <- case_when(long_df_all_beta0g$species == 8 ~ "AGPE",
                                     long_df_all_beta0g$species == 2 ~ "ELRI",
                                     long_df_all_beta0g$species == 3 ~ "ELVI",
                                     long_df_all_beta0g$species == 4 ~ "FESU",
                                     long_df_all_beta0g$species == 5 ~ "LOAR",
                                     long_df_all_beta0g$species == 6 ~ "POAL",
                                     long_df_all_beta0g$species == 7 ~ "POAU",
                                     long_df_all_beta0g$species == 1 ~ "POSY")


summary_df_all_beta0g <- long_df_all_beta0g %>%
  group_by(year,spec,endo) %>% 
  summarize(
    median = median(estimate),
    lower = quantile(estimate, 0.05),
    upper = quantile(estimate, 0.95),
    probgzero = mean(estimate>0),
    .groups = "drop")

#plotting endo estimates stacked
ggplot(summary_df_all_beta0g, aes(x = year, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "growth (r)",
       title = "Growth rate of E+ and E- grow with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

#plotting endo estimates grids
ggplot(summary_df_all_beta0g, aes(x = year, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "growth (r)",
       title = "Growth rate of E+ and E- with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_wrap(~spec, scales = "free_y", nrow = 2, ncol = 4)


##PLOTTING CORRELATION COEFFICIENTS
#take a random subset of posterior draws for Omega
all_corr_postg<-params_all_g$Omega[sample(dim(params_all_g$Omega)[1],size=1000,replace=F),,2,1]
dim(all_corr_postg)

## Convert to long data frame for correlation coefficients
long_df_all_corrg <- as.data.frame.table(all_corr_postg,
                                         responseName = "estimate")
str(long_df_all_corrg)

long_df_all_corrg <- as.data.frame.table(all_corr_postg,
                                         responseName = "value") %>%
  rename(draw = Var1, species = Var2, corr = value) %>%
  mutate(draw = as.integer(draw), species=as.integer(species))

long_df_all_corrg$spec <- case_when(long_df_all_corrg$species == 8 ~ "AGPE",
                                     long_df_all_corrg$species == 2 ~ "ELRI",
                                     long_df_all_corrg$species == 3 ~ "ELVI",
                                     long_df_all_corrg$species == 4 ~ "FESU",
                                     long_df_all_corrg$species == 5 ~ "LOAR",
                                     long_df_all_corrg$species == 6 ~ "POAL",
                                     long_df_all_corrg$species == 7 ~ "POAU",
                                     long_df_all_corrg$species == 1 ~ "POSY")

summary_df_all_corrg <- long_df_all_corrg %>%
  group_by(spec) %>% 
  summarize(
    median = median(corr),
    lower = quantile(corr, 0.05),
    upper = quantile(corr, 0.95),
    probgzero = mean(corr>0),
    .groups = "drop")

#plotting correlation coefficients
ggplot(long_df_all_corrg)+
  geom_histogram(aes (x=corr), fill="mediumpurple",binwidth = 0.02)+
  facet_grid("spec")+xlim(-1,1) +
  geom_vline(data = summary_df_all_corrg,
             aes(xintercept = median),
             colour = "mediumpurple4",
             linewidth=0.75,
             linetype = "dashed")

#finding the mean correlation coefficients
long_df_all_corrg %>% group_by(spec) %>% summarize(mean = mean(corr, na.rm = TRUE))


##PLOTTING ENDO EFFECT (DIFFERENCE)
#take a random subset of posterior draws for endo effect
all_endoeffect_postg<-params_all_g$endo_effect[sample(dim(params_all_g$endo_effect)[1],size=1000,replace=F),,]
dim(all_endoeffect_postg)

# Convert to long data frame for endo effect
long_df_all_g <- as.data.frame.table(all_endoeffect_postg,
                                     responseName = "value") %>%
  rename(draw = iterations, species = Var2, year = Var3, endo_effect = value) %>%
  mutate(draw = as.integer(draw), year = as.integer(year)+2006, species=as.integer(species))

long_df_all_g$spec <- case_when(long_df_all_g$species == 8 ~ "AGPE",
                                long_df_all_g$species == 2 ~ "ELRI",
                                long_df_all_g$species == 3 ~ "ELVI",
                                long_df_all_g$species == 4 ~ "FESU",
                                long_df_all_g$species == 5 ~ "LOAR",
                                long_df_all_g$species == 6 ~ "POAL",
                                long_df_all_g$species == 7 ~ "POAU",
                                long_df_all_g$species == 1 ~ "POSY")

summary_df_all_g <- long_df_all_g %>%
  group_by(year,spec) %>% 
  summarize(
    median = median(endo_effect),
    lower = quantile(endo_effect, 0.05),
    upper = quantile(endo_effect, 0.95),
    probgzero = mean(endo_effect>0),
    .groups = "drop")

#plotting endo effect stacked
ggplot(summary_df_all_g, aes(x = year, y = median)) +
  geom_line(linewidth = 0.5, col = "mediumpurple3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "mediumpurple", color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- growth rates with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

#plotting endo effect grids
ggplot(summary_df_all_g, aes(x = year, y = median)) +
  geom_line(linewidth = 0.5, col = "mediumpurple3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "mediumpurple", color = NA) + #should color = species
  labs(x = "Year", y = "Endophyte effect",
       title = "Difference of E+ and E- growth rates with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_wrap(~spec, scales = "free_y", nrow = 2, ncol = 4)

