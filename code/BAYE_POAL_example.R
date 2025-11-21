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

##glmer model
poal_flow_mod<- glmer(flw_count_t>0~endo_01*log_tillers_centered+(endo_01|year_t)+(1|plot),family="binomial", data=poaals)
coef(poal_flow_mod)
coefs<-coef(poal_flow_mod)
E_minus_year<-coefs$year_t$`(Intercept)`
E_plus_year<-coefs$year_t$`(Intercept)`+coefs$year_t$endo_01
plot(E_minus_year, pch=1, col="goldenrod")
points(E_plus_year,pch=1, col="slateblue")

## try this for survival
poal_surv_mod<- glmer(surv_t1~endo_01*log_tillers_centered+(endo_01|year_t)+(1|plot),family="binomial", data=poaals)
coefs<-coef(poal_surv_mod)
E_minus_year<-coefs$year_t$`(Intercept)`
E_plus_year<-coefs$year_t$`(Intercept)`+coefs$year_t$endo_01
plot(E_minus_year, pch=1, col="goldenrod")
points(E_plus_year, pch=1, col="slateblue")

##bayesian version of same model for flowering
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
poal_flow_model = stan_model(file="R Studio Codes/flowering.stan")
poal_flow_sampling<-sampling(flow_model,
                        data=poal_flow_dat,
                        chains = 3,
                        iter = 5000,
                        warmup = 1000)
mcmc_trace(poal_flow_sampling,par=c('endo_effect[5]'))
mcmc_dens(poal_flow_sampling,par=c('beta_size'))

##FOR FLOWERING
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
    year = as.integer(year)
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
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "logit(Flowering)",
    fill = "Group",
    color = "Group"
  ) +
  theme_minimal()

##difference
endoeffect_postf<-params_poal_f$endo_effect[sample(12000,size=500,replace=F),]
long_df_f <- as.data.frame.table(endoeffect_postf,
                                 responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(
    draw = as.integer(draw),
    year = as.integer(year)
  )
summary_df_poal_f <- long_df_f %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop"
  )
ggplot(summary_df_poal_f, aes(x = year, y = median)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = "slateblue1") +
  scale_fill_manual(values = "slateblue1" )+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "Endophyte effect") +
  geom_hline(yintercept = 0) +
  theme_minimal()
ggplot(summary_df_poal_f, aes(x = year, y = probgzero)) +
  scale_color_manual(values = "slateblue1") +
  scale_fill_manual(values = "slateblue1" )+
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Pr(E+ > E-)") +
  theme_minimal()


#QUESTIONS FROM AKIEM for above: 1) what does "sampling" in this context? 
#2) how did you know how many markov chains, iterations and warm-up iterations you needed?
#3) why isn't there a surv model below?

## same but for survival -- note we can use the exact same model.
## even though it's called "flowering", I will give it survival data
poaals %>% 
  select(surv_t1,endo_01,log_tillers_centered,year_t,plot) %>% 
  drop_na() -> poaals_surv
surv_dat<-list(n_obs=nrow(poaals_surv),
               y=poaals_surv$surv_t1,
               n_yrs = length(unique(poaals_surv$year_t)),
               n_plots = max(poaals_surv$plot),
               n_endo = 2,
               endo_01=poaals_surv$endo_01,
               size=poaals_surv$log_tillers_centered,
               year_index=poaals_surv$year_t-2006,
               plot=poaals_surv$plot)
surv_sampling<-sampling(flow_model,
                        data=surv_dat,
                        chains = 3,
                        iter = 8000,
                        warmup = 2000)
mcmc_trace(surv_sampling,pars='tau_plot[111]')
params<-rstan::extract(surv_sampling,pars=c('beta_0','endo_effect'))
dim(params$beta_0)
#take a random subset of posterior draws
beta0_post<-params$beta_0[sample(18000,size=500,replace=F),,]
dim(beta0_post)

# Convert to long data frame
long_df_poal_Surv <- as.data.frame.table(beta0_post,
                               responseName = "value") %>%
  rename(draw = iterations, group = Var2, year = Var3) %>%
  mutate(
    draw = as.integer(draw),
    group = factor(group, labels = c("E-", "E+")),
    year = as.integer(year)+2006
  )
summary_df_poal_Surv <- long_df_poal_Surv %>%
  group_by(group, year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .groups = "drop"
  )
ggplot(summary_df_poal_Surv, aes(x = year, y = median, color = group, fill = group)) +
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

##survival difference between E+ and E-
endoeffect_post<-params$endo_effect[sample(18000,size=500,replace=F),]
long_df_poal_Surv_dif <- as.data.frame.table(endoeffect_post,
                               responseName = "value") %>%
  rename(draw = iterations, year = Var2) %>%
  mutate(
    draw = as.integer(draw),
    year = as.integer(year)
  )
summary_df <- long_df %>%
  group_by(year) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    probgzero = mean(value>0),
    .groups = "drop"
  )
ggplot(summary_df, aes(x = year, y = median)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "Endophyte effect") +
  geom_hline(yintercept = 0) +
  theme_minimal()
ggplot(summary_df, aes(x = year, y = probgzero)) +
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Pr(E+ > E-)") +
  theme_minimal()

