#Purpose: Vizualizing the difference between E+ and E- plants in how their vital rates with year
#In a bayesian framework

#Important packages
library(tidyverse)
library(lme4)
library(scales)
library(rstan)
library(googlesheets4)
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
summary(all_flow_sampling_ppt)

#extracting parameters
params_all_f_ppt<-rstan::extract(all_flow_sampling_ppt,pars=c('beta_0','beta_clim','beta_size_clim','endo_effect'))
dim(params_all_f_ppt$beta_0)
dim(params_all_f_ppt$beta_clim)
dim(params_all_f_ppt$beta_size_clim)


##making size x variables for graphs
(ppt_tot_dummy<-seq(from=min(all_flow_ppt$ppt_tot_scaled,na.rm=T),to=max(all_flow_ppt$ppt_tot_scaled,na.rm=T),by=0.1))

#creating logistic function 
logistic<-function(x){1/(1+exp(-x))}

#defining a predictor function
predict_c <- function(fit, size, climate, endo, species){
  params<-rstan::extract(fit,pars=c('beta_0','beta_size','beta_clim','beta_size_clim'))
  beta_0 <- params$beta_0[, species, endo + 1]
  beta_size <- params$beta_size[, species, endo + 1]
  beta_clim <- params$beta_clim[, species, endo + 1]
  beta_size_clim <- params$beta_size_clim[, species, endo + 1]
  
  beta_0 + beta_size*size + beta_clim*climate + beta_size_clim*size*climate
}

#WE ARE LOST
#p_endo0 <- predict_c(all_flow_sampling_ppt, 0, 0, 0, 1)

p_endo0_posy <- sapply(ppt_tot_dummy, function(x) predict_c(all_flow_sampling_ppt, 0, x, 0, 1))
p_endo1_posy <- sapply(ppt_tot_dummy, function(x) predict_c(all_flow_sampling_ppt, 0, x, 1, 1))

flow_endo0_median_posy <- apply(p_endo0, 2, median)
flow_endo1_median_posy <- apply(p_endo1, 2, median)

?apply

plot(x=ppt_tot_dummy, y=flow_endo0_median_posy, col="deeppink1", lty=1, type="l")
lines(x=ppt_tot_dummy, y=flow_endo1_median_posy, col="cornflowerblue", lty=1, type="l")


#plot(x=grasclim$ppt_tot_scaled, y=flow_endo0_median_posy, col="deeppink1", lty=1, type="l")
#lines(x=grasclim$ppt_tot_scaled, y=flow_endo1_median_posy, col="cornflowerblue", lty=1, type="l")

# visualize the interaction effect

plot(density(beta_ppt_endo))
mean(beta_ppt_endo > 0)
mean(beta_ppt_endo < 0)

hist(params_all_f_ppt$beta_clim)
abline(v=mean(params_all_f_ppt$beta_clim),col="deeppink1",lwd=3)

(sum(params_all_f_ppt$beta_clim < 0) / length(params_all_f_ppt$beta_clim))
(sum(params_all_f_ppt$beta_clim > 0) / length(params_all_f_ppt$beta_clim))



# visualize the interaction effect
beta_clim_endo <- rstan::extract(poal_flow_sampling_c)$beta_clim_endo
plot(density(beta_clim_endo))
mean(beta_clim_endo > 0)
mean(beta_clim_endo < 0)

#trying to make plots, not going very well
plot(grasclim$ppt_tot_scaled,grasclim$flw_count_t,amount=0.02)

p = logistic(beta_0 + beta_size*size + beta_endo*endo_01+ beta_clim*climate + beta_size_endo*size*endo_01 + beta_clim_endo*endo_01*climate)

lines(x=grasclim$log_ppt_tot_centered, y=logistic(poal_f_c_Eminus_intercepts+poal_f_c_Eplus_intercepts*grasclim$log_ppt_tot_centered), col="deeppink1")
lines(x=grasclim$log_ppt_tot_centered, y=logistic(poal_f_c_Eminus_slopes+poal_f_c_Eplus_slopes*grasclim$log_ppt_tot_centered), col="cornflowerblue")



#link for making caterpillar plots: https://www.rdocumentation.org/packages/mcmcplots/versions/0.4.3/topics/caterplot





##PLOTTING ENDO ESTIMATES
#take a random subset of posterior draws for endo effect
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

##making size x variables for graphs
(ppt_tot_dummy<-seq(from=min(all_flow_ppt$ppt_tot_scaled,na.rm=T),to=max(all_flow_ppt$ppt_tot_scaled,na.rm=T),by=0.1))

#find the coefficient to use or calculation of coefficients?
coef(long_df_all_beta0f_ppt)
coef(summary_df_all_beta0f_ppt)

plot(x=ppt_tot_dummy, y=summary_df_all_beta0f_ppt[])

#plotting endo estimates
ggplot(summary_df_all_beta0f_ppt, aes(x = ppt_tot_dummy, y = median, colour = endo, fill = endo)) +
  scale_color_manual(values = c("deeppink1", "cornflowerblue")) +
  scale_fill_manual(values = c("deeppink1", "cornflowerblue")) +
  geom_line(linewidth = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) + 
  labs(x = "Year", y = "probability of flowering",
       title = "Change in probability of E+ and E- flowering with year") +
  geom_hline(yintercept = 0) +
  theme_minimal()+
  facet_grid("spec")

