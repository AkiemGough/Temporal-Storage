data {
  int<lower=0> n_obs;
  int<lower=0, upper=1> y[n_obs];
  int<lower=0> n_yrs;
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0> n_spp;
  int<lower=0,upper=1> endo_01[n_obs];
  int<lower=0,upper=1> original[n_obs];
  real size[n_obs];
  int<lower=1> year_index[n_obs];
  int<lower=1> plot[n_obs];
  int<lower=1> species[n_obs];
}
parameters {
  real beta_0[n_spp,n_endo,n_yrs];//year random effects, unique to E+ and E-
  real tau_plot[n_plots];//plot random effects
  real beta_size[n_spp];//size
  real beta_size_endo[n_spp];//size:endo
  real meanflow[n_spp,n_endo];
  real beta_orig;//original effect
  real<lower=0> sigma_year[n_spp];//year variance
  real<lower=0> sigma_plot;//plot variance -- shared across species
}
transformed parameters{
  real p[n_obs];
  for(i in 1:n_obs){
  p[i] = beta_0[species[i],(endo_01[i]+1),year_index[i]] 
  + beta_size[species[i]]*size[i] 
  + beta_size_endo[species[i]]*size[i]*endo_01[i]
  + beta_orig*original[i]
  + tau_plot[plot[i]];
  }
}
model {
  tau_plot ~ normal(0,sigma_plot);
  sigma_plot ~ exponential(1);
  for(i in 1:n_spp){
    beta_0[i,1,] ~ normal(meanflow[i,1],sigma_year[i]);
    beta_0[i,2,] ~ normal(meanflow[i,2],sigma_year[i]);
  }
  sigma_year ~ exponential(1);
  beta_size ~ normal(1,10);
  beta_size_endo ~ normal(0,10);
  beta_orig ~ normal(1,10);
  for (i in 1:n_spp) {
    for (j in 1:n_endo) {
      meanflow[i,j] ~ normal(0, 5);
    }
}
  y ~ bernoulli_logit(p);
}
generated quantities {
  real endo_effect[n_spp,n_yrs];
  for (i in 1:n_spp) {
    for (j in 1:n_yrs) {
    endo_effect[i,j] = beta_0[i,2,j] - beta_0[i,1,j];
    }
  }
}

