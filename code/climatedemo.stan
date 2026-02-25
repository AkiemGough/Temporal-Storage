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
  real climate[n_obs]; 
  int<lower=1> year_index[n_obs];
  int<lower=1> plot[n_obs];
  int<lower=1> species[n_obs];
}

parameters {
  real beta_0[n_spp,n_endo];//species random effects unique to E+ and E-
  real beta_size[n_spp,n_endo];//size
  real beta_clim[n_spp,n_endo];//climate variable
  real beta_orig[2];//original effect -- shared across species
  real beta_size_clim[n_spp,n_endo];//size:climate
  real tau_plot[n_plots];//plot random effects
  real gamma_year[n_yrs];//year random effects
  real<lower=0> sigma_plot;//plot variance -- shared across species
  real<lower=0> sigma_year;//year variance -- shared across species
}

transformed parameters{
  real p[n_obs];
  for(i in 1:n_obs){
  p[i] = beta_0[species[i],(endo_01[i]+1)] 
  + beta_size[species[i],(endo_01[i]+1)]*size[i] 
  + beta_clim[species[i],(endo_01[i]+1)]*climate[i] 
  + beta_size_clim[species[i],(endo_01[i]+1)]*size[i]*climate[i]
  + beta_orig[original[i]+1]
  + tau_plot[plot[i]] 
  + gamma_year[year_index[i]];
  }
}

model {
  tau_plot ~ normal(0,sigma_plot);
  sigma_plot ~ exponential(1);
  gamma_year ~ normal(0,sigma_year);
  sigma_year ~ exponential(1);
  beta_orig ~ normal(0,1);
  for (i in 1:n_spp) {
    for (j in 1:n_endo) {
      beta_0[i,j] ~ normal(0,1); 
      beta_size[i,j] ~ normal(0,1);
      beta_size[i,j] ~ normal(0,1);
      beta_clim[i,j] ~ normal(0,1);
      beta_size_clim[i,j] ~ normal(0,1);
    }
  }
  y ~ bernoulli_logit(p);
}

generated quantities {
  real endo_effect[n_spp];
  for (i in 1:n_spp) {
    endo_effect[i] = beta_0[i,2] - beta_0[i,1];
    }
  }

