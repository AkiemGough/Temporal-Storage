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
  int<lower=1> K; //monthly lags
  matrix[n_obs, K] climate; //standardized climate lags
  int<lower=1> year_index[n_obs];
  int<lower=1> plot[n_obs];
  int<lower=1> species[n_obs];
}

parameters {
  matrix [n_spp,n_endo] beta_0;//species random effects unique to E+ and E-
  matrix [n_spp,n_endo] beta_size;//size
  matrix [n_spp,n_endo] beta_clim;//climate variable
  real beta_orig; 
  matrix [n_spp,n_endo] beta_size_clim;//size:climate
  // 1. Shifted to standard normal multipliers (raw parameters)
  vector [n_plots] tau_plot_raw;
  vector [n_yrs] gamma_year_raw;
  real<lower=0> sigma_plot;//plot variance -- shared across species
  real<lower=0> sigma_year;//year variance -- shared across species
  array[n_spp] simplex[K] w;//simplex means we are forcing w to sum to 1, to represent the weighting - Dirichlet-constrained weights
}

transformed parameters{
  // 2. Recompose the random effects here so your loop doesn't have to change
  vector[n_plots] tau_plot = tau_plot_raw * sigma_plot;
  vector[n_yrs] gamma_year = gamma_year_raw * sigma_year;
  
  real p[n_obs];
  for(i in 1:n_obs){
    p[i] = beta_0[species[i],(endo_01[i]+1)] 
    + beta_size[species[i],(endo_01[i]+1)]*size[i] 
    + beta_clim[species[i],(endo_01[i]+1)]*(climate[i,]*w[species[i]]) 
    + beta_size_clim[species[i],(endo_01[i]+1)]*size[i]*(climate[i,]*w[species[i]])
    + beta_orig * original[i]
    + tau_plot[plot[i]] 
    + gamma_year[year_index[i]];
  }
}

model {
  // 3. Give the raw multipliers a standard normal prior
  tau_plot_raw ~ normal(0, 1);
  gamma_year_raw ~ normal(0, 1);
  
  sigma_plot ~ exponential(1);
  sigma_year ~ exponential(1);
  beta_orig ~ normal(0,10);
  
  for (i in 1:n_spp) {
    w[i] ~ dirichlet(rep_vector(1.0,K)); //uniform over simplex
    for (j in 1:n_endo) {
      beta_0[i,j] ~ normal(0,1); 
      beta_size[i,j] ~ normal(1,10);
      beta_clim[i,j] ~ normal(0,10);
      beta_size_clim[i,j] ~ normal(0,10);
    }
  }
  y ~ bernoulli_logit(p);
}

generated quantities {
  real y_rep[n_obs];
  for(i in 1:n_obs){
    y_rep[i] = bernoulli_logit_rng(p[i]);
  }
}
