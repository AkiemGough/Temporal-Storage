data {
  int<lower=0> n_obs;
  int<lower=0> y[n_obs];
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

transformed data {
  array[n_obs] int endo_idx;
  for(i in 1:n_obs) {
    endo_idx[i] = endo_01[i] + 1;
  }
}

parameters {
  matrix [n_spp,n_endo] beta_0;//species random effects unique to E+ and E-
  vector [n_spp] beta_size;//size
  matrix [n_spp, n_endo] beta_clim;       // climate main effect
  real beta_orig; 
  
  vector [n_plots] tau_plot_raw;//plot random effects
  vector [n_yrs] gamma_year_raw;//year random effects
  real<lower=0> sigma_plot;//plot variance -- shared across species
  real<lower=0> sigma_year;//year variance -- shared across species
  real<lower=0> phi;
  array[n_spp] simplex[K] w_clim;         // climate lag weights
}

transformed parameters{
  vector[n_plots] tau_plot = tau_plot_raw * sigma_plot;
  vector[n_yrs] gamma_year = gamma_year_raw * sigma_year;
  
  vector[n_obs] log_lambda;
  vector[n_obs] weighted_clim;

  // Calculate separate weighted environmental indexes per observation
  for(i in 1:n_obs) {
    weighted_clim[i] = climate[i, ] * w_clim[species[i]];
  }

  for(i in 1:n_obs){
  log_lambda[i] = beta_0[species[i],(endo_01[i]+1)] 
  + beta_size[species[i]]*size[i] 
  + beta_clim[species[i], endo_idx[i]] * weighted_clim[i] 
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
  beta_orig ~ normal(0,2);
  
 for (i in 1:n_spp) {
   w_clim[i] ~ dirichlet(rep_vector(2.0, K)); 
  }
  
  to_vector(beta_0) ~ normal(0, 1);  
  to_vector(beta_size) ~ normal(1, 2);
  to_vector(beta_clim) ~ normal(0, 1);
  
  y ~ neg_binomial_2_log(log_lambda, phi);
}

generated quantities {
  array[n_obs] int y_rep;
  for(i in 1:n_obs){
    y_rep[i] = neg_binomial_2_log_rng(log_lambda[i], phi);
  }
  
  //add log likelihood to output
  vector[n_obs] log_lik;
  for(i in 1:n_obs){
    log_lik[i] = neg_binomial_2_log_lpmf(y[i] | log_lambda[i], phi);
  }
}
