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
  matrix[n_obs, K] precip; //standardized precipitation lags
  matrix[n_obs, K] temper; //standardized temperature lags
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
  matrix [n_spp, n_endo] beta_0;          // unique to E+ and E-
  vector [n_spp] beta_size;               // size
  matrix [n_spp, n_endo] beta_prec;       // precipitation main effect
  matrix [n_spp, n_endo] beta_temp;       // temperature main effect
  real beta_orig; 
  
  vector [n_plots] tau_plot_raw;
  vector [n_yrs] gamma_year_raw;
  real<lower=0> sigma_plot;
  real<lower=0> sigma_year;
  
  // Split into separate weight simplexes for each climate variable
  array[n_spp] simplex[K] w_prec;         // Precipitation lag weights
  array[n_spp] simplex[K] w_temp;         // Temperature lag weights
}

transformed parameters {
  vector[n_plots] tau_plot = tau_plot_raw * sigma_plot;
  vector[n_yrs] gamma_year = gamma_year_raw * sigma_year;
  
  vector[n_obs] p;
  vector[n_obs] weighted_prec;
  vector[n_obs] weighted_temp;

  // Calculate separate weighted environmental indexes per observation
  for(i in 1:n_obs) {
    weighted_prec[i] = precip[i, ] * w_prec[species[i]];
    weighted_temp[i] = temper[i, ] * w_temp[species[i]];
  }

  // Linear predictor using the independent weights
  for(i in 1:n_obs) {
    p[i] = beta_0[species[i], endo_idx[i]] 
         + beta_size[species[i]] * size[i] 
         + beta_prec[species[i], endo_idx[i]] * weighted_prec[i] 
         + beta_temp[species[i], endo_idx[i]] * weighted_temp[i]
         + beta_orig * original[i]
         + tau_plot[plot[i]] 
         + gamma_year[year_index[i]];
  }
}

model {
  tau_plot_raw ~ normal(0, 1);
  gamma_year_raw ~ normal(0, 1);
  
  sigma_plot ~ exponential(1);
  sigma_year ~ exponential(1);
  beta_orig ~ normal(0, 2);               // Slightly tightened from 10
  
  // Separate Dirichlet priors for each environmental variable
  // Using 2.0 concentration to keep chains stable amidst collinearity
  for (i in 1:n_spp) {
    w_prec[i] ~ dirichlet(rep_vector(2.0, K)); 
    w_temp[i] ~ dirichlet(rep_vector(2.0, K)); 
  }
  
  // Vectorized matrix priors
  to_vector(beta_0) ~ normal(0, 2);  
  to_vector(beta_size) ~ normal(1, 2);
  to_vector(beta_prec) ~ normal(0, 1.5);
  to_vector(beta_temp) ~ normal(0, 1.5);
  
  y ~ bernoulli_logit(p);
}

generated quantities {
  array[n_obs] int y_rep;
  for(i in 1:n_obs){
    y_rep[i] = bernoulli_logit_rng(p[i]);
  }
}
