data {
  int<lower=0> n_obs;
  int<lower=0, upper=1> y[n_obs];
  int<lower=0> n_yrs;
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0> n_spp;
  int<lower=0,upper=1> endo_01[n_obs];
  int<lower=0,upper=1> original[n_obs];
  vector[n_obs] size;                     // Changed from real array to vector
  int<lower=1> K;                         // monthly lags
  matrix[n_obs, K] precip;               // standardized precipitation lags
    matrix[n_obs, K] temper;             // standardized temperature lags
  int<lower=1> year_index[n_obs];
  int<lower=1> plot[n_obs];
  int<lower=1> species[n_obs];
}

transformed data {
  // Pre-calculate index to handle the 0/1 endophyte indexing safely
  array[n_obs] int endo_idx;
  for(i in 1:n_obs) {
    endo_idx[i] = endo_01[i] + 1;
  }
}

parameters {
  matrix [n_spp, n_endo] beta_0;          // unique to E+ and E-
  matrix [n_spp, n_endo] beta_size;       // size effect
  matrix [n_spp, n_endo] beta_clim;       // climate effect
  matrix [n_spp, n_endo] beta_size_clim;  // interaction
  real beta_orig; 
  
  vector [n_plots] tau_plot_raw;
  vector [n_yrs] gamma_year_raw;
  real<lower=0> sigma_plot;
  real<lower=0> sigma_year;
  array[n_spp] simplex[K] w;              // Dirichlet-constrained weights
}

transformed parameters {
  vector[n_plots] tau_plot = tau_plot_raw * sigma_plot;
  vector[n_yrs] gamma_year = gamma_year_raw * sigma_year;
  vector[n_obs] p;
  vector[n_obs] weighted_clim;

  // Vectorize the climate-weighting per observation
  for(i in 1:n_obs) {
    weighted_clim[i] = climate[i, ] * w[species[i]];
  }

  // Fully vectorized linear predictor calculation
  // Stan maps the row/column extraction instantly across the vectors
  for(i in 1:n_obs) {
    p[i] = beta_0[species[i], endo_idx[i]] 
         + beta_size[species[i], endo_idx[i]] * size[i] 
         + beta_clim[species[i], endo_idx[i]] * weighted_clim[i] 
         + beta_size_clim[species[i], endo_idx[i]] * size[i] * weighted_clim[i]
         + beta_orig * original[i]
         + tau_plot[plot[i]] 
         + gamma_year[year_index[i]];
  }
}

model {
  // 1. Non-centered random effect priors
  tau_plot_raw ~ normal(0, 1);
  gamma_year_raw ~ normal(0, 1);
  
  sigma_plot ~ exponential(1);
  sigma_year ~ exponential(1);
  
  // 2. Tighter, weakly informative priors (Logit Scale)
  beta_orig ~ normal(0, 2); 
  
  // Slightly higher concentration prior pushes weights to be stable/flatter 
  // unless the data strongly dictates an individual lag is important.
  for (i in 1:n_spp) {
    w[i] ~ dirichlet(rep_vector(2.0, K)); 
  }
  
  // Vectorized priors across the matrices
  to_vector(beta_0) ~ normal(0, 2);          // Centered around 0 on logit scale
  to_vector(beta_size) ~ normal(1, 2);       // Assuming size has a positive baseline
  to_vector(beta_clim) ~ normal(0, 1.5);     // Tighter prior isolates the slope from 'w'
  to_vector(beta_size_clim) ~ normal(0, 1);  // Interactions are structurally smaller
  
  // 3. Vectorized Likelihood
  y ~ bernoulli_logit(p);
}

generated quantities {
  array[n_obs] int y_rep;                    // Changed to int array for valid Bernoulli output
  for(i in 1:n_obs){
    y_rep[i] = bernoulli_logit_rng(p[i]);
  }
}
