data {
  int<lower=0> n_obs;
  array[n_obs] real<lower=0> y; 
  int<lower=0> n_yrs;
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0> n_spp;
  array[n_obs] int<lower=0,upper=1> endo_01;
  array[n_obs] int<lower=0,upper=1> original; // Assuming 0 or 1
  array[n_obs] real climate; 
  array[n_obs] int<lower=1> year_index;
  array[n_obs] int<lower=1> plot;
  array[n_obs] int<lower=1> species;
}

parameters {
  array[n_spp, n_endo] real beta_0;
  array[n_spp, n_endo] real beta_clim;
  array[2] real beta_orig; // Array of size 2
  array[n_plots] real tau_plot;
  array[n_yrs] real gamma_year;
  real<lower=0> sigma_plot;
  real<lower=0> sigma_year;
  real<lower=0> shape; 
}

transformed parameters {
  array[n_obs] real<lower=0> mu;  // Renamed from 'p' to 'mu' to match likelihood
  for(i in 1:n_obs) {
    mu[i] = exp(beta_0[species[i], (endo_01[i] + 1)]  
            + beta_clim[species[i], (endo_01[i] + 1)] * climate[i]
            + beta_orig[original[i] + 1] // Added indexing [+1]
            + tau_plot[plot[i]]  
            + gamma_year[year_index[i]]);
  }
}

model {
  // Priors
  shape ~ exponential(1); 
  sigma_plot ~ exponential(1);
  sigma_year ~ exponential(1);
  tau_plot ~ normal(0, sigma_plot);
  gamma_year ~ normal(0, sigma_year);
  beta_orig ~ normal(0, 0.5); // Vectorized prior fir beta_orig
  
  for (i in 1:n_spp) {
    for (j in 1:n_endo) {
      beta_0[i, j] ~ normal(0, 0.5); 
      beta_clim[i, j] ~ normal(0, 0.5);
    }
  }
// Likelihood
  // Note: Vectorized gamma is faster, but shape/mu[i] requires a loop
  for (i in 1:n_obs) {
    y[i] ~ gamma(shape, shape / mu[i]);
  }
}

generated quantities {
  array[n_spp] real endo_effect;
  for (i in 1:n_spp) {
    endo_effect[i] = beta_0[i, 2] - beta_0[i, 1];
  }
}
