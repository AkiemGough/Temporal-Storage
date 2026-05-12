data {
  int<lower=0> n_obs;
  real y[n_obs];
  int<lower=0> n_yrs;
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0> n_spp;
  int<lower=0,upper=1> endo_01[n_obs];
  int<lower=1> year_index[n_obs];
  int<lower=1> plot[n_obs];
  int<lower=1> species[n_obs];
}

parameters {
  matrix[n_endo, n_yrs] beta_raw[n_spp]; // Standard Normal "z-scores"
  vector[n_plots] tau_raw;               // For plot effects
  cholesky_factor_corr[n_endo] Omega[n_spp]; 
  // ... (keep sigma, sigma_plot, sigma_year, meangrow)
  real meangrow[n_spp,n_endo];//
  vector<lower=0>[n_endo] sigma_year[n_spp];// separate SD for each endo level within species
  real<lower=0> sigma_plot;//plot variance -- shared across species
  real<lower=0> sigma; //residual variance
}

transformed parameters {
  real beta_0[n_spp, n_endo, n_yrs];
  vector[n_plots] tau_plot = tau_raw * sigma_plot; // Non-centered plot effects
  
  for (i in 1:n_spp) {
    // scale and correlate the raw z-scores
    matrix[n_endo, n_endo] L_Sigma = diag_pre_multiply(sigma_year[i], Omega[i]);
    for (t in 1:n_yrs) {
      vector[n_endo] b = to_vector(meangrow[i]) + L_Sigma * col(beta_raw[i], t);
      for (k in 1:n_endo) beta_0[i, k, t] = b[k];
    }
  }
  // ... (keep your mu calculation loop)
  real mu[n_obs];
  for(i in 1:n_obs){
  mu[i] = beta_0[species[i],(endo_01[i]+1),year_index[i]] 
  + tau_plot[plot[i]];
  }
}

model {
  for (i in 1:n_spp) {
    Omega[i] ~ lkj_corr_cholesky(2);
    to_vector(beta_raw[i]) ~ std_normal(); // Standard Normal prior
    sigma_year[i] ~ exponential(1);
  }
  
  tau_raw ~ std_normal(); // Non-centered plot effect prior
  sigma_plot ~ exponential(1);
  sigma ~ exponential(1); // Don't forget a prior for sigma!
  
  // Likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  real endo_effect[n_spp,n_yrs];
  for (i in 1:n_spp) {
    for (j in 1:n_yrs) {
    endo_effect[i,j] = beta_0[i,2,j] - beta_0[i,1,j];
    }
  }
  
  real y_rep[n_obs];
  for(i in 1:n_obs){
    y_rep[i] = normal_rng(mu[i], sigma);
  }
}

