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
  // Non-centered primitive components
  matrix[n_endo, n_yrs] beta_0_raw[n_spp]; 
  vector[n_plots] tau_plot_raw;            
  vector[n_spp] beta_size;                 
  vector[n_spp] beta_size_endo;            
  vector[n_endo] meanflow[n_spp];
  real beta_orig;                                        
  vector<lower=0>[n_endo] sigma_year[n_spp]; 
  real<lower=0> sigma_plot;          
  cholesky_factor_corr[n_endo] L_Omega[n_spp]; // Cholesky factor for stability
}

transformed parameters {
  vector[n_endo] beta_0[n_spp, n_yrs];
  vector[n_plots] tau_plot;
  real p[n_obs];

  // 1. Non-center the plot random effects
  tau_plot = tau_plot_raw * sigma_plot;

  // 2. Non-center the Multivariate Year effects via Cholesky factor
  for (i in 1:n_spp) {
    matrix[n_endo, n_endo] L_Sigma_i = diag_pre_multiply(sigma_year[i], L_Omega[i]);
    for (t in 1:n_yrs) {
      beta_0[i, t] = meanflow[i] + L_Sigma_i * col(beta_0_raw[i], t);
    }
  }

  // 3. Compute the linear predictor
  for(i in 1:n_obs){
    p[i] = beta_0[species[i], year_index[i]][endo_01[i] + 1] 
    + beta_size[species[i]] * size[i] 
    + beta_size_endo[species[i]] * size[i] * endo_01[i]
    + beta_orig * original[i]
    + tau_plot[plot[i]];
  }
}

model {
  // Priors for Non-centered parameters (Standard Normals)
  tau_plot_raw ~ normal(0, 1);
  sigma_plot ~ exponential(1);

  for (i in 1:n_spp) {
    L_Omega[i] ~ lkj_corr_cholesky(2); // Much faster and more stable than lkj_corr
    sigma_year[i] ~ exponential(1);    
    meanflow[i] ~ normal(0, 5);
    to_vector(beta_0_raw[i]) ~ normal(0, 1); 
  }

  // Fixed effects priors
  beta_size ~ normal(1, 10); 
  beta_size_endo ~ normal(0, 10);
  beta_orig ~ normal(1, 10); 

  // Likelihood
  y ~ bernoulli_logit(p);
}

generated quantities {
  int y_rep[n_obs];                // Declaration using classic rstan syntax compatibility
  matrix[n_endo, n_endo] Omega[n_spp]; 
  matrix[n_spp, n_yrs] endo_effect;

  // 1. Recover original correlation matrices
  for(i in 1:n_spp) {
    Omega[i] = multiply_lower_tri_self_transpose(L_Omega[i]);
  }

  // 2. Generate posterior predictive distributions
  for(i in 1:n_obs){
    y_rep[i] = bernoulli_logit_rng(p[i]);
  }

  // 3. Calculate endo effects
  for (i in 1:n_spp) {
    for (j in 1:n_yrs) {
      endo_effect[i, j] = beta_0[i, j][2] - beta_0[i, j][1];
    }
  }
}

