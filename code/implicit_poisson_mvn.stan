data {
  int<lower=0> n_obs;
  int<lower=0> y[n_obs];                     // 1. CHANGED: removed <upper=1> since y is now counts
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
  real p[n_obs]; // Note: you might want to rename this to 'log_lambda' for clarity, but keeping 'p' means fewer changes.

  // 1. Non-center the plot random effects
  tau_plot = tau_plot_raw * sigma_plot;

  // 2. Non-center the Multivariate Year effects via Cholesky factor
  for (i in 1:n_spp) {
    matrix[n_endo, n_endo] L_Sigma_i = diag_pre_multiply(sigma_year[i], L_Omega[i]);
    for (t in 1:n_yrs) {
      beta_0[i, t] = meanflow[i] + L_Sigma_i * col(beta_0_raw[i], t);
    }
  }

  // 3. Compute the linear predictor (now on the log-scale)
  vector[n_obs] log_lambda; 
  for(i in 1:n_obs){
    log_lambda[i] = beta_0[species[i], year_index[i]][endo_01[i] + 1] 
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
    L_Omega[i] ~ lkj_corr_cholesky(2); 
    sigma_year[i] ~ exponential(1);    
    meanflow[i] ~ normal(0, 2);             // 4. ADJUSTED: Tightened from normal(0,5) because log-scale changes fast
    to_vector(beta_0_raw[i]) ~ normal(0, 1); 
  }

  // Fixed effects priors (Adjusted SDs from 10 to 2 or 3 for log scale stability)
  beta_size ~ normal(0, 2); 
  beta_size_endo ~ normal(0, 2);
  beta_orig ~ normal(0, 2); 

  // Likelihood
  y ~ poisson_log(log_lambda);                     
}

generated quantities {
  int y_rep[n_obs]; 
  matrix[n_endo, n_endo] Omega[n_spp]; 
  matrix[n_spp, n_yrs] endo_effect;

  // 1. Recover original correlation matrices
  for(i in 1:n_spp) {
    Omega[i] = multiply_lower_tri_self_transpose(L_Omega[i]);
  }

  // 2. Generate posterior predictive distributions
  for(i in 1:n_obs){
    y_rep[i] = poisson_log_rng(log_lambda[i]);     
  }

  // 3. Calculate endo effects
  for (i in 1:n_spp) {
    for (j in 1:n_yrs) {
      endo_effect[i, j] = beta_0[i, j][2] - beta_0[i, j][1];
    }
  }
}
