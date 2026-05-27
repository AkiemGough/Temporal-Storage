data {
  int<lower=0> n_obs;
  int<lower=0> y[n_obs];  
  int<lower=0> n_yrs;
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0> n_spp;
  int<lower=0,upper=1> endo_01[n_obs];
  int<lower=0,upper=1> original[n_obs];
  vector[n_obs] size; 
  int<lower=1> year_index[n_obs];
  int<lower=1> plot[n_obs];
  int<lower=1> species[n_obs];
}

parameters {
  // We keep beta_0, but we will sample vectors directly out of it in a clean format
  vector[n_endo] beta_0_vec[n_spp, n_yrs]; 
  vector[n_plots] tau_plot;                
  vector[n_spp] beta_size;                 
  vector[n_spp] beta_size_endo;            
  vector[n_endo] meanflow[n_spp]; // Swapped to vector array for direct multi_normal compatibility
  real beta_orig;                                        
  vector<lower=0>[n_endo] sigma_year[n_spp]; 
  real<lower=0> sigma_plot;          
  corr_matrix[n_endo] Omega[n_spp]; 
}

transformed parameters {
  vector[n_obs] log_lambda; 
  for(i in 1:n_obs){
    // Accessing beta_0_vec using the vector indices safely
    log_lambda[i] = beta_0_vec[species[i], year_index[i]][endo_01[i] + 1] 
    + (beta_size[species[i]] * size[i]) 
    + (beta_size_endo[species[i]] * size[i] * endo_01[i])
    + (beta_orig * original[i])
    + tau_plot[plot[i]];
  }
}

model {
  // Global Hyper-priors
  tau_plot ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(1);

  beta_size ~ normal(1, 10); 
  beta_size_endo ~ normal(0, 10);
  beta_orig ~ normal(1, 10); 

  // Species-specific structures
  for (i in 1:n_spp) {
    Omega[i] ~ lkj_corr(2);            
    sigma_year[i] ~ exponential(1);    
    meanflow[i] ~ normal(0, 5); // Direct vector sampling
    
    // Compute the covariance matrix for species i
    matrix[n_endo, n_endo] Sigma_i = quad_form_diag(Omega[i], sigma_year[i]);
    
    for (t in 1:n_yrs) {
      // Sampling the parameter vector directly prevents initialization NaN loops
      beta_0_vec[i, t] ~ multi_normal(meanflow[i], Sigma_i);
    }
  }

  // Likelihood
  y ~ poisson_log(log_lambda);
}

generated quantities {
  real endo_effect[n_spp, n_yrs]; 
  for (i in 1:n_spp) {
    for (j in 1:n_yrs) {
      endo_effect[i, j] = beta_0_vec[i, j][2] - beta_0_vec[i, j][1];
    }
  }
}
