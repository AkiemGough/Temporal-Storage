data {
  int<lower=0> n_obs;
  array[n_obs] int<lower=0> y;  //GEMINI: Removed upper=1 constraint; y can now be any non-negative integer
  int<lower=0> n_yrs;
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0> n_spp;
  array[n_obs] int<lower=0,upper=1> endo_01;
  array[n_obs] int<lower=0,upper=1> original;
  array[n_obs] real size;
  array[n_obs] int<lower=1> year_index;
  array[n_obs] int<lower=1> plot;
  array[n_obs] int<lower=1> species;
}

parameters {
  real beta_0[n_spp, n_endo, n_yrs]; 
  real tau_plot[n_plots];            
  real beta_size[n_spp];             
  real beta_size_endo[n_spp];        
  real meanflow[n_spp, n_endo];
  real beta_orig;                    
  vector<lower=0>[n_endo] sigma_year[n_spp]; 
  real<lower=0> sigma_plot;          
  array[n_spp] corr_matrix[n_endo] Omega; 
}

transformed parameters {
  real log_lambda[n_obs]; //GEMINI: Renamed 'p' to 'log_lambda' for clarity (representing log-expected counts)
  for(i in 1:n_obs){
    log_lambda[i] = beta_0[species[i], (endo_01[i] + 1), year_index[i]] 
    + beta_size[species[i]] * size[i] 
    + beta_size_endo[species[i]] * size[i] * endo_01[i]
    + beta_orig * original[i]
    + tau_plot[plot[i]];
  }
}

model {
  // Priors
  for (i in 1:n_spp) {
    Omega[i] ~ lkj_corr(2);            
    sigma_year[i] ~ exponential(1);    
  }
  
  for (i in 1:n_spp) {
    matrix[n_endo, n_endo] Sigma_i = quad_form_diag(Omega[i], sigma_year[i]);
    for (t in 1:n_yrs) {
      vector[n_endo] b;
      vector[n_endo] mu_meanflow;
      for (k in 1:n_endo) {
        b[k] = beta_0[i, k, t];
        mu_meanflow[k] = meanflow[i, k]; 
      }
      b ~ multi_normal(mu_meanflow, Sigma_i);
    }
  }

  tau_plot ~ normal(0, sigma_plot);
  sigma_plot ~ exponential(1);

  beta_size ~ normal(1, 10); 
  beta_size_endo ~ normal(0, 10);
  beta_orig ~ normal(1, 10); 

  for (i in 1:n_spp) {
    for (j in 1:n_endo) {
      meanflow[i, j] ~ normal(0, 5);
    }
  }

  // 3. Changed likelihood from bernoulli_logit to poisson_log
  y ~ poisson_log(log_lambda);
}

generated quantities {
  array[n_spp, n_yrs] real endo_effect; 
  for (i in 1:n_spp) {
    for (j in 1:n_yrs) {
      endo_effect[i, j] = beta_0[i, 2, j] - beta_0[i, 1, j];
    }
  }
}
