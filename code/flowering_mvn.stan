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
  real beta_0[n_spp,n_endo,n_yrs];//year random effects, unique to E+ and E-
  real tau_plot[n_plots];//plot random effects
  real beta_size[n_spp];//size
  real beta_size_endo[n_spp];//size:endo
  real meanflow[n_spp,n_endo];
  real beta_orig;//original effect
  vector<lower=0>[n_endo] sigma_year[n_spp];// separate SD for each endo level within species
  real<lower=0> sigma_plot;//plot variance -- shared across species
  array[n_spp] corr_matrix[n_endo] Omega; // correlation matrix for each species
}
transformed parameters{
  real p[n_obs];
  for(i in 1:n_obs){
  p[i] = beta_0[species[i],(endo_01[i]+1),year_index[i]] 
  + beta_size[species[i]]*size[i] 
  + beta_size_endo[species[i]]*size[i]*endo_01[i]
  + beta_orig*original[i]
  + tau_plot[plot[i]];
  }
}
model {
    // Priors for Omega and sigma_year
  for (i in 1:n_spp) {
    Omega[i] ~ lkj_corr(2);            
    sigma_year[i] ~ exponential(1);    
  }
  
  // Hierarchical prior on beta_0
  for (i in 1:n_spp) {
    // species-specific covariance matrix:
    // Sigma_i[j,k] = Omega[i][j,k] * sd_i[j] * sd_i[k]
    matrix[n_endo, n_endo] Sigma_i = quad_form_diag(Omega[i], sigma_year[i]);

    for (t in 1:n_yrs) {
      //need to make a stand-in vector (b) because multi_normal cannot handle arrays
      vector[n_endo] b;
      for (k in 1:n_endo){b[k] = beta_0[i,k,t];}
      b ~ multi_normal(to_vector(meanflow[i,]), Sigma_i);
    }
  }
  //plot effects
  tau_plot ~ normal(0,sigma_plot);
  sigma_plot ~ exponential(1);
  //other coefficients
  beta_size ~ normal(1,10); // more likely positive
  beta_size_endo ~ normal(0,10);
  beta_orig ~ normal(1,10); // more likely positive
  for (i in 1:n_spp) {
    for (j in 1:n_endo) {
      meanflow[i,j] ~ normal(0, 5);
    }
}
  y ~ bernoulli_logit(p);
}

generated quantities {
  real endo_effect[n_spp,n_yrs];
  for (i in 1:n_spp) {
    for (j in 1:n_yrs) {
    endo_effect[i,j] = beta_0[i,2,j] - beta_0[i,1,j];
    }
  }
}

