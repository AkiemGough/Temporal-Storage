//climate demo growth
data {
  int<lower=0> n_obs;
  real y[n_obs];
  int<lower=0> n_yrs;
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0> n_spp;
  int<lower=0,upper=1> endo_01[n_obs];
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
  matrix [n_spp, n_endo] beta_prec;       // precipitation main effect
  matrix [n_spp, n_endo] beta_temp;       // temperature main effect
  
  vector [n_plots] tau_plot_raw;
  vector [n_yrs] gamma_year_raw;
  real<lower=0> sigma_plot;
  real<lower=0> sigma_year;
  real<lower=0> sigma; //residual variance
  
  // Split into separate weight simplexes for each climate variable
  array[n_spp] simplex[K] w_prec;         // Precipitation lag weights
  array[n_spp] simplex[K] w_temp;         // Temperature lag weights
  real<lower=2> nu; //student's t df, variance undefined for nu<2
}

transformed parameters {
  vector[n_plots] tau_plot = tau_plot_raw * sigma_plot;
  vector[n_yrs] gamma_year = gamma_year_raw * sigma_year;
  
  vector[n_obs] mu;
  vector[n_obs] weighted_prec;
  vector[n_obs] weighted_temp;

  // Calculate separate weighted environmental indexes per observation
  for(i in 1:n_obs) {
    weighted_prec[i] = precip[i, ] * w_prec[species[i]];
    weighted_temp[i] = temper[i, ] * w_temp[species[i]];
  }

  // Linear predictor using the independent weights
  for(i in 1:n_obs) {
    mu[i] = beta_0[species[i], endo_idx[i]] 
         + beta_prec[species[i], endo_idx[i]] * weighted_prec[i] 
         + beta_temp[species[i], endo_idx[i]] * weighted_temp[i]
         + tau_plot[plot[i]] 
         + gamma_year[year_index[i]];
  }
}

model {
  tau_plot_raw ~ normal(0, 1);
  gamma_year_raw ~ normal(0, 1);
  
  sigma_plot ~ exponential(1);
  sigma_year ~ exponential(1);
  sigma ~ exponential(1);
  nu ~ gamma(2, 0.1);
  
  // Separate Dirichlet priors for each environmental variable
  // Using 2.0 concentration to keep chains stable amidst collinearity
  for (i in 1:n_spp) {
    w_prec[i] ~ dirichlet(rep_vector(2.0, K)); 
    w_temp[i] ~ dirichlet(rep_vector(2.0, K)); 
  }
  
  // Vectorized matrix priors
  to_vector(beta_0) ~ normal(0, 1);  
  to_vector(beta_prec) ~ normal(0, 0.5);
  to_vector(beta_temp) ~ normal(0, 0.5);
  
  y ~  student_t(nu, mu, sigma);
}

generated quantities {
  real y_rep[n_obs];
  for(i in 1:n_obs){
    y_rep[i] = student_t_rng(nu, mu[i], sigma);
  }
}

