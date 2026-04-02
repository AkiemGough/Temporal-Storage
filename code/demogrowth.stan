//climate demo growth
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
  real beta_0[n_spp,n_endo];//species random effects unique to E+ and E-
  real tau_plot[n_plots];//plot random effects
  real gamma_year[n_yrs];//year random effects
  real<lower=0> sigma_plot;//plot variance -- shared across species
  real<lower=0> sigma_year;//year variance -- shared across species
  real<lower=0> sigma; //residual variance
}

transformed parameters{
  real mu[n_obs];
  for(i in 1:n_obs){
  mu[i] = beta_0[species[i],(endo_01[i]+1)] 
  + tau_plot[plot[i]] 
  + gamma_year[year_index[i]];
  }
}

model {
  tau_plot ~ normal(0,sigma_plot);
  sigma_plot ~ exponential(1);
  gamma_year ~ normal(0,sigma_year);
  sigma_year ~ exponential(1);
  sigma ~ exponential(1);
  for (i in 1:n_spp) {
    for (j in 1:n_endo) {
      beta_0[i,j] ~ normal(0,1);
    }
  }
  y ~ normal(mu, sigma);
}

generated quantities {
  real endo_effect[n_spp];
  for (i in 1:n_spp) {
    endo_effect[i] = beta_0[i,2] - beta_0[i,1];
    }
  }

