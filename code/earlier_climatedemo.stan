data {
  int<lower=0> n_obs;
  int<lower=0, upper=1> y[n_obs];
  int<lower=0> n_yrs;
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0,upper=1> endo_01[n_obs];
  real size[n_obs];
  int<lower=1> year_index[n_obs];
  real climate[n_obs]; 
  int<lower=1> plot[n_obs];
}
parameters {
  real beta_0;//general intercept
  real beta_endo;//endophyte
  real beta_size;//size
  real beta_clim;//climate variable
  real beta_size_endo;//size:endo
  real beta_clim_endo;//climate:endo
  real tau_plot[n_plots];//plot random effects
  real gamma_year[n_yrs];//year random effects
  real<lower=0> sigma_plot;//plot variance
  real<lower=0> sigma_year;//year variance
}
transformed parameters{
  real p[n_obs];
  for(i in 1:n_obs){
  p[i] = beta_0 + beta_size*size[i] + beta_endo*endo_01[i]+ beta_clim*climate[i]
  + beta_size_endo*size[i]*endo_01[i] + beta_endo_clim*endo_01[i]*climate[i]
  + tau_plot[plot[i]] + gamma_year[year_index[i]];
  }
}
model {
  tau_plot ~ normal(0,sigma_plot);
  sigma_plot ~ normal(0, 1);
  //to_vector(beta_0[1,]) ~ normal(meanflow[1],sigma_year);
  //to_vector(beta_0[2,]) ~ normal(meanflow[2],sigma_year);
  gamma_year ~ normal(0,sigma_year)
  sigma_year ~ normal(0, 1); 
  beta_endo ~ normal(0,1); 
  beta_size ~ normal(1,10); # i dont really know what the 0, 1 and 10 represent
  beta_clim ~ normal(0, 1);
  beta_size_endo ~ normal(0,10);
  beta_clim_endo ~ normal(0,10);
  y ~ bernoulli_logit(p);
}
generated quantities { //do we need this amymore?
  real endo_effect[n_yrs]; //if not years, whst would endo effect be indexed by
  for (i in 1:n_yrs) {
    endo_effect[i] = beta_0[2,i] - beta_0[1,i];
  }
}

