data {
  int<lower=0> n_obs;
  int<lower=0, upper=1> y[n_obs];
  real<lower=0> n_clim; #should this be integer?
  int<lower=0> n_plots;
  int<lower=0> n_endo;
  int<lower=0,upper=1> endo_01[n_obs];
  real size[n_obs];
  real climate[n_obs]; #should this be integer?
  int<lower=1> plot[n_obs];
}
parameters {
  real beta_0[n_endo,n_clim];//climate random effects, unique to E+ and E-
  real tau_plot[n_plots];//plot random effects
  real beta_size;//size
  real beta_size_endo;//size:endo
  real meanflow[n_endo];
  real<lower=0> sigma_clim;//climate variance
  real<lower=0> sigma_plot;//plot variance
}
transformed parameters{
  real p[n_obs];
  for(i in 1:n_obs){
  p[i] = beta_0[(endo_01[i]+1),climate[i]] 
  + beta_size*size[i] 
  + beta_size_endo*size[i]*endo_01[i]
  + tau_plot[plot[i]];
  }
}
model {
  tau_plot ~ normal(0,sigma_plot);
  sigma_plot ~ normal(0, 1);
  to_vector(beta_0[1,]) ~ normal(meanflow[1],sigma_clim);
  to_vector(beta_0[2,]) ~ normal(meanflow[2],sigma_clim);
  sigma_clim ~ normal(0, 1);
  beta_size ~ normal(1,10);
  beta_size_endo ~ normal(0,10);
  y ~ bernoulli_logit(p);
}
generated quantities {
  real endo_effect[n_clim];
  for (i in 1:n_clim) {
    endo_effect[i] = beta_0[2,i] - beta_0[1,i];
  }
}

