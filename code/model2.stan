data {
  int<lower=0> n_obs;
  int<lower=0, upper=1> y[n_obs];
  int<lower=0> n_yrs;
  int<lower=0> n_plots; 
  int<lower=1> n_spec; //I DONT REALLY UNDERSTAND
  int<lower=0> n_orig; //JUST MAKING STUFF UP
  int<lower=0> n_endo;
  int<lower=0,upper=1> endo_01[n_obs];
  real size[n_obs]; //this is just stylistic colors, its all plain text
  int<lower=1> year_index[n_obs];
  int<lower=1> plot[n_obs];
  int<lower=1> spec[n_obs];
}
parameters {
  real beta_0[n_spec,n_endo,n_yrs];//year random effects, unique to E+ and E- //functional to maybe put species here
  real tau_plot[n_plots];//plot random effects
  real beta_size[n_spec];//size
  real beta_size_endo[n_spec];//size:endo
  real beta_spec_size_endo;//DONT REALLY KNOW WHAT I AM DOING
  real meanflow[n_endo];
  real<lower=0> sigma_year;//year variance
  real<lower=0> sigma_plot;//plot variance
}
//akiem asking: does spec get added as a parameter as well, how?

transformed parameters{
  real p[n_obs];
  for(i in 1:n_obs){
  p[i] = beta_0[(endo_01[i]+1),year_index[i]] 
  + beta_size*size[i] 
  + beta_size_endo*size[i]*endo_01[i]
  + beta_species*spec[i]
  +
  + tau_plot[plot[i]];
  }
}

#akiem asking: can you break down how the model was written?

model {
  tau_plot ~ normal(0,sigma_plot);
  sigma_plot ~ normal(0, 1);
  to_vector(beta_0[1,]) ~ normal(meanflow[1],sigma_year);
  to_vector(beta_0[2,]) ~ normal(meanflow[2],sigma_year);
  sigma_year ~ normal(0, 1);
  beta_size ~ normal(1,10);
  beta_size_endo ~ normal(0,10);
  y ~ bernoulli_logit(p);
}
generated quantities {
  real endo_effect[n_yrs];
  for (i in 1:n_yrs) {
    endo_effect[i] = beta_0[2,i] - beta_0[1,i];
  }
}
