// Stan model for estimation of temporal changes

data {
  int < lower = 1 > N; // Number of Bones and teeth
  int < lower = 1 > NT; // Number of time intervals
  vector[N] y_mean; // Means of Normal distributions of isotopic values
  vector[N] y_sigma; // SD of Normal distributions of isotopic values
  real t[NT]; // index indicating the time
  matrix[N, NT] x; // predictor matrix containing the renewal percentage for each interval and bone
  
  // Hyperparameters
  // int < lower = 0 > mu_df;
  // real mu_mean;
  // real < lower = 0 > mu_sd;
  // real rho_mean;
  // real < lower = 0 > rho_sd;
  // real alpha_mean;
  // real < lower = 0 > alpha_sd;
}

parameters {
  real mu;
  real<lower=0> alpha;
  real<lower=0> rho;
  vector[NT] interval; // regression coefficients (estimators of isotopic values for each interval)
}
transformed parameters{
  vector[NT] muInt;
  matrix[NT, NT] sdInt;
  vector[N] sdY;
  vector[N] meanY;
  meanY = x * interval;
  sdY = y_sigma;
  muInt = rep_vector(mu, NT);
  sdInt = cholesky_decompose(cov_exp_quad(t, alpha, rho));
}
model {
  // priors
  mu ~ student_t(3,0,20); // df, mean, sd
  rho ~ normal(1,0.25);
  alpha ~ normal(2, 0.5);
  interval ~ multi_normal_cholesky(muInt, sdInt);
  // likelihood
  y_mean ~ normal(meanY , sdY); 
}
