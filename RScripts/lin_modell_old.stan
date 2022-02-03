// Stan model for estimation of temporal changes

data {
  int < lower = 1 > N; // Number of Bones and teeth
  int < lower = 1 > T; // Number of time intervals
  vector[N] y_mean; // Means of Normal distributions of isotopic values
  vector[N] y_sigma; // SD of Normal distributions of isotopic values
  real t[T]; // index indicating the time
  matrix[N, T] x; // predictor matrix containing the renewal percentage for each interval and bone
}

transformed data {
  matrix[T, T] K = cov_exp_quad(t, 2.0, 1.0); // GP Kernelmatrix
  vector[T] mu = rep_vector(mean(y_mean), T);
  for (i in 1:T)
    K[T, T] = K[T, T] + 0.5;
}

parameters {
  vector[T] interval; // regression coefficients (estimators of isotopic values for each interval)
}

model {
  interval ~ multi_normal(mu, K); // prior
  y_mean ~ normal(x * interval , y_sigma); // likelihood
}
