data {
  int<lower=0> N; // Número de observaciones
  int<lower=0> K; // Número de variables predictoras
  matrix[N, K] X; // Matriz de variables predictoras
  int<lower=0, upper=1> y[N]; // Variable respuesta binaria
}

parameters {
  vector[K] beta; // Coeficientes de regresión
}

model {
  // Prior
  beta ~ normal(0, 100); // Prior normal para los coeficientes

  // Likelihood
  y ~ bernoulli_logit(X * beta); // Likelihood logístico
}
