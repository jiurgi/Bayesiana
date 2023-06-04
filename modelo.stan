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
  beta ~ normal(0, 5); // Prior normal para los coeficientes

  // Likelihood
  for (i in 1:N) {
    y[i] ~ bernoulli_logit(X[i,] * beta); // Likelihood logístico
  }
}
