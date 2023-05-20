data {
  int<lower=0> N; // Número de observaciones
  vector[N] x;    // Variable independiente
  vector[N] y;    // Variable dependiente
}

parameters {
  real alpha;     // Término de intersección
  real beta;      // Término de pendiente
  real<lower=0> sigma;  // Desviación estándar
}

model {
  y ~ normal(alpha + beta * x, sigma);  // Modelo lineal
}

