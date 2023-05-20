datos <- Microdatos_encuesta_de_percepción_MCV_y_diccionario_de_datos[,c(10,11,13,16,557)]

# 10: Comuna, 11: Sexo, 13: Edad, 16: Estrato, 557: Vulnerabilidad 

head(datos)
as.factor(datos$RVE5)



barplot(datos$RVE5, datos$CS2)
library(rstan)
library(StanHeaders)
library(ggplot2)



'''
library(rstan)



modelo <- "
data {
  int<lower=0> N;                // Número de observaciones
  vector[N] edad;                // Variable edad
  int<lower=1, upper=2> sexo[N]; // Variable sexo codificada como 1 = Masculino, 2 = Femenino
  int<lower=1, upper=3> estrato[N]; // Variable estrato socioeconómico codificada como 1 = Bajo, 2 = Medio, 3 = Alto
  int<lower=0, upper=1> y[N];     // Variable dependiente codificada como 0 = No, 1 = Sí
}

parameters {
  real alpha;                    // Intercepto
  real beta_edad;                // Coeficiente de edad
  real beta_sexo;                // Coeficiente de sexo
  real beta_estrato;             // Coeficiente de estrato socioeconómico
}

model {
  vector[N] p;                   // Probabilidades estimadas

  // Prior
  alpha ~ normal(0, 2.5);
  beta_edad ~ normal(0, 2.5);
  beta_sexo ~ normal(0, 2.5);
  beta_estrato ~ normal(0, 2.5);

  // Likelihood
  for (i in 1:N) {
    p[i] = inv_logit(alpha + beta_edad * edad[i] + beta_sexo * sexo[i] + beta_estrato * estrato[i]);
    y[i] ~ bernoulli(p[i]);
  }
}
"

dato <- list(
  N = dim(datos)[1],
  edad = datos$CS2,
  sexo = datos$CS1,
  estrato = datos$CCS2,
  y = datos$RVE5
)

ajuste <- stan(model_code = modelo,
               data = dato,
               iter = 2000,
               chains = 4)

print(ajuste)'''

library(rstan)
library(StanHeaders)
library(ggplot2)
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)



stan_data <- list(N = length(x), x = x, y = y)
model <- stan("modelo.stan")

fit <- sampling(model, data = list(N = length(x), x = x, y = y), iter = 2000, chains = 4)
