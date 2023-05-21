datos <- Microdatos_encuesta_de_percepción_MCV_y_diccionario_de_datos[,c(10,11,13,16,557)]

# 10: Comuna, 11: Sexo, 13: Edad, 16: Estrato, 557: Vulnerabilidad 

head(datos)
as.factor(datos$RVE5)



barplot(datos$RVE5, datos$CS2)
library(rstan)
library(StanHeaders)
library(ggplot2)


Comuna <- datos$Q6
Sexo <- datos$CS1
Edad <- datos$CS2
Estrato <- datos$CCS2
Vulnerabilidad <- datos$RVE5

# Resumen estadístico
summary(Edad)   # Resumen de la variable Edad

# Distribución de frecuencia
table(Comuna)   # Tabla de frecuencia de la variable Comuna
table(Sexo)     # Tabla de frecuencia de la variable Sexo
table(Estrato)  # Tabla de frecuencia de la variable Estrato
table(Vulnerabilidad)  # Tabla de frecuencia de la variable Vulnerabilidad

# Gráficos
barplot(table(Comuna))   # Gráfico de barras para la variable Comuna
pie(table(Sexo))         # Gráfico de sectores para la variable Sexo
hist(Edad)               # Histograma para la variable Edad

# Análisis comparativo
table(Comuna, Sexo)   # Tabla de contingencia entre Comuna y Sexo
table(Estrato, Vulnerabilidad)   # Tabla de contingencia entre Estrato y Vulnerabilidad

# Conclusiones y hallazgos
# En esta sección puedes escribir tus observaciones y conclusiones basadas en los resultados obtenidos, por ejemplo:
# - La variable Edad tiene una media de 46.71 años y una desviación estándar de 18.05 años.
# - La Comuna 14 tiene el mayor número de observaciones, seguida por la Comuna 7 y 15.
# - Se observa una asociación entre el Estrato y la Vulnerabilidad, con un mayor porcentaje de personas vulnerables en el Estrato bajo.



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
