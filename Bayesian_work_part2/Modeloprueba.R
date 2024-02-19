library(dplyr)
library(ggplot2)
library(readxl)
library(HDInterval)
library(rstan)
library(bayesplot)
Microdatos_encuesta_de_percepción_MCV_y_diccionario_de_datos <- read_excel("Microdatos encuesta de percepción MCV y diccionario de datos.xlsx")

datos <- Microdatos_encuesta_de_percepción_MCV_y_diccionario_de_datos[,c(10,11,13,16,557)]

# 10: Comuna, 11: Sexo, 13: Edad, 16: Estrato, 557: Vulnerabilidad

Comuna <- datos$Q6
Sexo <- datos$CS1
Edad <- datos$CS2
Estrato <- datos$CCS2
Vulnerabilidad <- datos$RVE5


# Codificar Vulnerabilidad y Sexo como 0 y 1
Vulnerabilidad <- ifelse(Vulnerabilidad == 1, 0, 1)
Sexo <- ifelse(Sexo == 1, 0, 1)

x1 <- as.factor(Comuna)
x2 <- as.factor(Sexo)
x3 <- Edad
x4 <- as.factor(Estrato)

# Prepara los datos para el modelo
N <- nrow(datos)
K <- 4  # Número de variables predictoras en tus datos
X <- model.matrix(~ x1 + x2 + x3 + x4)  #Matriz de datos predictores
y <- as.vector(Vulnerabilidad)

stan_data <- list(
  "X" = X,
  "y" = y,
  "N" = N, # Numero de observaciones
  "K" = ncol(X) # numero de varaibles
)

ffit <- stan(file = 'modeloprueba.stan',
            data = stan_data, chains = 3, iter = 10000)

print(ffit) #Resumen del modelo, verificando Rhat

traceplot(ffit) # Para ver como las cadenas oscilan entorno a los mismos valores

# Primer modelo reducido
X2 <- model.matrix(~ x1)  #Matriz de datos predictores

stan_data2 <- list(
  "X" = X2,
  "y" = y,
  "N" = N, # Numero de observaciones
  "K" = ncol(X2) # numero de varaibles
)

ffit2 <- stan(file = 'modeloprueba.stan',
             data = stan_data2, chains = 3, iter = 10000)

print(ffit2)

### Prueba metodo Efficient approximate leave-one-out cross-validation (LOO)
library(loo)

log_lik <- extract_log_lik(ffit, merge_chains = F)
r_eff <- relative_eff(exp(log_lik), cores = 2)

loo <- loo(log_lik, r_eff = r_eff, cores = 2)

log_lik2 <- extract_log_lik(ffit2, merge_chains = F)
r_eff2 <- relative_eff(exp(log_lik2), cores = 2)

loo2 <- loo(log_lik2, r_eff = r_eff2, cores = 2)

comp <- loo_compare(loo, loo2)