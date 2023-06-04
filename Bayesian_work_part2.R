library(dplyr)
library(ggplot2)
library(readxl)
library(HDInterval)
library(rstan)
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

# se convierten las variables que son categoricas en factor
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

fit <- stan(file = 'modelo.stan',
            data = stan_data, chains = 4, iter = 2000)

print(fit) #Resumen del modelo, verificando Rhat

traceplot(fit)

Beta.poste  = extract(modelo, pars = "beta")
Beta.poste  = Beta.poste[[1]]
Sigma.poste = extract(fit2, pars = "sigma2")
Sigma.poste = Sigma.poste[[1]]


# Obtener los coeficientes de regresión y los intervalos de credibilidad
coeficients <- extract(fit, "beta")$beta
lower_ci <- apply(coeficients, 2, quantile, prob = 0.025)
upper_ci <- apply(coeficients, 2, quantile, prob = 0.975)

# Crear un data frame con los coeficientes y los intervalos de credibilidad
df <- data.frame(Coeficiente = colnames(coeficients),
                 Valor = colMeans(coeficients),
                 Lower_CI = lower_ci,
                 Upper_CI = upper_ci)

# Graficar los coeficientes de regresión con intervalos de credibilidad
ggplot(df, aes(x = Coeficiente, y = Valor, ymin = Lower_CI, ymax = Upper_CI)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_pointrange() +
  xlab("Variable Predictora") +
  ylab("Coeficiente") +
  ggtitle("Coeficientes de Regresión con Intervalos de Credibilidad") +
  theme_bw()
