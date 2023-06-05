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

fit <- stan(file = 'modelo.stan',
            data = stan_data, chains = 4, iter = 2000)

print(fit) #Resumen del modelo, verificando Rhat

traceplot(fit) # Para ver como las cadenas oscilan entorno a los mismos valores

Beta.poste  <- extract(fit, pars = "beta")
Beta.poste  <- Beta.poste[[1]]

mcmc_dens(fit)
mcmc_dens_overlay(fit)

## Graficos HDI para los Beta's
# Para los primeros 6 Beta's (1,6)

dev.new()
# Dimensiones de la pantalla
screen_width <- 1920
screen_height <- 1080

# Número de gráficas
num_graphs <- 6

# Calcula el número de filas y columnas
num_rows <- floor(sqrt(num_graphs))
num_cols <- ceiling(num_graphs / num_rows)

# Calcula el ancho y la altura de cada gráfica
graph_width <- floor(screen_width / num_cols)
graph_height <- floor(screen_height / num_rows)

# Establece el tamaño del área de trazado
par(oma = c(0, 0, 0, 0))  # Margen exterior
par(mfrow = c(num_rows, num_cols))
for(i in 1:6){
  #Inicio
  HDI.interval.beta <- hdi(Beta.poste[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(Beta.poste[,i])
  plot(DENSITITY.BETA, main = "Densidad Posterior", xlab = parse(text=(paste0("beta[",i,"]"))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "#95D5B2")
  #Fin
}

# Para los siguientes Beta's (7,12)
dev.new()
# Dimensiones de la pantalla
screen_width <- 1920
screen_height <- 1080

# Número de gráficas
num_graphs <- 6

# Calcula el número de filas y columnas
num_rows <- floor(sqrt(num_graphs))
num_cols <- ceiling(num_graphs / num_rows)

# Calcula el ancho y la altura de cada gráfica
graph_width <- floor(screen_width / num_cols)
graph_height <- floor(screen_height / num_rows)

# Establece el tamaño del área de trazado
par(oma = c(0, 0, 0, 0))  # Margen exterior
par(mfrow = c(num_rows, num_cols))
for(i in 7:12){
  #Inicio
  HDI.interval.beta <- hdi(Beta.poste[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(Beta.poste[,i])
  plot(DENSITITY.BETA, main = "Densidad Posterior", xlab = parse(text=(paste0("beta[",i,"]"))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "#95D5B2")
  #Fin
}

# Para los siguientes 6 Beta's (13, 18)
dev.new()
# Dimensiones de la pantalla
screen_width <- 1920
screen_height <- 1080

# Número de gráficas
num_graphs <- 6

# Calcula el número de filas y columnas
num_rows <- floor(sqrt(num_graphs))
num_cols <- ceiling(num_graphs / num_rows)

# Calcula el ancho y la altura de cada gráfica
graph_width <- floor(screen_width / num_cols)
graph_height <- floor(screen_height / num_rows)

# Establece el tamaño del área de trazado
par(oma = c(0, 0, 0, 0))  # Margen exterior
par(mfrow = c(num_rows, num_cols))
for(i in 13:18){
  #Inicio
  HDI.interval.beta <- hdi(Beta.poste[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(Beta.poste[,i])
  plot(DENSITITY.BETA, main = "Densidad Posterior", xlab = parse(text=(paste0("beta[",i,"]"))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "#95D5B2")
  #Fin
}


# Para los Beta's restantes (19, 23)
dev.new()
# Dimensiones de la pantalla
screen_width <- 1920
screen_height <- 1080

# Número de gráficas
num_graphs <- 6

# Calcula el número de filas y columnas
num_rows <- floor(sqrt(num_graphs))
num_cols <- ceiling(num_graphs / num_rows)

# Calcula el ancho y la altura de cada gráfica
graph_width <- floor(screen_width / num_cols)
graph_height <- floor(screen_height / num_rows)

# Establece el tamaño del área de trazado
par(oma = c(0, 0, 0, 0))  # Margen exterior
par(mfrow = c(num_rows, num_cols))
for(i in 19:23){
  #Inicio
  HDI.interval.beta <- hdi(Beta.poste[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(Beta.poste[,i])
  plot(DENSITITY.BETA, main = "Densidad Posterior", xlab = parse(text=(paste0("beta[",i,"]"))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "#95D5B2")
  #Fin
}
