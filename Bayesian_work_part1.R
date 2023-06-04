datos <- Microdatos_encuesta_de_percepción_MCV_y_diccionario_de_datos[,c(10,11,13,16,557,256,242)]

# 10: Comuna, 11: Sexo, 13: Edad, 16: Estrato, 557: Vulnerabilidad, 256: Tiempo de viaje al trabajo, 242: # de libros leidos


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
Libros_ultimo_anio <- datos$MV1
Tiempo_viaje_trabajo <- datos$CR1A


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

## Otros graficos:
# Reemplazar los valores N/A por NA en las variables Libros_ultimo_anio y Tiempo_viaje_trabajo
Libros_ultimo_anio[Libros_ultimo_anio == "N/A"] <- NA
Tiempo_viaje_trabajo[Tiempo_viaje_trabajo == "N/A"] <- NA

# Convertir las variables a tipo numérico
Libros_ultimo_anio <- as.numeric(Libros_ultimo_anio)
Tiempo_viaje_trabajo <- as.numeric(Tiempo_viaje_trabajo)

# 2. Variable Edad
ggplot(datos, aes(x = Edad, fill = factor(Vulnerabilidad))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Edad según Vulnerabilidad", x = "Edad", y = "Densidad") +
  scale_fill_discrete(name = "Vulnerabilidad") +
  scale_fill_brewer(palette = "green", labels = c("Sí","No"))

# Visualización de la distribución de la variable Comuna según la Vulnerabilidad
ggplot(datos, aes(x = factor(Comuna))) +
  geom_bar(aes(fill = factor(Vulnerabilidad)), color = "black", bins = 20) +
  labs(x = "Comuna", y = "Frecuencia", fill = "Vulnerabilidad") +
  ggtitle("Distribución de Comuna según Vulnerabilidad") +
  scale_fill_discrete(name = "Vulnerabilidad") +
  scale_fill_brewer(palette = "green", labels = c("Sí","No"))

# Visualización de la distribución de la variable Edad según la Vulnerabilidad
ggplot(datos, aes(x = Edad)) +
  geom_histogram(aes(fill = factor(Vulnerabilidad)), color = "black", bins = 20) +
  labs(x = "Edad", y = "Frecuencia", fill = "Vulnerabilidad") +
  ggtitle("Distribución de Edad según Vulnerabilidad") +
  scale_fill_discrete(name = "Vulnerabilidad") +
  scale_fill_brewer(palette = "green", labels = c("Sí","No"))

# Visualización de la distribución de la variable Estrato según la Vulnerabilidad
ggplot(datos, aes(x = factor(Estrato))) +
  geom_bar(aes(fill = factor(Vulnerabilidad)), color = "black", bins = 20) +
  labs(x = "Estrato Socioeconómico", y = "Frecuencia", fill = "Vulnerabilidad") +
  ggtitle("Distribución de Estrato Socioeconómico según Vulnerabilidad") +
  scale_fill_discrete(name = "Vulnerabilidad") +
  scale_fill_brewer(palette = "green", labels = c("Sí","No"))

# Visualización de la distribución de la variable Sexo según la Vulnerabilidad
ggplot(datos, aes(x = factor(Sexo))) +
  geom_bar(aes(fill = factor(Vulnerabilidad)), color = "black", bins = 20) +
  labs(x = "Sexo", y = "Frecuencia", fill = "Vulnerabilidad") +
  scale_x_discrete(labels = c("Hombres", "Mujeres")) +
  ggtitle("Distribución de Sexo según Vulnerabilidad") +
  scale_fill_discrete(name = "Vulnerabilidad") +
  scale_fill_brewer(palette = "green", labels = c("Sí","No"))


# Definir los límites de los rangos de edad
edades <- seq(18, 92, by = 10)
etiquetas_edades <- paste(edades, edades + 9, sep = "-")

# Crear una nueva variable que represente los rangos de edad
datos$rango_edad <- cut(Edad, breaks = c(edades, Inf), labels = etiquetas_edades, include.lowest = TRUE)

# Visualización de la distribución de la variable Edad por rangos
ggplot(datos, aes(x = rango_edad)) +
  geom_bar(fill = "#95D5B2", color = "black") +
  labs(x = "Rangos de Edad", y = "Frecuencia") +
  ggtitle("Distribución de Edad por Rangos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x para mayor legibilidad

# Visualización de la distribución de la variable "Cuántos libros leyó en el último año" según la Comuna
ggplot(datos, aes(x = factor(Comuna), y = Libros_ultimo_anio_corr)) +
  geom_boxplot(fill = "#52B788") +
  labs(x = "Comuna", y = "Número de libros leídos en el último año") +
  ggtitle("Distribución de libros leídos en el último año por Comuna")

# Visualización de la distribución de la variable "Cuánto tiempo se demora usted en su viaje de ida al trabajo" según la Vulnerabilidad
ggplot(datos, aes(x = factor(Vulnerabilidad), y = Tiempo_viaje_trabajo)) +
  geom_boxplot(fill = "#D8F3DC") +
  labs(x = "Vulnerabilidad", y = "Tiempo de viaje de ida al trabajo") +
  ggtitle("Distribución de tiempo de viaje de ida al trabajo por Vulnerabilidad")

ggplot(datos, aes(x = Libros_ultimo_anio_corr)) +
  geom_histogram(fill = "#95D5B2", color = "black", bins = 10) +
  labs(x = "Número de libros leídos en el último año", y = "Frecuencia") +
  ggtitle("Distribución de libros leídos en el último año")

# Visualización de la distribución de la variable "Cuánto tiempo se demora usted en su viaje de ida al trabajo"
ggplot(datos, aes(x = Tiempo_viaje_trabajo)) +
  geom_histogram(fill = "#D8F3DC", color = "black", bins = 10) +
  labs(x = "Tiempo de viaje de ida al trabajo", y = "Frecuencia") +
  ggtitle("Distribución de tiempo de viaje de ida al trabajo")
  
 
