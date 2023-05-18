datos <- Microdatos_encuesta_de_percepción_MCV_y_diccionario_de_datos[,c(10,11,13,16,557)]

# 10: Comuna, 11: Sexo, 13: Edad, 16: Estrato, 557: Vulnerabilidad 

head(datos)
as.factor(datos$RVE5)



barplot(datos$RVE5, datos$CS2)



