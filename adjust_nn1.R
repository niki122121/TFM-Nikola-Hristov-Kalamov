
# Ejemplo trivial con 20 ejemplos empleando dos distribuciones distintas
# cada una siendo una recta constante (y=0.5, y=0.8).
# En este caso se puede interpretar como un cambio en el valor 8, desde el cual se sigue otra distribución
n = 20
x = c(1:n)
d1 = rep(0.5,7)
d2 = rep(0.8,13)
datos = c(d1,d2)


# código para la red neuronal con 2 capas ocultas cada una de 2 neuronas
# ---------------------------------------------------------------------------------------------------------------------------
# IMPORTANTE: este sección se debe ejecutar varias veces hasta obtener el mejor resultado posible. 
# De vez en cuando los pesos no convergen y salta un error, simplemente hay que ejecutar esta sección
# de nuevo.
library(neuralnet)

# ajuste de datos
datos_ajustados = data.frame(datos_scale = scale(datos), x)

nn_result = neuralnet(datos_scale ~ x, data=datos_ajustados, hidden=c(2,2), linear.output=TRUE, threshold=0.015, stepmax=400000)

# predicción dentro de muestra
prediccion = predict(nn_result, datos_ajustados)

# gráficas
par(mfrow=c(1,1))
plot(x, datos_ajustados$datos_scale, col="blue")
lines(x, datos_ajustados$datos_scale, type = "b", lty = 1, col = "blue")
points(x, prediccion, col = "red", pch="*")
lines(x, prediccion, type = "b", lty = 2, col = "red", pch="*")
# ---------------------------------------------------------------------------------------------------------------------------



# Cálculo de ERRORES:
# parámetro "error" interno de la librería neuralnet, cuanto menor la red es más precisa.
nn_result$result.matrix[1,]

# cálculo de la precisión de los datos, cuanto mayor y más cercano a 1 la red es más precisa.
desviacion=((datos_ajustados$datos_scale-prediccion[,1])/datos_ajustados$datos_scale)
precision=1-abs(mean(desviacion))
precision

# correlación de los datos originales con los predichos, cuanto mayor y más cercano a 1 la red es más precisa.
cor(datos_ajustados$datos_scale, prediccion[,1])


# representación de la red neuronal resultante
plot(nn_result)


