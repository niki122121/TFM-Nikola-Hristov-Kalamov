
# ejemplo trivial con 50 datos (bytes) completamente aleatorios (se simula el comportamiento de resultados de un hash).
set.seed(123123)
n = 50
x = c(1:n)
datos = floor(runif(n, 0, 255))


# código para la red neuronal
# ---------------------------------------------------------------------------------------------------------------------------
# IMPORTANTE: este sección se debe ejecutar varias veces hasta obtener el mejor resultado posible. 
# De vez en cuando los pesos no convergen y salta un error, simplemente hay que ejecutar esta sección
# de nuevo.
library(neuralnet)

# ajuste de datos
datos_ajustados = data.frame(datos_scale = scale(datos), x)

nn_result = neuralnet(datos_scale ~ x, data=datos_ajustados, hidden=c(6,6), linear.output=TRUE, threshold=0.015, stepmax=400000)

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


