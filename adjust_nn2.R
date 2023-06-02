
# Ejemplo un poco más complejo con 30 ejemplos empleando 5 distribuciones distintas
# siendo la primera y segunda una recta constante (y=0.5 , y=0.8), la tercera una recta inclinada (y=-0.1x + c),
# la cuarta una función cúbica (y=k*x^3 + c) y la final un seno (y=k*sin(x) + c)
# En este caso se puede interpretar como un cambio en el valor 8, 12, 17 y 21, desde los cuales se sigue otras distribuciones
n = 30
x = c(1:n)
d1 = rep(0.5,7)
d2 = rep(0.8,4)
d3 = 1.9 - (0.1*x[12:16])
d4 = 0.0002 * x[17:20]* x[17:20] * x[17:20] - 0.6
d5 = 0.3*sin(x[21:30]) + 0.75
datos = c(d1,d2,d3,d4,d5)

# código para la red neuronal con 3 capas ocultas de 2,3 y 3 neuronas cada una.
# ---------------------------------------------------------------------------------------------------------------------------
# IMPORTANTE: este sección se debe ejecutar varias veces hasta obtener el mejor resultado posible. 
# De vez en cuando los pesos no convergen y salta un error, simplemente hay que ejecutar esta sección
# de nuevo.
library(neuralnet)

# ajuste de datos
datos_ajustados = data.frame(datos_scale = scale(datos), x)

nn_result = neuralnet(datos_scale ~ x, data=datos_ajustados, hidden=c(2,3,3), linear.output=TRUE, threshold=0.015, stepmax=400000)

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


