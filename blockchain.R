library(digest)
library(gmp)

# Función auxiliar de creación del blockchain
vecToSha256 = function(inptVec){
  resultVec = rep(as.raw(0), length(inptVec)*32)
  
  resultVec[1:32] = digest(as.raw(inptVec[1]), algo="sha256", serialize=FALSE, raw=TRUE)
  counter = 2
  for(n in seq(33,length(inptVec)*32,by=32)){
    resultVec[n:(n+31)] = digest(c(as.raw(inptVec[counter]), resultVec[(n-32):n]), algo="sha256", serialize=FALSE, raw=TRUE)
    counter = counter + 1
  }
  return(resultVec)
}

# función auxiliar, combina el output de 32 bytes de la función digest en un número grande para ser 
# representado gráficamente
raw_to_bigz = function(rawInpt_vec){
  resultLeng = floor(length(rawInpt_vec)/32)
  resultVec = c(as.bigz(0), resultLeng)
  for(n in 1:resultLeng){
    resultVec[n] = as.bigz(paste("0x",paste(rawInpt_vec[((n-1)*32+1):(n*32)] ,collapse = "",sep=""), collapse = "",sep=""))
  }
  return(resultVec)
}

# Ejemplo trivial con 20 valores aleatorios
x = c(1:20)
datos = c(5,34,12,123,88,33,122,7,14,34,234,251,20,9,74,93,192,226,5,12)
datos_hash_raw = vecToSha256(datos)
datos_hash = raw_to_bigz(datos_hash_raw)

# Un actor deshonesto modifica los datos con un cambio muy ligero, el décimo valor pasa de 34 a 35
datos_cambiados = datos
datos_cambiados[10] = 35  #DE 34 => 35
datos_cambiados_hash_raw = vecToSha256(datos_cambiados)
datos_cambiados_hash = raw_to_bigz(datos_cambiados_hash_raw)


# plot de los resultasdos, se ve que la gráfica de los datos en sí (plot izquierdo) no se aprecia el cambio realizado por 
# el actor deshonesto (se ve que los valores originales en azul se corresponded casi identicamente con los
# valores modificados en rojo). Sin embargo, en la gráfica del blockchain se observa como no solo el décimo
# valor sino todo el resto de valores cambian completamente (el blockchain original en azul y el blockchain
# alterado en rojo son completamente distintos).
par(mfrow=c(1,2))
plot(x, datos, col="blue")
lines(x, datos, type = "b", lty = 1, col = "blue")
points(x, datos_cambiados, col = "red", pch="*")
lines(x, datos_cambiados, type = "b", lty = 2, col = "red", pch="*")

plot(x, datos_hash, col="blue")
lines(x, datos_hash, type = "b", lty = 1, col = "blue")
points(x, datos_cambiados_hash, col = "red", pch="*")
lines(x, datos_cambiados_hash, type = "b", lty = 2, col = "red", pch="*")

