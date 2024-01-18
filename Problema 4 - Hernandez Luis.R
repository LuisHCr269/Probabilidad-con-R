# Hernández De la cruz Luis Alberto

# Problema 4, tarea 3

# Hay una urna con 5 bolas blancas y 7 rojas. Cada vez que se saca una bola se vuelve a meter y
# se a ̃nade otra m ́as del mismo color de la que se sac ́o. Con este procedimiento se sacan tres bolas.
# Encuentra (con simulaciones) la probabilidad de que sean...

num_simulaciones <- 10000

evolucion_a <- numeric(num_simulaciones)
evolucion_b <- numeric(num_simulaciones)
evolucion_c <- numeric(num_simulaciones)

bolas_blancas <- 5
bolas_rojas <- 7

# Rojas
a_contador <- 0  
# Exactamente una blanca
b_contador <- 0  
# Todas blancas
c_contador <- 0  

for (i in 1:num_simulaciones) {
  bolas_blancas_sim <- bolas_blancas
  bolas_rojas_sim <- bolas_rojas
  
  extracciones <- character(3)  # Vector para guardar las extracciones
  for (j in 1:3) {
    # Calcular las probabilidades de extracción
    prob_blancas <- bolas_blancas_sim / (bolas_blancas_sim + bolas_rojas_sim)
    prob_rojas <- bolas_rojas_sim / (bolas_blancas_sim + bolas_rojas_sim)
    
    # Realizar la extracción
    bola_extraida <- sample(c("blanca", "roja"), 1, prob = c(prob_blancas, prob_rojas))
    
    
    # Actualizar el número de bolas en la urna
    if (bola_extraida == "blanca") {
      bolas_blancas_sim <- bolas_blancas_sim + 1
    } else {
      bolas_rojas_sim <- bolas_rojas_sim + 1
    }
    
    # Guardar la extracción en el vector
    extracciones[j] <- bola_extraida
  }
  
  # Contar la cantidad de bolas blancas y rojas en las extracciones
  blancas <- sum(extracciones == "blanca")
  rojas <- sum(extracciones == "roja")
  
  # Actualizar los contadores según las condiciones
  if (rojas == 3) {
    a_contador <- a_contador + 1
  }
  if (blancas == 1) {
    b_contador <- b_contador + 1
  }
  if (blancas == 3) {
    c_contador <- c_contador + 1
  }
  
  # Almacenar la probabilidad en cada iteración
  evolucion_a[i] <- a_contador / i
  evolucion_b[i] <- b_contador / i
  evolucion_c[i] <- c_contador / i
}

# Crear gráficos de líneas para seguir la evolución de las probabilidades
plot(1:num_simulaciones, evolucion_a, type = "l", col = "red", xlab = "Iteración", ylab = "Probabilidad", main = "Evolución de Probabilidad (Todas Rojas)")
lines(1:num_simulaciones, evolucion_b, col = "blue")
lines(1:num_simulaciones, evolucion_c, col = "green")
legend("topright", legend = c("Todas Rojas", "Una Blanca", "Todas Blancas"), col = c("red", "blue", "green"), lty = 1)

