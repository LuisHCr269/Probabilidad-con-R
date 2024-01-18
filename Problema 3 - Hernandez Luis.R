# Hernandez De la cruz Luis Alberto

tiros <- function(r, k, n_simulaciones = 10000) {
  resultados <- integer(n_simulaciones)
  
  for (i in 1:n_simulaciones) {
    lanzamientos <- 0
    aguilas <- 0
    
    while (aguilas < r) {
      lanzamiento <- sample(c("águila", "sol"), 1)
      lanzamientos <- lanzamientos + 1
      
      if (lanzamiento == "águila") {
        aguilas <- aguilas + 1
      }
    }
    
    resultados[i] <- lanzamientos
  }
  
  probabilidad_k <- sum(resultados == k) / n_simulaciones
  return(probabilidad_k)
}

# Calcula la probabilidad para k = 3
probabilidad_k_3 <- tiros(r = 3, k = 3)
cat("Aproximación de P[X = 3]:", probabilidad_k_3, "\n")

# Calcula la probabilidad para k = 10
probabilidad_k_10 <- tiros(r = 3, k = 10)
cat("Aproximación de P[X = 10]:", probabilidad_k_10, "\n")
