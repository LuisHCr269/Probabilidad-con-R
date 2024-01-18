probabilidad_pares_seis <- function(n) {
  contador_pares_seis <- 0
  total_simulaciones <- 100000  # Número de simulaciones
  
  for (i in 1:total_simulaciones) {
    pares_seis <- 0
    
    for (j in 1:n) {
      dado1 <- sample(1:6, 1, replace = TRUE)
      dado2 <- sample(1:6, 1, replace = TRUE)
      
      if (dado1 == 6 && dado2 == 6) {
        pares_seis <- pares_seis + 1
      }
    }
    
    if (pares_seis >= 1) {
      contador_pares_seis <- contador_pares_seis + 1
    }
  }
  
  probabilidad <- contador_pares_seis / total_simulaciones
  return(probabilidad)
}

n <- c(3, 10, 100)

for (valor_n in n) {
  probabilidad <- probabilidad_pares_seis(valor_n)
  print(paste("Para n =", valor_n, "la probabilidad aproximada es:", probabilidad))
}
