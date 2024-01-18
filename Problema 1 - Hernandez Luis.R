#Contador de casos favorables para el inciso a
contador_a <- 0

#Repeticiones 
N <- 1000

for(i in 1:N){
  r0 <- 7
  b0 <- 5
  r <- r0
  b <- b0
  urna <- c(rep(0,r),rep(1,b))
  #0 roja
  #1 blanca
  
  #Simulamos extracción
  #Repetimos tres veces
  for(i in 1:3){
    bola <- sample(urna,1)
    if(bola==1){
      b <- b + 1
    }
    else{
      r <- r + 1
    }
    #reconstruimos la urna
    urna <- c(rep(0,r),rep(1,b))
  }
  
  #Verificamos si es un caso favorable
  #Inciso a), 
  #Para que las tres sean rojas, debe ocurrir que r sea r0 + 3
  if(r==r0+3){
    contador_a <- contador_a + 1
  }
}

proba_a = contador_a/N
proba_a

proba_real = (7*8*9)/(12*13*14)
proba_real

