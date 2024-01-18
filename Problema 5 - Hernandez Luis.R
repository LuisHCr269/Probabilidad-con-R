Dado <- (1:6)
mean(Dado)
valor <- 3.4
valorredondeado <- round(valor)
muestra <- sample(x=1:4,size=2)


muestra <- sample(x=1:20, size=1)
muestra

args(round)
round(9.3839292,5) #rdeondea

args(sample)
for(i in Dado){
if(i<4){Dado[i-1]<-Dado[i-1]+1} else{Dado[i-1]<-Dado[i-1]+3}
}

Dado <- (1:20)
contador <- 0
veces <- 1000
Iterador <- 1:veces
for (i in Iterador){ objeto <- sample(Dado,size = 1)
  if (objeto %%2 == 0){
    contador <- contador + 1
  } }
contador
