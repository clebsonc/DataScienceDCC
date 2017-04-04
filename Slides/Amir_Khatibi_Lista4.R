# Lista 4
----------------------------------------------------------------------------------
  help(distributions)
# Questão 1 
### X ∼ Bin(n = 20, θ = 0.15).
#1
  x <- 0:20
  px <- dbinom(x, 20, 0.15)
  par(mfrow=c(1,2)) # janela grafica com uma linha de 2 plots
  plot(x, px, type = "h") # para usar linhas verticais at\’{e} os pontos (x,px)
  Fx <- pbinom(x, 20, 0.15)
  plot(x, Fx, type = "s") # o argumento "s"

#2
 X = 3 , P(X=3) = 0.24
  
#3
 X pertence intervalo de (0,7)
  
#4  
  E(x) = nθ = 20 * 0.15 = 3
  - Valores entorno valoe esperado tem maior probabilidades em relacao de outros valores para X
  - Ele tem maior probabilidade nesse ponto
  
#5 
  sum(dbinom(0:7, 20, 0.15))  = 0.994 quase igual 1
  pbinom(7,20, 0.15) - pbinom(0, 20, 0.15) = 0.95
  pbinom(7,20, 0.15) - pbinom(0-0.01, 20, 0.15) = 0.994 quase igual 1
  -Subtraiu 0.01 de 5 na chamada da segunda função para nao incluir ponto 5 no pbinomial
  porque pbinom(5, 20, 0.15) calcula a função de probabilidade acumulada F no ponto 5 
  ou seja F(5) = P(X ≤ 5) mas agente nao quer 5 dentro dessa.

#6 
  qbinom(0.95, 20, 0.15) = 6
  Calcula o quantil x associado com a de probabilidade acumulada 0.20
  ou seja calcula o valor de x tal que F(x) = P(X ≤ x) = 0.95
  
#7
  pbinom(6,20, 0.15) = 0.978
  
#8 
  a = rbinom(1000, 20, 0.15)
  plot(a)
  - Sim, maiorias cairam no faixa de (0,7)
  length(a[a <= 7 & a >= 0])/10   => 99.2%
  
#9
  w1 = dbinom(0:6, 20, 0.15)
  w1
  w2 = rbinom(100, 20, 0.15)
  w2
  for(i in 0:6){
    print(length(w2[w2 == i])/100)
  }
  
  sao muito parecidos sim: 
    w1: 0.03875953 0.13679835 0.22933840 0.24282890 0.18212167 0.10284518 0.04537287
    w2: 0.05       0.14       0.15       0.23       0.23       0.13       0.04
  
----------------------------------------------------------------------------------
# Questão 2
  
#1
  par(mfrow=c(2,3)) 
    # λ = 0.73
  x <- 0:10
  px <- dpois(x, 0.73)
  plot(x, px, type = "h") 
  Fx <- dpois(x, 0.73)
  plot(x, Fx, type = "s") 
  plot(x, Fx, type = "o") 
    
    # λ = 10
  x <- 0:40
  px <- dpois(x, 10)
  plot(x, px, type = "h") 
  Fx <- dpois(x, 10)
  plot(x, Fx, type = "s") 
  plot(x, Fx, type = "o") 
      
#2
  com λ = 0.73:, p(X) esta maximo no ponto 0 e 
  com λ = 10:, p(X) esta maximo no ponto 10
  sim, esta bem proximo de valor esperado E(X) = λ  
     
#3
  para λ = 0.73  X pertence intervalo de (0,3)
  para λ = 10    X pertence intervalo de (2,20)
  
#4
  para λ = 0.73:
  ppois(3, 0.73) - ppois(0-0.01, 0.73)   = 0.9933523
  
  para λ = 10:
  ppois(20, 10)  - ppois(2-0.01, 10)     = 0.9979123

#5 
  para λ = 0.73:
  P1 = dpois(0:6, 0.73)
  P1
  P2 = rpois(100, 0.73)
  P2
  for(i in 0:6){
    print(length(P2[P2 == i])/100)
  }
  
  sao parecidos sim: 
    P1: 0.4819089901 0.3517935628 0.1284046504 0.0312451316 0.0057022365 0.0008325265 0.0001012907
    P2: 0.4          0.4          0.12         0.07         0.01         0            0

  
----------------------------------------------------------------------------------
# Questão 3
P(X = k) = C / k^(1+α)
F(k) = P(X ≤ k) = sigma(P(X=i))| 1<i<k = C*sigma(1/i^(1+α))| 1<i<k 
par(mfrow=c(1,2)) 

#1
 α = 1/2 => C = 1/ζ(1+α) => C = 1/2.612 = 0.3828484
 k  = 0:100
 pk = 0.3828484 / (k^1.5)
 plot(k, pk, type = "h") 
 
 
 fk_list <- c()
 for (k in 0:100){ 
  fk=0
  for(i in 1:k){
  fk = fk + (1/(i^1.5))
  }
  fk_list = append(fk_list, 0.3828484 * fk)
 }
 k  = 0:100
 Fk = fk_list
 plot(k, Fk, type = "s") 
 
 -------------
   
 α = 1 => C = 1/ζ(1+α) => C = 1/1.645 = 0.6079027
 k  = 0:100
 pk = 0.6079027 / (k^2)
 plot(k, pk, type = "h") 
 
 
 fk_list <- c()
 for (k in 0:100){ 
   fk=0
   for(i in 1:k){
     fk = fk + (1/(i^2))
   }
   fk_list = append(fk_list, 0.6079027 * fk)
 }
 k  = 0:100
 Fk = fk_list
 plot(k, Fk, type = "s") 
 
 ------------- 
 α = 2 => C = 1/ζ(1+α) => C = 1/1.202 = 0.8319468
 k  = 0:100
 pk = 0.8319468 / (k^3)
 plot(k, pk, type = "h") 
 
 
 fk_list <- c()
 for (k in 0:100){ 
   fk=0
   for(i in 1:k){
     fk = fk + (1/(i^3))
   }
   fk_list = append(fk_list, 0.8319468 * fk)
 }
 k  = 0:100
 Fk = fk_list
 plot(k, Fk, type = "s") 
 
#2
 par(mfrow=c(1,1)) 
 k0  = 0:99
 k1  = 1:100
 pk0 = 0.3828484 / (k0^1.5)
 pk1 = 0.3828484 / (k1^1.5)
 plot(pk1/pk0, type = "s") 
 
 quando k cresce, k/(k + 1) eh sempre menor que 1 mas cada vez mais proximo de 1
 
#3
 Quando α maior, crescimento eh mais rapido e tambem mais rapido, fracao de k/(k + 1) fica perto de 1.
 
#4
 alpha = 2 => C = 1/ζ(1+α) => C = 1/1.202 = 0.8319468
 k  = 0:100
 pk = 0.8319468 / (k^3)
 plot(log(k), log(pk), col = "red") 
 
 abline(log(0.8319468), -(1 + alpha))
 
 a linha eh exatamente acima de plot anterior e isso significa que a linha mostra a inclinacao de plot log
 
#5
 rzipf = function(nsim = 1, alpha = 1, Cte = 1/1.645)
 {
   res = numeric(nsim)
   for(i in 1:nsim){
     x = -1
     k = 1
     F = p = Cte
     U = runif(1)
     while( x == -1){
       if(U < F) x = k
       else{
         p = p * (k/(k+1))^(1+alpha)
         F = F + p
         k = k+1
       }
     }
     res[i] = x
   }
   res
 }
   
 -----------test
 
 rzipf(400) 
 rzipf(400, 1/2, 1/2.62)
 
 
 rzipf(400, 1/2, 1/2.62)
 rzipf(400, 1, 1/1.645)
 rzipf(400, 2, 1/1.202)
 
  resultado de rzipf(400, 1, 1/1.645)
 > rzipf(400, 1, 1/1.645)
 [1]   1  63   3   3   1   1   1   1   2   1   5   1   1   3   3   1   1   1   4   1   2   1   1   1   1
 [26]   1   4   1   3   6   3   3   1   1   1   1   1   1   1   1   1   1   1  24   2   1   1   1   1   2
 [51]   4   2   4   1   1   1   5   2   3   2   1   3  52  22   1  53   1   1   2   1   1   1   1   1   1
 [76]   2  23   2   1   1   1   1   1   1  14   2   7   1   1   1   1   3   3  16   1   4   1   1   1   1
 [101]   2   1   1   1   1   2   1   3   3   7   4   1   2   2  20   8   4   1   1   1   2   1   1   1   1
 [126]   2   1   1   2   1   1   1   1   1   3   1   1   1   1   1   1  34   1   2   2   1   1   1   1   1
 [151]   1   1   1   2   1   2   1  23   2   1   5   1   2   1   2   1   1   2   2   9   4   1   1   1   5
 [176]   1   1   1   1   1   1 113   1   1   1   1   1   1   1   1   7   7  70   1   2  80   1   4   1   1
 [201]   1   8   1   2   5   1   6  10   1   1   7   4   1   2   6   1   1   1   1   2   4   1   1   1   1
 [226]   1   3   1   1   6   1   1   3   2   8   1  19   2   2   1   4   1   1   2   1   1   4   1   1   1
 [251]   1   1   7   8   1  19   1   1   5   3   1   1   2   6  10   1   1   4   2   2   3   1   1  47   3
 [276]   5   2   2   1   1   1   1   4   1   1   5   1   3   1   1   1   2   1   1   2   1   7   1  10   1
 [301]   1   1   3   2   5   2   1   2   2   2  22   1   2   2   1   1   1   1   1   1   1   3   2   2   1
 [326]   1   3   1   1   1   1   3   2   1   1   1   2   2   1   1   1   2   1   1   1   1   1   5   1   2
 [351]   8   2   1   3   2   1   1   1   1   1   1   1   2   1   1   1   2   1   1  15   1   1   1   1  18
 [376]   1   1   1   2   2   1   8   1   1   2   1   1   5   3   4   1   1   1   2   5   1   3   4   1   1











      