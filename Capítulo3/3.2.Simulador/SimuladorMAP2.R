# SIMULACIÓN DEL MAP

# Función Mrp2: Calcula el vector estacionario phi y la matriz de 
# transición P_star para un proceso MAP2.

# lambda <- vector de tasas de tiempos exponenciales
# P0 <- matriz de probabilidades de transición sin llegada
# P1 <- " con llegada

Mrp2 <- function(lambda, P0, P1) {
  I2 <- diag(2) # crea la matriz identidad 2x2
  P_star <- solve(I2 - P0) %*% P1 # calcula la P estrella
  phi1 <- P_star[2,1] / (1 - P_star[1,1] + P_star[2,1]) # primera componente de phi
  phi <- c(phi1, 1 - phi1) # vector estacionario phi
  return(list(phi = phi, P_star = P_star))
}


# Función simulate_map2: Simula un MAP2.

# ss <- numero de llegadas a simular
# D0, D1 <- matrices de tasas del proceso MAP2

simulate_map2 <- function(ss, D0, D1) {
  library(expm) # para operaciones matriciales como exponencial
  library(MCMCpack) # rdirichlet-like functionality
  
  A <- cbind(D0, D1) # concatena D0 y D1
  lambda <- -diag(D0) # tasas de salida de los estados
  
  # Transition probabilities
  P <- matrix(0, 2, 4) # cálculo de la matriz de transición P
  for (i in 1:2) {
    P[i, ] <- A[i, ] / lambda[i] # se divide cada fila de A entre lambda[i] (suman 1)
  }
  diag(P[, 1:2]) <- 0 # pone a 0 la diagonal de P0
  
  P0 <- P[, 1:2] # separa la parte sin llegada  
  P1 <- P[, 3:4] # con llegada
  
  # llama a Mrp2 para calcular:
  mrp <- Mrp2(lambda, P0, P1) 
  phi <- mrp$phi # vector de probabilidad inicial
  
  # Generator matrix
  D <- D0 + D1 # generador total del proceso
  
  # Resuelve el sistema lineal para pi: solve pi * D = 0 con p1 + p2 = 1
  E <- cbind(D, rep(1, 2)) 
  u <- c(0, 0, 1)
  pi <- qr.solve(t(E), u)
  
  # Initialization
  X <- c(0)         # tiempos absolutos de transición
  e <- c(0)         # tiempos entre transiciones
  G <- c(1)         # indicadores de llegada (1 = hay llegada)
  t <- c(0)         # tiempos entre llegadas
  S <- c(1)         # acumulador de llegadas
  I <- sample(1:2, 1, prob = phi) # selecciona el estado inicial aleatoriamente según phi
  
  while (tail(S, 1) <= ss) { # simula transiciones hasta que se hayan producido ss llegadas
    j <- tail(I, 1) # I es el vector de estados visitados, j será el estado actual
    rate <- -A[j, j] # tasa de salida del estado actual j
    e_next <- rexp(1, rate) # genera tiempo aleatorio exp hasta la siguiente transición
    X <- c(X, tail(X, 1) + e_next) # actualiza con el tiempo absoluto en que ocurre la sig transición 
    
    p <- A[j, ] / (-A[j, j]) # probabilidad de transición desde j normalizando por la tasa total
    p[j] <- 0 # elimina la autotransición sin llegada
    p <- p / sum(p) # normaliza el vector p para que sume 1
    
    i <- sample(1:4, 1, prob = p) # selecciona el próximo estado del proceso
    k <- ifelse(i %% 2 == 0, i / 2 - 1, floor(i / 2)) # determina si es con (k=1) o sin llegada (k=0)
    G <- c(G, k) 
    I <- c(I, i - k * 2) # actualiza el estado
    S <- c(S, tail(S, 1) + k) # " el vector de llegadas 
    e <- c(e, e_next) # " el vector de tiempos
  }
  
  arrival_indices <- which(G != 0) # extrae los tiempos y estados asociados a llegadas reales
  X_arr <- X[arrival_indices]
  I_arr <- I[arrival_indices]
  t_interarrival <- diff(X_arr[1:(ss+1)]) # calcula los tiempos entre llegadas
  
  return(list(
    X = X, # tiempos acumulados de transición
    I = I, # vector de estados visitados
    G = G, # vector de ceros (transición sin llegada) y unos (con llegada)
    X_arr = X_arr, # tiempos de las llegadas (X con G==1)
    I_arr = I_arr, # estados correspondientes a cada llegada (I con G==1)
    t = t_interarrival, # tiempos entre llegadas (lo que nos interesa)
    P0 = P0, # probs de transicion sin llegada
    P1 = P1, # " con llegada
    lambda = lambda, # tasas de salida
    D = D, # matriz generadora total
    pi = pi # probabildiades estacionarias proceso subyacente
  ))
}


# EJEMPLO DE USO

# Parámetros de ejemplo
D0 <- matrix(c(-3, 0.6, 2.5, -10), nrow = 2, byrow = TRUE)
D1 <- matrix(c(1.05, 1.35, 3.5, 4), nrow = 2, byrow = TRUE)

# Simulación
res <- simulate_map2(1000, D0, D1)

# Ver algunas salidas
head(res$t)       # tiempos entre llegadas
head(res$I_arr)   # estados en las llegadas
