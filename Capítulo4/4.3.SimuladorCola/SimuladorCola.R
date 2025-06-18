#' Simulador de cola MAP₂/PH3/C con tiempos de impaciencia
#'
#' @param D0 Matriz D0 del MAP₂
#' @param D1 Matriz D1 del MAP₂
#' @param alpha Parámetro para tiempos de servicio PH3
#' @param S Parámetro para tiempos de servicio PH3
#' @param scale Parámetro de escala para tiempos de impaciencia BS
#' @param shape Parámetro de forma para tiempos de impaciencia BS
#' @param C Número de servidores
#' @param n Número de clientes a simular
#' @return Una lista con todos los resultados de la simulación


Mrp2 <- function(P0, P1) {
  I2 <- diag(2)
  P_star <- solve(I2 - P0) %*% P1
  phi1 <- P_star[2,1] / (1 - P_star[1,1] + P_star[2,1])
  phi <- c(phi1, 1 - phi1)
  return(list(phi = phi, P_star = P_star))
}


# Función simulate_map2: Simula un MAP2.

simulate_map2 <- function(ss, D0, D1) {
  library(expm) # for matrix exponential if needed
  library(MCMCpack) # for rdirichlet-like functionality (if needed)
  
  A <- cbind(D0, D1)
  lambda <- -diag(D0)
  
  # Transition probabilities
  P <- matrix(0, 2, 4)
  for (i in 1:2) {
    P[i, ] <- A[i, ] / lambda[i]
  }
  diag(P[, 1:2]) <- 0
  
  P0 <- P[, 1:2]
  P1 <- P[, 3:4]
  
  mrp <- Mrp2(P0, P1)
  phi <- mrp$phi
  
  # Generator matrix
  D <- D0 + D1
  
  # Stationary distribution pi: solve pi * D = 0
  E <- cbind(D, rep(1, 2))
  u <- c(0, 0, 1)
  pi <- qr.solve(t(E), u)
  
  # Initialization
  X <- c(0)
  e <- c(0)
  G <- c(1)
  t <- c(0)
  S <- c(1)
  I <- sample(1:2, 1, prob = phi)
  
  while (tail(S, 1) <= ss) {
    j <- tail(I, 1)
    rate <- -A[j, j]
    e_next <- rexp(1, rate)
    X <- c(X, tail(X, 1) + e_next)
    
    p <- A[j, ] / (-A[j, j])
    p[j] <- 0
    p <- p / sum(p)
    
    i <- sample(1:4, 1, prob = p)
    k <- ifelse(i %% 2 == 0, i / 2 - 1, floor(i / 2))
    G <- c(G, k)
    I <- c(I, i - k * 2)
    S <- c(S, tail(S, 1) + k)
    e <- c(e, e_next)
  }
  
  arrival_indices <- which(G != 0)
  X_arr <- X[arrival_indices]
  I_arr <- I[arrival_indices]
  t_interarrival <- diff(X_arr[1:(ss+1)])
  
  return(list(
    X = X, I = I, G = G,
    X_arr = X_arr, I_arr = I_arr,
    t = t_interarrival,
    P0 = P0, P1 = P1,
    lambda = lambda, D = D,
    pi = pi
  ))
}

library(VGAM) # Para la B-S


simulate_MAP2_PH3_C_queue <- function(D0, D1, alpha, S, scale, shape, C, n) {
  
  # 1. Simular tiempos de llegada (MAP₂)
  x <- simulate_map2(n, D0, D1)$X_arr
  
  # 2. Simular tiempos de impaciencia (BS)
  y <- rbisa(n, scale = scale, shape = shape)
  
  # 3. Simular tiempos de servicio (PH3)
  s <- rphtype(n, prob = alpha, rates = S)
  
  # Inicializar vectores de resultados
  q <- numeric(n) # Tiempos en cola
  z <- numeric(n) # Tiempo total en el sistema
  r <- numeric(C) # Tiempos de liberación de cada servidor
  l <- integer(n)  # 1 si fue servido, 0 si abandonó
  h <- 0 
  
  # Paso 1–3: Inicialización de primeros C clientes
  for (i in 1:C) {
    q[i] <- 0
    z[i] <- x[i] + s[i]
    r[i] <- x[i] + s[i]
    l[i] <- 1
  }
  
  # Paso 4: Siguientes clientes
  for (i in (C+1):n) {
    E=which(r<x[i]) # Paso 6: servidores libres
    
    if(length(E)>0){ # Si hay al menos un servidor libre
      q[i] <- 0
      z[i] <- x[i]+s[i]
      l[i] <- 1
      h <- which.min(r) # identifica el servidor libre
      r[h] <- x[i]+s[i]
      
    }else{
      # Paso 10: todos los servidores están ocupados
      I <- which(l[1:(i-1)]==1) # clientes anteriores a i que entrán a servicio
      
      if(length(I)==0){
        # No hay clientes en servicio todavía
        next
      }
      
      if(i==I[length(I)]+1){ # i es el siguiente al último en servicio
        u <- min(r)-x[i] # tiempo que i esperará para entrar en servicio
        
        if(u>y[i]){ # es mayor que su imp <- abandona
          q[i] <- y[i]
          z[i] <- x[i] + y[i]
          l[i] <- 0
          s[i] <- NA
        }else{ # entra en servicio
          q[i] <- u
          z[i] <- x[i] + q[i] + s[i]
          l[i] <- 1
          h <- which.min(r)
          r[h] <- x[i] + q[i] + s[i]
        }
      }else{ # Paso 20: no es el primero en la cola
        # Calculamos los clientes en cola:
        en_cola <- which(l[1:(i-1)]==0) # clientes sin servir anteriores a i
        m <- sum((x[i]-x[en_cola])<y[en_cola]) # cuántos no abandonan
        
        if(m==0){
          # Paso 23: todos abandonan <- volvemos a 13-18
          u <- min(r)-x[i]
          
          if(u>y[i]){ # es mayor que su imp <- abandona
            q[i] <- y[i]
            z[i] <- x[i] + y[i]
            l[i] <- 0
            s[i] <- NA
          }else{ # entra en servicio
            q[i] <- u
            z[i] <- x[i] + q[i] + s[i]
            l[i] <- 1
            h <- which.min(r)
            r[h] <- x[i] + q[i] + s[i]
          }
        }else{ # al menos uno entrará
          j <- en_cola[length(en_cola)] # último en la cola
          u <- q[j]+min(r)-x[i] # tiempo que i esperará para entrar en servicio
          
          if(u>y[i]){ # es mayor que su imp <- abandona
            q[i] <- y[i]
            z[i] <- x[i] + y[i]
            l[i] <- 0
            s[i] <- NA
          }else{ # entra en servicio
            q[i] <- u 
            z[i] <- x[i] + q[i] + s[i]
            l[i] <- 1
            h <- which.min(r)
            r[h] <- x[i] + q[i] + s[i]
          }
        }
      }
    }
  }
  
  return(list(
    arrival_times = x,
    impatience_times = y,
    service_times = s,
    queue_times = q,
    departure_times = z,
    served = l,
    server_release_times=r,
    p=sum(l)
  ))
}

## SLOT G1

# Parámetros del MAP₂

D0 <- matricesMAP2$G1[["D0"]]
D1 <- matricesMAP2$G1[["D1"]]

# Parámetros de la distribución PH₃ (3 fases)
alpha <- paramPH3$G1[["alpha"]]
S <- paramPH3$G1[["S"]]

# Parámetros de impaciencia (BS)
shape <- paramBS$G1[["shape"]]
scale <- paramBS$G1[["scale"]]

# Parámetros del sistema
C <- 3 # Número de servidores
n <- slot_sizes[[1]]  # Número de clientes a simular

# Ejecutar la simulación
set.seed(123)  # Para reproducibilidad
results <- simulador_cola(D0, D1, alpha, S, scale, shape, C, n)

# Visualizar resultados
str(results)  # Estructura de los resultados

# Métricas resumidas
cat("Número total de clientes:", n, "\n")
cat("Clientes servidos:", results$p, "\n")
cat("Clientes que abandonaron:", n - results$p, "\n")
cat("Tasa de abandono:", round((n - results$p)/n * 100, 2), "%\n")


## SLOT G2

# Parámetros del MAP₂

D0 <- matricesMAP2$G2[["D0"]]
D1 <- matricesMAP2$G2[["D1"]]

# Parámetros de la distribución PH₃ (3 fases)
alpha <- paramPH3$G2[["alpha"]]
S <- paramPH3$G2[["S"]]

# Parámetros de impaciencia (BS)
shape <- paramBS$G2[["shape"]] #alpha
scale <- paramBS$G2[["scale"]] #beta

# Parámetros del sistema
C <- 3 # Número de servidores
n <- slot_sizes[[2]]  # Número de clientes a simular

# Ejecutar la simulación
set.seed(123)  # Para reproducibilidad
results <- simulate_MAP2_PH3_C_queue(D0, D1, alpha, S, scale, shape, C, n)

# Visualizar resultados
str(results)  # Estructura de los resultados

# Métricas resumidas
cat("Número total de clientes:", n, "\n")
cat("Clientes servidos:", results$p, "\n")
cat("Clientes que abandonaron:", n - results$p, "\n")
cat("Tasa de abandono:", round((n - results$p)/n * 100, 2), "%\n")
summary(results$queue_times)
