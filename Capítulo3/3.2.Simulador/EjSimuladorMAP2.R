# SIMULACIÓN DEL MAP

# Función Mrp2: Calcula el vector estacionario phi y la matriz de 
# transición P_star para un proceso MAP2.

# lambda <- vector de tasas de tiempos exponenciales
# P0 <- matriz de probabilidades de transición sin llegada
# P1 <- " con llegada

Mrp2 <- function(P0, P1) {
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
  mrp <- Mrp2(P0, P1) 
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



# COMPROBACIÓN CALCULANDO LOS MOMENTOS TEÓRICOS Y EMPÍRICOS

calculate_POP1 <- function(D0,D1){
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
  
  return(list(P0, P1))
}

calculate_POP1(D0,D1)[[1]]

calculate_moments <- function(D0, D1, simulated_data) {
  
  # Cálculo de P0 y P1
  P0 <- calculate_POP1(D0,D1)[[1]]
  P1 <- calculate_POP1(D0,D1)[[2]]
  
  # Cáculo de P_star y phi
  lambda <- -diag(D0) # tasas de salida de los estados
  mrp <- Mrp2(P0, P1)
  P_star <- mrp$P_star
  phi <- mrp$phi
  
  # 3. Cálculo de los momentos teóricos (Ecuación 3.4)
  moments_theoretical <- numeric(4)
  inv_D0 <- solve(-D0)
  
  for (n in 1:4) {
    moments_theoretical[n] <- factorial(n) * phi %*% (inv_D0 %^% n) %*% matrix(1, nrow = 2, ncol = 1)
  }
  
  # 4. Cálculo de momentos empíricos a partir de los datos simulados
  t <- simulated_data$t  # Tiempos entre llegadas simulados
  moments_empirical <- c(
    mean(t),               # Primer momento (media)
    mean(t^2),             # Segundo momento
    mean(t^3),             # Tercer momento
    mean(t^4)              # Cuarto momento
  )
  
  # 5. Crear tabla comparativa
  comparison <- data.frame(
    Momento = c("Primero (media)", "Segundo", "Tercero", "Cuarto"),
    Teórico = moments_theoretical,
    Empírico = moments_empirical,
    Diferencia = moments_theoretical - moments_empirical,
    Error.Relativo = abs(moments_theoretical - moments_empirical)/moments_theoretical * 100
  )
  
  return(list(
    theoretical = moments_theoretical,
    empirical = moments_empirical,
    comparison = comparison,
    phi = phi,
    P_star = P_star
  ))
}

# Ejemplo de uso con los datos simulados anteriormente
res <- simulate_map2(1000, D0, D1)
moment_results <- calculate_moments(D0, D1, res)

# Mostrar resultados
print("Resultados de los Momentos:")
print(moment_results$comparison)

# Mostrar P* y phi para verificación
print("Matriz P*:")
print(moment_results$P_star)
print("Distribución estacionaria phi:")
print(moment_results$phi)

print("¿Filas de P* suman 1?")
print(rowSums(moment_results$P_star))  # Debe ser c(1, 1)
print("¿phi es estacionaria?")
print(moment_results$phi %*% moment_results$P_star - moment_results$phi)  # Debe ser ~0

hist(res$t, breaks = 50, prob = TRUE, main = "Distribución de tiempos entre llegadas")


# REPRESENTACIÓN DE LAS DISTRIBUCIONES


# Función de distribución teórica acumulada (CDF) del MAP2
cdf_theoretical <- function(t_vals, phi, D0) {
  F_t <- numeric(length(t_vals))
  for (i in seq_along(t_vals)) {
    t <- t_vals[i]
    exp_term <- expm(D0 * t)
    aux <- phi %*% (diag(2) - exp_term) %*% matrix(1, nrow = 2)
    F_t[i] <- aux
  }
  return(F_t)
}

# Crear secuencia de tiempos para evaluar CDFs
t_vals <- seq(0, max(res$t), length.out = 500)

# Calcular la función de distribución teórica
phi <- moment_results$phi
F_theoretical <- cdf_theoretical(t_vals, phi, D0)

# Calcular la función de distribución empírica
F_empirical <- ecdf(res$t)

# Representar ambas en el mismo gráfico
plot(t_vals, F_empirical(t_vals), type = "s", lwd = 2, col = "blue",
     xlab = "Tiempo entre llegadas", ylab = "F(t)")
lines(t_vals, F_theoretical, col = "red", lwd = 2, lty = 2)

legend("bottomright", legend = c("Empírica (ECDF)", "Teórica (MAP2)"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))



# TABLA COMPARATIVA

library(kableExtra)

# Añadir columna "Distribución"
comparison_table <- cbind(
  Distribución = rep("MAP2", nrow(moment_results$comparison)),
  moment_results$comparison
)

# Redondear valores para mejor presentación
comparison_table$Teórico <- round(comparison_table$Teórico, 4)
comparison_table$Empírico <- round(comparison_table$Empírico, 4)
comparison_table$Diferencia <- round(comparison_table$Diferencia, 4)
comparison_table$Error.Relativo <- round(comparison_table$Error.Relativo, 2)

# Poner la columna "Momento" en negrita
comparison_table$Momento <- paste0("<b>", comparison_table$Momento, "</b>")

# Mostrar la tabla con estilo
kable(comparison_table, "html", escape = FALSE,
      align = c('l', 'l', rep('c', 4)),
      col.names = c("Distribución", "Momento", "Teórico", "Empírico", "Diferencia", "Error Relativo (%)")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(1, width = "2.5cm") %>%
  column_spec(2:6, width = "2.5cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#3498db")

