
# MEDIDAS DE INTERÉS DE LA COLA

## SIMULADOR COLA MAP2/PH3/C CON IMPACIENCIA BS

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


simulate_MAP2_PH3_C_queue <- function(D0, D1, alpha, S_matrix, scale, shape, C, n) {
  
  # 1. Simular tiempos de llegada (MAP₂)
  arrival_times <- simulate_map2(n,D0, D1)$X_arr
  
  # 2. Simular tiempos de impaciencia (BS)
  impatience_times <- rbirnbaum_saunders(n, beta = scale, alpha = shape)
  
  # 3. Simular tiempos de servicio (PH3)
  service_times <- rphtype(n, prob = alpha, rates = S_matrix)
  
  # Inicializar vectores de resultados
  queue_times <- numeric(n) # Tiempos en cola
  system_times <- numeric(n) # Tiempo total en el sistema
  server_release <- numeric(C) # Tiempos de liberación de cada servidor
  served <- integer(n)  # 1 si fue servido, 0 si abandonó
  # Solo se usan para clientes servidos:
  actual_service_times <- rep(NA, n)  # Tiempos de servicio reales
  
  # 4-5. Procesar los primeros C clientes (entran directamente a servicio)
  for (i in 1:min(C, n)) {
    queue_times[i] <- 0  # No esperan cola
    system_times[i] <- arrival_times[i] + service_times[i] 
    server_release[i] <- system_times[i] # Actualiza el tiempo de liberación
    served[i] <- 1
    actual_service_times[i] <- service_times[i]
  }
  
  # Si hay más clientes que servidores, procesar el resto
  if (n > C) {
    for (i in (C+1):n) {
      # 7. Identificar servidores disponibles
      available_servers <- which(server_release <= arrival_times[i])
      
      # 8. Si hay servidores disponibles se atiende inmediatamente
      if (length(available_servers) > 0) {
        queue_times[i] <- 0 # No esperan cola
        system_times[i] <- arrival_times[i] + service_times[i]
        served[i] <- 1
        actual_service_times[i] <- service_times[i]
        
        # Asignar al servidor que se libera primero
        h <- available_servers[which.min(server_release[available_servers])]
        server_release[h] <- system_times[i]
        
      } else { # Todos los servidores están ocupados
        # 12. Si es el primero en la cola : todos los de antes atendidos
        if (sum(served[1:(i-1)] == 1) == C) {  # Todos los anteriores están siendo atendidos
          u <- min(server_release) - arrival_times[i] # tiempo que debo esperar para ser servido (hasta que un sevidor se libere)
          
          # 14. Si abandona por impaciencia
          if (u > impatience_times[i]) { # el u es mayor que el tiempo de impaciencia
            queue_times[i] <- impatience_times[i] # esta en la cola el tiempo de impaciencia
            system_times[i] <- arrival_times[i] + impatience_times[i]
            served[i] <- 0
            # actual_service_times[i] permanece NA
            
          } else {
            # 16. Entra a servicio
            queue_times[i] <- u # espera hasta que un servidor esta libre
            system_times[i] <- arrival_times[i] + u + service_times[i] # tiempo total en el sistema
            served[i] <- 1 # marca como servido
            actual_service_times[i] <- service_times[i] # almacena el tiempo de servicio real
            
            h <- which.min(server_release) # identifica cuál es el servidor que se ha liberado primero
            server_release[h] <- system_times[i] # actualiza su tiempo de liberación
          }
          
        } else { # 19. No es el primero en la cola
          # 20. Identificar clientes en cola: 
          queueing_customers <- which(served[1:(i-1)] == 0 & # los clientes en cola son aquellos sin servir
                                        system_times[1:(i-1)] > arrival_times[i]) # que todavía están en el sistema
          
          # 21. Calcular cuántos entrarán a servicio
          # m: cuántos clientes en cola no abandonarán antes de que se libere un servidor
          m <- sum(arrival_times[queueing_customers] + impatience_times[queueing_customers] >= min(server_release))
          
          if (m == 0) {
            # 23. Todos abandonarán, procesar como primero en cola: repito 13-18
            u <- min(server_release) - arrival_times[i]
            
            if (u > impatience_times[i]) {
              queue_times[i] <- impatience_times[i]
              system_times[i] <- arrival_times[i] + impatience_times[i]
              served[i] <- 0
            } else {
              queue_times[i] <- u
              system_times[i] <- arrival_times[i] + u + service_times[i]
              served[i] <- 1
              actual_service_times[i] <- service_times[i]
              
              h <- which.min(server_release)
              server_release[h] <- system_times[i]
            }
          } else { # Al menos un cliente en cola será servido
            # 25. Encontrar el último que entrará a servicio
            j <- queueing_customers[m] # último cliente en cola que entra a servicio antes de i
            u <- queue_times[j] + min(server_release) - arrival_times[i] # tiempo total de espera de i
            
            if (u > impatience_times[i]) { # si u supera la impaciencia de i, abandona
              queue_times[i] <- impatience_times[i]
              system_times[i] <- arrival_times[i] + impatience_times[i]
              served[i] <- 0
            } else {
              queue_times[i] <-u
              system_times[i] <- arrival_times[i] + queue_times[i] + service_times[i]
              served[i] <- 1
              actual_service_times[i] <- service_times[i]
              
              h <- which.min(server_release)
              server_release[h] <- system_times[i]
            }
          }
        }
      }
    }
  }
  
  # Filtrar tiempos de servicio reales (solo para clientes servidos)
  service_times_output <- actual_service_times[served == 1]
  
  # Devolver resultados como una lista
  list(
    arrival_times = arrival_times, #xi
    impatience_times = impatience_times, #yi
    service_times = service_times_output, #si
    served = served, #Ii
    system_times = system_times, #zi
    queue_times = queue_times, #qi
    server_release_times = server_release, #r(h)
    p = sum(served)  # Número de clientes servidos
  )
}


##########################################################################

# PARÁMETROS PARA CADA SLOT

# MAP2 <- MatricesMAP2_minf2

# PH3 <- ParamServicio.R

# BS <- ParamImpaciencia.R

# LONGITUD DE LAS MUESTRAS DE CADA SLOT

slot_sizes <- list(
  G1 = 3819,
  G2 = 9274,
  G3 = 18942,
  G4 = 2989,
  G5 = 10714,
  G6 = 3251
)

slot_served <- list(
  
  G1 <- 3599,
  G2 <- 7209,
  G3 <- 16284,
  G4 <- 2667,
  G5 <- 9936,
  G6 <- 3045
)


# TASAS DE ABANDONO EMPÍRICAS

emp_g1 <- 100-slot_served[[1]]*100/slot_sizes[[1]]
emp_g2 <- 100-slot_served[[2]]*100/slot_sizes[[2]]
emp_g3 <- 100-slot_served[[3]]*100/slot_sizes[[3]]
emp_g4 <- 100-slot_served[[4]]*100/slot_sizes[[4]]
emp_g5 <- 100-slot_served[[5]]*100/slot_sizes[[5]]
emp_g6 <- 100-slot_served[[6]]*100/slot_sizes[[6]]

tasas_empíricas <- list(emp_g1,emp_g2,emp_g3,emp_g4,emp_g5,emp_g6)


#########################################################################################


## SLOT G1

# Parámetros del MAP₂

D0 <- matricesMAP2_minf2$G1[["D0"]]
D1 <- matricesMAP2_minf2$G1[["D1"]]

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
results <- simulate_MAP2_PH3_C_queue(D0, D1, alpha, S, scale, shape, C, n)

# Visualizar resultados
str(results)  # Estructura de los resultados

# Métricas resumidas
cat("Número total de clientes:", n, "\n")
cat("Clientes servidos:", results$p, "\n")
cat("Clientes que abandonaron:", n - results$p, "\n")
cat("Tasa de abandono:", round((n - results$p)/n * 100, 2), "%\n")


## SLOT G2

# Parámetros del MAP₂

D0 <- matricesMAP2_minf2$G2[["D0"]]
D1 <- matricesMAP2_minf2$G2[["D0"]]

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


## SLOT G5

# Parámetros del MAP₂

D0 <- matricesMAP2_minf2$G5[["D0"]]
D1 <- matricesMAP2_minf2$G5[["D0"]]

# Parámetros de la distribución PH₃ (3 fases)
alpha <- paramPH3$G5[["alpha"]]
S <- paramPH3$G5[["S"]]

# Parámetros de impaciencia (BS)
shape <- paramBS$G5[["shape"]] #alpha
scale <- paramBS$G5[["scale"]] #beta

# Parámetros del sistema
C <- 3 # Número de servidores
n <- slot_sizes[[5]]  # Número de clientes a simular

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


# (...) similar para el resto de slots


# DEBEMOS SIMULAR PARA CADA SLOT 100 VECES, Y PROMEDIAR LOS RESULTADOS

slot_results <- list()

# SLOT G1

D0 <- matricesMAP2_minf2$G1[["D0"]]
D1 <- matricesMAP2_minf2$G1[["D0"]]

alpha <- paramPH3$G1[["alpha"]]
S <- paramPH3$G1[["S"]]

shape <- paramBS$G1[["shape"]]
scale <- paramBS$G1[["scale"]]

C <- 3
n <- slot_sizes[[1]]
nsim <- 100

# Inicializar acumuladores
served_counts <- numeric(nsim)
abandon_counts <- numeric(nsim)
avg_queue_times <- numeric(nsim)
avg_system_times <- numeric(nsim)
abandon_rates <- numeric(nsim)
avg_service_times <- numeric(nsim)

set.seed(123)  # Fijar semilla para reproducibilidad

for (i in 1:nsim) {
  res <- simulate_MAP2_PH3_C_queue(D0, D1, alpha, S, scale, shape, C, n)
  
  served_counts[i] <- res$p
  abandon_counts[i] <- n - res$p
  avg_queue_times[i] <- mean(res$queue_times)
  avg_system_times[i] <- mean(res$system_times)
  abandon_rates[i] <- 100 * (n - res$p) / n
  avg_service_times[i] <- mean(res$service_times)
}

# Resultados promedio

# Guardar promedios en la lista
slot_results$G1 <- list(
  served = mean(served_counts),
  abandoned = mean(abandon_counts),
  abandon_rate = mean(abandon_rates),
  avg_queue_time = mean(avg_queue_times),
  avg_system_time = mean(avg_system_times),
  avg_service_time = mean(avg_service_times)
)

# Mostrar por consola
cat("RESULTADOS PROMEDIO TRAS", nsim, "SIMULACIONES PARA SLOT G1\n")
cat("--------------------------------------------------------\n")
print(slot_results$G1)



# ESTUDIO COMPLETO

# Valores de servidores a probar
C_values <- c(3, 5, 7, 9, 11)

# Lista de slots disponibles
slot_names <- names(matricesMAP2)

# Número de simulaciones
nsim <- 100

# Lista para guardar todos los resultados
all_results_minf2 <- list()

set.seed(123)

for (slot in slot_names) {
  
  # Extraer parámetros del slot
  D0 <- matricesMAP2_minf2[[slot]][["D0"]]
  D1 <- matricesMAP2_minf2[[slot]][["D1"]]
  
  alpha <- paramPH3[[slot]][["alpha"]]
  S <- paramPH3[[slot]][["S"]]
  
  if (is.null(S) || length(S) == 0) {
    warning(paste("Saltando slot", slot, "por S inválido"))
    next
  }
  
  shape <- paramBS[[slot]][["shape"]]
  scale <- paramBS[[slot]][["scale"]]
  
  n <- slot_sizes[[slot]]
  
  # Lista para resultados de cada valor de C en este slot
  slot_result <- list()
  
  for (C in C_values) {
    served_counts <- numeric(nsim)
    abandon_counts <- numeric(nsim)
    avg_queue_times <- numeric(nsim)
    avg_system_times <- numeric(nsim)
    abandon_rates <- numeric(nsim)
    avg_service_times <- numeric(nsim)
    
    for (i in 1:nsim) {
      res <- simulate_MAP2_PH3_C_queue(D0, D1, alpha, S, scale, shape, C, n)
      
      served_counts[i] <- res$p
      abandon_counts[i] <- n - res$p
      avg_queue_times[i] <- mean(res$queue_times)
      avg_system_times[i] <- mean(res$system_times)
      abandon_rates[i] <- 100 * (n - res$p) / n
      avg_service_times[i] <- mean(res$service_times)
    }
    
    # Guardar resultados promedio para este C
    slot_result[[paste0("C", C)]] <- list(
      served = mean(served_counts),
      abandoned = mean(abandon_counts),
      abandon_rate = mean(abandon_rates),
      avg_queue_time = mean(avg_queue_times),
      avg_system_time = mean(avg_system_times),
      avg_service_time = mean(avg_service_times)
    )
  }
  
  # Guardar resultados por slot
  all_results_minf2[[slot]] <- slot_result
}


library(kableExtra)

# Crear dataframe con los datos de tasas de abandono
abandon_data <- data.frame(
  Intervalo = c("G₁", "G₂", "G₃", "G₄", "G₅", "G₆"),
  Empírico = round(c(tasas_empíricas[[1]], tasas_empíricas[[2]],tasas_empíricas[[3]],tasas_empíricas[[4]],
               tasas_empíricas[[5]],tasas_empíricas[[6]]),2),
  C3 = c(6.17, 51, 36.5, 18.8, 8.71, 2.98),
  C5 = c(0.53, 24.3, 10.9, 3.19, 0.99, 0.2),
  C7 = c(0.03, 9.4, 2.39, 0.4, 0.09, 0.01),
  C9 = c(0.00, 2.99, 0.39, 0.04, 0.01, 0.00),
  C11 = c(0.00, 0.79, 0.05, 0.00, 0.00, 0.00)
)


# Crear la tabla con formato
abandon_table <- abandon_data %>%
  kable(
    format = "html",
    align = c('c', rep('c', 6)),
    col.names = c("Intervalo", "Empírico", "C = 3", "C = 5", "C = 7", "C = 9", "C = 11")) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#3498db") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "2cm") %>%
  column_spec(2:7, width = "2cm") %>%
  # Resaltar valores más cercanos a los empíricos en negrita
  column_spec(3, bold = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)) %>%  # Solo para C=3
  column_spec(4, bold = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)) %>%  # Para C=5 (G2, G3, G4)
  column_spec(5, bold = FALSE) %>%
  column_spec(6, bold = FALSE) %>%
  column_spec(7, bold = FALSE)
abandon_table
