
# TABLA 7

# Modificamos el simulador para que también registre el numero
# de clientes en cola en diferentes instantes de tiempo.

simulate_MAP2_PH3_C_queue_with_queue_length <- function(D0, D1, alpha, S_matrix, scale, shape, C, n) {
  
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
  queue_length_at_arrival <- integer(n) # Número de personas en cola al llegar

  
  # 4-5. Procesar los primeros C clientes (entran directamente a servicio)
  for (i in 1:min(C, n)) {
    queue_length_at_arrival[i] <- 0
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
        queue_length_at_arrival[i] <- 0
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
          queue_length_at_arrival[i] <- 0
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
          queue_length_at_arrival[i] <- length(queueing_customers)
          
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
              queue_times[i] <- min(server_release) - arrival_times[i]
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
    p = sum(served),  # Número de clientes servidos
    queue_length_at_arrival = queue_length_at_arrival
  )
}


# ESTUDIO COMPLETO CON CÁLCULO DE π_j (versión simplificada)

# Valores de servidores a probar
C_values <- c(3, 5, 7, 9, 11)

# Lista de slots disponibles
slot_names <- names(matricesMAP2)

# Número de simulaciones
nsim <- 100

# Máximo valor de j para π_j
max_j <- 5

# Lista para guardar solo los resultados de π_j
pi_j_results <- list()

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
  
  # Lista para resultados de π_j en este slot
  slot_pi_j <- list()
  
  for (C in C_values) {
    # Matriz para almacenar las π_j en cada simulación
    pi_j_matrix <- matrix(0, nrow = nsim, ncol = max_j + 1) # De 0 a max_j
    
    for (i in 1:nsim) {
      # Usar la función que registra la longitud de la cola al llegar
      res <- simulate_MAP2_PH3_C_queue_with_queue_length(D0, D1, alpha, S, scale, shape, C, n)
      
      # Calcular π_j para esta simulación
      queue_lengths <- res$queue_length_at_arrival
      warmup <- round(0.1 * length(queue_lengths)) # Eliminar periodo transitorio
      queue_lengths <- queue_lengths[(warmup+1):length(queue_lengths)]
      
      # Calcular frecuencias relativas
      pi_j <- table(factor(queue_lengths, levels = 0:max_j)) / length(queue_lengths)
      pi_j_matrix[i,] <- pi_j
    }
    
    # Calcular promedio de π_j entre todas las simulaciones
    pi_j_means <- colMeans(pi_j_matrix)
    names(pi_j_means) <- paste0("pi_", 0:max_j)
    
    # Guardar solo los resultados de π_j para este C
    slot_pi_j[[paste0("C", C)]] <- pi_j_means
  }
  
  # Guardar resultados π_j por slot
  pi_j_results[[slot]] <- slot_pi_j
}

pi_j_results



# TABLA
library(kableExtra)

# 1. Datos originales (8 columnas)
tabla_data <- data.frame(
  Intervalos = rep(c("G₁", "G₂", "G₃", "G₄", "G₅", "G₆"), each=5),
  Servidores = rep(c("C = 3", "C = 5", "C = 7", "C = 9", "C = 11"), 6),
  π0 = c(0.953, 0.997, 1, 1, 1, 0.092, 0.615, 0.875, 0.965, 0.991, 0.334, 
         0.87, 0.978, 0.997, 1, 0.775, 0.975, 0.998, 1, 1, 0.906, 0.992, 
         1, 1, 1, 0.976, 0.999, 1, 1, 1),
  π1 = c(0.042, 0.003, 0, 0, 0, 0.131, 0.206, 0.089, 0.028, 0.007, 0.284, 0.104,
         0.019, 0.003, 0, 0.167, 0.022, 0.002, 0, 0, 0.073, 0.007, 0, 0, 0,
         0.021, 0.001, 0, 0, 0),
  π2 = c(0.005, 0, 0, 0, 0, 0.145, 0.098, 0.026, 0.006, 0.001, 0.19, 0.022, 0.002,
         0, 0, 0.044, 0.002, 0, 0, 0, 0.016, 0, 0, 0, 0, 0.002, 0, 0, 0, 0),
  π3 = c(0, 0, 0, 0, 0, 0.136, 0.045, 0.007, 0.001, 0, 0.105, 0.004, 0, 0, 0, 
         0.011, 0, 0, 0, 0, 0.003, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  π4 = c(0, 0, 0, 0, 0, 0.117, 0.021, 0.002, 0, 0, 0.051, 0, 0, 0, 0,
         0.002, 0, 0, 0, 0, 0.001, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  π5 = c(0, 0, 0, 0, 0, 0.095, 0.009, 0, 0, 0, 0.022, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

tabla_data$sum_resto <- 1 - rowSums(tabla_data[,3:8])

# Versión estable que funciona:
tabla_final <- tabla_data %>%
  kable(
    format = "html",
    align = 'c',
    col.names = c("Intervalos", "Servidores", 
                  "$\\pi_0$", "$\\pi_1$", "$\\pi_2$", "$\\pi_3$", "$\\pi_4$", "$\\pi_5$",
                  "$\\sum_{j=6}^\\infty\\pi_j$"),
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  ) %>%
  # Aplicar formatos ANTES de collapse_rows
  column_spec(1, bold = TRUE) %>%
  column_spec(2, bold = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#3498db") %>%
  # Combinar filas (hacerlo al final)
  collapse_rows(columns = c(1), valign = "middle")


tabla_final
