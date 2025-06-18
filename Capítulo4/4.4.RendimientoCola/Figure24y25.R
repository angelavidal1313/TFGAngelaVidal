
# DISTRIBUCIÓN DE LOS TIEMPOS DE ESPERA PARA LOS VALORES MÁS ADECUADOS DE C


# Configuración de valores de C para cada slot

C_values <- c(3, 5, 5, 5, 3, 3)
names(C_values) <- c("G1", "G2", "G3", "G4", "G5", "G6")

# Número de simulaciones
nsim <- 100

# Lista para almacenar los tiempos de espera virtual de cada slot
virtual_waits <- list()

set.seed(123)  # Para reproducibilidad

for (slot in names(matricesMAP2)) {
  # Extraer parámetros del slot
  D0 <- matricesMAP2[[slot]][["D0"]]
  D1 <- matricesMAP2[[slot]][["D1"]]
  alpha <- paramPH3[[slot]][["alpha"]]
  S <- paramPH3[[slot]][["S"]]
  shape <- paramBS[[slot]][["shape"]]
  scale <- paramBS[[slot]][["scale"]]
  n <- slot_sizes[[slot]]
  C <- C_values[slot]
  
  # Vector para almacenar todos los tiempos de espera virtual de las simulaciones
  all_virtual_waits <- numeric()
  
  for (i in 1:nsim) {
    res <- simulate_MAP2_PH3_C_queue(D0, D1, alpha, S, scale, shape, C, n)
    
    # Calcular tiempos de espera virtual para esta simulación
    # El tiempo de espera virtual es el tiempo de cola que experimentaría un cliente
    virtual_waits_sim <- res$queue_times[res$served==1]
    
    # Agregar a la lista total
    all_virtual_waits <- c(all_virtual_waits, virtual_waits_sim)
  }
  
  # Almacenar todos los tiempos de espera virtual para este slot
  virtual_waits[[slot]] <- all_virtual_waits
}

# Función para graficar todas las CDFs juntas
plot_combined_cdfs <- function(virtual_waits) {
  # Configurar el gráfico
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  # Colores y estilos de línea para cada slot
  colors <- c("G1" = "blue", "G2" = "red", "G3" = "darkgreen",
              "G4" = "purple", "G5" = "orange", "G6" = "brown")
  line_types <- c(1, 2, 3, 4, 5, 6)  # Diferentes estilos de línea
  
  # Encontrar el máximo tiempo de espera entre todos los slots para el eje x
  max_wait <- max(unlist(lapply(virtual_waits, function(x) if(length(x)>0) max(x) else 0)))
  
  # Crear gráfico vacío
  plot(0, 0, type = "n",
       xlim = c(0, 0.5),
       ylim = c(0.3, 1),
       xlab = "Tiempo de espera virtual",
       ylab = "Probabilidad acumulada",
       panel.first = grid())
  
  # Añadir cada CDF
  for (i in seq_along(virtual_waits)) {
    slot <- names(virtual_waits)[i]
    waits <- virtual_waits[[slot]]
    
    if (length(waits) > 0) {
      ecdf_fun <- ecdf(waits)
      x <- seq(0, max(waits), length.out = 1000)
      y <- ecdf_fun(x)
      
      lines(x, y, 
            col = colors[slot], 
            lwd = 2,
            lty = line_types[i])
    }
  }
  
  # Añadir leyenda
  legend("bottomright",
         legend = paste(names(virtual_waits), "(C =", C_values[names(virtual_waits)], ")"),
         col = colors,
         lty = line_types,
         lwd = 2,
         cex = 0.8,
         bty = "y")
  
  # Añadir línea horizontal en y=1
  abline(h = 1, lty = 3, col = "gray")
}

# Generar el gráfico combinado
plot_combined_cdfs(virtual_waits)


########################################################

# MISMO ANÁLISIS PARA G2 VARIANDO C

# Configuración de valores de C para cada slot
C_values <- c(3, 5, 7, 9, 11)

# Número de simulaciones
nsim <- 100

# Lista para almacenar los tiempos de espera virtual de cada slot
results_G2 <- list()

set.seed(123)  # Para reproducibilidad

for (C in C_values) {
  # Extraer parámetros del slot
  D0 <- matricesMAP2[["G2"]][["D0"]]
  D1 <- matricesMAP2[["G2"]][["D1"]]
  alpha <- paramPH3[["G2"]][["alpha"]]
  S <- paramPH3[["G2"]][["S"]]
  shape <- paramBS[["G2"]][["shape"]]
  scale <- paramBS[["G2"]][["scale"]]
  n <- slot_sizes[[2]]
  
  # Vector para almacenar todos los tiempos de espera virtual de las simulaciones
  all_waits <- numeric()
  
  for (i in 1:nsim) {
    res <- simulate_MAP2_PH3_C_queue(D0, D1, alpha, S, scale, shape, C, n)
    
    # Calcular tiempos de espera virtual para esta simulación
    # El tiempo de espera virtual es el tiempo de cola que experimentaría un cliente
    waits_sim <- res$queue_times[res$served==1]
    
    # Agregar a la lista total
    all_waits <- c(all_waits, waits_sim)
  }
  
  # Almacenar todos los tiempos de espera virtual para este slot
  results_G2[[as.character(C)]] <- all_waits
}


# Configurar el gráfico SIN grid (eliminamos panel.first)
plot(NA, xlim = c(0, 1), ylim = c(0, 1),
     main = "ECDF de tiempos de espera virtual - G2",
     xlab = "Tiempo de espera virtual", 
     ylab = "Probabilidad acumulada")

# Colores personalizados para cada C (puedes modificarlos)
cols <- c("C=3" = "firebrick3",
          "C=5" = "darkorange",
          "C=7" = "goldenrod2",
          "C=9" = "forestgreen",
          "C=11" = "dodgerblue3")

# Función para calcular ECDF con límite en x=1
ecdf_limited <- function(x, xmax = 1) {
  fn <- ecdf(x)
  function(x) {
    y <- fn(pmin(x, max(x)))
    y[x > xmax] <- 1
    y
  }
}

# Dibujar cada ECDF limitada
for (i in seq_along(results_G2)) {
  waits <- results_G2[[i]]
  if (length(waits) > 0) {
    x_vals <- seq(0, 1, length.out = 1000)
    y_vals <- ecdf_limited(waits)(x_vals)
    lines(x_vals, y_vals, col = cols[i], lwd = 2.5)
  }
}

# Añadir leyenda simple
legend("bottomright", 
       legend = paste("C =", C_values),
       col = cols,
       lwd = 2.5,
       cex = 0.9,
       bty = "y")  # bty="n" elimina el recuadro de la leyenda
