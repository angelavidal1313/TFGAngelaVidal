delta <- function(x,par){
  
  tau = par[1] 
  ro1emp = par[2] 
  mu1emp = par[3] 
  mu2emp = par[4] 
  mu3emp = par[5]
  
  f = (1/x[3]/x[1]*x[2]*x[4]*(x[3]^3*x[1]+2*x[4]*x[3]^2*x[1]+x[4]^2*x[3]*x[1]-3*x[1]*x[3]^2*x[2]-5*x[1]*x[3]*x[2]*x[4]-3*x[1]^2*x[3]*x[4]+x[3]^3*x[2]+x[1]^3*x[4]-x[4]^2*x[1]^2+2*x[2]*x[1]^2*x[4]-2*x[2]*x[4]^2*x[1]-2*x[2]^2*x[4]*x[3]-x[2]^2*x[3]^2+x[4]^2*x[3]*x[2]+2*x[4]*x[3]^2*x[2]+x[2]^2*x[1]*x[3]+2*x[2]*x[1]^2*x[3]+x[2]^2*x[1]*x[4]+x[1]^3*x[3]-2*x[1]^2*x[3]^2-x[2]^2*x[4]^2)/(2*x[4]*x[3]^2*x[2]+2*x[4]^2*x[3]*x[2]+x[4]^2*x[3]*x[1]-2*x[2]^2*x[4]*x[3]-2*x[2]^2*x[4]^2-2*x[2]*x[4]^2*x[1]+x[2]^2*x[1]*x[3]+2*x[2]^2*x[1]*x[4]+2*x[2]*x[1]^2*x[4]-2*x[1]*x[3]*x[2]*x[4])-ro1emp)^2 + tau*(((-(x[4]+x[2])/(x[3]*x[2]+x[2]*x[4]+x[4]*x[1])-mu1emp)/mu1emp)^2 + ((2/x[1]/x[3]*(x[4]*x[3]-x[2]*x[4]+x[2]*x[1])/(x[3]*x[2]+x[2]*x[4]+x[4]*x[1])-mu2emp)/mu2emp)^2 + ((-6/x[1]^2/x[3]^2*(x[4]*x[3]^2-x[2]*x[4]*x[3]-x[2]*x[4]*x[1]+x[2]*x[1]^2)/(x[3]*x[2]+x[2]*x[4]+x[4]*x[1])-mu3emp)/mu3emp)^2);
  
  return (f)
}


delta_beta <- function(x, par, t, r){
  
  
  tryCatch({
    
    tau1 = par[1] 
    tau2=par[2]
    ro1emp = par[3] 
    mu1emp = par[4] 
    mu2emp = par[5] 
    mu3emp = par[6]
    
    f <-  tau2*(1/x[3]/x[1]*x[2]*x[4]*(x[3]^3*x[1]+2*x[4]*x[3]^2*x[1]+x[4]^2*x[3]*x[1]-3*x[1]*x[3]^2*x[2]-5*x[1]*x[3]*x[2]*x[4]-3*x[1]^2*x[3]*x[4]+x[3]^3*x[2]+x[1]^3*x[4]-x[4]^2*x[1]^2+2*x[2]*x[1]^2*x[4]-2*x[2]*x[4]^2*x[1]-2*x[2]^2*x[4]*x[3]-x[2]^2*x[3]^2+x[4]^2*x[3]*x[2]+2*x[4]*x[3]^2*x[2]+x[2]^2*x[1]*x[3]+2*x[2]*x[1]^2*x[3]+x[2]^2*x[1]*x[4]+x[1]^3*x[3]-2*x[1]^2*x[3]^2-x[2]^2*x[4]^2)/(2*x[4]*x[3]^2*x[2]+2*x[4]^2*x[3]*x[2]+x[4]^2*x[3]*x[1]-2*x[2]^2*x[4]*x[3]-2*x[2]^2*x[4]^2-2*x[2]*x[4]^2*x[1]+x[2]^2*x[1]*x[3]+2*x[2]^2*x[1]*x[4]+2*x[2]*x[1]^2*x[4]-2*x[1]*x[3]*x[2]*x[4])-ro1emp)^2 + tau2*(((-(x[4]+x[2])/(x[3]*x[2]+x[2]*x[4]+x[4]*x[1])-mu1emp)/mu1emp)^2 + ((2/x[1]/x[3]*(x[4]*x[3]-x[2]*x[4]+x[2]*x[1])/(x[3]*x[2]+x[2]*x[4]+x[4]*x[1])-mu2emp)/mu2emp)^2 + ((-6/x[1]^2/x[3]^2*(x[4]*x[3]^2-x[2]*x[4]*x[3]-x[2]*x[4]*x[1]+x[2]*x[1]^2)/(x[3]*x[2]+x[2]*x[4]+x[4]*x[1])-mu3emp)/mu3emp)^2)
    f_aux <- 0
    
    for(i in 1:length(t)){
      f_aux= tau1*(r[i] + (x[4]*x[2]*exp(-2*t[i]*(x[4] + x[2]))*(exp(t[i]*(x[4] + x[2])) - 1)^2*(x[3] + x[4] - x[1])*(x[1] - x[3] + x[2]))/((x[4] + x[2])^4*((exp(-2*t[i]*(x[4] + x[2]))*(2*x[4]^2*x[2]^2 + 2*x[3]*x[4]*x[2]^2 - 2*x[3]*x[4]^2*x[2] - 2*x[3]^2*x[4]*x[2] - 2*x[4]*x[1]*x[2]^2 - 2*x[4]*x[1]^2*x[2] + 2*x[4]^2*x[1]*x[2] - 2*x[4]^2*x[2]^2*exp(t[i]*(x[4] + x[2])) + 5*t[i]*x[4]^2*x[2]^3*exp(t[i]*(x[4] + x[2])) + 5*t[i]*x[4]^3*x[2]^2*exp(t[i]*(x[4] + x[2])) + 4*x[3]*x[4]*x[1]*x[2] + t[i]*x[3]*x[2]^4*exp(t[i]*(x[4] + x[2])) + t[i]*x[4]^4*x[1]*exp(t[i]*(x[4] + x[2])) + t[i]*x[4]*x[2]^4*exp(t[i]*(x[4] + x[2])) + t[i]*x[4]^4*x[2]*exp(t[i]*(x[4] + x[2])) - 2*x[3]*x[4]*x[2]^2*exp(t[i]*(x[4] + x[2])) + 2*x[3]*x[4]^2*x[2]*exp(t[i]*(x[4] + x[2])) + 2*x[3]^2*x[4]*x[2]*exp(t[i]*(x[4] + x[2])) + 2*x[4]*x[1]*x[2]^2*exp(t[i]*(x[4] + x[2])) + 2*x[4]*x[1]^2*x[2]*exp(t[i]*(x[4] + x[2])) - 2*x[4]^2*x[1]*x[2]*exp(t[i]*(x[4] + x[2])) + 5*t[i]*x[3]*x[4]*x[2]^3*exp(t[i]*(x[4] + x[2])) - t[i]*x[3]*x[4]^3*x[2]*exp(t[i]*(x[4] + x[2])) - t[i]*x[4]*x[1]*x[2]^3*exp(t[i]*(x[4] + x[2])) + 5*t[i]*x[4]^3*x[1]*x[2]*exp(t[i]*(x[4] + x[2])) + 3*t[i]*x[3]*x[4]^2*x[2]^2*exp(t[i]*(x[4] + x[2])) - 2*t[i]*x[3]^2*x[4]*x[2]^2*exp(t[i]*(x[4] + x[2])) - 2*t[i]*x[3]^2*x[4]^2*x[2]*exp(t[i]*(x[4] + x[2])) - 2*t[i]*x[4]*x[1]^2*x[2]^2*exp(t[i]*(x[4] + x[2])) + 3*t[i]*x[4]^2*x[1]*x[2]^2*exp(t[i]*(x[4] + x[2])) - 2*t[i]*x[4]^2*x[1]^2*x[2]*exp(t[i]*(x[4] + x[2])) - 4*x[3]*x[4]*x[1]*x[2]*exp(t[i]*(x[4] + x[2])) + 4*t[i]*x[3]*x[4]*x[1]*x[2]^2*exp(t[i]*(x[4] + x[2])) + 4*t[i]*x[3]*x[4]^2*x[1]*x[2]*exp(t[i]*(x[4] + x[2])))^2)/(x[4] + x[2])^8)^(1/2)))^2   
    }
    
    return(f= f+f_aux)
    
  }, error = function(e) {
    message("Error interno en delta_beta: ", e$message)
    return(Inf)
  })
  
}


inference_new <- function(ro1emp, mu1emp, mu2emp, mu3emp, x0, t, r) {
  # Esta función ajusta un proceso MAP2 a datos empíricos
  
  # INPUT:
  # ro1emp: autocorrelación de primer orden
  # mu1emp, mu2emp, mu3emp: momentos empíricos de primer, segundo y tercer orden
  # x0: solución inicial para el problema de optimización (4 parámetros del MAP2)
  # t: vector de intervalos de tiempo para conteo de llegadas
  # r: vector de correlación del proceso de conteo
  
  # OUTPUT:
  # K: vector con los mejores parámetros encontrados
  # [tau1, tau2, error, D0_11, D0_12, D0_22, D1_21]
  
  i <- 0
  C <- matrix(ncol=7, nrow=0)  # Matriz para almacenar resultados
  
  # Vectores de pesos para los parámetros en el problema de optimización
  tau <- 2^seq(1, 15) # beta2
  tau_aux <- 2^seq(-10, 15) # beta1
  
  for (tau1 in tau) {
    for (tau2 in tau_aux) {
      
      par <- c(tau1, tau2, ro1emp, mu1emp, mu2emp, mu3emp)
      
      # Restricciones A*x <= B convertidas para constrOptim
      # ui %*% theta - ci >= 0
      ui <- rbind(
        c(0, 1, 0, 0),    # y ≥ 0
        c(0, 0, 0, 1),     # v ≥ 0
        c(-1, 0, 0, 0),     # x ≤ 0 → -x ≥ 0
        c(0, 0, -1, 0),     # u ≤ 0 → -u ≥ 0
        c(-1, -1, 0, 0),   # x + y ≤ 0 → -x - y ≥ 0
        c(0, 0, -1, -1)    # u + v ≤ 0 → -u - v ≥ 0
      )
      ci <- c(0, 0, 0, 0, 0, 0)
      
      # Optimización con restricciones
      opt_result <- constrOptim(
        theta = x0,
        f = function(x) minimiza_marcos(x, par, t, r),
        grad = NULL,
        ui = ui,
        ci = ci,
        method = "Nelder-Mead",
        control = list(pgtol = 1e-8,
                       factr=10000),
      )
      
      # Extraer resultados
      x <- opt_result$par
      fval <- opt_result$value
      
      # Construir matrices D0 y D1 del MAP2
      D0_est <- matrix(c(x[1], 0, x[2], x[3]), nrow=2)
      D1_est <- matrix(c(-x[1]-x[2], x[4], 0, -x[3]-x[4]), nrow=2)
      
      # Almacenar resultados
      current_result <- c(tau1, tau2, fval, D0_est[1,1], D0_est[1,2], D0_est[2,2], D1_est[2,1])
      C <- rbind(C, current_result)
      
      i <- i + 1
      # Opcional: imprimir progreso
      if (i %% 10 == 0) message("Iteración: ", i)
    }
  }
  
  # Añadir al final (antes del último }):
  if(nrow(C) == 0) {
    warning("Ninguna optimización convergió")
    return(NULL)
  }
  
  # Encontrar la fila con menor valor de fval (columna 3)
  best_idx <- which.min(C[,3])
  K <- C[best_idx,]
  
  return(K)
  

}


calcular_descriptores_empiricos <- function(data, top_percent = 0.01) {
  # Verificar que los datos sean una lista de vectores numéricos
  stopifnot(is.list(data), all(sapply(data, is.numeric)))
  
  # Aplanar todos los datos para calcular momentos
  tiempos <- unlist(data)
  n_total <- length(tiempos)
  
  mu_1 <- mean(tiempos)
  mu_2 <- mean(tiempos^2)
  mu_3 <- mean(tiempos^3)
  
  # 1. Seleccionar el top_percent de muestras más largas
  longitudes <- sapply(data, length)
  n_muestras <- length(data)
  n_seleccion <- max(1, round(n_muestras * top_percent))  # Al menos 1 muestra
  
  indices_largas <- order(longitudes, decreasing = TRUE)[1:n_seleccion]
  muestras_largas <- data[indices_largas]
  
  # 2. Calcular autocorrelación lag-1 para cada muestra seleccionada
  rhos <- sapply(muestras_largas, function(x) {
    if (length(x) < 2) return(NA)  # Se necesita al menos 2 observaciones
    cor(x[-1], x[-length(x)], method = "pearson")  # Coef. autocorrelación lag-1
  })
  
  # 3. Filtrar NAs y calcular la mediana
  rhos <- rhos[!is.na(rhos)]
  rho <- if (length(rhos) > 0) median(rhos) else NA
  
  list(
    mu_1 = mu_1,
    mu_2 = mu_2,
    mu_3 = mu_3,
    rho = rho
  )
}


n_tau_with_correlations <- function(datos, tau, hours) {
  num_days <- ncol(datos)  # Número de días
  num_int <- floor(hours/tau)  # Número de intervalos (ahora adaptable a hours)
  inicio_int <- seq(0, hours, by = tau)  # Puntos de inicio
  
  # Matriz para almacenar conteos por intervalo
  count_matrix <- matrix(0, nrow = num_days, ncol = num_int)
  
  for (day in 1:num_days) {
    tiempos <- as.numeric(datos[[day]])
    tiempos <- tiempos[!is.na(tiempos)]
    
    if (length(tiempos) == 0) next
    
    tiempos_acum <- cumsum(tiempos)
    tiempos_acum <- tiempos_acum[tiempos_acum <= hours]  # Filtrar por horas especificadas
    
    for (i in 1:num_int) {
      inicio <- inicio_int[i]
      fin <- inicio_int[i+1]
      count_matrix[day, i] <- sum(tiempos_acum > inicio & tiempos_acum <= fin)
    }
  }
  
  # Eliminar columnas sin variabilidad (todas cero o constantes)
  keep_cols <- apply(count_matrix, 2, function(x) var(x, na.rm = TRUE) > 0)
  count_matrix <- count_matrix[, keep_cols, drop = FALSE]
  
  # Calcular estadísticas si hay suficientes datos
  if (ncol(count_matrix) >= 2) {
    # Correlaciones entre intervalos consecutivos
    correlations <- sapply(1:(ncol(count_matrix)-1), function(i) {
      cor(count_matrix[, i], count_matrix[, i+1], use = "complete.obs")
    })
    
    # Medias y varianzas por intervalo
    means <- apply(count_matrix, 2, mean, na.rm = TRUE)
    variances <- apply(count_matrix, 2, var, na.rm = TRUE)
    
    median_corr <- median(correlations, na.rm = TRUE)
    median_mean <- median(means, na.rm = TRUE)
    median_var <- median(variances, na.rm = TRUE)
    
  } else {
    median_corr <- NA_real_
    median_mean <- NA_real_
    median_var <- NA_real_
  }

  return(list(
    count_matrix = count_matrix,
    median_correlation = median_corr,
    median_mean = median_mean,
    median_variance = median_var,
    all_correlations = if (exists("correlations")) correlations else NA,
    all_means = if (exists("means")) means else NA,
    all_variances = if (exists("variances")) variances else NA
  ))
}
