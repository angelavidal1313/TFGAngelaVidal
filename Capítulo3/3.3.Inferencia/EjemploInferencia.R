
# EJEMPLO INFERENCIA: Se simulan 50 llegadas de longitud 100 del MAP2 definido por las matrices D0 y D1

D0 <- matrix(c(-1.2, 0, 0.05, -4.5), nrow=2)
D1 <- matrix(c(1.1, 0.15, 0, 4.35), nrow=2)
set.seed(1122)
simulate_map2(100,D0,D1)

# Número de simulaciones
n_simulations <- 50

# Lista para almacenar los resultados
sim_results <- list()

# Realizar 50 simulaciones
for (i in 1:n_simulations) {
  sim_results[[i]] <- simulate_map2(100, D0, D1)$t
}

names(sim_results) <- paste0("Sim", 1:n_simulations)
head(sim_results, 2)

# Función para calcular los descriptores empiricos
calcular_descriptores_empiricos <- function(data, top_percent = 0.01) {

  stopifnot(is.list(data), all(sapply(data, is.numeric)))
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


# Ahora calculamos sus momentos empíricos

mom_emp_ejemplo <- calcular_descriptores_empiricos(sim_results)
mom_emp_ejemplo 


# Llevamos acabo la minimización de la eq 3.21 para hallar D0^ y D1^

# La función a minimizar (eq 13) es delta

set.seed(12)
x1 <- -runif(1, 0.1, 10)      # x1 <= 0
x2 <- runif(1, 0, -x1)         # x2 >= 0 y x1 + x2 <= 0
x3 <- -runif(1, 0.1, 10)       # x3 <= 0
x4 <- runif(1, 0, -x3)         # x4 >= 0 y x3 + x4 <= 0
x0_random <- c(x1, x2, x3, x4)

par <- c(1,mom_emp_ejemplo$rho,mom_emp_ejemplo$mu_1,mom_emp_ejemplo$mu_2,mom_emp_ejemplo$mu_3)

resultado <- optim(par=x0_random, 
                   fn = function(x) delta(x, par),
                   method = "Nelder-Mead")
x <- resultado$par
x

D0_est <- matrix(c(x[1], 0, x[2], x[3]), nrow = 2)
D1_est <- matrix(c(-x[1]-x[2], x[4], 0, -x[3]-x[4]), nrow = 2)
D0_est
D1_est

calcula_momentos(D0_est,D1_est)
mom_emp_ejemplo
calcula_momentos(D0,D1)


# COMPARACIÓN CORRELACIÓN EMPÍRICA VS TEÓRICA ESTIMADA

calcular_pi_estacionario <- function(D0, D1) {
  E <- D0 + D1
  m <- nrow(D0)
  
  # Construir sistema aumentado
  A <- rbind(t(E), rep(1, m))
  b <- c(rep(0, m), 1)
  
  # Resolver con descomposición QR (más estable)
  pi_vec <- qr.solve(A, b)
  
  # Verificar que es solución
  if(max(abs(pi_vec %*% E)) > 1e-8) {
    warning("El vector π podría no ser estacionario")
  }
  
  return(pi_vec)
}

correlacion_teorica_rho_t <- function(D0, D1, t) {
  require(expm)
  
  m <- nrow(D0)
  E <- D0 + D1
  e <- matrix(1, nrow = m, ncol = 1)
  
  # Calcular π de forma robusta
  pi_vec <- calcular_pi_estacionario(D0, D1)
  
  # Calcular lambda_star
  lambda_star <- as.numeric(pi_vec %*% D1 %*% e)
  
  # Términos intermedios
  I <- diag(m)
  term1 <- solve(E - e %*% pi_vec)
  expEt <- expm(E * t)
  
  # Mean(t)
  Mean_t <- lambda_star*t
  
  # Cálculo de phi(t)
  phi_t <- pi_vec %*% D1 %*% term1 %*% (expEt - I) %*% (expEt - I) %*% term1 %*% D1 %*% e
  phi_t <- as.numeric(phi_t)
  
  # Cálculo de Var(t)
  term_inv <- solve(e %*% pi_vec + E)
  term_D1e <- D1 %*% e
  
  Var_t <- (1 + 2 * lambda_star) * lambda_star * t - 
    2 * as.numeric(pi_vec %*% D1 %*% term_inv %*% term_D1e) * t -
    2 * as.numeric(pi_vec %*% D1 %*% (I - expEt) %*% term_inv %*% term_inv %*% term_D1e)
  
  # Calcular R(t)
  rho_t <- phi_t / Var_t
  
  return(list(rho=rho_t,Var_t=Var_t,Mean_t=Mean_t))
}


# Verificar π
pi_verif <- calcular_pi_estacionario(D0_est, D1_est)
print("Verificación de π:")
print(pi_verif %*% (D0_est + D1_est))  # Debería ser ≈ 0


calcular_correlacion_empirica <- function(data, max_lag = 20) {
  tiempos <- unlist(data)
  n <- length(tiempos)
  
  # Calcular autocorrelación para diferentes lags
  acf_result <- acf(tiempos, lag.max = max_lag, plot = FALSE)
  tau <- 0:max_lag
  R_empirica <- acf_result$acf
  
  list(tau = tau, R_empirica = R_empirica)
}

calcular_correlacion_tau <- function(data, tau) {
  tiempos <- unlist(data)
  
  # Calcular autocorrelación solo para el lag específico
  acf_result <- acf(tiempos, lag.max = tau, plot = FALSE)
  R_tau <- acf_result$acf[tau + 1]  # +1 porque el índice 1 corresponde a lag=0
  
  return(list(tau = tau, R_tau = R_tau))
}

# Función envolvente para calcular R(tau) teórica
calcular_correlacion_teorica <- function(D0, D1, max_lag = 20) {
  taus <- 0:max_lag
  R_teorica <- numeric(length(taus))
  
  for (i in seq_along(taus)) {
    tau <- taus[i]
    if (tau == 0) {
      R_teorica[i] <- 1 # Autocorrelación en lag 0 es 1
    } else {
      res <- correlacion_teorica_rho_t(D0, D1, t=tau)
      R_teorica[i] <- res$rho
    }
  }
  
  list(tau = taus, R_teorica = R_teorica)
}


# Calcular correlaciones
max_lag <- 20
empirica <- calcular_correlacion_empirica(sim_results, max_lag)
teorica <- calcular_correlacion_teorica(D0_est, D1_est, max_lag)


# Graficar (exactamente como lo tenías)
plot(empirica$tau, empirica$R_empirica, type = "l", lwd = 2, col = "black",
     xlab = expression(tau), ylab = expression(R(tau)),
     ylim = c(0, 0.25)) # Ajusté el límite y para mejor visualización
lines(teorica$tau, teorica$R_teorica, type = "l", lty = 2, lwd = 2, col = "red")
legend("topright", legend = c("Empírica", "Estimada"), 
       lty = c(1, 2), lwd = 2, col = c("black", "red"))


# EL NUEVO MÉTODO

# Minimizamos ahora la Eq 3.23 para diferentes valores de k

# k=1 <- t=1

# rho se calcula del 1% de muestra mas larga
mu1emp <- mom_emp_ejemplo$mu_1
mu2emp <- mom_emp_ejemplo$mu_2
mu3emp <- mom_emp_ejemplo$mu_3
ro1emp <- mom_emp_ejemplo$rho

t <- 1

# Dado que no tenemos un franja horaria especifica, la determinamos

# Paso 1: Calcular el máximo tiempo acumulado por cada columna (simulación)
max_por_columna <- sapply(sim_results, function(columna) {
  if (length(columna) == 0) return(0)  # Manejar columnas vacías
  max(cumsum(columna), na.rm = TRUE)  # Máximo del cumsum para la columna actual
})

# Paso 2: Obtener el máximo global
max_global <- ceiling(max(max_por_columna, na.rm = TRUE))

# Convertir a data frame
matriz_tiempos <- as.data.frame(sim_results)
calculo_corr <- calcular_correlacion_tau(sim_results,1)

# Solución inicial : la que minimiza el método anterior
x0 <- c(D0_est[1,1],D0_est[1,2],D0_est[2,2],D1_est[2,1])

# Configuración para k=1
t <- 1
r <- calculo_corr$R_tau
r

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est1 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est1 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est1)
  print("Matriz D1 estimada:")
  print(D1_est1)
}


# Vuelvo a repetir las gráficas

# Calcular correlaciones
max_lag <- 20
empirica <- calcular_correlacion_empirica(sim_results, max_lag)
teorica <- calcular_correlacion_teorica(D0_est, D1_est, max_lag)
teorica1 <- calcular_correlacion_teorica(D0_est1, D1_est1, max_lag)

# Graficar (exactamente como lo tenías)
plot(empirica$tau, empirica$R_empirica, type = "l", lwd = 2, col = "black",
     xlab = expression(tau), ylab = expression(R(tau)),
     main = "Función de correlación empírica vs. teórica",
     ylim = c(0, 0.25)) # Ajusté el límite y para mejor visualización
lines(teorica$tau, teorica$R_teorica, type = "l", lty = 2, lwd = 2, col = "red")
lines(teorica1$tau, teorica1$R_teorica, type = "l", lty = 2, lwd = 2, col = "blue")
legend("topright", legend = c("Empírica", "k=0","k=1"), 
       lty = c(1, 2,2), lwd = 2, col = c("black", "red", "blue"))


# k=3 <- t=(1,1.5,2)

t <- c(1,1.5,2)
r <- numeric(length(t))

for (i in seq_along(t)) {  # Usar seq_along(t) para iterar sobre todos los índices
  resultado <- calcular_correlacion_tau(sim_results,
    tau = t[i]
  )
  # Almacenar la correlación mediana (ajusta según lo que necesites)
  r[i] <- resultado$R_tau
}

r

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est3 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est3 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est3)
  print("Matriz D1 estimada:")
  print(D1_est3)
}
?constrOptim

# Vuelvo a repetir las gráficas

# Calcular correlaciones
max_lag <- 20
empirica <- calcular_correlacion_empirica(sim_results, max_lag)
teorica <- calcular_correlacion_teorica(D0_est, D1_est, max_lag)
teorica1 <- calcular_correlacion_teorica(D0_est1, D1_est1, max_lag)
teorica3 <- calcular_correlacion_teorica(D0_est3, D1_est3, max_lag)

# Graficar
plot(empirica$tau, empirica$R_empirica, type = "l", lwd = 2, col = "black",
     xlab = expression(tau), ylab = expression(R(tau)),
     ylim = c(0, 0.25)) # Ajusté el límite y para mejor visualización
lines(teorica$tau, teorica$R_teorica, type = "l", lty = 2, lwd = 2, col = "red")
lines(teorica1$tau, teorica1$R_teorica, type = "l", lty = 2, lwd = 2, col = "blue")
lines(teorica1$tau, teorica3$R_teorica, type = "l", lty = 2, lwd = 2, col = "green")
legend("topright", legend = c("Empírica", "k=0","k=1","k=3"), 
       lty = c(1, 2,2,2), lwd = 2, col = c("black", "red", "blue","green"))



