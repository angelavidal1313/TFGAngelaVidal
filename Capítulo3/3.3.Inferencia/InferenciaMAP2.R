
# INFERENCIA MAP2 PARA CADA SLOT


# PARA G1

# Cargar datos de tiempos entre llegadas (ejemplo)
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_7_9h.xlsx",
                 sheet = 1, col_names = FALSE)
tiempos_llegadas_g1 <- unlist(g1)
tiempos_llegadas_g1 <- tiempos_llegadas_g1[!is.na(tiempos_llegadas_g1)]
momentos_g1 <- calcular_momentos(tiempos_llegadas_g1)
g1_list <- lapply(g1, function(col) as.numeric(na.omit(col)))
momentos_emp_g1 <- calcular_descriptores_empiricos(g1_list)

mu1emp <- momentos_g1[[1]]
mu2emp <- momentos_g1[[2]]
mu3emp <- momentos_g1[[3]]
ro1emp <- momentos_g1[[4]]

# rho se calcula del 1% de muestra mas larga
mu1emp <- momentos_emp_g1$mu_1
mu2emp <- momentos_emp_g1$mu_2
mu3emp <- momentos_emp_g1$mu_3
ro1emp <- momentos_emp_g1$rho

calculo_corr <- n_tau_with_correlations(g1,0.25,2)

# Solución inicial calculado x0_g1

# Configuración para k=1
t <- 0.25
r <- calculo_corr$median_correlation

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g1, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g1 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g1 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g1)
  print("Matriz D1 estimada:")
  print(D1_est_g1)
}


# PARA G2

# Cargar datos de tiempos entre llegadas (ejemplo)
g2 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_9_11h.xlsx",
                 sheet = 1, col_names = FALSE)
tiempos_llegadas_g2 <- unlist(g2)
tiempos_llegadas_g2 <- tiempos_llegadas_g2[!is.na(tiempos_llegadas_g2)]
momentos_g2 <- calcular_momentos(tiempos_llegadas_g2)
g2_list <- lapply(g2, function(col) as.numeric(na.omit(col)))
momentos_emp_g2 <- calcular_descriptores_empiricos(g2_list)

mu1emp <- momentos_g2[[1]]
mu2emp <- momentos_g2[[2]]
mu3emp <- momentos_g2[[3]]
ro1emp <- momentos_g2[[4]]

# rho se calcula del 1% de muestra mas larga
mu1emp <- momentos_emp_g2$mu_1
mu2emp <- momentos_emp_g2$mu_2
mu3emp <- momentos_emp_g2$mu_3
ro1emp <- momentos_emp_g2$rho

calculo_corr <- n_tau_with_correlations(g2,0.25,2)

# Configuración para k=1
t <- 0.25
r <- calculo_corr$median_correlation

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g2, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g2 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g2 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g2)
  print("Matriz D1 estimada:")
  print(D1_est_g2)
}


# PARA G3

# Cargar datos de tiempos entre llegadas (ejemplo)
g3 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_11_16h.xlsx",
                 sheet = 1, col_names = FALSE)
tiempos_llegadas_g3 <- unlist(g3)
tiempos_llegadas_g3 <- tiempos_llegadas_g3[!is.na(tiempos_llegadas_g3)]
momentos_g3 <- calcular_momentos(tiempos_llegadas_g3)
g3_list <- lapply(g3, function(col) as.numeric(na.omit(col)))
momentos_emp_g3 <- calcular_descriptores_empiricos(g3_list)

mu1emp <- momentos_g3[[1]]
mu2emp <- momentos_g3[[2]]
mu3emp <- momentos_g3[[3]]
ro1emp <- momentos_g3[[4]]

# rho se calcula del 1% de muestra mas larga
mu1emp <- momentos_emp_g3$mu_1
mu2emp <- momentos_emp_g3$mu_2
mu3emp <- momentos_emp_g3$mu_3
ro1emp <- momentos_emp_g3$rho

calculo_corr <- n_tau_with_correlations(g3,0.25,5)

# Configuración para k=1
t <- 0.25
r <- calculo_corr$median_correlation

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g3, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g3 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g3 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g3)
  print("Matriz D1 estimada:")
  print(D1_est_g3)
}



# PARA G4

# Cargar datos de tiempos entre llegadas (ejemplo)
g4 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_16_17h.xlsx",
                 sheet = 1, col_names = FALSE)
tiempos_llegadas_g4 <- unlist(g4)
tiempos_llegadas_g4 <- tiempos_llegadas_g4[!is.na(tiempos_llegadas_g4)]
momentos_g4 <- calcular_momentos(tiempos_llegadas_g4)
g4_list <- lapply(g4, function(col) as.numeric(na.omit(col)))
momentos_emp_g4 <- calcular_descriptores_empiricos(g4_list)

mu1emp <- momentos_g4[[1]]
mu2emp <- momentos_g4[[2]]
mu3emp <- momentos_g4[[3]]
ro1emp <- momentos_g4[[4]]

# rho se calcula del 1% de muestra mas larga
mu1emp <- momentos_emp_g4$mu_1
mu2emp <- momentos_emp_g4$mu_2
mu3emp <- momentos_emp_g4$mu_3
ro1emp <- momentos_emp_g4$rho

calculo_corr <- n_tau_with_correlations(g4,0.25,1)

# Configuración para k=1
t <- 0.25
r <- calculo_corr$median_correlation

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g4, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g4 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g4 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g4)
  print("Matriz D1 estimada:")
  print(D1_est_g4)
}


# PARA G5

# Cargar datos de tiempos entre llegadas (ejemplo)
g5 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_17_22h.xlsx",
                 sheet = 1, col_names = FALSE)
tiempos_llegadas_g5 <- unlist(g5)
tiempos_llegadas_g5 <- tiempos_llegadas_g5[!is.na(tiempos_llegadas_g5)]
momentos_g5 <- calcular_momentos(tiempos_llegadas_g5)
g5_list <- lapply(g5, function(col) as.numeric(na.omit(col)))
momentos_emp_g5 <- calcular_descriptores_empiricos(g5_list)

mu1emp <- momentos_g5[[1]]
mu2emp <- momentos_g5[[2]]
mu3emp <- momentos_g5[[3]]
ro1emp <- momentos_g5[[4]]

# rho se calcula del 1% de muestra mas larga
mu1emp <- momentos_emp_g5$mu_1
mu2emp <- momentos_emp_g5$mu_2
mu3emp <- momentos_emp_g5$mu_3
ro1emp <- momentos_emp_g5$rho

calculo_corr <- n_tau_with_correlations(g5,0.25,5)

# Configuración para k=1
t <- 0.25
r <- calculo_corr$median_correlation

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g5, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g5 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g5 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g5)
  print("Matriz D1 estimada:")
  print(D1_est_g5)
}


# PARA G6

# Cargar datos de tiempos entre llegadas (ejemplo)
g6 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_22_24h.xlsx",
                 sheet = 1, col_names = FALSE)
tiempos_llegadas_g6 <- unlist(g6)
tiempos_llegadas_g6 <- tiempos_llegadas_g6[!is.na(tiempos_llegadas_g6)]
momentos_g6 <- calcular_momentos(tiempos_llegadas_g6)
g6_list <- lapply(g6, function(col) as.numeric(na.omit(col)))
momentos_emp_g6 <- calcular_descriptores_empiricos(g6_list)

mu1emp <- momentos_g6[[1]]
mu2emp <- momentos_g6[[2]]
mu3emp <- momentos_g6[[3]]
ro1emp <- momentos_g6[[4]]

# rho se calcula del 1% de muestra mas larga
mu1emp <- momentos_emp_g6$mu_1
mu2emp <- momentos_emp_g6$mu_2
mu3emp <- momentos_emp_g6$mu_3
ro1emp <- momentos_emp_g6$rho

calculo_corr <- n_tau_with_correlations(g6,0.25,2)

# Configuración para k=1
t <- 0.25
r <- calculo_corr$median_correlation

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g6, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g6 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g6 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g6)
  print("Matriz D1 estimada:")
  print(D1_est_g6)
}


matricesMAP2_minf2 <- list(
  G1 = list(
    D0 = D0_est_g1,
    D1 = D1_est_g1
  ),
  
  G2 = list(
    D0 = D0_est_g2,
    D1 = D1_est_g2
  ),
  
  G3 = list(
    D0 = D0_est_g3,
    D1 = D1_est_g3
  ),
  
  G4 = list(
    D0 = D0_est_g4,
    D1 = D1_est_g4
  ),
  
  G5 = list(
    D0 = D0_est_g5,
    D1 = D1_est_g5
  ),
  
  G6 = list(
    D0 = D0_est_g6,
    D1 = D1_est_g6
  )
)
