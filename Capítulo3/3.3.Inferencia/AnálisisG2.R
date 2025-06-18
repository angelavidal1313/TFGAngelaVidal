
# FIGURA 11

# PARA G2

library(readxl)

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


# Configuración para k=1
calculo_corr <- n_tau_with_correlations(g2,0.25,2)
t <- 0.25
r <- calculo_corr$median_correlation
r

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g2, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g2_k1 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g2_k1 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g2_k1)
  print("Matriz D1 estimada:")
  print(D1_est_g2_k1)
}

taus <- c(0.25,0.5,0.75,1)
corr_g2_k1 <- vector("list", length(taus))

for (i in seq_along(taus)) {
  corr_g2_k1[[i]] <- correlacion_teorica_rho_t(D0_est_g2_k1, D1_est_g2_k1, taus[i])
}
corr_g2_k1


# Configuración para k=2

t <- c(0.25,0.5)
r <- numeric(length(t))

for (i in seq_along(t)) {  # Usar seq_along(t) para iterar sobre todos los índices
  resultado <- n_tau_with_correlations(
    datos = g2,
    tau = t[i],
    hours = 2
  )
  # Almacenar la correlación mediana (ajusta según lo que necesites)
  r[i] <- resultado$median_correlation
}

r

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g2, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g2_k2 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g2_k2 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g2_k2)
  print("Matriz D1 estimada:")
  print(D1_est_g2_k2)
}

taus <- c(0.25,0.5,0.75,1)
corr_g2_k2 <- vector("list", length(taus))

for (i in seq_along(taus)) {
  corr_g2_k2[[i]] <- correlacion_teorica_rho_t(D0_est_g2_k2, D1_est_g2_k2, taus[i])
}
corr_g2_k2



# Configuración para k=3

t <- c(0.25,0.5,0.75)
r <- numeric(length(t))

for (i in seq_along(t)) {  # Usar seq_along(t) para iterar sobre todos los índices
  resultado <- n_tau_with_correlations(
    datos = g2,
    tau = t[i],
    hours = 2
  )
  # Almacenar la correlación mediana (ajusta según lo que necesites)
  r[i] <- resultado$median_correlation
}

r

K <- inference_new(ro1emp, mu1emp, mu2emp, mu3emp, x0_g2, t, r)
K

# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices# Verificar resultados antes de construir matrices
if (is.null(K)) {
  stop("La optimización no produjo resultados válidos. Revisa los parámetros iniciales y la función minimiza().")
} else {
  # Construir matrices solo si K es válido
  D0_est_g2_k3 <- matrix(c(K[4], 0, K[5], K[6]), nrow = 2)
  D1_est_g2_k3 <- matrix(c(-K[4]-K[5], K[7], 0, -K[6]-K[7]), nrow = 2)
  
  print("Matriz D0 estimada:")
  print(D0_est_g2_k3)
  print("Matriz D1 estimada:")
  print(D1_est_g2_k3)
}

taus <- c(0.25,0.5,0.75,1)
corr_g2_k3 <- vector("list", length(taus))

for (i in seq_along(taus)) {
  corr_g2_k3[[i]] <- correlacion_teorica_rho_t(D0_est_g2_k3, D1_est_g2_k3, taus[i])
}
corr_g2_k3


par(mfrow=c(2,2))
# DÍA 13 - Comparación de CDF empírica vs teóricas
library(ggplot2)
library(expm)

dia13_g2 <- g2$...13

# Función para calcular la CDF teórica de un MAP2
cdf_teorica_map2 <- function(x, D0, D1) {
  p <- stationary(D0,D1)  # Distribución estacionaria
  lambda <- -diag(D0)       # Tasas de salida
  1 - p %*% expm(D0 * x) %*% rep(1, nrow(D0))
}

# Rango de valores para evaluar la CDF
x_vals <- seq(0, 0.07, length.out = 100)

# Calcular CDF teóricas para cada k
cdf_k1 <- sapply(x_vals, function(x) cdf_teorica_map2(x, D0_est_g2, D1_est_g2))
cdf_k2 <- sapply(x_vals, function(x) cdf_teorica_map2(x, D0_est_g2_k2, D1_est_g2_k2))
cdf_k3 <- sapply(x_vals, function(x) cdf_teorica_map2(x, D0_est_g2_k3, D1_est_g2_k3))

# Crear data frame para ggplot
plot_data <- data.frame(
  x = c(x_vals, x_vals, x_vals, dia13_g2),
  y = c(cdf_k1, cdf_k2, cdf_k3, ecdf(dia13_g2)(dia13_g2)),
  group = factor(c(
    rep("k=1", length(x_vals)),
    rep("k=2", length(x_vals)),
    rep("k=3", length(x_vals)),
    rep("Datos empíricos", length(dia13_g2))
  ))
)

# Colores personalizados
colores <- c("Datos empíricos" = "red", 
             "k=1" = "blue",
             "k=2" = "orange",
             "k=3" = "purple")

# Crear el gráfico
ggplot(plot_data, aes(x = x, y = y, color = group)) +
  # Datos empíricos (puntos)
  geom_point(data = subset(plot_data, group == "Datos empíricos"), 
             aes(color = group), alpha = 0.6, size = 1) +
  # CDFs teóricas (líneas)
  geom_line(data = subset(plot_data, group != "Datos empíricos"), 
            aes(color = group), size = 0.8) +
  labs(x = "Tiempos entre llegadas",
       y = "Probabilidad Acumulada") +
  scale_color_manual(values = colores,name=NULL) +
  theme_minimal() +
  theme(legend.position = c(0.95, 0.05),  # Posición abajo-derecha (coordenadas normalizadas)
        legend.justification = c(1, 0),    # Punto de anclaje
        legend.background = element_rect(fill = "white", color = "black"),  # Cuadro blanco con borde
        legend.box.margin = margin(10, 10, 10, 10),
        # Ajustes para agrandar la leyenda:
        legend.key.size = unit(1.5, "lines"),       # Tamaño de los símbolos/colores
        legend.text = element_text(size = 12),      # Tamaño del texto
        legend.title = element_text(size = 14, face = "bold"), # Título (si lo mantienes)
        legend.spacing.y = unit(0.5, "cm")          # Espacio entre elementos
  )


# DÍA 26

library(ggplot2)
library(expm)

dia26_g2 <- g2$...26

# Función para calcular la CDF teórica de un MAP2
cdf_teorica_map2 <- function(x, D0, D1) {
  p <- stationary(D0,D1)  # Distribución estacionaria
  lambda <- -diag(D0)       # Tasas de salida
  1 - p %*% expm(D0 * x) %*% rep(1, nrow(D0))
}

# Rango de valores para evaluar la CDF
x_vals <- seq(0, 0.07, length.out = 100)

# Calcular CDF teóricas para cada k
cdf_k1 <- sapply(x_vals, function(x) cdf_teorica_map2(x, D0_est_g2_k1, D1_est_g2_k1))
cdf_k2 <- sapply(x_vals, function(x) cdf_teorica_map2(x, D0_est_g2_k2, D1_est_g2_k2))
cdf_k3 <- sapply(x_vals, function(x) cdf_teorica_map2(x, D0_est_g2_k3, D1_est_g2_k3))

# Crear data frame para ggplot
plot_data <- data.frame(
  x = c(x_vals, x_vals, x_vals, dia26_g2),
  y = c(cdf_k1, cdf_k2, cdf_k3, ecdf(dia26_g2)(dia26_g2)),
  group = factor(c(
    rep("k=1", length(x_vals)),
    rep("k=2", length(x_vals)),
    rep("k=3", length(x_vals)),
    rep("Datos empíricos", length(dia26_g2))
  ))
)

# Colores personalizados
colores <- c("Datos empíricos" = "red", 
             "k=1" = "blue",
             "k=2" = "orange",
             "k=3" = "purple")

# Crear el gráfico
ggplot(plot_data, aes(x = x, y = y, color = group)) +
  # Datos empíricos (puntos)
  geom_point(data = subset(plot_data, group == "Datos empíricos"), 
             aes(color = group), alpha = 0.6, size = 1) +
  # CDFs teóricas (líneas)
  geom_line(data = subset(plot_data, group != "Datos empíricos"), 
            aes(color = group), size = 0.8) +
  labs(x = "Tiempos entre llegadas",
       y = "Probabilidad Acumulada") +
  scale_color_manual(values = colores,name=NULL) +
  theme_minimal() +
  theme(legend.position = c(0.95, 0.05),  # Posición abajo-derecha (coordenadas normalizadas)
        legend.justification = c(1, 0),    # Punto de anclaje
        legend.background = element_rect(fill = "white", color = "black"),  # Cuadro blanco con borde
        legend.box.margin = margin(10, 10, 10, 10),
        # Ajustes para agrandar la leyenda:
        legend.key.size = unit(1.5, "lines"),       # Tamaño de los símbolos/colores
        legend.text = element_text(size = 12),      # Tamaño del texto
        legend.title = element_text(size = 14, face = "bold"), # Título (si lo mantienes)
        legend.spacing.y = unit(0.5, "cm")          # Espacio entre elementos
  )



# TABLA
library(kableExtra)

# 1. Datos originales (8 columnas)
tabla_data <- data.frame(
  τ = rep(c(0.25, 0.5, 0.75, 1), each = 3),
  E_Emp = rep(c(27.045, 53.114, 79.625, 104.886), each = 3),
  k_E = rep(paste0("k=",1:3), times = 4),
  E_Ajust = c(27.012, 26.989, 27.429, 54.024, 53.977, 54.858, 81.036, 80.966, 82.287, 108.048, 107.955, 109.717),
  V_Emp = rep(c(33.121, 82.304, 152.62, 231.365), each = 3),
  k_V = rep(paste0("k=",1:3), times = 4),
  V_Ajust = c(45.131, 46.38, 47.716, 115.46, 124.137, 134.461, 200.004, 224.495, 258.019, 292.567, 341.143, 416.3),
  R_Emp = rep(c(0.279, 0.374, 0.607, 0.585), each = 3),
  k_R = rep(paste0("k=",1:3), times = 4),
  R_Ajust = c(0.279, 0.338, 0.409, 0.267, 0.374, 0.548, 0.223, 0.351, 0.607, 0.183, 0.314, 0.632),
  VtM_Emp = rep(c(1.225, 1.55, 1.917, 2.206), each = 3),
  k_VtM = rep(paste0("k=",1:3), times = 4),
  VtM_Ajust = c(1.671, 1.718, 1.74, 2.137, 2.3, 2.45, 2.467, 2.773, 3.136, 2.708, 3.16,3.794)
  
)

# Versión estable que funciona:
tabla_final <- tabla_data %>%
  kable(
    format = "html",
    align = 'c',
    col.names = c("τ", "Emp", "k", "Ajust", "Emp", "k", "Ajust", "Emp", "k", "Ajust","Emp", "k", "Ajust"),
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  ) %>%
  add_header_above(c(
    " " = 1,
    "E[N(τ)]" = 3,
    "V[N(τ)]" = 3,
    "R(τ)" = 3,
    "VtM[N(τ)]" = 3
  )) %>%
  # Aplicar formatos ANTES de collapse_rows
  column_spec(1, bold = TRUE) %>%
  column_spec(c(3, 6, 9,12), bold = TRUE) %>%  # Aplicar a columnas k
  row_spec(0, bold = TRUE, color = "white", background = "#3498db") %>%
  # Combinar filas (hacerlo al final)
  collapse_rows(columns = c(1, 2, 5, 8,11), valign = "middle")


tabla_final
