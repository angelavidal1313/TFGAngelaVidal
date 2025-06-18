
# AJUSTE FINAL TIEMPOS DE SERVICIO

library(readxl)       # Para leer archivos de Excel
library(fitdistrplus) # Para ajustar distribuciones
library(ggplot2)      # Para visualización
library(goftest)
library(phaseR)
library(mapfit)
library(matrixdist)
library(phd)
library(expm)
library(actuar)


## SLOT G1

set.seed(1234)

# Leer los datos desde el archivo Excel
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_7_9h.xlsx", 
                 sheet = 1, col_names = FALSE)

g1_vector <- as.numeric(as.matrix(g1))
g1_vector <- na.omit(g1_vector)  # Eliminar valores NA
g1_vector <- g1_vector[g1_vector > 0]  # Mantener solo valores positivos
tiempos_g1<-g1_vector


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g1_ph3 <- phfit.point(ph = ph_model, x = tiempos_g1)

ajuste_g1_ph3
summary(ajuste_g1_ph3)

alpha<-ajuste_g1_ph3$alpha
S<-ajuste_g1_ph3$Q
S<-as.matrix(S)
alpha1 <- alpha
S1 <- S


cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g1, cdf_ph3)
ks_g1_ph3<-ks.test(tiempos_g1, cdf_ph3)


# Crear el gráfico ECDF vs teórica

plot_data <- data.frame(
  tiempo = tiempos_g1
)

ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = cdf_ph3,
                aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste G1","\nKolmogorov-Smirnov D =", round(ks_g1_ph3$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "PH(3)" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )  + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos


## SLOT G2

set.seed(1234)

# Leer los datos desde el archivo Excel
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_9_11h.xlsx", 
                 sheet = 1, col_names = FALSE)

g1_vector <- as.numeric(as.matrix(g1))
g1_vector <- na.omit(g1_vector)  # Eliminar valores NA
g1_vector <- g1_vector[g1_vector > 0]  # Mantener solo valores positivos
tiempos_g1<-g1_vector


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g1_ph3 <- phfit.point(ph = ph_model, x = tiempos_g1)

ajuste_g1_ph3
summary(ajuste_g1_ph3)

alpha<-ajuste_g1_ph3$alpha
S<-ajuste_g1_ph3$Q
S<-as.matrix(S)
alpha2 <- alpha
S2 <- S


cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g1, cdf_ph3)
ks_g2_ph3<-ks.test(tiempos_g1, cdf_ph3)


# Crear el gráfico ECDF vs teórica

plot_data <- data.frame(
  tiempo = tiempos_g1
)

ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = cdf_ph3,
                aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste G2","\nKolmogorov-Smirnov D =", round(ks_g2_ph3$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "PH(3)" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )  + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos


## SLOT G3

set.seed(1234)

# Leer los datos desde el archivo Excel
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_11_16h.xlsx", 
                 sheet = 1, col_names = FALSE)

g1_vector <- as.numeric(as.matrix(g1))
g1_vector <- na.omit(g1_vector)  # Eliminar valores NA
g1_vector <- g1_vector[g1_vector > 0]  # Mantener solo valores positivos
tiempos_g1<-g1_vector


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g1_ph3 <- phfit.point(ph = ph_model, x = tiempos_g1)

ajuste_g1_ph3
summary(ajuste_g1_ph3)

alpha<-ajuste_g1_ph3$alpha
S<-ajuste_g1_ph3$Q
S<-as.matrix(S)
alpha3 <- alpha
S3 <- S


cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g1, cdf_ph3)
ks_g3_ph3<-ks.test(tiempos_g1, cdf_ph3)


# Crear el gráfico ECDF vs teórica

plot_data <- data.frame(
  tiempo = tiempos_g1
)

ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = cdf_ph3,
                aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste G3","\nKolmogorov-Smirnov D =", round(ks_g3_ph3$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "PH(3)" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )  + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos



## SLOT G4

set.seed(1234)

# Leer los datos desde el archivo Excel
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_16_17h.xlsx", 
                 sheet = 1, col_names = FALSE)

g1_vector <- as.numeric(as.matrix(g1))
g1_vector <- na.omit(g1_vector)  # Eliminar valores NA
g1_vector <- g1_vector[g1_vector > 0]  # Mantener solo valores positivos
tiempos_g1<-g1_vector


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g1_ph3 <- phfit.point(ph = ph_model, x = tiempos_g1)

ajuste_g1_ph3
summary(ajuste_g1_ph3)

alpha<-ajuste_g1_ph3$alpha
S<-ajuste_g1_ph3$Q
S<-as.matrix(S)
alpha4 <- alpha
S4 <- S


cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g1, cdf_ph3)
ks_g4_ph3<-ks.test(tiempos_g1, cdf_ph3)


# Crear el gráfico ECDF vs teórica

plot_data <- data.frame(
  tiempo = tiempos_g1
)

ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = cdf_ph3,
                aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste G4","\nKolmogorov-Smirnov D =", round(ks_g4_ph3$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "PH(3)" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )  + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos



## SLOT G5

set.seed(1234)

# Leer los datos desde el archivo Excel
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_17_22h.xlsx", 
                 sheet = 1, col_names = FALSE)

g1_vector <- as.numeric(as.matrix(g1))
g1_vector <- na.omit(g1_vector)  # Eliminar valores NA
g1_vector <- g1_vector[g1_vector > 0]  # Mantener solo valores positivos
tiempos_g1<-g1_vector


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g1_ph3 <- phfit.point(ph = ph_model, x = tiempos_g1)

ajuste_g1_ph3
summary(ajuste_g1_ph3)

alpha<-ajuste_g1_ph3$alpha
S<-ajuste_g1_ph3$Q
S<-as.matrix(S)
alpha5 <- alpha
S5 <- S


cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g1, cdf_ph3)
ks_g5_ph3<-ks.test(tiempos_g1, cdf_ph3)


# Crear el gráfico ECDF vs teórica

plot_data <- data.frame(
  tiempo = tiempos_g1
)

ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = cdf_ph3,
                aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste G5","\nKolmogorov-Smirnov D =", round(ks_g5_ph3$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "PH(3)" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )  + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos




## SLOT G6

set.seed(1234)

# Leer los datos desde el archivo Excel
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempo_servicio_22_24h.xlsx", 
                 sheet = 1, col_names = FALSE)

g1_vector <- as.numeric(as.matrix(g1))
g1_vector <- na.omit(g1_vector)  # Eliminar valores NA
g1_vector <- g1_vector[g1_vector > 0]  # Mantener solo valores positivos
tiempos_g1<-g1_vector


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g1_ph3 <- phfit.point(ph = ph_model, x = tiempos_g1)

ajuste_g1_ph3
summary(ajuste_g1_ph3)

alpha<-ajuste_g1_ph3$alpha
S<-ajuste_g1_ph3$Q
S<-as.matrix(S)
alpha6 <- alpha
S6 <- S


cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g1, cdf_ph3)
ks_g6_ph3<-ks.test(tiempos_g1, cdf_ph3)


# Crear el gráfico ECDF vs teórica

plot_data <- data.frame(
  tiempo = tiempos_g1
)

ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = cdf_ph3,
                aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste G6","\nKolmogorov-Smirnov D =", round(ks_g6_ph3$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "PH(3)" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )  + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos


# CREAMOS LA LISTA CON TODOS LOS PARÁMETROS

paramPH3 <- list(
  G1 = list(
    alpha = alpha1,
    S = S1
  ),
  
  G2 = list(
    alpha = alpha2,
    S = S2
  ),
  
  G3 = list(
    alpha = alpha3,
    S = S3
  ),
  
  G4 = list(
    alpha = alpha4,
    S = S4
  ),
  
  G5 = list(
    alpha = alpha5,
    S = S5
  ),
  
  G6 = list(
    alpha = alpha6,
    S = S6
  )
)


# alpha1 = (2.25e-08, 2.7e-11, 1)
# S1= (-9.52e01, 9.59e01...)