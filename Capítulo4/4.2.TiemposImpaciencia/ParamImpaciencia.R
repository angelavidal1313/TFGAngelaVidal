
# AJUSTE FINAL TIEMPOS DE IMPACIENCIA

library(readxl)
library(survival)
library(fitdistrplus)
library(ggplot2)
library(GofCens)
library(flexsurv)
library(EnvStats)
library(VGAM)


# BIRNBAUM SAUNDERS

# Definir función de densidad y supervivencia
dbirnbaum_saunders <- function(x, alpha, beta, log = FALSE) {
  z <- (sqrt(x / beta) - sqrt(beta / x)) / alpha
  dens <- (1 / (2 * alpha * x)) * (sqrt(x / beta) + sqrt(beta / x)) * dnorm(z)
  if (log) return(log(dens))
  return(dens)
}

pbirnbaum_saunders <- function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  z <- (sqrt(q / beta) - sqrt(beta / q)) / alpha
  p <- pnorm(z, lower.tail = lower.tail, log.p = log.p)
  return(p)
}

bs_custom <- list(name = "birnbaum_saunders", 
                  pars = c("alpha", "beta"), 
                  location = "beta",
                  transforms = list(log, log), 
                  inv.transforms = list(exp, exp),
                  inits = function(t) c(0.5, mean(t)))



## SLOT G1

g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_7_9h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g1)[1] <- "TiemposEspera"
colnames(g1)[2] <- "Censura"
g1 <- g1[g1$TiemposEspera > 0, ]
eventos <- as.numeric(g1$Censura == 0)
cens <- g1$Censura == 1


# CENSURA
# 1 <- son servidos.
# 0 <- se marchan sin ser servidos.


# Crear objeto de censura
g1_cens <- Surv(time = g1$TiemposEspera, event = eventos)


# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]
alpha1 <- alpha_bs
beta_1 <- beta_bs

cat("Parámetros estimados:\n")
cat("Alpha:", alpha_bs, "\n")
cat("Beta:", beta_bs, "\n")

media_teorica <- beta_bs * (1 + 0.5 * alpha_bs^2)
media_empirica <- mean(g1$TiemposEspera)

cat("Media teórica BS:", media_teorica, "\n")
cat("Media empírica:", media_empirica, "\n")


# Gráfico ECDF censurada vs ajuste Birnbaum-Saunders

cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens,               # Vector de censura (1 si censurado, 0 si no)
  censoring.side  = "right",           # Censura por salida antes del servicio
  distribution    = "exp",             # Solo se necesita para generar la estructura base
  param.list      = list(rate = 1),    # No importa el valor, será sobrescrito por la línea BS
  x.col           = "black",           # Color de la empírica
  y.or.fitted.col = NA,                # No mostrar la exponencial (o cualquier ajuste de aquí)
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Birnbaum-Saunders G1",
  xlab            = "Tiempo de Espera",
  ylab            = "F(t)",
  xlim            = c(0, 0.12),
  ylim            = c(0, 0.35)
)

# Generar secuencia de tiempos
tiempos <- seq(0, max(g1$TiemposEspera, na.rm = TRUE), length.out = 200)

# Añadir curva de la distribución Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "red", lty = 5, lwd = 2)

# Leyenda actualizada
legend("bottomright",
       legend = c("Empírica (KM)", "Birnbaum-Saunders"),
       col    = c("black",         "red"),
       lty    = c(1,               5),
       bty    = "n",
       lwd    = 2)

# Función para simular datos Birnbaum-Saunders
rbirnbaum_saunders <- function(n, alpha, beta) {
  u <- runif(n)  # Uniformes en [0, 1]
  # Aplicar la inversa de la CDF (método de transformación inversa)
  t <- (beta/4) * (alpha * qnorm(u) + sqrt((alpha * qnorm(u))^2 + 4))^2
  return(t)
}

cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens,               # Vector de censura (1 si censurado, 0 si no)
  censoring.side  = "right",           # Censura por salida antes del servicio
  distribution    = "exp",             # Solo se necesita para generar la estructura base
  param.list      = list(rate = 1),    # No importa el valor, será sobrescrito por la línea BS
  x.col           = "black",           # Color de la empírica
  y.or.fitted.col = NA,                # No mostrar la exponencial (o cualquier ajuste de aquí)
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Birnbaum-Saunders G1",
  xlab            = "Tiempo de Espera",
  ylab            = "F(t)",
  xlim            = c(0, 0.12),
  ylim            = c(0, 0.35)
)

# Simular datos BS con tus parámetros estimados (alpha1 y beta_1)
set.seed(123)  # Para reproducibilidad
bs_simulados <- rbirnbaum_saunders(n = nrow(g1), alpha = alpha1, beta = beta_1)

# Calcular la ECDF de la simulación
ecdf_simulados <- ecdf(bs_simulados)

# Añadir la ECDF simulada al gráfico existente
lines(tiempos, ecdf_simulados(tiempos), 
      col = "blue", lty = 1, lwd = 2)

# Actualizar la leyenda para incluir la simulación
legend("bottomright",
       legend = c("Empírica (KM)", "Birnbaum-Saunders Teórica", "BS Simulada"),
       col    = c("black",         "red",                      "blue"),
       lty    = c(1,               5,                          1),
       bty    = "n",
       lwd    = 2)


# Comparar estadísticos clave
cat(
  "Media simulada:", mean(bs_simulados), "\n",
  "Media teórica BS:", beta_1 * (1 + 0.5 * alpha1^2), "\n",
  "Media empírica:", mean(g1$TiemposEspera), "\n",
  "Parámetros usados: alpha =", alpha1, "beta =", beta_1
)

head(g1$TiemposEspera)
head(bs_simulados)
bs_simulados



## SLOT G2

g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_9_11h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g1)[1] <- "TiemposEspera"
colnames(g1)[2] <- "Censura"
g1 <- g1[g1$TiemposEspera > 0, ]
eventos <- as.numeric(g1$Censura == 0)
cens <- g1$Censura == 1


# CENSURA
# 1 <- son servidos.
# 0 <- se marchan sin ser servidos.


# Crear objeto de censura
g1_cens <- Surv(time = g1$TiemposEspera, event = eventos)


# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]
alpha2 <- alpha_bs
beta_2 <- beta_bs


# Gráfico ECDF censurada vs ajuste Birnbaum-Saunders

cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens,               # Vector de censura (1 si censurado, 0 si no)
  censoring.side  = "right",           # Censura por salida antes del servicio
  distribution    = "exp",             # Solo se necesita para generar la estructura base
  param.list      = list(rate = 1),    # No importa el valor, será sobrescrito por la línea BS
  x.col           = "black",           # Color de la empírica
  y.or.fitted.col = NA,                # No mostrar la exponencial (o cualquier ajuste de aquí)
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Birnbaum-Saunders G2",
  xlab            = "Tiempo de Espera",
  ylab            = "F(t)",
  xlim            = c(0, 0.12),
  ylim            = c(0, 0.35)
)

# Generar secuencia de tiempos
tiempos <- seq(0, max(g1$TiemposEspera, na.rm = TRUE), length.out = 200)

# Añadir curva de la distribución Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "red", lty = 5, lwd = 2)

# Leyenda actualizada
legend("bottomright",
       legend = c("Empírica (KM)", "Birnbaum-Saunders"),
       col    = c("black",         "red"),
       lty    = c(1,               5),
       bty    = "n",
       lwd    = 2)



## SLOT G3

g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_11_16h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g1)[1] <- "TiemposEspera"
colnames(g1)[2] <- "Censura"
g1 <- g1[g1$TiemposEspera > 0, ]
eventos <- as.numeric(g1$Censura == 0)
cens <- g1$Censura == 1


# CENSURA
# 1 <- son servidos.
# 0 <- se marchan sin ser servidos.


# Crear objeto de censura
g1_cens <- Surv(time = g1$TiemposEspera, event = eventos)


# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]
alpha3 <- alpha_bs
beta_3 <- beta_bs


# Gráfico ECDF censurada vs ajuste Birnbaum-Saunders

cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens,               # Vector de censura (1 si censurado, 0 si no)
  censoring.side  = "right",           # Censura por salida antes del servicio
  distribution    = "exp",             # Solo se necesita para generar la estructura base
  param.list      = list(rate = 1),    # No importa el valor, será sobrescrito por la línea BS
  x.col           = "black",           # Color de la empírica
  y.or.fitted.col = NA,                # No mostrar la exponencial (o cualquier ajuste de aquí)
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Birnbaum-Saunders G3",
  xlab            = "Tiempo de Espera",
  ylab            = "F(t)",
  xlim            = c(0, 0.12),
  ylim            = c(0, 0.35)
)

# Generar secuencia de tiempos
tiempos <- seq(0, max(g1$TiemposEspera, na.rm = TRUE), length.out = 200)

# Añadir curva de la distribución Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "red", lty = 5, lwd = 2)

# Leyenda actualizada
legend("bottomright",
       legend = c("Empírica (KM)", "Birnbaum-Saunders"),
       col    = c("black",         "red"),
       lty    = c(1,               5),
       bty    = "n",
       lwd    = 2)



## SLOT G4

g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_16_17h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g1)[1] <- "TiemposEspera"
colnames(g1)[2] <- "Censura"
g1 <- g1[g1$TiemposEspera > 0, ]
eventos <- as.numeric(g1$Censura == 0)
cens <- g1$Censura == 1


# CENSURA
# 1 <- son servidos.
# 0 <- se marchan sin ser servidos.


# Crear objeto de censura
g1_cens <- Surv(time = g1$TiemposEspera, event = eventos)


# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]
alpha4 <- alpha_bs
beta_4 <- beta_bs


# Gráfico ECDF censurada vs ajuste Birnbaum-Saunders

cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens,               # Vector de censura (1 si censurado, 0 si no)
  censoring.side  = "right",           # Censura por salida antes del servicio
  distribution    = "exp",             # Solo se necesita para generar la estructura base
  param.list      = list(rate = 1),    # No importa el valor, será sobrescrito por la línea BS
  x.col           = "black",           # Color de la empírica
  y.or.fitted.col = NA,                # No mostrar la exponencial (o cualquier ajuste de aquí)
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Birnbaum-Saunders G4",
  xlab            = "Tiempo de Espera",
  ylab            = "F(t)",
  xlim            = c(0, 0.12),
  ylim            = c(0, 0.35)
)

# Generar secuencia de tiempos
tiempos <- seq(0, max(g1$TiemposEspera, na.rm = TRUE), length.out = 200)

# Añadir curva de la distribución Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "red", lty = 5, lwd = 2)

# Leyenda actualizada
legend("bottomright",
       legend = c("Empírica (KM)", "Birnbaum-Saunders"),
       col    = c("black",         "red"),
       lty    = c(1,               5),
       bty    = "n",
       lwd    = 2)


## SLOT G5

g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_17_22h.xlsx", 
                 sheet = 1, col_names = FALSE)
g1 <- g1[-1,]
g1[] <- lapply(g1, as.numeric)
colnames(g1)[1] <- "TiemposEspera"
colnames(g1)[2] <- "Censura"
g1 <- g1[g1$TiemposEspera > 0, ]
eventos <- as.numeric(g1$Censura == 0)
cens <- g1$Censura == 1


# CENSURA
# 1 <- son servidos.
# 0 <- se marchan sin ser servidos.


# Crear objeto de censura
g1_cens <- Surv(time = g1$TiemposEspera, event = eventos)


# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]
alpha5 <- alpha_bs
beta_5 <- beta_bs


# Gráfico ECDF censurada vs ajuste Birnbaum-Saunders

cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens,               # Vector de censura (1 si censurado, 0 si no)
  censoring.side  = "right",           # Censura por salida antes del servicio
  distribution    = "exp",             # Solo se necesita para generar la estructura base
  param.list      = list(rate = 1),    # No importa el valor, será sobrescrito por la línea BS
  x.col           = "black",           # Color de la empírica
  y.or.fitted.col = NA,                # No mostrar la exponencial (o cualquier ajuste de aquí)
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Birnbaum-Saunders G5",
  xlab            = "Tiempo de Espera",
  ylab            = "F(t)",
  xlim            = c(0, 0.12),
  ylim            = c(0, 0.35)
)

# Generar secuencia de tiempos
tiempos <- seq(0, max(g1$TiemposEspera, na.rm = TRUE), length.out = 200)

# Añadir curva de la distribución Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "red", lty = 5, lwd = 2)

# Leyenda actualizada
legend("bottomright",
       legend = c("Empírica (KM)", "Birnbaum-Saunders"),
       col    = c("black",         "red"),
       lty    = c(1,               5),
       bty    = "n",
       lwd    = 2)



## SLOT G6

g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_22_24h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g1)[1] <- "TiemposEspera"
colnames(g1)[2] <- "Censura"
g1 <- g1[g1$TiemposEspera > 0, ]
eventos <- as.numeric(g1$Censura == 0)
cens <- g1$Censura == 1


# CENSURA
# 1 <- son servidos.
# 0 <- se marchan sin ser servidos.


# Crear objeto de censura
g1_cens <- Surv(time = g1$TiemposEspera, event = eventos)


# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]
alpha6 <- alpha_bs
beta_6 <- beta_bs


# Gráfico ECDF censurada vs ajuste Birnbaum-Saunders

cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens,               # Vector de censura (1 si censurado, 0 si no)
  censoring.side  = "right",           # Censura por salida antes del servicio
  distribution    = "exp",             # Solo se necesita para generar la estructura base
  param.list      = list(rate = 1),    # No importa el valor, será sobrescrito por la línea BS
  x.col           = "black",           # Color de la empírica
  y.or.fitted.col = NA,                # No mostrar la exponencial (o cualquier ajuste de aquí)
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Birnbaum-Saunders G6",
  xlab            = "Tiempo de Espera",
  ylab            = "F(t)",
  xlim            = c(0, 0.12),
  ylim            = c(0, 0.35)
)

# Generar secuencia de tiempos
tiempos <- seq(0, max(g1$TiemposEspera, na.rm = TRUE), length.out = 200)

# Añadir curva de la distribución Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "red", lty = 5, lwd = 2)

# Leyenda actualizada
legend("bottomright",
       legend = c("Empírica (KM)", "Birnbaum-Saunders"),
       col    = c("black",         "red"),
       lty    = c(1,               5),
       bty    = "n",
       lwd    = 2)



# CREAMOS LA LISTA CON TODOS LOS PARÁMETROS

paramBS <- list(
  G1 = list(
    shape = alpha1,
    scale = beta_1
  ),
  
  G2 = list(
    shape = alpha2,
    scale = beta_2
  ),
  
  G3 = list(
    shape = alpha3,
    scale = beta_3
  ),
  
  G4 = list(
    shape = alpha4,
    scale = beta_4
  ),
  
  G5 = list(
    shape = alpha5,
    scale = beta_5
  ),
  
  G6 = list(
    shape = alpha6,
    scale = beta_6
  )
)


# shape1 = 230.99
# scale1= 1248.96

