# TIEMPOS DE IMPACIENCIA

# DATOS CENSURADOS

library(readxl)
library(survival)
library(fitdistrplus)
library(ggplot2)
library(GofCens)
library(flexsurv)
library(EnvStats)

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


# Utilizamos la librería survival
?Surv

# Crear objeto de censura
g1_cens <- Surv(time = g1$TiemposEspera, event = eventos)

# Ajuste de distribuciones
ajuste_exp <- survreg(g1_cens ~ 1, dist = "exponential")
ajuste_ln <- survreg(g1_cens ~ 1, dist = "lognormal")
ajuste_weibull <- survreg(g1_cens ~ 1, dist = "weibull")

summary(ajuste_exp)
summary(ajuste_ln)
summary(ajuste_weibull)

# Obtenemos los parametros

# Exponencial
lambda_exp <- 1 / exp(ajuste_exp$coefficients)

# Lognormal
mu_ln <- ajuste_ln$coefficients
sigma_ln <- ajuste_ln$scale

# Weibull
shape_wb <- 1 / ajuste_weibull$scale
scale_wb <- exp(ajuste_weibull$coefficients)

?ecdfPlotCensored
?cdfCompareCensored


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

# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]


# 3) Plot base: empírica + exponencial
cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens, # los censurados son los de censura = 1
  censoring.side  = "right",      # es censura por salida antes del servicio
  distribution    = "exp",
  param.list      = list(rate = lambda_exp),
  x.col           = "black",
  y.or.fitted.col = "red",
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Ajustes Teóricos",
  xlab            = "Tiempo de Espera",
  ylab            = "Probabilidad acumulada",
  xlim            = c(0, 0.12),
  ylim            = c(0, 0.35) 
)

# Generar secuencia de tiempos para líneas teóricas
tiempos <- seq(0, max(g1$TiemposEspera), length.out = 200)

# Añadir curva lognormal
lines(tiempos, plnorm(tiempos, meanlog = mu_ln, sdlog = sigma_ln), 
      col = "blue", lty = 3, lwd = 2)

# Añadir curva Weibull
lines(tiempos, pweibull(tiempos, shape = shape_wb, scale = scale_wb), 
      col = "darkgreen", lty = 4, lwd = 2)

# Línea para Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "purple", lty = 5, lwd = 2)

legend("bottomright",
       legend = c("Empírica (KM)", "Exponencial", "Lognormal", "Weibull", "Birnbaum-Saunders"),
       col    = c("black",         "red",         "blue",     "darkgreen", "purple"),
       lty    = c(1,               2,             3,          4,           5),
       bty    = "o",  # 'o' para recuadro (por defecto), también puedes usar 'n' para sin recuadro
       box.lwd = 1,    # Grosor del recuadro
       box.col = "black", # Color del recuadro
       cex    = 1.2,  # Aumentar tamaño del texto (1 es el tamaño normal)
       lwd    = 2)

# FUNCIÓN EN GENERAL PARA LOS GRÁFICOS

# 1. Crear una lista para almacenar los gráficos
graficos <- list()

# 2. Función modificada para guardar gráficos en la lista
generar_grafico <- function(tiempos, cens, lambda, mu_ln, sigma_ln, shape_wb, scale_wb, alpha_bs, beta_bs, titulo) {
  # Guardar el gráfico actual en la lista
  gr <- recordPlot({
    cdfCompareCensored(
      x = tiempos,
      censored = cens,
      censoring.side = "right",
      distribution = "exp",
      param.list = list(rate = lambda),
      x.col = "black",
      y.or.fitted.col = "red",
      main = titulo,
      xlab = "Tiempo de Espera",
      ylab = "F(t)",
      xlim = c(0, 0.12),
      ylim = c(0, 0.35))
      
      seq_tiempos <- seq(0, max(tiempos), length.out = 200)
      lines(seq_tiempos, plnorm(seq_tiempos, meanlog = mu_ln, sdlog = sigma_ln), 
            col = "blue", lty = 3, lwd = 2)
      lines(seq_tiempos, pweibull(seq_tiempos, shape = shape_wb, scale = scale_wb), 
            col = "darkgreen", lty = 4, lwd = 2)
      lines(seq_tiempos, pbirnbaum_saunders(seq_tiempos, alpha = alpha_bs, beta = beta_bs), 
            col = "purple", lty = 5, lwd = 2)})
    return(gr)
}

grafico1 <- generar_grafico(g1$TiemposEspera, cens, lambda_exp, mu_ln, sigma_ln, shape_wb, scale_wb, alpha_bs, beta_bs, "ECDF Censurada vs Teórica G1")

############################################

# PARA SLOT G2

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

# Ajuste de distribuciones
ajuste_exp <- survreg(g1_cens ~ 1, dist = "exponential")
ajuste_ln <- survreg(g1_cens ~ 1, dist = "lognormal")
ajuste_weibull <- survreg(g1_cens ~ 1, dist = "weibull")

summary(ajuste_exp)
summary(ajuste_ln)
summary(ajuste_weibull)

# Obtenemos los parametros

# Exponencial
lambda_exp <- 1 / exp(ajuste_exp$coefficients)

# Lognormal
mu_ln <- ajuste_ln$coefficients
sigma_ln <- ajuste_ln$scale

# Weibull
shape_wb <- 1 / ajuste_weibull$scale
scale_wb <- exp(ajuste_weibull$coefficients)


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

# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]


# 3) Plot base: empírica + exponencial
cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens, # los censurados son los de censura = 1
  censoring.side  = "right",      # es censura por salida antes del servicio
  distribution    = "exp",
  param.list      = list(rate = lambda_exp),
  x.col           = "black",
  y.or.fitted.col = "red",
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Ajustes Teóricos",
  xlab            = "Tiempo de Espera",
  ylab            = "Probabilidad acumulada",
  xlim            = c(0, 0.22),
  ylim            = c(0, 0.5) 
)

# Generar secuencia de tiempos para líneas teóricas
tiempos <- seq(0, max(g1$TiemposEspera), length.out = 200)

# Añadir curva lognormal
lines(tiempos, plnorm(tiempos, meanlog = mu_ln, sdlog = sigma_ln), 
      col = "blue", lty = 3, lwd = 2)

# Añadir curva Weibull
lines(tiempos, pweibull(tiempos, shape = shape_wb, scale = scale_wb), 
      col = "darkgreen", lty = 4, lwd = 2)


# Línea para Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "purple", lty = 5, lwd = 2)

legend("bottomright",
       legend = c("Empírica (KM)", "Exponencial", "Lognormal", "Weibull", "Birnbaum-Saunders"),
       col    = c("black",         "red",         "blue",     "darkgreen", "purple"),
       lty    = c(1,               2,             3,          4,           5),
       bty    = "o",  # 'o' para recuadro (por defecto), también puedes usar 'n' para sin recuadro
       box.lwd = 1,    # Grosor del recuadro
       box.col = "black", # Color del recuadro
       cex    = 1.2,  # Aumentar tamaño del texto (1 es el tamaño normal)
       lwd    = 2)

grafico2 <- generar_grafico(g1$TiemposEspera, cens, lambda_exp, mu_ln, sigma_ln, shape_wb, scale_wb, alpha_bs, beta_bs, "ECDF Censurada vs Teórica G2")

#############################################

# PARA SLOT G3

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

# Ajuste de distribuciones
ajuste_exp <- survreg(g1_cens ~ 1, dist = "exponential")
ajuste_ln <- survreg(g1_cens ~ 1, dist = "lognormal")
ajuste_weibull <- survreg(g1_cens ~ 1, dist = "weibull")

summary(ajuste_exp)
summary(ajuste_ln)
summary(ajuste_weibull)

# Obtenemos los parametros

# Exponencial
lambda_exp <- 1 / exp(ajuste_exp$coefficients)

# Lognormal
mu_ln <- ajuste_ln$coefficients
sigma_ln <- ajuste_ln$scale

# Weibull
shape_wb <- 1 / ajuste_weibull$scale
scale_wb <- exp(ajuste_weibull$coefficients)

?ecdfPlotCensored
?cdfCompareCensored


# 3) Plot base: empírica + exponencial
cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens, # los censurados son los de censura = 1
  censoring.side  = "right",      # es censura por salida antes del servicio
  distribution    = "exp",
  param.list      = list(rate = lambda_exp),
  x.col           = "black",
  y.or.fitted.col = "red",
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Ajustes Teóricos",
  xlab            = "Tiempo de Espera",
  ylab            = "Probabilidad acumulada",
  xlim            = c(0, 0.2),
  ylim            = c(0, 0.5) 
)

# Generar secuencia de tiempos para líneas teóricas
tiempos <- seq(0, max(g1$TiemposEspera), length.out = 200)

# Añadir curva lognormal
lines(tiempos, plnorm(tiempos, meanlog = mu_ln, sdlog = sigma_ln), 
      col = "blue", lty = 3, lwd = 2)

# Añadir curva Weibull
lines(tiempos, pweibull(tiempos, shape = shape_wb, scale = scale_wb), 
      col = "darkgreen", lty = 4, lwd = 2)


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

# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]

# Línea para Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "purple", lty = 5, lwd = 2)

legend("bottomright",
       legend = c("Empírica (KM)", "Exponencial", "Lognormal", "Weibull", "Birnbaum-Saunders"),
       col    = c("black",         "red",         "blue",     "darkgreen", "purple"),
       lty    = c(1,               2,             3,          4,           5),
       bty    = "o",  # 'o' para recuadro (por defecto), también puedes usar 'n' para sin recuadro
       box.lwd = 1,    # Grosor del recuadro
       box.col = "black", # Color del recuadro
       cex    = 1.2,  # Aumentar tamaño del texto (1 es el tamaño normal)
       lwd    = 2)

grafico3 <- generar_grafico(g1$TiemposEspera, cens, lambda_exp, mu_ln, sigma_ln, shape_wb, scale_wb, alpha_bs, beta_bs, "ECDF Censurada vs Teórica G3")



###########################################################

# PARA SLOT G4

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

# Ajuste de distribuciones
ajuste_exp <- survreg(g1_cens ~ 1, dist = "exponential")
ajuste_ln <- survreg(g1_cens ~ 1, dist = "lognormal")
ajuste_weibull <- survreg(g1_cens ~ 1, dist = "weibull")

summary(ajuste_exp)
summary(ajuste_ln)
summary(ajuste_weibull)

# Obtenemos los parametros

# Exponencial
lambda_exp <- 1 / exp(ajuste_exp$coefficients)

# Lognormal
mu_ln <- ajuste_ln$coefficients
sigma_ln <- ajuste_ln$scale

# Weibull
shape_wb <- 1 / ajuste_weibull$scale
scale_wb <- exp(ajuste_weibull$coefficients)


# 3) Plot base: empírica + exponencial
cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens, # los censurados son los de censura = 1
  censoring.side  = "right",      # es censura por salida antes del servicio
  distribution    = "exp",
  param.list      = list(rate = lambda_exp),
  x.col           = "black",
  y.or.fitted.col = "red",
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Ajustes Teóricos",
  xlab            = "Tiempo de Espera",
  ylab            = "Probabilidad acumulada",
  xlim            = c(0, 0.15),
  ylim            = c(0, 0.5) 
)

# Generar secuencia de tiempos para líneas teóricas
tiempos <- seq(0, max(g1$TiemposEspera), length.out = 200)

# Añadir curva lognormal
lines(tiempos, plnorm(tiempos, meanlog = mu_ln, sdlog = sigma_ln), 
      col = "blue", lty = 3, lwd = 2)

# Añadir curva Weibull
lines(tiempos, pweibull(tiempos, shape = shape_wb, scale = scale_wb), 
      col = "darkgreen", lty = 4, lwd = 2)


# BIRNBAUM SAUNDERS

bs_custom <- list(name = "birnbaum_saunders", 
                  pars = c("alpha", "beta"), 
                  location = "beta",
                  transforms = list(log, log), 
                  inv.transforms = list(exp, exp),
                  inits = function(t) c(0.5, mean(t)))

# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]

# Línea para Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "purple", lty = 5, lwd = 2)

legend("bottomright",
       legend = c("Empírica (KM)", "Exponencial", "Lognormal", "Weibull", "Birnbaum-Saunders"),
       col    = c("black",         "red",         "blue",     "darkgreen", "purple"),
       lty    = c(1,               2,             3,          4,           5),
       bty    = "o",  # 'o' para recuadro (por defecto), también puedes usar 'n' para sin recuadro
       box.lwd = 1,    # Grosor del recuadro
       box.col = "black", # Color del recuadro
       cex    = 1.2,  # Aumentar tamaño del texto (1 es el tamaño normal)
       lwd    = 2)

grafico4 <- generar_grafico(g1$TiemposEspera, cens, lambda_exp, mu_ln, sigma_ln, shape_wb, scale_wb, alpha_bs, beta_bs, "ECDF Censurada vs Teórica G4")



###########################################################

# PARA SLOT G5

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

# Ajuste de distribuciones
ajuste_exp <- survreg(g1_cens ~ 1, dist = "exponential")
ajuste_ln <- survreg(g1_cens ~ 1, dist = "lognormal")
ajuste_weibull <- survreg(g1_cens ~ 1, dist = "weibull")

summary(ajuste_exp)
summary(ajuste_ln)
summary(ajuste_weibull)

# Obtenemos los parametros

# Exponencial
lambda_exp <- 1 / exp(ajuste_exp$coefficients)

# Lognormal
mu_ln <- ajuste_ln$coefficients
sigma_ln <- ajuste_ln$scale

# Weibull
shape_wb <- 1 / ajuste_weibull$scale
scale_wb <- exp(ajuste_weibull$coefficients)


# 3) Plot base: empírica + exponencial
cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens, # los censurados son los de censura = 1
  censoring.side  = "right",      # es censura por salida antes del servicio
  distribution    = "exp",
  param.list      = list(rate = lambda_exp),
  x.col           = "black",
  y.or.fitted.col = "red",
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Ajustes Teóricos",
  xlab            = "Tiempo de Espera",
  ylab            = "Probabilidad acumulada",
  xlim            = c(0, 0.18),
  ylim            = c(0, 0.5) 
)

# Generar secuencia de tiempos para líneas teóricas
tiempos <- seq(0, max(g1$TiemposEspera), length.out = 200)

# Añadir curva lognormal
lines(tiempos, plnorm(tiempos, meanlog = mu_ln, sdlog = sigma_ln), 
      col = "blue", lty = 3, lwd = 2)

# Añadir curva Weibull
lines(tiempos, pweibull(tiempos, shape = shape_wb, scale = scale_wb), 
      col = "darkgreen", lty = 4, lwd = 2)


# BIRNBAUM SAUNDERS

bs_custom <- list(name = "birnbaum_saunders", 
                  pars = c("alpha", "beta"), 
                  location = "beta",
                  transforms = list(log, log), 
                  inv.transforms = list(exp, exp),
                  inits = function(t) c(0.5, mean(t)))

# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]

# Línea para Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "purple", lty = 5, lwd = 2)

legend("bottomright",
       legend = c("Empírica (KM)", "Exponencial", "Lognormal", "Weibull", "Birnbaum-Saunders"),
       col    = c("black",         "red",         "blue",     "darkgreen", "purple"),
       lty    = c(1,               2,             3,          4,           5),
       bty    = "o",  # 'o' para recuadro (por defecto), también puedes usar 'n' para sin recuadro
       box.lwd = 1,    # Grosor del recuadro
       box.col = "black", # Color del recuadro
       cex    = 1.2,  # Aumentar tamaño del texto (1 es el tamaño normal)
       lwd    = 2)

grafico5 <- generar_grafico(g1$TiemposEspera, cens, lambda_exp, mu_ln, sigma_ln, shape_wb, scale_wb, alpha_bs, beta_bs, "ECDF Censurada vs Teórica G5")



###########################################################

# PARA SLOT G6

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

# Ajuste de distribuciones
ajuste_exp <- survreg(g1_cens ~ 1, dist = "exponential")
ajuste_ln <- survreg(g1_cens ~ 1, dist = "lognormal")
ajuste_weibull <- survreg(g1_cens ~ 1, dist = "weibull")

summary(ajuste_exp)
summary(ajuste_ln)
summary(ajuste_weibull)

# Obtenemos los parametros

# Exponencial
lambda_exp <- 1 / exp(ajuste_exp$coefficients)

# Lognormal
mu_ln <- ajuste_ln$coefficients
sigma_ln <- ajuste_ln$scale

# Weibull
shape_wb <- 1 / ajuste_weibull$scale
scale_wb <- exp(ajuste_weibull$coefficients)


# 3) Plot base: empírica + exponencial
cdfCompareCensored(
  x               = g1$TiemposEspera,
  censored        = cens, # los censurados son los de censura = 1
  censoring.side  = "right",      # es censura por salida antes del servicio
  distribution    = "exp",
  param.list      = list(rate = lambda_exp),
  x.col           = "black",
  y.or.fitted.col = "red",
  x.lty           = 1,
  y.or.fitted.lty = 2,
  main            = "ECDF Censurada vs Ajustes Teóricos",
  xlab            = "Tiempo de Espera",
  ylab            = "Probabilidad acumulada",
  xlim            = c(0, 0.3),
  ylim            = c(0, 0.8) 
)

# Generar secuencia de tiempos para líneas teóricas
tiempos <- seq(0, max(g1$TiemposEspera), length.out = 200)

# Añadir curva lognormal
lines(tiempos, plnorm(tiempos, meanlog = mu_ln, sdlog = sigma_ln), 
      col = "blue", lty = 3, lwd = 2)

# Añadir curva Weibull
lines(tiempos, pweibull(tiempos, shape = shape_wb, scale = scale_wb), 
      col = "darkgreen", lty = 4, lwd = 2)


# BIRNBAUM SAUNDERS

bs_custom <- list(name = "birnbaum_saunders", 
                  pars = c("alpha", "beta"), 
                  location = "beta",
                  transforms = list(log, log), 
                  inv.transforms = list(exp, exp),
                  inits = function(t) c(0.5, mean(t)))

# Ajuste con flexsurvreg
ajuste_bs <- flexsurvreg(formula = g1_cens ~ 1, dist = bs_custom)
summary(ajuste_bs)

alpha_bs <- ajuste_bs$res["alpha", "est"]
beta_bs <- ajuste_bs$res["beta", "est"]

# Línea para Birnbaum-Saunders
lines(tiempos, 
      pbirnbaum_saunders(tiempos, alpha = alpha_bs, beta = beta_bs), 
      col = "purple", lty = 5, lwd = 2)

legend("bottomright",
       legend = c("Empírica (KM)", "Exponencial", "Lognormal", "Weibull", "Birnbaum-Saunders"),
       col    = c("black",         "red",         "blue",     "darkgreen", "purple"),
       lty    = c(1,               2,             3,          4,           5),
       bty    = "o",  # 'o' para recuadro (por defecto), también puedes usar 'n' para sin recuadro
       box.lwd = 1,    # Grosor del recuadro
       box.col = "black", # Color del recuadro
       cex    = 1.2,  # Aumentar tamaño del texto (1 es el tamaño normal)
       lwd    = 2)

grafico6 <- generar_grafico(g1$TiemposEspera, cens, lambda_exp, mu_ln, sigma_ln, shape_wb, scale_wb, alpha_bs, beta_bs, "ECDF Censurada vs Teórica G6")


#############################################

library(cowplot)

# Convertir cada gráfico base a un objeto ggdraw
plot_list <- lapply(list(grafico1, grafico2, grafico3, grafico4, grafico5, grafico6), 
                    function(x) {
                      ggdraw() + draw_plot(~replayPlot(x))
                    })

# Combinar con leyenda
plot_grid(
  plotlist = plot_list,
  nrow = 3, ncol = 2,
  labels = LETTERS[1:6],
  label_size = 12
) + 
  draw_grob(legend, x = 0.9, y = 0.5) # Posición manual de la leyenda

# Mostrar todos los gráficos juntos en una cuadrícula 3x2
grid.arrange(grafico1, grafico2, grafico3, grafico4, nrow = 2, ncol = 2)
