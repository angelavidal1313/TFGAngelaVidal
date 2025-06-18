## LOGNORMAL

# Parámetros para diferentes distribuciones lognormales
params <- list(
  list(meanlog = 0, sdlog = 0.5),
  list(meanlog = 0, sdlog = 1),
  list(meanlog = 1, sdlog = 0.5),
  list(meanlog = 1, sdlog = 1)
)

x <- seq(0, 10, length.out = 1000)

colores <- c("blue", "red", "green", "purple")


etiquetas <- list(
  bquote(mu == 0 ~ "," ~ sigma^2 == 0.25),
  bquote(mu == 0 ~ "," ~ sigma^2 == 1),
  bquote(mu == 1 ~ "," ~ sigma^2 == 0.25),
  bquote(mu == 1 ~ "," ~ sigma^2 == 1)
)


plot(x, dlnorm(x, meanlog = params[[1]]$meanlog, sdlog = params[[1]]$sdlog),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)")

# Añadir las demás distribuciones de densidad
for (i in 2:length(params)) {
  lines(x, dlnorm(x, meanlog = params[[i]]$meanlog, sdlog = params[[i]]$sdlog),
        lwd = 2, col = colores[i])
}

legend("topright", legend = etiquetas, col = colores, lwd = 2, cex = 1.2)


## WEIBULL

# Parámetros para diferentes distribuciones Weibull
params <- list(
  list(shape = 1, scale = 1),
  list(shape = 1.5, scale = 1),
  list(shape = 2, scale = 1),
  list(shape = 2, scale = 2)
)

x <- seq(0, 10, length.out = 1000)

# Colores para cada distribución
colores <- c("blue", "red", "green", "purple")

etiquetas <- list(
  bquote(alpha == 1 ~ "," ~ lambda == 1),
  bquote(alpha == 1.5 ~ "," ~ lambda == 1),
  bquote(alpha == 2 ~ "," ~ lambda == 1),
  bquote(alpha == 2 ~ "," ~ lambda == 2)
)

plot(x, dweibull(x, shape = params[[1]]$shape, scale = params[[1]]$scale),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)")

# Añadir las demás distribuciones de densidad
for (i in 2:length(params)) {
  lines(x, dweibull(x, shape = params[[i]]$shape, scale = params[[i]]$scale),
        lwd = 2, col = colores[i])
}

legend("topright", legend = etiquetas, col = colores, lwd = 2, cex = 1.2)


## EXPONENCIAL

# Parámetros para diferentes distribuciones exponenciales
lambdas <- c(0.5, 1, 1.5, 2)

x <- seq(0, 10, length.out = 1000)

colores <- c("blue", "red", "green", "purple")

etiquetas <- lapply(lambdas, function(lambda) {
  bquote(lambda == .(lambda))
})

plot(x, dexp(x, rate = lambdas[1]),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)")

# Añadir las demás distribuciones de densidad
for (i in 2:length(lambdas)) {
  lines(x, dexp(x, rate = lambdas[i]),
        lwd = 2, col = colores[i])
}

legend("topright", legend = etiquetas, col = colores, lwd = 2, cex = 1.2)



## PARETO

# Cargar paquete necesario para la distribución Pareto
if (!require("VGAM")) install.packages("VGAM")
library(VGAM)

# Parámetros para diferentes distribuciones Pareto
params <- list(
  list(shape = 0.5, scale = 1),   # α = 1.5, x_m = 1
  list(shape = 1.5, scale = 1),     # α = 2, x_m = 1
  list(shape = 2.5, scale = 1),   # α = 2.5, x_m = 1
  list(shape = 2, scale = 4)      # α = 2, x_m = 2
)


x <- seq(0.9, 10, length.out = 1000)


colores <- c("blue", "red", "green", "purple")


etiquetas <- lapply(params, function(p) {
  bquote(alpha == .(p$shape) ~ "," ~ x[m] == .(p$scale))
})


plot(x, dpareto(x, scale = params[[1]]$scale, shape = params[[1]]$shape),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)",
     xlim = c(0.9, 10), ylim = c(0, 2))

# Añadir las demás distribuciones
for (i in 2:length(params)) {
  lines(x, dpareto(x, scale = params[[i]]$scale, shape = params[[i]]$shape),
        lwd = 2, col = colores[i])
}


legend("topright", legend = etiquetas, col = colores, lwd = 2, cex = 1.2)




## TIPO FASE (2)


library(expm)  # Para cálculo de matrices exponenciales

# Definir parámetros de la distribución tipo fase
params_PH <- list(
  list(alpha = c(1, 0), T = matrix(c(-2, 1, 0, -3), nrow = 2, byrow = TRUE)),
  list(alpha = c(0.5, 0.5), T = matrix(c(-3, 2, 0, -4), nrow = 2, byrow = TRUE)),
  list(alpha = c(0.7, 0.3), T = matrix(c(-4, 2, 0, -5), nrow = 2, byrow = TRUE)),
  list(alpha = c(0.3, 0.7), T = matrix(c(-5, 3, 0, -6), nrow = 2, byrow = TRUE))
)


x <- seq(0, 5, length.out = 1000)


colores <- c("blue", "red", "green", "purple")


dPH <- function(x, alpha, T) {
  e <- rep(1, nrow(T))  # Vector de unos con la dimensión correcta
  sapply(x, function(t) as.numeric(alpha %*% expm(T * t) %*% (-T %*% e)))
}


plot(x, dPH(x, params_PH[[1]]$alpha, params_PH[[1]]$T),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)")


for (i in 2:length(params_PH)) {
  lines(x, dPH(x, params_PH[[i]]$alpha, params_PH[[i]]$T), 
        lwd = 2, col = colores[i])
}


legend("topright", 
       legend = c("PH(1,0) con T1", "PH(0.5,0.5) con T2", "PH(0.7,0.3) con T3", "PH(0.3,0.7) con T4"), 
       col = colores, lwd = 2, cex = 1.2)


##### VARIANDO ALPHA

par(mfrow=c(1,2))
# Definir parámetros de la distribución tipo fase
params_PH <- list(
  list(alpha = c(1, 0), T = matrix(c(-2, 1, 0, -3), nrow = 2, byrow = TRUE)),
  list(alpha = c(0.5, 0.5), T = matrix(c(-2, 1, 0, -3), nrow = 2, byrow = TRUE)),
  list(alpha = c(0.7, 0.3), T = matrix(c(-2, 1, 0, -3), nrow = 2, byrow = TRUE)),
  list(alpha = c(0.3, 0.7), T = matrix(c(-2, 1, 0, -3), nrow = 2, byrow = TRUE))
)


x <- seq(0, 5, length.out = 1000)


colores <- c("blue", "red", "green", "purple")


dPH <- function(x, alpha, T) {
  e <- rep(1, nrow(T))  # Vector de unos con la dimensión correcta
  sapply(x, function(t) as.numeric(alpha %*% expm(T * t) %*% (-T %*% e)))
}


plot(x, dPH(x, params_PH[[1]]$alpha, params_PH[[1]]$T),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)",
     ylim = c(0,2.5))


for (i in 2:length(params_PH)) {
  lines(x, dPH(x, params_PH[[i]]$alpha, params_PH[[i]]$T), 
        lwd = 2, col = colores[i])
}

# Añadir la leyenda
legend("topright", 
       legend = c("PH(1,0) con T", "PH(0.5,0.5) con T", "PH(0.7,0.3) con T", "PH(0.3,0.7) con T"), 
       col = colores, lwd = 2, cex = 1)



##### VARIANDO LA MATRIZ

# Definir parámetros de la distribución tipo fase
params_PH <- list(
  list(alpha = c(1, 0), T = matrix(c(-2, 1, 0, -3), nrow = 2, byrow = TRUE)),
  list(alpha = c(1, 0), T = matrix(c(-2, 2, 0, -3), nrow = 2, byrow = TRUE)),
  list(alpha = c(1, 0), T = matrix(c(-4, 2, 0, -5), nrow = 2, byrow = TRUE)),
  list(alpha = c(1, 0), T = matrix(c(-4, 3, 0, -5), nrow = 2, byrow = TRUE))
)


x <- seq(0, 5, length.out = 1000)


colores <- c("blue", "red", "green", "purple")


dPH <- function(x, alpha, T) {
  e <- rep(1, nrow(T))  # Vector de unos con la dimensión correcta
  sapply(x, function(t) as.numeric(alpha %*% expm(T * t) %*% (-T %*% e)))
}


plot(x, dPH(x, params_PH[[1]]$alpha, params_PH[[1]]$T),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)",
     ylim = c(0,2))


for (i in 2:length(params_PH)) {
  lines(x, dPH(x, params_PH[[i]]$alpha, params_PH[[i]]$T), 
        lwd = 2, col = colores[i])
}

legend("topright", 
       legend = c("PH(1,0) con T1", "PH(1,0) con T2", "PH(1,0) con T3", "PH(1,0) con T4"), 
       col = colores, lwd = 2, cex = 1)




## TIPO FASE (3)

par(mfrow=c(1,2))

##### VARIANDO ALPHA (n=3)

# Definir parámetros de la distribución tipo fase para n=3
params_PH <- list(
  list(alpha = c(1, 0, 0), 
       T = matrix(c(-2, 1, 0, 
                    0, -3, 1, 
                    0, 0, -4), nrow = 3, byrow = TRUE)),
  list(alpha = c(0.5, 0.3, 0.2), 
       T = matrix(c(-2, 1, 0, 
                    0, -3, 1, 
                    0, 0, -4), nrow = 3, byrow = TRUE)),
  list(alpha = c(0.7, 0.2, 0.1), 
       T = matrix(c(-2, 1, 0, 
                    0, -3, 1, 
                    0, 0, -4), nrow = 3, byrow = TRUE)),
  list(alpha = c(0.2, 0.5, 0.3), 
       T = matrix(c(-2, 1, 0, 
                    0, -3, 1, 
                    0, 0, -4), nrow = 3, byrow = TRUE))
)

x <- seq(0, 5, length.out = 1000)

colores <- c("blue", "red", "green", "purple")


dPH <- function(x, alpha, T) {
  e <- rep(1, nrow(T))  # Vector de unos con la dimensión correcta
  sapply(x, function(t) as.numeric(alpha %*% expm(T * t) %*% (-T %*% e)))
}


plot(x, dPH(x, params_PH[[1]]$alpha, params_PH[[1]]$T),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)",
     ylim = c(0, 2.5))


for (i in 2:length(params_PH)) {
  lines(x, dPH(x, params_PH[[i]]$alpha, params_PH[[i]]$T), 
        lwd = 2, col = colores[i])
}

legend("topright", 
       legend = c("PH(1,0,0) con T", "PH(0.5,0.3,0.2) con T", "PH(0.7,0.2,0.1) con T", "PH(0.2,0.5,0.3) con T"), 
       col = colores, lwd = 2, cex = 1)


##### VARIANDO LA MATRIZ (n=3)

# Definir parámetros de la distribución tipo fase para n=3
params_PH <- list(
  list(alpha = c(1, 0, 0), 
       T = matrix(c(-2, 1, 0, 
                    0, -3, 1, 
                    0, 0, -4), nrow = 3, byrow = TRUE)),
  list(alpha = c(1, 0, 0), 
       T = matrix(c(-2, 1, 0, 
                    0, -4, 1, 
                    0, 0, -5), nrow = 3, byrow = TRUE)),
  list(alpha = c(1, 0, 0), 
       T = matrix(c(-3, 2, 0, 
                    0, -5, 1, 
                    0, 0, -6), nrow = 3, byrow = TRUE)),
  list(alpha = c(1, 0, 0), 
       T = matrix(c(-4, 1, 0, 
                    0, -5, 2, 
                    0, 0, -7), nrow = 3, byrow = TRUE))
)


plot(x, dPH(x, params_PH[[1]]$alpha, params_PH[[1]]$T),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)",
     ylim = c(0, 2.5))


for (i in 2:length(params_PH)) {
  lines(x, dPH(x, params_PH[[i]]$alpha, params_PH[[i]]$T), 
        lwd = 2, col = colores[i])
}


legend("topright", 
       legend = c("PH(1,0,0) con T1", "PH(1,0,0) con T2", "PH(1,0,0) con T3", "PH(1,0,0) con T4"), 
       col = colores, lwd = 2, cex = 1)



# BIRNBAUM SAUNDERS

par(mfrow=c(1,1))
# Cargar paquete necesario para la distribución Birnbaum-Saunders
if (!require("VGAM")) install.packages("VGAM")
library(VGAM)

# Parámetros para diferentes distribuciones Birnbaum-Saunders
params <- list(
  list(shape = 0.5, scale = 1),   # α = 0.5, β = 1
  list(shape = 1.0, scale = 1),   # α = 1.0, β = 1
  list(shape = 1.5, scale = 1),   # α = 1.5, β = 1
  list(shape = 1.0, scale = 2)    # α = 1.0, β = 2
)

x <- seq(0.1, 5, length.out = 1000)


colores <- c("blue", "red", "green", "purple")


etiquetas <- lapply(params, function(p) {
  bquote(alpha == .(p$shape) ~ "," ~ beta == .(p$scale))
})


plot(x, dbisa(x, scale = params[[1]]$scale, shape = params[[1]]$shape),
     type = "l", lwd = 2, col = colores[1],
     xlab = "x", ylab = "f(x)",
     xlim = c(0.1, 5), ylim = c(0, 1.5))

# Añadir las demás distribuciones
for (i in 2:length(params)) {
  lines(x, dbisa(x, scale = params[[i]]$scale, shape = params[[i]]$shape),
        lwd = 2, col = colores[i])
}


legend("topright", legend = etiquetas, col = colores, lwd = 2, cex = 1.2)

