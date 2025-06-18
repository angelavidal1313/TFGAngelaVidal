library(readxl)
library(ggplot2)
library(fitdistrplus)
library(actuar)  # Para la distribución Pareto
library(gridExtra)  # Para organizar múltiples gráficos
library(PhaseType)
library(mapfit)
library(Matrix)
library(expm)


analizar_grupo <- function(datos, nombre_grupo) {
  # Limpieza de datos
  datos_vector <- as.numeric(as.matrix(datos))
  datos_vector <- na.omit(datos_vector)
  datos_vector <- datos_vector[datos_vector > 0]
  ln_datos <- log(datos_vector)
  
  # Ajustes de distribuciones
  fit_exp <- fitdist(datos_vector, "exp")
  fit_pareto <- tryCatch(
    fitdist(datos_vector, "pareto", start = list(shape = 1, scale = min(datos_vector))),
    error = function(e) NULL
  )
  fit_weibull <- fitdist(datos_vector, "weibull")
  fit_lnorm <- fitdist(datos_vector, "lnorm")
  
  # Funciones de densidad para Y = ln(X)
  dlogexp <- function(y, lambda) lambda * exp(y - lambda * exp(y))
  dlogpareto <- function(y, alpha, xm) (alpha*xm^alpha)/((exp(y)+xm)^(alpha+1))*exp(y)
  dlogweibull <- function(y, shape, scale) shape * (exp(y)/scale)^shape*exp(-(exp(y)/scale)^shape)
  dloglnorm <- function(y, meanlog, sdlog) dnorm(y, mean = meanlog, sd = sdlog)
  
  # Crear gráficos
  plot_exp <- ggplot(data.frame(y = ln_datos), aes(x = y)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7) +
    stat_function(fun = dlogexp, args = list(lambda = fit_exp$estimate["rate"]), color = "red", linewidth = 1) +
    labs(title = paste(nombre_grupo, "- Exponencial"), x = "Y = ln(X)", y = "Densidad") +
    theme_minimal()
  
  plot_pareto <- if (!is.null(fit_pareto)) {
    ggplot(data.frame(y = ln_datos), aes(x = y)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7) +
      stat_function(fun = dlogpareto, args = list(alpha = fit_pareto$estimate["shape"], xm = fit_pareto$estimate["scale"]), color = "red", linewidth = 1) +
      labs(title = paste(nombre_grupo, "- Pareto"), x = "Y = ln(X)", y = "Densidad") +
      theme_minimal()
  } else {
    ggplot() + 
      geom_text(aes(x = 0, y = 0, label = "Ajuste Pareto falló"), size = 5) + 
      theme_void()
  }
  
  plot_weibull <- ggplot(data.frame(y = ln_datos), aes(x = y)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7) +
    stat_function(fun = dlogweibull, args = list(shape = fit_weibull$estimate["shape"], scale = fit_weibull$estimate["scale"]), color = "red", linewidth = 1) +
    labs(title = paste(nombre_grupo, "- Weibull"), x = "Y = ln(X)", y = "Densidad") +
    theme_minimal()
  
  plot_lnorm <- ggplot(data.frame(y = ln_datos), aes(x = y)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7) +
    stat_function(fun = dloglnorm, args = list(meanlog = fit_lnorm$estimate["meanlog"], sdlog = fit_lnorm$estimate["sdlog"]), color = "red", linewidth = 1) +
    labs(title = paste(nombre_grupo, "- Log-Normal"), x = "Y = ln(X)", y = "Densidad") +
    theme_minimal()
  
  # Combinar gráficos en una cuadrícula 2x2
  grid.arrange(plot_exp, plot_pareto, plot_weibull, plot_lnorm, nrow = 2, ncol = 2, 
               top = paste("Análisis de distribución para", nombre_grupo))
}

# Cargar todos los grupos de datos
grupos <- list(
  g1 = read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_7_9h.xlsx", sheet = 1, col_names = FALSE),
  g2 = read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_9_11h.xlsx", sheet = 1, col_names = FALSE),
  g3 = read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_11_16h.xlsx", sheet = 1, col_names = FALSE),
  g4 = read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_16_17h.xlsx", sheet = 1, col_names = FALSE),
  g5 = read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_17_22h.xlsx", sheet = 1, col_names = FALSE),
  g6 = read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempo_servicio_22_24h.xlsx", sheet = 1, col_names = FALSE)
)

analizar_grupo(grupos$g1, names(grupos)[[1]])
analizar_grupo(grupos$g2, names(grupos)[[2]])
analizar_grupo(grupos$g3, names(grupos)[[3]])
analizar_grupo(grupos$g4, names(grupos)[[4]])
analizar_grupo(grupos$g5, names(grupos)[[5]])
analizar_grupo(grupos$g6, names(grupos)[[6]])
               


# EN CONJUNTO


analizar_grupo_superpuesto <- function(datos, nombre_grupo) {
  library(ggplot2)
  library(fitdistrplus)
  library(expm)
  library(mapfit)
  
  # Limpieza de datos
  datos_vector <- as.numeric(as.matrix(datos))
  datos_vector <- na.omit(datos_vector)
  datos_vector <- datos_vector[datos_vector > 0]
  ln_datos <- log(datos_vector)
  
  # Ajustes clásicos
  fit_exp <- fitdist(datos_vector, "exp")
  fit_pareto <- tryCatch(
    fitdist(datos_vector, "pareto", start = list(shape = 1, scale = min(datos_vector))),
    error = function(e) NULL
  )
  fit_weibull <- fitdist(datos_vector, "weibull")
  fit_lnorm <- fitdist(datos_vector, "lnorm")
  
  # Ajustes Phase-Type
  ajuste_ph2 <- tryCatch({
    ph2_model <- mapfit::ph(2)
    phfit.point(ph = ph2_model, x = datos_vector)
  }, error = function(e) NULL)
  
  ajuste_ph3 <- tryCatch({
    ph3_model <- mapfit::ph(3)
    phfit.point(ph = ph3_model, x = datos_vector)
  }, error = function(e) NULL)
  
  # Base ggplot
  p <- ggplot(data.frame(y = ln_datos), aes(x = y)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "gray80", color = "black", alpha = 0.6)
  
  # Funciones de densidad transformadas CORREGIDAS
  dlogexp <- function(y, rate) dexp(exp(y), rate) * exp(y)
  dlogweibull <- function(y, shape, scale) dweibull(exp(y), shape, scale) * exp(y)
  dloglnorm <- function(y, meanlog, sdlog) dlnorm(exp(y), meanlog, sdlog) * exp(y)
  
  # Añadir curvas CORREGIDAS
  p <- p +
    stat_function(fun = dlogexp, args = list(rate = fit_exp$estimate[["rate"]]), 
                  color = "red", size = 0.8, aes(linetype = "Exponencial")) +
    stat_function(fun = dlogweibull, args = list(shape = fit_weibull$estimate[["shape"]], 
                                                 scale = fit_weibull$estimate[["scale"]]), 
                  color = "blue", size = 0.8, aes(linetype = "Weibull")) +
    stat_function(fun = dloglnorm, args = list(meanlog = fit_lnorm$estimate[["meanlog"]], 
                                               sdlog = fit_lnorm$estimate[["sdlog"]]), 
                  color = "green4", size = 0.8, aes(linetype = "Log-Normal"))
  
  # Añadir Pareto si está disponible
  if (!is.null(fit_pareto)) {
    dlogpareto <- function(y, shape, scale) dpareto(exp(y), shape, scale) * exp(y)
    p <- p + stat_function(fun = dlogpareto, 
                           args = list(shape = fit_pareto$estimate[["shape"]], 
                                       scale = fit_pareto$estimate[["scale"]]),
                           color = "orange", size = 0.8, aes(linetype = "Pareto"))
  }
  
  # Añadir PH2 y PH3
  print(ajuste_ph2$alpha)
  print(ajuste_ph3$alpha)
  if (!is.null(ajuste_ph2)) {
    alpha_ph2 <- ajuste_ph2$alpha
    S_ph2 <- ajuste_ph2$Q
    t_ph2 <- -rowSums(S_ph2)
    
    dlogph2 <- function(y) {
      x <- exp(y)
      sapply(x, function(xi) as.numeric(alpha_ph2 %*% expm(S_ph2 * xi) %*% t_ph2) * xi)
    }
    p <- p + stat_function(fun = dlogph2, color = "purple", size = 0.8, aes(linetype = "PH2"))
  }
  
  if (!is.null(ajuste_ph3)) {
    alpha_ph3 <- ajuste_ph3$alpha
    S_ph3 <- ajuste_ph3$Q
    t_ph3 <- -rowSums(S_ph3)
    
    dlogph3 <- function(y) {
      x <- exp(y)
      sapply(x, function(xi) as.numeric(alpha_ph3 %*% expm(S_ph3 * xi) %*% t_ph3) * xi)
    }
    p <- p + stat_function(fun = dlogph3, color = "hotpink", size = 0.8, aes(linetype = "PH3"))
  }
  
  # Ajustes finales
  p <- p +
    labs(title = paste("Ajuste de distribuciones para", nombre_grupo),
         x = "Y = ln(X)", y = "Densidad") +
    theme_minimal() +
    scale_linetype_manual(name = "Distribución", 
                          values = c("Exponencial" = "solid", "Pareto" = "solid", 
                                     "Weibull" = "solid", "Log-Normal" = "solid",
                                     "PH2" = "solid", "PH3" = "solid")) +
    theme(legend.position = "right",
          legend.direction = "vertical")
  
  return(p)
}


# Para los graficos de G1-G6

library(ggplot2)
library(patchwork)

# Generar gráficos sin títulos individuales

p2 <- analizar_grupo_superpuesto(grupos$g1, "") +  # Cadena vacía como título
  theme(legend.position = "none",
        plot.title = element_blank())  # Eliminar título

p3 <- analizar_grupo_superpuesto(grupos$g3, "") +
  theme(legend.position = "none",
        plot.title = element_blank())

p4 <- analizar_grupo_superpuesto(grupos$g4, "") +
  theme(legend.position = "none",
        plot.title = element_blank())

p5 <- analizar_grupo_superpuesto(grupos$g5, "") +
  theme(legend.position = "none",
        plot.title = element_blank())

p6 <- analizar_grupo_superpuesto(grupos$g6, "") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_blank())

# Diseño del layout (2x2 + 1 fila inferior)
layout <- "
AABB
CCDD
EEEE
"

# Combinar gráficos sin títulos individuales
combined_plot <- (p2 + p3 + p4 + p5 + p6) + 
  plot_layout(design = layout)

# Mostrar el gráfico combinado
combined_plot



