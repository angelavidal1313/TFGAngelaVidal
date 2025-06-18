
## ANÁLISIS DE LOS TIEMPOS DE SERVICIO

# Ajustar distribuciones a los tiempos de servicio

# Weibull, exponencial, lognormal
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

set.seed(1234)

## SLOT G1

# Leer los datos desde el archivo Excel
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_7_9h.xlsx", 
                 sheet = 1, col_names = FALSE)

g1_vector <- as.numeric(as.matrix(g1))  # Convertir todo en un vector numérico



## LOGNORMAL

# 1. Para que pueda ajustarse una lognormal
g1_vector <- na.omit(g1_vector)  # Eliminar valores NA
g1_vector <- g1_vector[g1_vector > 0]  # Mantener solo valores positivos
tiempos_g1<-g1_vector

# 2. Ajustar distribución log-normal
ajuste_g1_ln <- fitdist(tiempos_g1, "lnorm")

# Mostrar resumen del ajuste
summary(ajuste_g1_ln)
meanlog <-ajuste_g1_ln$estimate["meanlog"]
sdlog<-ajuste_g1_ln$estimate["sdlog"]


# 3. Test de Kolmogorov-Smirnov
ks_g1_ln<-ks.test(tiempos_g1, "plnorm", meanlog, sdlog)
ks.test(tiempos_g1, "plnorm", meanlog, sdlog)

# Dado que el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.08 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g1, "plnorm", meanlog, sdlog)
ad_test<-ad.test(tiempos_g1, "plnorm", meanlog, sdlog)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es adecuado.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g1_ln)

# Donde mejor observamos la no bondad del ajuste es en el QQ-plot.

## 5. Gráfico CDF

# Crear data frame para ggplot
plot_data <- data.frame(
  tiempo = tiempos_g1
)

ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = plnorm, 
                args = list(meanlog = ajuste_g1_ln$estimate["meanlog"], 
                            sdlog = ajuste_g1_ln$estimate["sdlog"]),
                aes(color = "Log-Normal"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste Log-Normal (mean =", round(ajuste_g1_ln$estimate["meanlog"], 3), 
                     ", sd =", round(ajuste_g1_ln$estimate["sdlog"], 3), ")",
                     "\nKolmogorov-Smirnov D =", round(ks_g1_ln$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "Log-Normal" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos



## WEIBULL

# 2. Ajustar distribución weibull
ajuste_g1_wb <- fitdist(tiempos_g1, "weibull")

# Mostrar resumen del ajuste
summary(ajuste_g1_wb)
shape <-ajuste_g1_wb$estimate["shape"]
scale<-ajuste_g1_wb$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks_g1_wb<-ks.test(tiempos_g1, "pweibull", shape, scale)
ks.test(tiempos_g1, "pweibull", shape, scale)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.09 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g1, "pweibull", shape, scale)
ad_test<-ad.test(tiempos_g1, "pweibull", shape, scale)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno, aunque
# parece mejor que el anterior.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g1_wb)

# Efectivamente, los gráficos se ajustan mejor

## 5. Gráfico CDF

# Crear data frame para ggplot
plot_data <- data.frame(
  tiempo = tiempos_g1
)

# Crear el gráfico ECDF vs teórica
ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = pweibull, 
                args = list(shape = shape, 
                            scale = scale),
                aes(color = "Weibull"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste Weibull (shape =", round(shape, 3), 
                     ", scale =", round(scale, 3), ")",
                     "\nKolmogorov-Smirnov D =", round(ks_g1_wb$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "Weibull" = "green")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos


# Comparemos AICs

ajuste_g1_ln$aic
ajuste_g1_wb$aic

# En este caso, aunque por poco, es menor el de la Weibull. Sin embargo, es mayor el estadístico
# del KS test.



## EXPONENCIAL

# 2. Ajustar distribución exponencial
ajuste_g1_exp <- fitdist(tiempos_g1, "exp")

# Mostrar resumen del ajuste
summary(ajuste_g1_exp)
rate <-ajuste_g1_exp$estimate["rate"]


# 3. Test de Kolmogorov-Smirnov
ks_g1_exp<-ks.test(tiempos_g1, "pexp", rate)
ks.test(tiempos_g1, "pexp", rate)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.13 

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g1, "pexp", rate)
ad_test<-ad.test(tiempos_g1, "pexp", rate)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno, aunque
# parece mejor que el anterior.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g1_exp)

# Gráficos similares a la Weibull.



## 5. Gráfico CDF

# Crear data frame para ggplot
plot_data <- data.frame(
  tiempo = tiempos_g1
)

# Crear el gráfico ECDF vs teórica
ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = pexp, 
                args = list(rate = rate),
                aes(color = "Exponencial"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste Exponencial (rate =", round(rate, 3), 
                     ")",
                     "\nKolmogorov-Smirnov D =", round(ks_g1_exp$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "Exponencial" = "purple")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )  + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos



# Comparemos AICs

ajuste_g1_ln$aic
ajuste_g1_wb$aic
ajuste_g1_exp$aic

# Sigue siendo mejor la Weibull.



## PHASE TYPE (2)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(2)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g1_ph2 <- phfit.point(ph = ph_model, x = tiempos_g1)

ajuste_g1_ph2
summary(ajuste_g1_ph2)

alpha<-ajuste_g1_ph2$alpha
S<-ajuste_g1_ph2$Q
S<-as.matrix(S)

cdf_ph2 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}


# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g1, cdf_ph2)
ks_g1_ph2<-ks.test(tiempos_g1, cdf_ph2)


# De nuevo, el p-valor es muy bajo (2.515-12), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (No coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g1, cdf_ph2)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# Comparamos AICs

ajuste_g1_ln$aic
ajuste_g1_wb$aic
ajuste_g1_exp$aic
ajuste_g1_ph2$aic

# El mejor AIC es el de la PH(2)



# Crear el gráfico ECDF vs teórica
ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = cdf_ph2,
                aes(color = "PH(2)"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste Tipo-Fase orden 2","\nKolmogorov-Smirnov D =", round(ks_g1_ph2$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "PH(2)" = "orange")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos


## PHASE TYPE (3)


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


cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}


# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g1, cdf_ph3)
ks_g1_ph3<-ks.test(tiempos_g1, cdf_ph3)


# De nuevo, el p-valor es muy bajo (2.515-12), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (No coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g1, cdf_ph3)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# Comparamos AICs

ajuste_g1_ln$aic
ajuste_g1_wb$aic
ajuste_g1_exp$aic
ajuste_g1_ph2$aic
ajuste_g1_ph3$aic

# El mejor AIC es el de la PH(3)


# Crear el gráfico ECDF vs teórica
ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = cdf_ph3,
                aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste Tipo-Fase orden 3","\nKolmogorov-Smirnov D =", round(ks_g1_ph3$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "PH(3)" = "hotpink")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )  + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos



## PARETO

# 2. Ajustar distribución weibull
ajuste_g1_par <- fitdist(tiempos_g1, "pareto") # dpareto viene en el paquete actuar

# Mostrar resumen del ajuste
summary(ajuste_g1_par)
shape_par <-ajuste_g1_par$estimate["shape"]
scale_par<-ajuste_g1_par$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks_g1_par<-ks.test(tiempos_g1, "ppareto", shape_par, scale_par)
ks.test(tiempos_g1, "ppareto", shape_par, scale_par)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.14 muy alto

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g1, "ppareto", shape_par, scale_par)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno, aunque
# parece mejor que el anterior.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g1_par)

# Efectivamente, los gráficos se ajustan mejor

## 5. Gráfico CDF

# Crear data frame para ggplot
plot_data <- data.frame(
  tiempo = tiempos_g1
)

# Crear el gráfico ECDF vs teórica
ggplot(plot_data, aes(tiempo)) +
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  stat_function(fun = ppareto, 
                args = list(shape = shape_par, 
                            scale = scale_par),
                aes(color = "Pareto"), size = 1, linetype = "dashed") +
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajuste Pareto (shape =", round(shape_par, 3), 
                     ", scale =", round(scale_par, 3), ")",
                     "\nKolmogorov-Smirnov D =", round(ks_g1_par$statistic, 4)),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", "Pareto" = "turquoise")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) + 
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos


# Comparemos AICs

ajuste_g1_ln$aic
ajuste_g1_wb$aic
ajuste_g1_exp$aic
ajuste_g1_ph2$aic
ajuste_g1_ph3$aic
ajuste_g1_par$aic

# Sigue siendo mejor la PH(3).





## Gráfico comparativo para todas


# Gráfico comparativo conjunto con la CDF PH
ggplot(plot_data, aes(tiempo)) +
  # ECDF empírica
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  # Log-Normal teórica
  stat_function(fun = plnorm, 
                args = list(meanlog = meanlog, sdlog = sdlog),
                aes(color = "Log-Normal"), size = 1, linetype = "dashed") +
  # Weibull teórica
  stat_function(fun = pweibull, 
                args = list(shape = shape, scale = scale),
                aes(color = "Weibull"), size = 1, linetype = "dashed") +
  # Exponencial teórica
  stat_function(fun = pexp, 
                args = list(rate = rate),
                aes(color = "Exponencial"), size = 1, linetype = "dashed") +
  # Phase-Type2 teórica
  stat_function(fun = cdf_ph2, aes(color = "PH(2)"), size = 1, linetype = "dashed") +
  # Phase-Type3 teórica
  stat_function(fun = cdf_ph3, aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  # Pareto teórica
  stat_function(fun = ppareto, 
                args = list(shape = shape_par, scale = scale_par),
                aes(color = "Pareto"), size = 1, linetype = "dashed") +
  # Títulos y etiquetas
  labs(
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", 
                                "Log-Normal" = "red", 
                                "Weibull" = "green", 
                                "Exponencial" = "purple",
                                "PH(2)" = "orange",
                                "Pareto" = "turquoise",
                                "PH(3)" = "hotpink")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )+
  coord_cartesian(xlim = c(0, 0.21))  # Limita el eje x sin recortar datos




# Gráfico comparativo conjunto con la CDF PH
ggplot(plot_data, aes(tiempo)) +
  # ECDF empírica
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  # Log-Normal teórica
  stat_function(fun = plnorm, 
                args = list(meanlog = meanlog, sdlog = sdlog),
                aes(color = "Log-Normal"), size = 0.8) +
  # Weibull teórica
  stat_function(fun = pweibull, 
                args = list(shape = shape, scale = scale),
                aes(color = "Weibull"), size = 0.8) +
  # Exponencial teórica
  stat_function(fun = pexp, 
                args = list(rate = rate),
                aes(color = "Exponencial"), size = 0.8) +
  # Phase-Type2 teórica
  stat_function(fun = cdf_ph2, aes(color = "PH(2)"), size = 0.8) +
  # Phase-Type3 teórica
  stat_function(fun = cdf_ph3, aes(color = "PH(3)"), size = 0.8) +
  # Pareto teórica
  stat_function(fun = ppareto, 
                args = list(shape = shape_par, scale = scale_par),
                aes(color = "Pareto"), size = 0.8) +
  # Títulos y etiquetas
  labs(
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "black", 
                                "Log-Normal" = "green4", 
                                "Weibull" = "blue", 
                                "Exponencial" = "red",
                                "PH(2)" = "purple",
                                "Pareto" = "orange",
                                "PH(3)" = "brown")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  )+
  coord_cartesian(xlim = c(0, 0.21))  # Limita el eje x sin recortar datos



################################################################################



## SLOT G2


# Leer los datos desde el archivo Excel
g2 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_9_11h.xlsx", 
                 sheet = 1, col_names = FALSE)

g2_vector <- as.numeric(as.matrix(g2))  # Convertir todo en un vector numérico



## LOGNORMAL

# 1. Para que pueda ajustarse una lognormal
g2_vector <- na.omit(g2_vector)  # Eliminar valores NA
g2_vector <- g2_vector[g2_vector > 0]  # Mantener solo valores positivos
tiempos_g2<-g2_vector

# 2. Ajustar distribución log-normal
ajuste_g2_ln <- fitdist(tiempos_g2, "lnorm")

# Mostrar resumen del ajuste
summary(ajuste_g2_ln)
meanlog <-ajuste_g2_ln$estimate["meanlog"]
sdlog<-ajuste_g2_ln$estimate["sdlog"]


# 3. Test de Kolmogorov-Smirnov
ks_g2_ln<-ks.test(tiempos_g2, "plnorm", meanlog, sdlog)
ks.test(tiempos_g2, "plnorm", meanlog, sdlog)

# Dado que el p-valor es muy bajo (1.595e-1o), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.04 (no coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g2, "plnorm", meanlog, sdlog)
ad_test<-ad.test(tiempos_g2, "plnorm", meanlog, sdlog)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g2_ln)

# Donde mejor observamos la no bondad del ajuste es en el QQ-plot.



## WEIBULL

# 2. Ajustar distribución weibull
ajuste_g2_wb <- fitdist(tiempos_g2, "weibull")

# Mostrar resumen del ajuste
summary(ajuste_g2_wb)
shape <-ajuste_g2_wb$estimate["shape"]
scale<-ajuste_g2_wb$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g2, "pweibull", shape, scale)
ks_g2_wb<-ks.test(tiempos_g2, "pweibull", shape, scale)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.08 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g2, "pweibull", shape, scale)
ad_test<-ad.test(tiempos_g2, "pweibull", shape, scale)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g2_wb)


# Comparemos AICs

ajuste_g2_ln$aic
ajuste_g2_wb$aic

# Es menor el de la LN, por tanto es mejor su ajuste.



## EXPONENCIAL

# 2. Ajustar distribución exponencial
ajuste_g2_exp <- fitdist(tiempos_g2, "exp")

# Mostrar resumen del ajuste
summary(ajuste_g2_exp)
rate <-ajuste_g2_exp$estimate["rate"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g2, "pexp", rate)
ks_g2_exp<-ks.test(tiempos_g2, "pexp", rate)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.10 

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g2, "pexp", rate)
ad_test<-ad.test(tiempos_g2, "pexp", rate)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g2_exp)

# Gráficos similares a la Weibull.

# Comparemos AICs

ajuste_g2_ln$aic
ajuste_g2_wb$aic
ajuste_g2_exp$aic

# Sigue siendo mejor la LN.




## PHASE TYPE (2)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(2)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g2_ph2 <- phfit.point(ph = ph_model, x = tiempos_g2)

ajuste_g2_ph2
summary(ajuste_g2_ph2)

alpha<-ajuste_g2_ph2$alpha
S<-ajuste_g2_ph2$Q
S<-as.matrix(S)

cdf_ph <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizar el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g2, cdf_ph)
ks_g2_ph2<-ks.test(tiempos_g2, cdf_ph)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (no coincide)


# Comparamos AICs

ajuste_g2_ln$aic
ajuste_g2_wb$aic
ajuste_g2_exp$aic
ajuste_g2_ph2$aic

# El mejor AIC sigue siendo el de la LN


## PHASE TYPE (3)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g2_ph3 <- phfit.point(ph = ph_model, x = tiempos_g2)

ajuste_g2_ph3
summary(ajuste_g2_ph3)

alpha<-ajuste_g2_ph3$alpha
S<-ajuste_g2_ph3$Q
S<-as.matrix(S)

cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}


# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g2, cdf_ph3)
ks_g2_ph3<-ks.test(tiempos_g2, cdf_ph3)


# De nuevo, el p-valor es muy bajo (2.515-12), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (No coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g2, cdf_ph3)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# Comparamos AICs

ajuste_g2_ln$aic
ajuste_g2_wb$aic
ajuste_g2_exp$aic
ajuste_g2_ph2$aic
ajuste_g2_ph3$aic

# El mejor AIC es el de la PH(3)



## PARETO

# 2. Ajustar distribución weibull
ajuste_g2_par <- fitdist(tiempos_g2, "pareto") # dpareto viene en el paquete actuar

# Mostrar resumen del ajuste
summary(ajuste_g2_par)
shape_par <-ajuste_g2_par$estimate["shape"]
scale_par<-ajuste_g2_par$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks_g2_par<-ks.test(tiempos_g2, "ppareto", shape_par, scale_par)
ks.test(tiempos_g2, "ppareto", shape_par, scale_par)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.12 muy alto

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g2, "ppareto", shape_par, scale_par)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno, aunque
# parece mejor que el anterior.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g2_par)

# 5. Comparamos AICs

ajuste_g2_ln$aic
ajuste_g2_wb$aic
ajuste_g2_exp$aic
ajuste_g2_ph2$aic
ajuste_g2_ph3$aic
ajuste_g2_par$aic


# Sigue siendo mejor la ph(3), aunque la pareto es mejor por ejemplo que la wb o la exp




## Gráfico comparativo para todas

plot_data <- data.frame(
  tiempo = tiempos_g2
)


# Gráfico comparativo conjunto con la CDF PH
ggplot(plot_data, aes(tiempo)) +
  # ECDF empírica
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  # Log-Normal teórica
  stat_function(fun = plnorm, 
                args = list(meanlog = meanlog, sdlog = sdlog),
                aes(color = "Log-Normal"), size = 1, linetype = "dashed") +
  # Weibull teórica
  stat_function(fun = pweibull, 
                args = list(shape = shape, scale = scale),
                aes(color = "Weibull"), size = 1, linetype = "dashed") +
  # Exponencial teórica
  stat_function(fun = pexp, 
                args = list(rate = rate),
                aes(color = "Exponencial"), size = 1, linetype = "dashed") +
  # Phase-Type2 teórica
  stat_function(fun = cdf_ph2, aes(color = "PH(2)"), size = 1, linetype = "dashed") +
  # Phase-Type3 teórica
  stat_function(fun = cdf_ph3, aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  # Pareto teórica
  stat_function(fun = ppareto, 
                args = list(shape = shape_par, scale = scale_par),
                aes(color = "Pareto"), size = 1, linetype = "dashed") +
  # Títulos y etiquetas
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajustes para G2"),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "darkgray", 
                                "Log-Normal" = "green4", 
                                "Weibull" = "blue", 
                                "Exponencial" = "red",
                                "PH(2)" = "purple",
                                "Pareto" = "orange",
                                "PH(3)" = "hotpink")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) +
  coord_cartesian(xlim = c(0, 0.21))  # Limita el eje x sin recortar datos



################################################################################


## SLOT G3


# Leer los datos desde el archivo Excel
g3 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_11_16h.xlsx", 
                 sheet = 1, col_names = FALSE)

g3_vector <- as.numeric(as.matrix(g3))  # Convertir todo en un vector numérico



## LOGNORMAL

# 1. Para que pueda ajustarse una lognormal
g3_vector <- na.omit(g3_vector)  # Eliminar valores NA
g3_vector <- g3_vector[g3_vector > 0]  # Mantener solo valores positivos
tiempos_g3<-g3_vector

# 2. Ajustar distribución log-normal
ajuste_g3_ln <- fitdist(tiempos_g3, "lnorm")

# Mostrar resumen del ajuste
summary(ajuste_g3_ln)
meanlog <-ajuste_g3_ln$estimate["meanlog"]
sdlog<-ajuste_g3_ln$estimate["sdlog"]


# 3. Test de Kolmogorov-Smirnov
ks_g3_ln<-ks.test(tiempos_g3, "plnorm", meanlog, sdlog)
ks.test(tiempos_g3, "plnorm", meanlog, sdlog)

# Dado que el p-valor es muy bajo (1.595e-1o), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.04 (no coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g3, "plnorm", meanlog, sdlog)
ad_test<-ad.test(tiempos_g3, "plnorm", meanlog, sdlog)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g3_ln)

# Donde mejor observamos la no bondad del ajuste es en el QQ-plot.



## WEIBULL

# 2. Ajustar distribución weibull
ajuste_g3_wb <- fitdist(tiempos_g3, "weibull")

# Mostrar resumen del ajuste
summary(ajuste_g3_wb)
shape <-ajuste_g3_wb$estimate["shape"]
scale<-ajuste_g3_wb$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g3, "pweibull", shape, scale)
ks_g3_wb<-ks.test(tiempos_g3, "pweibull", shape, scale)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.08 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g3, "pweibull", shape, scale)
ad_test<-ad.test(tiempos_g3, "pweibull", shape, scale)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g3_wb)


# Comparemos AICs

ajuste_g3_ln$aic
ajuste_g3_wb$aic

# Es menor el de la LN, por tanto es mejor su ajuste.



## EXPONENCIAL

# 2. Ajustar distribución exponencial
ajuste_g3_exp <- fitdist(tiempos_g3, "exp")

# Mostrar resumen del ajuste
summary(ajuste_g3_exp)
rate <-ajuste_g3_exp$estimate["rate"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g3, "pexp", rate)
ks_g3_exp<-ks.test(tiempos_g3, "pexp", rate)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.10 

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g3, "pexp", rate)
ad_test<-ad.test(tiempos_g3, "pexp", rate)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g3_exp)

# Gráficos similares a la Weibull.

# Comparemos AICs

ajuste_g3_ln$aic
ajuste_g3_wb$aic
ajuste_g3_exp$aic

# Sigue siendo mejor la LN.




## PHASE TYPE (2)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(2)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g3_ph2 <- phfit.point(ph = ph_model, x = tiempos_g3)

ajuste_g3_ph2
summary(ajuste_g3_ph2)

alpha<-ajuste_g3_ph2$alpha
S<-ajuste_g3_ph2$Q
S<-as.matrix(S)

cdf_ph <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizar el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g3, cdf_ph)
ks_g3_ph2<-ks.test(tiempos_g3, cdf_ph)


# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (no coincide)


# Comparamos AICs

ajuste_g3_ln$aic
ajuste_g3_wb$aic
ajuste_g3_exp$aic
ajuste_g3_ph2$aic

# El mejor AIC sigue siendo el de la LN



## PHASE TYPE (3)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g3_ph3 <- phfit.point(ph = ph_model, x = tiempos_g3)

ajuste_g3_ph3
summary(ajuste_g3_ph3)

alpha<-ajuste_g3_ph3$alpha
S<-ajuste_g3_ph3$Q
S<-as.matrix(S)

cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}


# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g3, cdf_ph3)
ks_g3_ph3<-ks.test(tiempos_g3, cdf_ph3)


# De nuevo, el p-valor es muy bajo (2.515-12), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (No coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g3, cdf_ph3)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# Comparamos AICs

ajuste_g3_ln$aic
ajuste_g3_wb$aic
ajuste_g3_exp$aic
ajuste_g3_ph2$aic
ajuste_g3_ph3$aic

# El mejor AIC es el de la PH(3), aunque por poco (la segunda la LN).



## PARETO

# 2. Ajustar distribución weibull
ajuste_g3_par <- fitdist(tiempos_g3, "pareto") # dpareto viene en el paquete actuar

# Mostrar resumen del ajuste
summary(ajuste_g3_par)
shape_par <-ajuste_g3_par$estimate["shape"]
scale_par<-ajuste_g3_par$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks_g3_par<-ks.test(tiempos_g3, "ppareto", shape_par, scale_par)
ks.test(tiempos_g3, "ppareto", shape_par, scale_par)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.12 muy alto

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g3, "ppareto", shape_par, scale_par)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno, aunque
# parece mejor que el anterior.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g3_par)

# 5. Comparamos AICs

ajuste_g3_ln$aic
ajuste_g3_wb$aic
ajuste_g3_exp$aic
ajuste_g3_ph2$aic
ajuste_g3_ph3$aic
ajuste_g3_par$aic


# Sigue siendo mejor la ph(3), la pareto es mejor que la exp


## Gráfico comparativo para todas

plot_data <- data.frame(
  tiempo = tiempos_g3
)


# Gráfico comparativo conjunto con la CDF PH
ggplot(plot_data, aes(tiempo)) +
  # ECDF empírica
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  # Log-Normal teórica
  stat_function(fun = plnorm, 
                args = list(meanlog = meanlog, sdlog = sdlog),
                aes(color = "Log-Normal"), size = 1, linetype = "dashed") +
  # Weibull teórica
  stat_function(fun = pweibull, 
                args = list(shape = shape, scale = scale),
                aes(color = "Weibull"), size = 1, linetype = "dashed") +
  # Exponencial teórica
  stat_function(fun = pexp, 
                args = list(rate = rate),
                aes(color = "Exponencial"), size = 1, linetype = "dashed") +
  # Phase-Type2 teórica
  stat_function(fun = cdf_ph2, aes(color = "PH(2)"), size = 1, linetype = "dashed") +
  # Phase-Type3 teórica
  stat_function(fun = cdf_ph3, aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  # Pareto teórica
  stat_function(fun = ppareto, 
                args = list(shape = shape_par, scale = scale_par),
                aes(color = "Pareto"), size = 1, linetype = "dashed") +
  # Títulos y etiquetas
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajustes para G3"),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", 
                                "Log-Normal" = "red", 
                                "Weibull" = "green", 
                                "Exponencial" = "purple",
                                "PH(2)" = "orange",
                                "Pareto" = "turquoise",
                                "PH(3)" = "pink")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) +
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos


################################################################################


## SLOT G4


# Leer los datos desde el archivo Excel
g4 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_16_17h.xlsx", 
                 sheet = 1, col_names = FALSE)

g4_vector <- as.numeric(as.matrix(g4))  # Convertir todo en un vector numérico



## LOGNORMAL

# 1. Para que pueda ajustarse una lognormal
g4_vector <- na.omit(g4_vector)  # Eliminar valores NA
g4_vector <- g4_vector[g4_vector > 0]  # Mantener solo valores positivos
tiempos_g4<-g4_vector

# 2. Ajustar distribución log-normal
ajuste_g4_ln <- fitdist(tiempos_g4, "lnorm")

# Mostrar resumen del ajuste
summary(ajuste_g4_ln)
meanlog <-ajuste_g4_ln$estimate["meanlog"]
sdlog<-ajuste_g4_ln$estimate["sdlog"]


# 3. Test de Kolmogorov-Smirnov
ks_g4_ln<-ks.test(tiempos_g4, "plnorm", meanlog, sdlog)
ks.test(tiempos_g4, "plnorm", meanlog, sdlog)

# Dado que el p-valor es muy bajo (1.595e-1o), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.05 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g4, "plnorm", meanlog, sdlog)
ad_test<-ad.test(tiempos_g4, "plnorm", meanlog, sdlog)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g4_ln)

# Donde mejor observamos la no bondad del ajuste es en el QQ-plot.



## WEIBULL

# 2. Ajustar distribución weibull
ajuste_g4_wb <- fitdist(tiempos_g4, "weibull")

# Mostrar resumen del ajuste
summary(ajuste_g4_wb)
shape <-ajuste_g4_wb$estimate["shape"]
scale<-ajuste_g4_wb$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks_g4_wb<-ks.test(tiempos_g4, "pweibull", shape, scale)
ks.test(tiempos_g4, "pweibull", shape, scale)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.07 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g4, "pweibull", shape, scale)
ad_test<-ad.test(tiempos_g4, "pweibull", shape, scale)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g4_wb)


# Comparemos AICs

ajuste_g4_ln$aic
ajuste_g4_wb$aic

# Es menor el de la LN, por tanto es mejor su ajuste.



## EXPONENCIAL

# 2. Ajustar distribución exponencial
ajuste_g4_exp <- fitdist(tiempos_g4, "exp")

# Mostrar resumen del ajuste
summary(ajuste_g4_exp)
rate <-ajuste_g4_exp$estimate["rate"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g4, "pexp", rate)
ks_g4_exp<-ks.test(tiempos_g4, "pexp", rate)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.12

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g4, "pexp", rate)
ad_test<-ad.test(tiempos_g4, "pexp", rate)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g4_exp)

# Gráficos similares a la Weibull.

# Comparemos AICs

ajuste_g4_ln$aic
ajuste_g4_wb$aic
ajuste_g4_exp$aic

# Sigue siendo mejor la LN.




## PHASE TYPE (2)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(2)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g4_ph2 <- phfit.point(ph = ph_model, x = tiempos_g4)

ajuste_g4_ph2
summary(ajuste_g4_ph2)

alpha<-ajuste_g4_ph2$alpha
S<-ajuste_g4_ph2$Q
S<-as.matrix(S)

cdf_ph <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizar el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g4, cdf_ph)
ks_g4_ph2<-ks.test(tiempos_g4, cdf_ph)


# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.05 (no coincide)


# Comparamos AICs

ajuste_g4_ln$aic
ajuste_g4_wb$aic
ajuste_g4_exp$aic
ajuste_g4_ph2$aic

# El mejor AIC sigue en este caso es el de la PH(2).



## PHASE TYPE (3)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g4_ph3 <- phfit.point(ph = ph_model, x = tiempos_g4)

ajuste_g4_ph3
summary(ajuste_g4_ph3)

alpha<-ajuste_g4_ph3$alpha
S<-ajuste_g4_ph3$Q
S<-as.matrix(S)

cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}


# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g4, cdf_ph3)
ks_g4_ph3<-ks.test(tiempos_g4, cdf_ph3)


# De nuevo, el p-valor es muy bajo (2.515-12), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (No coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g4, cdf_ph3)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# Comparamos AICs

ajuste_g4_ln$aic
ajuste_g4_wb$aic
ajuste_g4_exp$aic
ajuste_g4_ph2$aic
ajuste_g4_ph3$aic

# El mejor AIC es el de la PH(3), aunque por poco (la segunda la LN).



## PARETO

# 2. Ajustar distribución weibull
ajuste_g4_par <- fitdist(tiempos_g4, "pareto") # dpareto viene en el paquete actuar

# Mostrar resumen del ajuste
summary(ajuste_g4_par)
shape_par <-ajuste_g4_par$estimate["shape"]
scale_par<-ajuste_g4_par$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks_g4_par<-ks.test(tiempos_g4, "ppareto", shape_par, scale_par)
ks.test(tiempos_g4, "ppareto", shape_par, scale_par)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.13 muy alto

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g4, "ppareto", shape_par, scale_par)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno, aunque
# parece mejor que el anterior.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g4_par)

# 5. Comparamos AICs

ajuste_g4_ln$aic
ajuste_g4_wb$aic
ajuste_g4_exp$aic
ajuste_g4_ph2$aic
ajuste_g4_ph3$aic
ajuste_g4_par$aic


# Sigue siendo mejor la ph(3), la pareto es la peor



## Gráfico comparativo para todas

plot_data <- data.frame(
  tiempo = tiempos_g4
)


# Gráfico comparativo conjunto con la CDF PH
ggplot(plot_data, aes(tiempo)) +
  # ECDF empírica
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  # Log-Normal teórica
  stat_function(fun = plnorm, 
                args = list(meanlog = meanlog, sdlog = sdlog),
                aes(color = "Log-Normal"), size = 1, linetype = "dashed") +
  # Weibull teórica
  stat_function(fun = pweibull, 
                args = list(shape = shape, scale = scale),
                aes(color = "Weibull"), size = 1, linetype = "dashed") +
  # Exponencial teórica
  stat_function(fun = pexp, 
                args = list(rate = rate),
                aes(color = "Exponencial"), size = 1, linetype = "dashed") +
  # Phase-Type2 teórica
  stat_function(fun = cdf_ph2, aes(color = "PH(2)"), size = 1, linetype = "dashed") +
  # Phase-Type3 teórica
  stat_function(fun = cdf_ph3, aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  # Pareto teórica
  stat_function(fun = ppareto, 
                args = list(shape = shape_par, scale = scale_par),
                aes(color = "Pareto"), size = 1, linetype = "dashed") +
  # Títulos y etiquetas
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajustes para G4"),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", 
                                "Log-Normal" = "red", 
                                "Weibull" = "green", 
                                "Exponencial" = "purple",
                                "PH(2)" = "orange",
                                "Pareto" = "turquoise",
                                "PH(3)" = "pink")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) +
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos



################################################################################


## SLOT G5


# Leer los datos desde el archivo Excel
g5 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_17_22h.xlsx", 
                 sheet = 1, col_names = FALSE)

g5_vector <- as.numeric(as.matrix(g5))  # Convertir todo en un vector numérico



## LOGNORMAL

# 1. Para que pueda ajustarse una lognormal
g5_vector <- na.omit(g5_vector)  # Eliminar valores NA
g5_vector <- g5_vector[g5_vector > 0]  # Mantener solo valores positivos
tiempos_g5<-g5_vector

# 2. Ajustar distribución log-normal
ajuste_g5_ln <- fitdist(tiempos_g5, "lnorm")

# Mostrar resumen del ajuste
summary(ajuste_g5_ln)
meanlog <-ajuste_g5_ln$estimate["meanlog"]
sdlog<-ajuste_g5_ln$estimate["sdlog"]


# 3. Test de Kolmogorov-Smirnov
ks_g5_ln<-ks.test(tiempos_g5, "plnorm", meanlog, sdlog)
ks.test(tiempos_g5, "plnorm", meanlog, sdlog)

# Dado que el p-valor es muy bajo (1.595e-1o), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.04 (casi coincide, 0.03)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g5, "plnorm", meanlog, sdlog)
ad_test<-ad.test(tiempos_g5, "plnorm", meanlog, sdlog)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g5_ln)

# Donde mejor observamos la no bondad del ajuste es en el QQ-plot.



## WEIBULL

# 2. Ajustar distribución weibull
ajuste_g5_wb <- fitdist(tiempos_g5, "weibull")

# Mostrar resumen del ajuste
summary(ajuste_g5_wb)
shape <-ajuste_g5_wb$estimate["shape"]
scale<-ajuste_g5_wb$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g5, "pweibull", shape, scale)
ks_g5_wb<-ks.test(tiempos_g5, "pweibull", shape, scale)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.08 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g5, "pweibull", shape, scale)
ad_test<-ad.test(tiempos_g5, "pweibull", shape, scale)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g5_wb)


# Comparemos AICs

ajuste_g5_ln$aic
ajuste_g5_wb$aic

# Es menor el de la LN, por tanto es mejor su ajuste.



## EXPONENCIAL

# 2. Ajustar distribución exponencial
ajuste_g5_exp <- fitdist(tiempos_g5, "exp")

# Mostrar resumen del ajuste
summary(ajuste_g5_exp)
rate <-ajuste_g5_exp$estimate["rate"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g5, "pexp", rate)
ks_g5_exp<-ks.test(tiempos_g5, "pexp", rate)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.12

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g5, "pexp", rate)
ad_test<-ad.test(tiempos_g5, "pexp", rate)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g5_exp)

# Gráficos similares a la Weibull.

# Comparemos AICs

ajuste_g5_ln$aic
ajuste_g5_wb$aic
ajuste_g5_exp$aic

# Sigue siendo mejor la LN.




## PHASE TYPE (2)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(2)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g5_ph2 <- phfit.point(ph = ph_model, x = tiempos_g5)

ajuste_g5_ph2
summary(ajuste_g5_ph2)

alpha<-ajuste_g5_ph2$alpha
S<-ajuste_g5_ph2$Q
S<-as.matrix(S)

cdf_ph <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizar el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g5, cdf_ph)
ks_g5_ph2<-ks.test(tiempos_g5, cdf_ph)


# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (no coincide)


# Comparamos AICs

ajuste_g5_ln$aic
ajuste_g5_wb$aic
ajuste_g5_exp$aic
ajuste_g5_ph2$aic

# El mejor AIC sigue en este caso es el de la PH(2).


## PHASE TYPE (3)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g5_ph3 <- phfit.point(ph = ph_model, x = tiempos_g5)

ajuste_g5_ph3
summary(ajuste_g5_ph3)

alpha<-ajuste_g5_ph3$alpha
S<-ajuste_g5_ph3$Q
S<-as.matrix(S)

cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}


# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g5, cdf_ph3)
ks_g5_ph3<-ks.test(tiempos_g5, cdf_ph3)


# De nuevo, el p-valor es muy bajo (2.515-12), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (No coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g5, cdf_ph3)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# Comparamos AICs

ajuste_g5_ln$aic
ajuste_g5_wb$aic
ajuste_g5_exp$aic
ajuste_g5_ph2$aic
ajuste_g5_ph3$aic

# El mejor AIC es el de la PH(3), aunque por poco (la segunda la LN).


## PARETO

# 2. Ajustar distribución weibull
ajuste_g5_par <- fitdist(tiempos_g5, "pareto") # dpareto viene en el paquete actuar

# Mostrar resumen del ajuste
summary(ajuste_g5_par)
shape_par <-ajuste_g5_par$estimate["shape"]
scale_par<-ajuste_g5_par$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks_g5_par<-ks.test(tiempos_g5, "ppareto", shape_par, scale_par)
ks.test(tiempos_g5, "ppareto", shape_par, scale_par)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.12 muy alto

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g5, "ppareto", shape_par, scale_par)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno, aunque
# parece mejor que el anterior.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g5_par)

# 5. Comparamos AICs

ajuste_g5_ln$aic
ajuste_g5_wb$aic
ajuste_g5_exp$aic
ajuste_g5_ph2$aic
ajuste_g5_ph3$aic
ajuste_g5_par$aic


# Sigue siendo mejor la ph(3), la pareto es mejor que la exp


## Gráfico comparativo para todas

plot_data <- data.frame(
  tiempo = tiempos_g5
)


# Gráfico comparativo conjunto con la CDF PH
ggplot(plot_data, aes(tiempo)) +
  # ECDF empírica
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  # Log-Normal teórica
  stat_function(fun = plnorm, 
                args = list(meanlog = meanlog, sdlog = sdlog),
                aes(color = "Log-Normal"), size = 1, linetype = "dashed") +
  # Weibull teórica
  stat_function(fun = pweibull, 
                args = list(shape = shape, scale = scale),
                aes(color = "Weibull"), size = 1, linetype = "dashed") +
  # Exponencial teórica
  stat_function(fun = pexp, 
                args = list(rate = rate),
                aes(color = "Exponencial"), size = 1, linetype = "dashed") +
  # Phase-Type2 teórica
  stat_function(fun = cdf_ph2, aes(color = "PH(2)"), size = 1, linetype = "dashed") +
  # Phase-Type3 teórica
  stat_function(fun = cdf_ph3, aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  # Pareto teórica
  stat_function(fun = ppareto, 
                args = list(shape = shape_par, scale = scale_par),
                aes(color = "Pareto"), size = 1, linetype = "dashed") +
  # Títulos y etiquetas
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajustes para G5"),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", 
                                "Log-Normal" = "red", 
                                "Weibull" = "green", 
                                "Exponencial" = "purple",
                                "PH(2)" = "orange",
                                "Pareto" = "turquoise",
                                "PH(3)" = "pink")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) +
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos



################################################################################


## SLOT G6


# Leer los datos desde el archivo Excel
g6 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempo_servicio_22_24h.xlsx", 
                 sheet = 1, col_names = FALSE)

g6_vector <- as.numeric(as.matrix(g6))  # Convertir todo en un vector numérico



## LOGNORMAL

# 1. Para que pueda ajustarse una lognormal
g6_vector <- na.omit(g6_vector)  # Eliminar valores NA
g6_vector <- g6_vector[g6_vector > 0]  # Mantener solo valores positivos
tiempos_g6<-g6_vector

# 2. Ajustar distribución log-normal
ajuste_g6_ln <- fitdist(tiempos_g6, "lnorm")

# Mostrar resumen del ajuste
summary(ajuste_g6_ln)
meanlog <-ajuste_g6_ln$estimate["meanlog"]
sdlog<-ajuste_g6_ln$estimate["sdlog"]


# 3. Test de Kolmogorov-Smirnov
ks_g6_ln<-ks.test(tiempos_g6, "plnorm", meanlog, sdlog)
ks.test(tiempos_g6, "plnorm", meanlog, sdlog)

# Dado que el p-valor es muy bajo (1.595e-1o), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.05 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g6, "plnorm", meanlog, sdlog)
ad_test<-ad.test(tiempos_g6, "plnorm", meanlog, sdlog)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g6_ln)

# Donde mejor observamos la no bondad del ajuste es en el QQ-plot.



## WEIBULL

# 2. Ajustar distribución weibull
ajuste_g6_wb <- fitdist(tiempos_g6, "weibull")

# Mostrar resumen del ajuste
summary(ajuste_g6_wb)
shape <-ajuste_g6_wb$estimate["shape"]
scale<-ajuste_g6_wb$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g6, "pweibull", shape, scale)
ks_g6_wb<-ks.test(tiempos_g6, "pweibull", shape, scale)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.07 (coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g6, "pweibull", shape, scale)
ad_test<-ad.test(tiempos_g6, "pweibull", shape, scale)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g6_wb)


# Comparemos AICs

ajuste_g6_ln$aic
ajuste_g6_wb$aic

# Es menor el de la LN, por tanto es mejor su ajuste.



## EXPONENCIAL

# 2. Ajustar distribución exponencial
ajuste_g6_exp <- fitdist(tiempos_g6, "exp")

# Mostrar resumen del ajuste
summary(ajuste_g6_exp)
rate <-ajuste_g6_exp$estimate["rate"]


# 3. Test de Kolmogorov-Smirnov
ks.test(tiempos_g6, "pexp", rate)
ks_g6_exp<-ks.test(tiempos_g6, "pexp", rate)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.12

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g6, "pexp", rate)
ad_test<-ad.test(tiempos_g6, "pexp", rate)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno

# 4. Gráficos de diagnóstico completos
plot(ajuste_g6_exp)

# Gráficos similares a la Weibull.

# Comparemos AICs

ajuste_g6_ln$aic
ajuste_g6_wb$aic
ajuste_g6_exp$aic

# Sigue siendo mejor la LN.




## PHASE TYPE (2)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(2)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g6_ph2 <- phfit.point(ph = ph_model, x = tiempos_g6)

ajuste_g6_ph2
summary(ajuste_g6_ph2)

alpha<-ajuste_g6_ph2$alpha
S<-ajuste_g6_ph2$Q
S<-as.matrix(S)

cdf_ph <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}

# Realizar el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g6, cdf_ph)
ks_g6_ph2<-ks.test(tiempos_g6, cdf_ph)


# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.05 (no coincide)


# Comparamos AICs

ajuste_g6_ln$aic
ajuste_g6_wb$aic
ajuste_g6_exp$aic
ajuste_g6_ph2$aic

# El mejor AIC en este caso es el de la PH(2).


## PHASE TYPE (3)


# Crear un objeto PH (con mapfit)
ph_model <- mapfit::ph(3)
print(ph_model)


# Ajustar el modelo PH a los datos usando fit
ajuste_g6_ph3 <- phfit.point(ph = ph_model, x = tiempos_g6)

ajuste_g6_ph3
summary(ajuste_g6_ph3)

alpha<-ajuste_g6_ph3$alpha
S<-ajuste_g6_ph3$Q
S<-as.matrix(S)

cdf_ph3 <- function(x) {
  ones <- rep(1, nrow(S))  # Vector de unos
  sapply(x, function(xi) 1 - (alpha %*% expm(S * xi) %*% ones)[1])
}


# Realizamos el test de Kolmogorov-Smirnov (KS)
ks.test(tiempos_g6, cdf_ph3)
ks_g6_ph3<-ks.test(tiempos_g6, cdf_ph3)


# De nuevo, el p-valor es muy bajo (2.515-12), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.06 (No coincide)

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g6, cdf_ph3)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno.

# Comparamos AICs

ajuste_g6_ln$aic
ajuste_g6_wb$aic
ajuste_g6_exp$aic
ajuste_g6_ph2$aic
ajuste_g6_ph3$aic

# El mejor AIC es el de la PH(3), aunque por poco (la segunda la LN).


## PARETO

# 2. Ajustar distribución weibull
ajuste_g6_par <- fitdist(tiempos_g6, "pareto") # dpareto viene en el paquete actuar

# Mostrar resumen del ajuste
summary(ajuste_g6_par)
shape_par <-ajuste_g6_par$estimate["shape"]
scale_par<-ajuste_g6_par$estimate["scale"]


# 3. Test de Kolmogorov-Smirnov
ks_g6_par<-ks.test(tiempos_g6, "ppareto", shape_par, scale_par)
ks.test(tiempos_g6, "ppareto", shape_par, scale_par)

# De nuevo, el p-valor es muy bajo (2.2e-16), rechazamos la hipótesis nula, 
# concluyendo así que el ajuste no es bueno. D=0.12 muy alto

# Recibimos un Warning que indica que hay empates en la muestra, lo que puede
# afectar a la precision del test. Por ello, ejecutamos también el test de 
# Anderson - Darling, más fiable para datos con empates.

ad.test(tiempos_g6, "ppareto", shape_par, scale_par)

# De nuevo obtenemos un p-valor muy bajo, por que el ajuste no es bueno, aunque
# parece mejor que el anterior.

# 4. Gráficos de diagnóstico completos
plot(ajuste_g6_par)

# 5. Comparamos AICs

ajuste_g6_ln$aic
ajuste_g6_wb$aic
ajuste_g6_exp$aic
ajuste_g6_ph2$aic
ajuste_g6_ph3$aic
ajuste_g6_par$aic


# Sigue siendo mejor la ph(3), la pareto es mejor que la exp (por poco)



## Gráfico comparativo para todas

plot_data <- data.frame(
  tiempo = tiempos_g6
)


# Gráfico comparativo conjunto con la CDF PH
ggplot(plot_data, aes(tiempo)) +
  # ECDF empírica
  stat_ecdf(aes(color = "Empírica"), geom = "step", size = 1) +
  # Log-Normal teórica
  stat_function(fun = plnorm, 
                args = list(meanlog = meanlog, sdlog = sdlog),
                aes(color = "Log-Normal"), size = 1, linetype = "dashed") +
  # Weibull teórica
  stat_function(fun = pweibull, 
                args = list(shape = shape, scale = scale),
                aes(color = "Weibull"), size = 1, linetype = "dashed") +
  # Exponencial teórica
  stat_function(fun = pexp, 
                args = list(rate = rate),
                aes(color = "Exponencial"), size = 1, linetype = "dashed") +
  # Phase-Type2 teórica
  stat_function(fun = cdf_ph2, aes(color = "PH(2)"), size = 1, linetype = "dashed") +
  # Phase-Type3 teórica
  stat_function(fun = cdf_ph3, aes(color = "PH(3)"), size = 1, linetype = "dashed") +
  # Pareto teórica
  stat_function(fun = ppareto, 
                args = list(shape = shape_par, scale = scale_par),
                aes(color = "Pareto"), size = 1, linetype = "dashed") +
  # Títulos y etiquetas
  labs(
    title = "Comparación de Distribuciones Acumuladas",
    subtitle = paste("Ajustes para G6"),
    x = "Tiempo de servicio",
    y = "Probabilidad Acumulada",
    color = "Distribución"
  ) +
  scale_color_manual(values = c("Empírica" = "blue", 
                                "Log-Normal" = "red", 
                                "Weibull" = "green", 
                                "Exponencial" = "purple",
                                "PH(2)" = "orange",
                                "Pareto" = "turquoise",
                                "PH(3)" = "pink")) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Mover la leyenda a la derecha
    legend.direction = "vertical",  # Orientar la leyenda en vertical
    legend.title = element_text(size = 10),  # Ajustar tamaño de título de leyenda si es necesario
    legend.text = element_text(size = 8)  # Ajustar tamaño del texto de la leyenda si es necesario
  ) +
  coord_cartesian(xlim = c(0, 0.25))  # Limita el eje x sin recortar datos



################################################################################


## TABLA CON LOS ESTADISTICOS D DEL KS TEST

ks_stats <- list()


ks_stats$G1 <- c(
  LN = round(as.numeric(ks_g1_ln$statistic), 2),
  Weibull = round(as.numeric(ks_g1_wb$statistic), 2),
  Exp = round(as.numeric(ks_g1_exp$statistic),2),
  PH2 = round(as.numeric(ks_g1_ph2$statistic), 2),
  PH3 = round(as.numeric(ks_g1_ph3$statistic), 2),
  Pareto = round(as.numeric(ks_g1_par$statistic),2)
)

ks_stats$G2 <- c(
  LN = round(as.numeric(ks_g2_ln$statistic), 2),
  Weibull = round(as.numeric(ks_g2_wb$statistic), 2),
  Exp = round(as.numeric(ks_g2_exp$statistic),2),
  PH2 = round(as.numeric(ks_g2_ph2$statistic), 2),
  PH3 = round(as.numeric(ks_g2_ph3$statistic), 2),
  Pareto = round(as.numeric(ks_g2_par$statistic),2)
)

ks_stats$G3 <- c(
  LN = round(as.numeric(ks_g3_ln$statistic), 2),
  Weibull = round(as.numeric(ks_g3_wb$statistic), 2),
  Exp = round(as.numeric(ks_g3_exp$statistic),2),
  PH2 = round(as.numeric(ks_g3_ph2$statistic), 2),
  PH3 = round(as.numeric(ks_g3_ph3$statistic), 2),
  Pareto = round(as.numeric(ks_g3_par$statistic),2)
)

ks_stats$G4 <- c(
  LN = round(as.numeric(ks_g4_ln$statistic), 2),
  Weibull = round(as.numeric(ks_g4_wb$statistic), 2),
  Exp = round(as.numeric(ks_g4_exp$statistic),2),
  PH2 = round(as.numeric(ks_g4_ph2$statistic), 2),
  PH3 = round(as.numeric(ks_g4_ph3$statistic), 2),
  Pareto = round(as.numeric(ks_g4_par$statistic),2)
)

ks_stats$G5 <- c(
  LN = round(as.numeric(ks_g5_ln$statistic), 2),
  Weibull = round(as.numeric(ks_g5_wb$statistic), 2),
  Exp = round(as.numeric(ks_g5_exp$statistic),2),
  PH2 = round(as.numeric(ks_g5_ph2$statistic), 2),
  PH3 = round(as.numeric(ks_g5_ph3$statistic), 2),
  Pareto = round(as.numeric(ks_g5_par$statistic),2)
)

ks_stats$G6 <- c(
  LN = round(as.numeric(ks_g6_ln$statistic), 2),
  Weibull = round(as.numeric(ks_g6_wb$statistic), 2),
  Exp = round(as.numeric(ks_g6_exp$statistic),2),
  PH2 = round(as.numeric(ks_g6_ph2$statistic), 2),
  PH3 = round(as.numeric(ks_g6_ph3$statistic), 2),
  Pareto = round(as.numeric(ks_g6_par$statistic),2)
)


# Crear la tabla final
ks_table <- data.frame(
  Distribución = c("LN", "Weibull", "Exp", "PH(2)","PH(3)", "Pareto"),
  G1 = c(ks_stats$G1["LN"], ks_stats$G1["Weibull"], ks_stats$G1["Exp"], ks_stats$G1["PH2"], ks_stats$G1["PH3"], ks_stats$G1["Pareto"]),
  G2 = c(ks_stats$G2["LN"], ks_stats$G2["Weibull"], ks_stats$G2["Exp"], ks_stats$G2["PH2"], ks_stats$G2["PH3"], ks_stats$G2["Pareto"]),
  G3 = c(ks_stats$G3["LN"], ks_stats$G3["Weibull"], ks_stats$G3["Exp"], ks_stats$G3["PH2"], ks_stats$G3["PH3"], ks_stats$G3["Pareto"]),
  G4 = c(ks_stats$G4["LN"], ks_stats$G4["Weibull"], ks_stats$G4["Exp"], ks_stats$G4["PH2"], ks_stats$G4["PH3"], ks_stats$G4["Pareto"]),
  G5 = c(ks_stats$G5["LN"], ks_stats$G5["Weibull"], ks_stats$G5["Exp"], ks_stats$G5["PH2"], ks_stats$G5["PH3"], ks_stats$G5["Pareto"]),
  G6 = c(ks_stats$G6["LN"], ks_stats$G6["Weibull"], ks_stats$G6["Exp"], ks_stats$G6["PH2"], ks_stats$G6["PH3"], ks_stats$G6["Pareto"])
)

# Mostrar la tabla
library(knitr)
library(kableExtra)
kable(ks_table, caption = "Valor del estadístico D del test de Kolmogorov-Smirnov", align = 'c')


## TABLA A EXPORTAR

# Función para encontrar los mínimos por columna (en este caso los valores de D)
find_mins_ks <- function(df) {
  mins <- sapply(df[-1], function(x) x == min(x))  # -1 para evitar la primera columna (Distribución)
  return(mins)
}

# Aplicar a nuestra tabla KS
mins_matrix_ks <- find_mins_ks(ks_table)

# Crear versión con negrita
ks_table_marked <- ks_table
for(col in 2:ncol(ks_table_marked)) {  # Comenzamos en la columna 2 para evitar la columna de Distribución
  ks_table_marked[,col] <- ifelse(mins_matrix_ks[,col-1], 
                                  paste0("<b>", ks_table[,col], "</b>"),  # Colocar en negrita
                                  as.character(ks_table[,col]))
}

# Mostrar tabla con knitr
knitr::kable(ks_table_marked, 
             caption = "Valor del estadístico D del test de Kolmogorov-Smirnov (negrita = menor D)", 
             align = c('l', rep('c', ncol(ks_table)-1)),
             col.names = c("Distribución", "G1", "G2", "G3", "G4", "G5", "G6"),
             escape = FALSE)  # Habilitar HTML para negrita


## FINAL


library(kableExtra)

# 1. Crear data frame limpio (sin columna duplicada)
ks_table_clean <- data.frame(
  Distribución = c("LN", "Weibull", "Exp", "PH(2)", "PH(3)", "Pareto"),
  G1 = c(0.08, 0.09, 0.13, 0.06, 0.06, 0.14),
  G2 = c(0.04, 0.08, 0.10, 0.06, 0.03, 0.12),
  G3 = c(0.03, 0.07, 0.12, 0.06, 0.03, 0.12),
  G4 = c(0.05, 0.07, 0.12, 0.05, 0.04, 0.13),
  G5 = c(0.04, 0.08, 0.10, 0.06, 0.03, 0.12),
  G6 = c(0.05, 0.07, 0.12, 0.05, 0.03, 0.12)
)

# 2. Función mejorada para encontrar mínimos (maneja valores numéricos)
find_mins_ks <- function(df) {
  mins <- sapply(df[-1], function(x) {
    x_num <- as.numeric(as.character(x))
    x_num == min(x_num, na.rm = TRUE)
  })
  return(mins)
}

# 3. Aplicar negritas a los mínimos
mins_matrix <- find_mins_ks(ks_table_clean)
ks_table_marked <- ks_table_clean

for(col in 2:ncol(ks_table_marked)) {
  ks_table_marked[[col]] <- ifelse(mins_matrix[,col-1],
                                   paste0("<b>", ks_table_clean[[col]], "</b>"),
                                   as.character(ks_table_clean[[col]]))
}

# 4. Poner en negrita TODA la columna Distribución
ks_table_marked$Distribución <- paste0("<b>", ks_table_marked$Distribución, "</b>")

# 5. Mostrar tabla final
kable(ks_table_marked, "html", escape = FALSE,
      align = c('l', rep('c', ncol(ks_table_marked)-1)),
      col.names = c("Distribución", "G1", "G2", "G3", "G4", "G5", "G6")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(1, width = "2.5cm") %>%  # Ancho para columna Distribución
  column_spec(2:7, width = "2cm") %>%  # Ancho para columnas G1-G6
  row_spec(0, bold = TRUE, color = "white", background = "#3498db")



# Con mapa de colores

library(gt)
library(scales)

# Crear tabla gt con formato
gt_table <- gt(ks_table) %>%
  tab_header(
    title = "Estadístico D de Kolmogorov-Smirnov",
    subtitle = "Comparación de ajustes para diferentes distribuciones"
  ) %>%
  fmt_number(columns = 2:7, decimals = 2) %>%
  cols_label(
    Distribución = "Distribución",
    G1 = md("**G₁**"),
    G2 = md("**G₂**"),
    G3 = md("**G₃**"),
    G4 = md("**G₄**"),
    G5 = md("**G₅**"),
    G6 = md("**G₆**")
  ) %>%
  data_color(
    columns = 2:7,
    colors = scales::col_numeric(
      palette = c("#FFEEEE", "#FF0000"),
      domain = c(0, max(ks_table[,-1]))
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

# Visualizar
gt_table

  



################################################################################


## TABLA CON LOS AIC


# Crear una lista para almacenar todos los resultados AIC
aic_values <- list()

# Almacenar los valores AIC para cada grupo
aic_values$G1 <- c(
  LN = round(ajuste_g1_ln$aic, 1),
  Weibull = round(ajuste_g1_wb$aic, 1),
  Exp = round(ajuste_g1_exp$aic, 1),
  PH2 = round(ajuste_g1_ph2$aic, 1),
  PH3 = round(ajuste_g1_ph3$aic,1),
  Pareto = round(ajuste_g1_par$aic,1)
)

aic_values$G2 <- c(
  LN = round(ajuste_g2_ln$aic, 1),
  Weibull = round(ajuste_g2_wb$aic, 1),
  Exp = round(ajuste_g2_exp$aic, 1),
  PH2 = round(ajuste_g2_ph2$aic, 1),
  PH3 = round(ajuste_g2_ph3$aic,1),
  Pareto = round(ajuste_g2_par$aic,1)
)

aic_values$G3 <- c(
  LN = round(ajuste_g3_ln$aic, 1),
  Weibull = round(ajuste_g3_wb$aic, 1),
  Exp = round(ajuste_g3_exp$aic, 1),
  PH2 = round(ajuste_g3_ph2$aic, 1),
  PH3 = round(ajuste_g3_ph3$aic,1),
  Pareto = round(ajuste_g3_par$aic,1)
)

aic_values$G4 <- c(
  LN = round(ajuste_g4_ln$aic, 1),
  Weibull = round(ajuste_g4_wb$aic, 1),
  Exp = round(ajuste_g4_exp$aic, 1),
  PH2 = round(ajuste_g4_ph2$aic, 1),
  PH3 = round(ajuste_g4_ph3$aic,1),
  Pareto = round(ajuste_g4_par$aic,1)
)

aic_values$G5 <- c(
  LN = round(ajuste_g5_ln$aic, 1),
  Weibull = round(ajuste_g5_wb$aic, 1),
  Exp = round(ajuste_g5_exp$aic, 1),
  PH2 = round(ajuste_g5_ph2$aic, 1),
  PH3 = round(ajuste_g5_ph3$aic,1),
  Pareto = round(ajuste_g5_par$aic,1)
)

aic_values$G6 <- c(
  LN = round(ajuste_g6_ln$aic, 1),
  Weibull = round(ajuste_g6_wb$aic, 1),
  Exp = round(ajuste_g6_exp$aic, 1),
  PH2 = round(ajuste_g6_ph2$aic, 1),
  PH3 = round(ajuste_g6_ph3$aic,1),
  Pareto = round(ajuste_g6_par$aic,1)
)

# Crear la tabla AIC (como en tu código anterior)
aic_table <- data.frame(
  Modelo = c("LN", "Weibull", "Exp", "PH(2)", "PH(3)", "Pareto"),
  G1 = c(aic_values$G1["LN"], aic_values$G1["Weibull"], aic_values$G1["Exp"], aic_values$G1["PH2"], aic_values$G1["PH3"], aic_values$G1["Pareto"]),
  G2 = c(aic_values$G2["LN"], aic_values$G2["Weibull"], aic_values$G2["Exp"], aic_values$G2["PH2"], aic_values$G2["PH3"], aic_values$G2["Pareto"]),
  G3 = c(aic_values$G3["LN"], aic_values$G3["Weibull"], aic_values$G3["Exp"], aic_values$G3["PH2"], aic_values$G3["PH3"], aic_values$G3["Pareto"]),
  G4 = c(aic_values$G4["LN"], aic_values$G4["Weibull"], aic_values$G4["Exp"], aic_values$G4["PH2"], aic_values$G4["PH3"], aic_values$G4["Pareto"]),
  G5 = c(aic_values$G5["LN"], aic_values$G5["Weibull"], aic_values$G5["Exp"], aic_values$G5["PH2"], aic_values$G5["PH3"], aic_values$G5["Pareto"]),
  G6 = c(aic_values$G6["LN"], aic_values$G6["Weibull"], aic_values$G6["Exp"], aic_values$G6["PH2"], aic_values$G6["PH3"], aic_values$G6["Pareto"])
)

# Mostrar la tabla con knitr::kable
knitr::kable(aic_table, 
             caption = "Valores del Criterio de Información de Akaike (AIC) para los modelos ajustados",
             align = c('l', rep('c', ncol(aic_table)-1)),
             col.names = c("Modelo", "G1", "G2", "G3", "G4", "G5", "G6"),
             format.args = list(digits = 1))


# Mostrar la tabla con kableExtra
aic_table %>%
  kable("html", caption = "Valores del AIC para los modelos ajustados", align = 'c',
        row.names = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE)  # Hacer la primera columna en negrita


# Función para encontrar los mínimos por columna
find_mins <- function(df) {
  mins <- sapply(df[-1], function(x) x == min(x))
  return(mins)
}

# Aplicar a nuestra tabla
mins_matrix <- find_mins(aic_table)

# Crear versión con negrita
aic_table_marked <- aic_table
for(col in 2:ncol(aic_table_marked)) {
  aic_table_marked[,col] <- ifelse(mins_matrix[,col-1], 
                                   paste0("<b>", aic_table[,col], "</b>"), 
                                   as.character(aic_table[,col]))
}

# Mostrar tabla con knitr
knitr::kable(aic_table_marked, 
             caption = "Valores AIC (negrita = mejor modelo para cada grupo)",
             align = c('l', rep('c', ncol(aic_table)-1)),
             col.names = c("Modelo", "G1", "G2", "G3", "G4", "G5", "G6"),
             format.args = list(digits = 1),
             escape = FALSE)  # Para permitir el formato HTML

# Mostrar la tabla con kableExtra
aic_table_marked %>%
  kable("html", caption = "Valores del AIC para los modelos ajustados", align = 'c',
        row.names = FALSE, escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE)  # Hacer la primera columna en negrita


## FINAL


# 1. Crear data frame limpio (sin columna duplicada)
aic_table_clean <- aic_table

# 2. Función mejorada para encontrar mínimos (maneja valores numéricos)
find_mins_ks <- function(df) {
  mins <- sapply(df[-1], function(x) {
    x_num <- as.numeric(as.character(x))
    x_num == min(x_num, na.rm = TRUE)
  })
  return(mins)
}

# 3. Aplicar negritas a los mínimos
mins_matrix <- find_mins_ks(aic_table_clean)
aic_table_marked <- aic_table_clean

for(col in 2:ncol(aic_table_marked)) {
  aic_table_marked[[col]] <- ifelse(mins_matrix[,col-1],
                                   paste0("<b>", aic_table_clean[[col]], "</b>"),
                                   as.character(aic_table_clean[[col]]))
}

# 4. Poner en negrita TODA la columna Distribución
aic_table_marked$Modelo <- paste0("<b>", aic_table_marked$Modelo, "</b>")


# 5. Mostrar tabla final
kable(aic_table_marked, "html", escape = FALSE,
      align = c('l', rep('c', ncol(aic_table_marked)-1)),
      col.names = c("Distribución", "G1", "G2", "G3", "G4", "G5", "G6")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(1, width = "2.5cm") %>%  # Ancho para columna Distribución
  column_spec(2:7, width = "2cm") %>%  # Ancho para columnas G1-G6
  row_spec(0, bold = TRUE, color = "white", background = "#3498db")


