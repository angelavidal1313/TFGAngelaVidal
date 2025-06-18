
## CROSS VALIDATION TIEMPOS SERVICIO

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
require(caret)


# Cargamos y preparamos los datos de los tiempos de servicio

setwd("/Users/angelavidalmonge/Desktop/TFG/TiemposServicio/CrossValidation")
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_7_9h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g1)[1] <- "TiemposServicio"
g2 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_9_11h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g2)[1] <- "TiemposServicio"
g3 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_11_16h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g3)[1] <- "TiemposServicio"
g4 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_16_17h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g4)[1] <- "TiemposServicio"
g5 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_servicio_17_22h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g5)[1] <- "TiemposServicio"
g6 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempo_servicio_22_24h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g6)[1] <- "TiemposServicio"



cv_distributions_ks <- function(data, variable, k = 10, seed = 1234) {
  
  set.seed(seed)
  
  # Extraer la variable objetivo como vector
  y <- data[[variable]]
  
  # Crear folds para validación cruzada
  folds <- createFolds(y, k = k)
  
  # Aplicar validación cruzada
  cv_ks <- lapply(folds, function(x){
    training_fold <- data[-x, ]
    test_fold <- data[x, ]
    
    tiempos_fold <- training_fold[[variable]]
    tiempos_test <- test_fold[[variable]]
    
    # Lognormal
    fit_lognormal <- fitdist(tiempos_fold, "lnorm")
    ks_lognormal <- ks.test(tiempos_test, "plnorm",
                            meanlog = fit_lognormal$estimate["meanlog"],
                            sdlog = fit_lognormal$estimate["sdlog"])$statistic
    
    # Weibull
    fit_weibull <- fitdist(tiempos_fold, "weibull")
    ks_weibull <- ks.test(tiempos_test, "pweibull",
                          shape = fit_weibull$estimate["shape"],
                          scale = fit_weibull$estimate["scale"])$statistic
    
    # Exponencial
    fit_exponential <- fitdist(tiempos_fold, "exp")
    ks_exponential <- ks.test(tiempos_test, "pexp",
                              rate = fit_exponential$estimate["rate"])$statistic
    
    # Pareto
    fit_pareto <- fitdist(tiempos_fold, "pareto")
    ks_pareto <- ks.test(tiempos_test, "ppareto",
                         shape = fit_pareto$estimate["shape"],
                         scale = fit_pareto$estimate["scale"])$statistic
    
    # PH(2)
    ph_model_2 <- mapfit::ph(2)
    ajuste_ph2 <- phfit.point(ph = ph_model_2, x = tiempos_fold)
    alpha2 <- ajuste_ph2$alpha
    S2 <- as.matrix(ajuste_ph2$Q)
    cdf_ph2 <- function(x) {
      ones <- rep(1, nrow(S2))
      sapply(x, function(xi) 1 - (alpha2 %*% expm(S2 * xi) %*% ones)[1])
    }
    ks_ph2 <- ks.test(tiempos_test, cdf_ph2)$statistic
    
    # PH(3)
    ph_model_3 <- mapfit::ph(3)
    ajuste_ph3 <- phfit.point(ph = ph_model_3, x = tiempos_fold)
    alpha3 <- ajuste_ph3$alpha
    S3 <- as.matrix(ajuste_ph3$Q)
    cdf_ph3 <- function(x) {
      ones <- rep(1, nrow(S3))
      sapply(x, function(xi) 1 - (alpha3 %*% expm(S3 * xi) %*% ones)[1])
    }
    ks_ph3 <- ks.test(tiempos_test, cdf_ph3)$statistic
    
    return(c(lognormal = ks_lognormal,
             weibull = ks_weibull,
             exponential = ks_exponential,
             pareto = ks_pareto,
             PH2 = ks_ph2,
             PH3 = ks_ph3))
  })
  
  # Convertir a matriz y calcular medias
  ks_matrix <- do.call(rbind, cv_ks)
  mean_ks <- colMeans(ks_matrix)
  
  # Devolver resultados ordenados
  return(mean_ks)
}


resultados_g1 <- cv_distributions_ks(g1, variable = "TiemposServicio")
print(resultados_g1)

resultados_g2 <- cv_distributions_ks(g2, variable = "TiemposServicio")
print(resultados_g2)

resultados_g3 <- cv_distributions_ks(g3, variable = "TiemposServicio")
print(resultados_g3)

resultados_g4 <- cv_distributions_ks(g4, variable = "TiemposServicio")
print(resultados_g4)

resultados_g5 <- cv_distributions_ks(g5, variable = "TiemposServicio")
print(resultados_g5)

resultados_g6 <- cv_distributions_ks(g6, variable = "TiemposServicio")
print(resultados_g6)


# Ejecutar y guardar resultados en lista nombrada
resultados <- list(
  resultados_g1, resultados_g2, resultados_g3, resultados_g4, resultados_g5, resultados_g6
)

# Unir en un solo data frame
tabla_resultados <- do.call(rbind, resultados)
tabla_resultados <- as.data.frame(tabla_resultados)
tabla_resultados$Grupo <- rownames(tabla_resultados)

# Reordenar columnas: primero Grupo, luego las distribuciones
tabla_resultados <- tabla_resultados[, c("Grupo", sort(colnames(tabla_resultados)[-ncol(tabla_resultados)]))]

# Mostrar tabla
print(tabla_resultados)

# (Opcional) Visualización bonita con knitr::kable
knitr::kable(tabla_resultados, digits = 4, caption = "Promedio del estadístico KS por distribución y grupo")



## FINAL

resultados_g1 <- round(as.numeric(resultados_g1),3)
resultados_g2 <- round(as.numeric(resultados_g2),3)
resultados_g3 <- round(as.numeric(resultados_g3),3)
resultados_g4 <- round(as.numeric(resultados_g4),3)
resultados_g5 <- round(as.numeric(resultados_g5),3)
resultados_g6 <- round(as.numeric(resultados_g6),3)

cv_table <- data.frame(
  Distribución = c("LN", "Weibull", "Exp", "Pareto", "PH(2)", "PH(3)"),
  G1 = resultados_g1,
  G2 = resultados_g2,
  G3 = resultados_g3,
  G4 = resultados_g4,
  G5 = resultados_g5,
  G6 = resultados_g6)

cv_table <- data.frame(
  Distribución = c("LN", "Weibull", "Exp", "Pareto", "PH(2)", "PH(3)"),
  G1 = c(0.085,0.093,0.136,0.142,0.07,0.06),
  G2 = c(0.078,0.085,0.107,0.12,0.048,0.036),
  G3 = c(0.059,0.079,0.118,0.123,0.039,0.036),
  G4 = c(0.066,0.087,0.133,0.134,0.061,0.052),
  G5 = c(0.067,0.081,0.108,0.12,0.049,0.033),
  G6 = c(0.062,0.082,0.119,0.124,0.061,0.049))

# 1. Crear data frame limpio (sin columna duplicada)
cv_table_clean <- cv_table

# 2. Función mejorada para encontrar mínimos (maneja valores numéricos)
find_mins_ks <- function(df) {
  mins <- sapply(df[-1], function(x) {
    x_num <- as.numeric(as.character(x))
    x_num == min(x_num, na.rm = TRUE)
  })
  return(mins)
}

# 3. Aplicar negritas a los mínimos
mins_matrix <- find_mins_ks(cv_table_clean)
cv_table_marked <- cv_table_clean

for(col in 2:ncol(cv_table_marked)) {
  cv_table_marked[[col]] <- ifelse(mins_matrix[,col-1],
                                    paste0("<b>", cv_table_clean[[col]], "</b>"),
                                    as.character(cv_table_clean[[col]]))
}

# 4. Poner en negrita TODA la columna Distribución
cv_table_marked$Distribución <- paste0("<b>", cv_table_marked$Distribución, "</b>")


# 5. Mostrar tabla final
kable(cv_table_marked, "html", escape = FALSE,
      align = c('l', rep('c', ncol(cv_table_marked)-1)),
      col.names = c("Distribución", "G1", "G2", "G3", "G4", "G5", "G6")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(1, width = "2.5cm") %>%  # Ancho para columna Distribución
  column_spec(2:7, width = "2cm") %>%  # Ancho para columnas G1-G6
  row_spec(0, bold = TRUE, color = "white", background = "#3498db")

