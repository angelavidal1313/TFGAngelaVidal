## CROSS VALIDATION TIEMPOS DE IMPACIENCIA 3

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
library(survival)     # Para survreg
library(caret)        # Para createFolds
library(phd)          # Para KScens
library(dplyr)        # Para manipulación de datos


# Cargamos y preparamos los datos de los tiempos de impaciencia

setwd("/Users/angelavidalmonge/Desktop/TFG/TiemposServicio/CrossValidation")
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_7_9h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g1)[1] <- "TiemposEspera"
colnames(g1)[2] <- "Censura"

g2 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_9_11h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g2)[1] <- "TiemposEspera"
colnames(g2)[2] <- "Censura"

g3 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_11_16h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g3)[1] <- "TiemposEspera"
colnames(g3)[2] <- "Censura"

g4 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_16_17h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g4)[1] <- "TiemposEspera"
colnames(g4)[2] <- "Censura"

g5 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_17_22h.xlsx", 
                 sheet = 1, col_names = FALSE)
g5 <- g5[-1,]
g5[] <- lapply(g5, as.numeric)
colnames(g5)[1] <- "TiemposEspera"
colnames(g5)[2] <- "Censura"

g6 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/datos_censurados_22_24h.xlsx", 
                 sheet = 1, col_names = FALSE)
colnames(g6)[1] <- "TiemposEspera"
colnames(g6)[2] <- "Censura"
g6 <- g6 %>% filter(TiemposEspera > 0)


# Crear folds manteniendo Censura
create_stratified_folds <- function(data, k = 10, seed = 1234) {
  set.seed(seed)
  
  idx_cens_0 <- which(data$Censura == 0)
  idx_cens_1 <- which(data$Censura == 1)
  
  folds_cens_0 <- createFolds(idx_cens_0, k = k, list = TRUE)
  folds_cens_1 <- createFolds(idx_cens_1, k = k, list = TRUE)
  
  # Combinar los folds de censura 0 y 1
  folds <- vector("list", k)
  for (i in 1:k) {
    idx_0 <- idx_cens_0[folds_cens_0[[i]]]
    idx_1 <- idx_cens_1[folds_cens_1[[i]]]
    folds[[i]] <- c(idx_0, idx_1)
  }
  
  return(folds)
}


# Para el ajuste de la BS

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


# Para calcular el KS test (manualmente)
library(survival)
library(flexsurv)

# Función para calcular D censurado

calculate_censored_ks <- function(surv_obj, dist_name, params) {
  # Función de supervivencia empírica (Kaplan-Meier)
  km_fit <- survfit(surv_obj ~ 1)
  times <- km_fit$time
  surv_emp <- km_fit$surv
  
  # Función de supervivencia teórica según la distribución
  surv_theo <- switch(
    dist_name,
    "exponential" = pexp(times, rate = params$lambda, lower.tail = FALSE),
    "lognormal"   = plnorm(times, meanlog = params$mu, sdlog = params$sigma, lower.tail = FALSE),
    "weibull"     = pweibull(times, shape = params$shape, scale = params$scale, lower.tail = FALSE),
    "birnbaum_saunders" = pbirnbaum_saunders(times, alpha = params$alpha, beta = params$beta, lower.tail = FALSE),
    stop("Distribución no reconocida")
  )
  
  # Calcular estadístico D
  D <- max(abs(surv_emp - surv_theo))
  return(D)
}



# Validación cruzada con censura
cv_ks_cens <- function(data, k = 10, seed = 1234) {
  
  data <- data[!is.na(data$TiemposEspera) & !is.na(data$Censura), ]
  data$TiemposEspera <- as.numeric(data$TiemposEspera)
  data$Censura <- as.numeric(data$Censura)
  
  folds <- create_stratified_folds(data, k = k, seed = seed)
  
  cv_ks <- lapply(seq_along(folds), function(i) {
    x <- folds[[i]]
    training_fold <- data[-x, ]
    test_fold <- data[x, ]
    
    tryCatch({
      # Supervivencia censurada
      eventos_train <- as.numeric(training_fold$Censura==0)
      eventos_test <- as.numeric(test_fold$Censura==0)
      cens_test <- test_fold$Censura == 1
      Surv_train <- Surv(time = training_fold$TiemposEspera, event = eventos_train)
      Surv_test <- Surv(time = test_fold$TiemposEspera, event=eventos_test)
      
      # EXPONENCIAL
      fit_exp <- survreg(Surv_train ~ 1, dist = "exponential")
      lambda_exp <- 1 / exp(fit_exp$coefficients)
      ks_exp <- calculate_censored_ks(Surv_test, "exponential", list(lambda = lambda_exp))
      
      # LOGNORMAL
      fit_logn <- survreg(Surv_train ~ 1, dist = "lognormal")
      mu_ln <- fit_logn$coefficients
      sigma_ln <- fit_logn$scale
      ks_logn <- calculate_censored_ks(Surv_test, "lognormal", list(mu = mu_ln, sigma = sigma_ln))
      
      # WEIBULL
      fit_weib <- survreg(Surv_train ~ 1, dist = "weibull")
      shape_wb <- 1 / fit_weib$scale
      scale_wb <- exp(fit_weib$coefficients)
      ks_weib <- calculate_censored_ks(Surv_test, "weibull", list(shape = shape_wb, scale = scale_wb))
      
      # BIRNBAUM-SAUNDERS
      fit_bs <- flexsurvreg(Surv_train ~ 1, dist = bs_custom)
      alpha_bs <- fit_bs$res["alpha", "est"]
      beta_bs  <- fit_bs$res["beta", "est"]
      ks_bs <- calculate_censored_ks(Surv_test, "birnbaum_saunders", list(alpha = alpha_bs, beta = beta_bs))
      
      return(c(exponential = ks_exp,
               lognormal = ks_logn,
               weibull = ks_weib,
               birnbaum_saunders = ks_bs))
    }, error = function(e) {
      message(sprintf("Fold %d falló: %s", i, e$message))
      return(NULL)
    })
  })
  
  # Filtrar resultados válidos
  cv_ks_valid <- Filter(Negate(is.null), cv_ks)
  
  if (length(cv_ks_valid) == 0) {
    stop("Todos los folds fallaron. Revisa los datos.")
  }
  
  ks_matrix <- do.call(rbind, cv_ks_valid)
  mean_ks <- colMeans(ks_matrix)
  return(mean_ks)
}


resultados_g1 <- cv_ks_cens(g1)
print(resultados_g1)

resultados_g2 <- cv_ks_cens(g2)
print(resultados_g2)

resultados_g3 <- cv_ks_cens(g3)
print(resultados_g3)

resultados_g4 <- cv_ks_cens(g4)
print(resultados_g4)

resultados_g5 <- cv_ks_cens(g5)
print(resultados_g5)

resultados_g6 <- cv_ks_cens(g6)
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
  Distribución = c("Exp", "LN", "Weibull","B-S"),
  G1 = resultados_g1,
  G2 = resultados_g2,
  G3 = resultados_g3,
  G4 = resultados_g4,
  G5 = resultados_g5,
  G6 = resultados_g6)

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

library(kableExtra)

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

