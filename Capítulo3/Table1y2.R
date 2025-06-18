library(readxl)

# Leer los datos desde el archivo Excel
g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_7_9h.xlsx", 
                 sheet = 1, col_names = FALSE)
g2 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_9_11h.xlsx", 
                  sheet = 1, col_names = FALSE)
g3 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_11_16h.xlsx", 
                 sheet = 1, col_names = FALSE)
g4 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_16_17h.xlsx", 
                 sheet = 1, col_names = FALSE)
g5 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_17_22h.xlsx", 
                 sheet = 1, col_names = FALSE)
g6 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_22_24h.xlsx", 
                 sheet = 1, col_names = FALSE)



## Forma 1: El slot se divide en intervalos disjuntos de longitud tau

n_tau <- function(datos, tau,hours) {
  num_days <- ncol(datos)  # Número de días
  num_int <- floor(2/tau)  # Número de intervalos de longitud tau en 2 horas
  inicio_int <- seq(0, 2, by = tau)  # Puntos de inicio de cada intervalo
  
  resultados <- matrix(0, nrow = num_days, ncol = num_int)
  
  for (day in 1:num_days) {
    tiempos <- as.numeric(datos[[day]])  # Tiempos entre llegadas para el día
    tiempos <- tiempos[!is.na(tiempos)]  # Eliminar NA
    
    if (length(tiempos) == 0) {  # Si no hay datos, pasar al siguiente día
      next
    }
    
    # Calcular tiempos acumulados de llegada
    tiempos_acum <- cumsum(tiempos)
    
    # Filtrar eventos que ocurren dentro de las 2 horas
    tiempos_acum <- tiempos_acum[tiempos_acum <= 2]
    
    # Contar llegadas en cada intervalo tau
    for (i in 1:num_int) {
      inicio <- inicio_int[i]
      fin <- inicio_int[i+1]
      resultados[day, i] <- sum(tiempos_acum > inicio & tiempos_acum <= fin)
    }
  }
  
  return(resultados)
}

n_tau2 <- function(df, tau, hours) {
  matrix <- as.matrix(df)
  n <- ncol(matrix)
  agrupacion <- tau
  particion <- hours / agrupacion
  
  matriz_llegadas <- list()
  count <- 0
  
  for (k in 1:n) {
    t <- matrix[, k]
    tt <- t[!is.na(t) & t != 0]  # Filtrar valores NA y ceros
    
    acumulado <- cumsum(tt)
    llegadas <- rep(0, particion)
    
    for (j in 1:particion) {
      x <- acumulado[agrupacion * (j - 1) < acumulado & acumulado <= agrupacion * j]
      llegadas[j] <- length(x)
    }
    
    matriz_llegadas[[length(matriz_llegadas) + 1]] <- llegadas
    count <- count + 1
  }
  
  matriz <- do.call(rbind, matriz_llegadas)
  
  if (!is.null(matriz)) {
    matriz <- matriz[rowSums(matriz, na.rm = TRUE) != 0, , drop = FALSE]  # Eliminar filas con suma 0
  }
  
  return(matriz)
}


n_tau(g1,0.2,2)
n_tau2(g1,0.2,2)


calculate_vmr1 <- function(datos,tau,hours) {
  
  n_tau_matrix<-n_tau2(datos,tau,hours)
  # Convertir la matriz a un vector (todos los conteos de todos los intervalos y días)
  all_counts <- as.vector(n_tau_matrix)
  
  # Eliminar posibles NA (si los hay)
  all_counts <- all_counts[!is.na(all_counts)]
  
  # Calcular varianza y media
  variance <- var(all_counts)
  mean_val <- mean(all_counts)
  
  # Calcular VMR
  vmr <- variance / mean_val
  
  return(vmr)
}

tau<-c(seq(0.2,1,by=0.2),2,3)

for (i in tau){
  print(calculate_vmr1(g1,i,2))
}

for (i in tau){
  print(calculate_vmr1(g1,i,2))
}

for (i in tau){
  print(calculate_vmr1(g3,i,5))
}


## PARA REALIZAR LA TABLA

# Crear un data frame vacío para almacenar los resultados
resultados_vmr <- data.frame(τ = tau)

# Lista de datasets y nombres
datasets <- list(g1, g2, g3, g4, g5, g6)
nombres <- c("G1", "G2", "G3", "G4", "G5", "G6")
horas <- c(2, 2, 5, 1, 5, 2)  # Ajusta según corresponda

# Calcular VMR para cada dataset y tau
for (j in 1:length(datasets)) {
  vmr_values <- sapply(tau, function(t) calculate_vmr1(datasets[[j]], t, horas[j]))
  resultados_vmr[[nombres[j]]] <- vmr_values
}

# Mostrar la tabla
print(resultados_vmr)

library(kableExtra)

# Tabla con formato mejorado
kable(round(resultados_vmr,3), caption = "Tabla de VMR para diferentes valores de τ") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))





## Forma 2: El slot se divide en intervalos solapados de longitud tau y salto de 0.1 en los inicios

n_tau_sliding <- function(datos, tau,hours) {
  num_days <- ncol(datos)  # Número de días
  step <- 0.1  # Paso para el inicio de los intervalos
  max_start <- hours - tau  # Máximo inicio posible para que el intervalo quede dentro de 2 horas
  start_points <- seq(0, max_start, by = step)  # Puntos de inicio de cada intervalo
  num_intervals <- length(start_points)  # Número total de intervalos a evaluar
  
  # Matriz para almacenar resultados (días x intervalos)
  resultados <- matrix(0, nrow = num_days, ncol = num_intervals)
  
  for (day in 1:num_days) {
    tiempos <- as.numeric(datos[[day]])  # Tiempos entre llegadas para el día
    tiempos <- tiempos[!is.na(tiempos)]  # Eliminar NA
    
    if (length(tiempos) == 0) {  # Si no hay datos, pasar al siguiente día
      next
    }
    
    # Calcular tiempos acumulados de llegada
    tiempos_acum <- cumsum(tiempos)
    
    # Filtrar eventos que ocurren dentro de las 2 horas
    tiempos_acum <- tiempos_acum[tiempos_acum <= 2]
    
    # Contar llegadas en cada intervalo [start, start+tau]
    for (i in 1:num_intervals) {
      inicio <- start_points[i]
      fin <- inicio + tau
      resultados[day, i] <- sum(tiempos_acum > inicio & tiempos_acum <= fin)
    }
  }
  
  # Asignar nombres a las columnas para identificar los intervalos
  colnames(resultados) <- paste0("[", start_points, ",", start_points + tau, "]")
  
  return(resultados)
}


calculate_vmr2 <- function(datos,tau,hours) {
  
  n_tau_matrix<-n_tau_sliding(datos,tau,hours)

  all_counts <- as.vector(n_tau_matrix)
  
  # Eliminar posibles NA
  all_counts <- all_counts[!is.na(all_counts)]
  
  # Calcular varianza y media
  variance <- var(all_counts)
  mean_val <- mean(all_counts)
  
  # Calcular VMR
  vmr <- variance / mean_val
  
  return(vmr)
}



## Forma 2: tomando las medianas


# Esta funcion devuelve lo que seria mi n_tau + medianas de: media, varianza, correlaciones.

proceso_conteo_v2 <- function(df, tau, hours) {
  matrix <- as.matrix(df)
  n <- ncol(matrix)
  agrupacion <- tau
  particion <- hours / agrupacion
  
  matriz_llegadas <- list()
  count <- 0
  
  for (k in 1:n) {
    t <- matrix[, k]
    tt <- t[!is.na(t) & t != 0]  # Filtrar valores NA y ceros
    
    acumulado <- cumsum(tt)
    llegadas <- rep(0, particion)
    
    for (j in 1:particion) {
      x <- acumulado[agrupacion * (j - 1) < acumulado & acumulado <= agrupacion * j]
      llegadas[j] <- length(x)
    }
    
    matriz_llegadas[[length(matriz_llegadas) + 1]] <- llegadas
    count <- count + 1
  }
  
  matriz <- do.call(rbind, matriz_llegadas)
  
  if (!is.null(matriz)) {
    matriz <- matriz[rowSums(matriz, na.rm = TRUE) != 0, , drop = FALSE]  # Eliminar filas con suma 0
  }
  
  correlaciones <- c()
  varianzas <- c()
  medias <- c()
  
  r <- nrow(matriz)
  
  if (r > 1) {
    for (i in 1:(r - 1)) {
      correlacion <- cor(matriz[i, ], matriz[i + 1, ], use = "complete.obs")
      correlaciones <- c(correlaciones, correlacion)
    }
  }
  
  for (i in 1:ncol(matriz)) {
    media <- mean(matriz[, i], na.rm = TRUE)
    varianza <- var(matriz[, i], na.rm = TRUE)
    medias <- c(medias, media)
    varianzas <- c(varianzas, varianza)
  }
  
  correlacion2 <- median(correlaciones, na.rm = TRUE)
  media2 <- median(medias, na.rm = TRUE)
  varianza2 <- median(varianzas, na.rm = TRUE)
  
  return(list(matriz = matriz, correlacion2 = correlacion2, media2 = media2, varianza2 = varianza2))
}


### Realicemos la tabla

vmr_marcos <- function(df, tau, hours) {
  matrix <- as.matrix(df)
  n <- ncol(matrix)
  agrupacion <- tau
  particion <- hours / agrupacion
  
  matriz_llegadas <- list()
  count <- 0
  
  for (k in 1:n) {
    t <- matrix[, k]
    tt <- t[!is.na(t) & t != 0]  # Filtrar valores NA y ceros
    
    acumulado <- cumsum(tt)
    llegadas <- rep(0, particion)
    
    for (j in 1:particion) {
      x <- acumulado[agrupacion * (j - 1) < acumulado & acumulado <= agrupacion * j]
      llegadas[j] <- length(x)
    }
    
    matriz_llegadas[[length(matriz_llegadas) + 1]] <- llegadas
    count <- count + 1
  }
  
  matriz <- do.call(rbind, matriz_llegadas)
  
  if (!is.null(matriz)) {
    matriz <- matriz[rowSums(matriz, na.rm = TRUE) != 0, , drop = FALSE]  # Eliminar filas con suma 0
  }
  
  varianzas <- c()
  medias <- c()
  
  
  for (i in 1:ncol(matriz)) {
    media <- mean(matriz[, i], na.rm = TRUE)
    varianza <- var(matriz[, i], na.rm = TRUE)
    medias <- c(medias, media)
    varianzas <- c(varianzas, varianza)
  }
  
  media2 <- median(medias, na.rm = TRUE)
  varianza2 <- median(varianzas, na.rm = TRUE)
  
  vmr <- varianza2/media2
  
  return(vmr)
}

vmr_marcos(g1,0.2,2)


## PARA REALIZAR LA TABLA

# Crear un data frame vacío para almacenar los resultados
resultados_vmr <- data.frame(τ = tau)

# Lista de datasets y nombres
datasets <- list(g1, g2, g3, g4, g5, g6)
nombres <- c("G1", "G2", "G3", "G4", "G5", "G6")
horas <- c(2, 2, 5, 1, 5, 2)  # Ajusta según corresponda

# Calcular VMR para cada dataset y tau
for (j in 1:length(datasets)) {
  vmr_values <- sapply(tau, function(t) calculate_vmr1(datasets[[j]], t, horas[j]))
  resultados_vmr[[nombres[j]]] <- vmr_values
}
warnings()

# Mostrar la tabla
print(resultados_vmr)

library(kableExtra)

# Tabla con formato mejorado
kable(round(resultados_vmr,3), caption = "Tabla de VMR para diferentes valores de τ") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


# Función para limpiar valores repetidos
limpiar_repetidos <- function(columna) {
  if (length(columna) == 0) return(columna)
  for (i in 2:length(columna)) {
    if (!is.na(columna[i]) && !is.na(columna[i-1]) && columna[i] == columna[i-1]) {
      columna[i] <- "-"
    }
  }
  return(columna)
}

# Aplicar la limpieza a cada columna
resultados_limpios <- round(resultados_vmr,3)
for (col in names(resultados_limpios)[-1]) {
  resultados_limpios[[col]] <- limpiar_repetidos(resultados_limpios[[col]])
}

# Mostrar tabla mejorada
kable(resultados_limpios, align = 'c') %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  column_spec(1, bold = TRUE, width = "2cm") %>%  # Columna τ
  column_spec(2:7, width = "2cm") %>%            # Columnas G1 a G6
  row_spec(0, bold = TRUE, color = "white", background = "#3498db")



