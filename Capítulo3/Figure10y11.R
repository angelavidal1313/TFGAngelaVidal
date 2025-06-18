# Librerías utilizadas
library(readxl)
library(ggplot2)

# FIGURE 3

# Un scatterplot (o gráfico de dispersión) es una representación gráfica que muestra 
# la relación entre dos variables numéricas. Consiste en un conjunto de puntos donde 
# cada punto representa un par de valores (x,y), correspondientes a las dos variables. 
# Este tipo de gráfico es muy útil para visualizar patrones, tendencias o correlaciones entre las variables.


# Cargar los archivos 

df_7_9 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_7_9h.xlsx", sheet = 1,col_names = FALSE)
df_9_11 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_9_11h.xlsx", sheet = 1,col_names = FALSE)
df_11_16 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_11_16h.xlsx", sheet = 1,col_names = FALSE)
df_16_17 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_16_17h.xlsx", sheet = 1,col_names = FALSE)
df_17_22 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_17_22h.xlsx", sheet = 1,col_names = FALSE)
df_22_24 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_22_24h.xlsx", sheet = 1,col_names = FALSE)

# Paso 1: Convertimos los datos a vectores numéricos y eliminamos los NA

g1 <- na.omit(as.numeric(unlist(df_7_9)))
g2 <- na.omit(as.numeric(unlist(df_9_11)))
g3 <- na.omit(as.numeric(unlist(df_11_16)))
g4 <- na.omit(as.numeric(unlist(df_16_17)))
g5 <- na.omit(as.numeric(unlist(df_17_22)))
g6 <- na.omit(as.numeric(unlist(df_22_24)))


## Scatterplot para G3 antes y despues de las 14/15h


# Convertir los datos a una matriz
g3 <- as.matrix(df_11_16)

tiempos_llegadas_por_dia <- list()

for (col in 1:ncol(g3)) {
  # Obtener los tiempos entre llegadas para el día actual
  tiempos_entre_llegadas <- na.omit(g3[, col])  # Eliminar NA
  
  tiempos_llegadas <- cumsum(tiempos_entre_llegadas) + 11

  tiempos_llegadas_por_dia[[col]] <- tiempos_llegadas
}

tiempos_llegadas_por_dia[1:5]

# Inicializar vectores para almacenar el número de llegadas antes y después de 14h y 15h
llegadas_antes_14h <- numeric(ncol(g3))
llegadas_despues_14h <- numeric(ncol(g3))
llegadas_antes_15h <- numeric(ncol(g3))
llegadas_despues_15h <- numeric(ncol(g3))

# Calcular el número de llegadas antes y después de 14h y 15h para cada día
for (col in 1:ncol(g3)) {
  tiempos_llegadas <- tiempos_llegadas_por_dia[[col]]
  
  llegadas_antes_14h[col] <- sum(tiempos_llegadas <= 14)
  llegadas_despues_14h[col] <- sum(tiempos_llegadas > 14)
  
  llegadas_antes_15h[col] <- sum(tiempos_llegadas <= 15)
  llegadas_despues_15h[col] <- sum(tiempos_llegadas > 15)
}

# Crear un data frame para los scatterplots
datos_scatter <- data.frame(
  Dia = 1:ncol(g3),
  Antes_14h = llegadas_antes_14h,
  Despues_14h = llegadas_despues_14h,
  Antes_15h = llegadas_antes_15h,
  Despues_15h = llegadas_despues_15h
)

# Configurar la ventana gráfica para 1 fila y 2 columnas (2 paneles)
par(mfrow = c(1, 2))

# Scatterplot para 14h
plot(datos_scatter$Antes_14h, datos_scatter$Despues_14h, 
     xlab = "Nº de llegadas 11-14h", ylab = "Nº de llegadas 14-16h",
     main = "Scatterplot: 14h", pch = 16, col = "blue")

# Scatterplot para 15h
plot(datos_scatter$Antes_15h, datos_scatter$Despues_15h, 
     xlab = "Nº de llegadas 11-15h", ylab = "Nº de llegadas 11-15h",
     main = "Scatterplot: 15h", pch = 16, col = "red")


# Correlación entre llegadas antes y después de las 14h
cor_14h <- cor(datos_scatter$Antes_14h, datos_scatter$Despues_14h)
cat("Coeficiente de correlación para 14h:", cor_14h, "\n")

# Correlación entre llegadas antes y después de las 15h
cor_15h <- cor(datos_scatter$Antes_15h, datos_scatter$Despues_15h)
cat("Coeficiente de correlación para 15h:", cor_15h, "\n")




## Scatterplor para G5 antes y despues de las 20/21h

g5 <- as.matrix(df_17_22)

tiempos_llegadas_por_dia <- list()

for (col in 1:ncol(g5)) {
  # Obtener los tiempos entre llegadas para el día actual
  tiempos_entre_llegadas <- na.omit(g5[, col])  # Eliminar NA
  
  # Calcular los tiempos acumulativos de llegada, comenzando desde 11:00 horas
  tiempos_llegadas <- cumsum(tiempos_entre_llegadas) + 17
  
  # Almacenar los tiempos acumulativos en la lista
  tiempos_llegadas_por_dia[[col]] <- tiempos_llegadas
}

tiempos_llegadas_por_dia[1:5]

# Inicializar vectores para almacenar el número de llegadas antes y después de 14h y 15h
llegadas_antes_20h <- numeric(ncol(g5))
llegadas_despues_20h <- numeric(ncol(g5))
llegadas_antes_21h <- numeric(ncol(g5))
llegadas_despues_21h <- numeric(ncol(g5))

# Calcular el número de llegadas antes y después de 14h y 15h para cada día
for (col in 1:ncol(g5)) {
  tiempos_llegadas <- tiempos_llegadas_por_dia[[col]]
  
  llegadas_antes_20h[col] <- sum(tiempos_llegadas <= 20)
  llegadas_despues_20h[col] <- sum(tiempos_llegadas > 20)
  
  llegadas_antes_21h[col] <- sum(tiempos_llegadas <= 21)
  llegadas_despues_21h[col] <- sum(tiempos_llegadas > 21)
}

# Crear un data frame para los scatterplots
datos_scatter <- data.frame(
  Dia = 1:ncol(g5),
  Antes_20h = llegadas_antes_20h,
  Despues_20h = llegadas_despues_20h,
  Antes_21h = llegadas_antes_21h,
  Despues_21h = llegadas_despues_21h
)

# Configurar la ventana gráfica para 1 fila y 2 columnas (2 paneles)
par(mfrow = c(1, 2))

# Scatterplot para 20h
plot(datos_scatter$Antes_20h, datos_scatter$Despues_20h, 
     xlab = "Nº de llegadas 17-20h", ylab = "Nº de llegadas 20-21h",
     main = "Scatterplot: 20h", pch = 16, col = "blue")

# Scatterplot para 21h
plot(datos_scatter$Antes_21h, datos_scatter$Despues_21h, 
     xlab = "Nº de llegadas 17-21h", ylab = "Nº de llegadas 21-22h",
     main = "Scatterplot: 21h", pch = 16, col = "red")


# Correlación entre llegadas antes y después de las 20h
cor_20h <- cor(datos_scatter$Antes_20h, datos_scatter$Despues_20h)
cat("Coeficiente de correlación para 14h:", cor_20h, "\n")

# Correlación entre llegadas antes y después de las 21h
cor_21h <- cor(datos_scatter$Antes_21h, datos_scatter$Despues_21h)
cat("Coeficiente de correlación para 15h:", cor_21h, "\n")
