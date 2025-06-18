# Librerías utilizadas
library(readxl)
library(ggplot2)

# GRÁFICOS QQ PLOT PARA LA EXPONENCIAL


# Cargar los archivos 

df_7_9 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_7_9h.xlsx", sheet = 1,col_names = FALSE)
df_9_11 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_9_11h.xlsx", sheet = 1,col_names = FALSE)
df_11_16 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_11_16h.xlsx", sheet = 1,col_names = FALSE)
df_16_17 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_16_17h.xlsx", sheet = 1,col_names = FALSE)
df_17_22 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_17_22h.xlsx", sheet = 1,col_names = FALSE)
df_22_24 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_22_24h.xlsx", sheet = 1,col_names = FALSE)


# Paso 1: Convertimos los datos a vectores numéricos y eliminamos los NA

g1 <- na.omit(as.numeric(unlist(df_7_9$...41)))
g2 <- na.omit(as.numeric(unlist(df_9_11$...1)))
g3 <- na.omit(as.numeric(unlist(df_11_16$...30))) #30
g4 <- na.omit(as.numeric(unlist(df_16_17$...33))) #3,42,38,33
g5 <- na.omit(as.numeric(unlist(df_17_22$...40)))
g6 <- na.omit(as.numeric(unlist(df_22_24$...40)))

# Paso 2: Estimamos la tasa lambda de la exponencial
# Como la media de una exponencial (mu) es 1/lambda, despejando tenemos que
# lambda = 1/mu
# En este caso mu es la media muestral de los datos

lambda1<-1/mean(g1)
lambda2<-1/mean(g2)
lambda3<-1/mean(g3)
lambda4<-1/mean(g4)
lambda5<-1/mean(g5)
lambda6<-1/mean(g6)



# Paso 3: Hacer el QQplot, calculando los cuantiles teoricos en cada caso con cada lambda

par(mfrow=c(2,3))


crear_qqplot <- function(datos, lambda, titulo) {
  cuantiles_teoricos <- qexp(ppoints(length(datos)), rate = lambda)
  qqplot(cuantiles_teoricos, datos, main = titulo,
         xlab = "Cuantiles Teóricos", ylab = "Cuantiles Empíricos", col="blue")
  abline(0, 1, col = "red",lty=2)  # Línea de referencia
}


crear_qqplot(g1, lambda1, "G1 (7-9h)")
crear_qqplot(g2, lambda2, "G2 (9-11h)")
crear_qqplot(g3, lambda3, "G3 (11-16h)")
crear_qqplot(g4, lambda4, "G4 (16-17h)")
crear_qqplot(g5, lambda5, "G5 (17-22h)")
crear_qqplot(g6, lambda6, "G6 (22-24h)")
