# Librerías utilizadas
library(readxl)
library(ggplot2)

# FIGURE 1


# Cargar los archivos 

df_7_9 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_7_9h.xlsx", sheet = 1,col_names = FALSE)
df_9_11 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_9_11h.xlsx", sheet = 1,col_names = FALSE)
df_11_16 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_11_16h.xlsx", sheet = 1,col_names = FALSE)
df_16_17 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_16_17h.xlsx", sheet = 1,col_names = FALSE)
df_17_22 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_17_22h.xlsx", sheet = 1,col_names = FALSE)
df_22_24 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_22_24h.xlsx", sheet = 1,col_names = FALSE)


# Creamos una lista para cada intervalo con sus datos, inicio y fin.

g1<-list(df=df_7_9,inicio=7,fin=9)
g2<-list(df=df_9_11,inicio=9,fin=11)
g3<-list(df=df_11_16,inicio=11,fin=16)
g4<-list(df=df_16_17,inicio=16,fin=17)
g5<-list(df=df_17_22,inicio=17,fin=22)
g6<-list(df=df_22_24,inicio=22,fin=24)


# Función para separar los datos por día de la semana

separar_por_dia <- function(df) {
  num_columnas <- ncol(df)
  
  dias_lista <- list("lunes" = list(), "martes" = list(), "miercoles" = list(),
                     "jueves" = list(), "domingo" = list())
  
  for (i in 1:num_columnas) {
    dia_semana <- (i - 1) %% 5  
    nombres_dias <- names(dias_lista)
    
    dias_lista[[nombres_dias[dia_semana + 1]]] <- append(dias_lista[[nombres_dias[dia_semana + 1]]], list(df[[i]]))
  }
  
  return(lapply(dias_lista, function(x) as.data.frame(do.call(cbind, x))))
}


# Separamos los datos de los archivos por dias

datos_7_9 <- separar_por_dia(df_7_9)
datos_9_11 <- separar_por_dia(df_9_11)
datos_11_16 <- separar_por_dia(df_11_16)
datos_16_17 <- separar_por_dia(df_16_17)
datos_17_22 <- separar_por_dia(df_17_22)
datos_22_24 <- separar_por_dia(df_22_24)


# Función para calcular el número medio de llamadas por intervalo horario

calcular_conteos <- function(df_dia, inicio, fin) {
  conteo_intervalo <- list()
  
  for (i in 1:ncol(df_dia)) {
    tiempos <- df_dia[[i]]  # Extraemos los tiempos entre llegadas
    tiempos_acumulados <- cumsum(tiempos) + inicio  # Convertimos a horas reales
    
    horas_en_intervalo <- inicio:(fin - 1)
    
    llamadas_intervalo <- sapply(horas_en_intervalo, function(h) {
      sum(tiempos_acumulados >= h & tiempos_acumulados < (h+1), na.rm = TRUE)
    })
    
    conteo_intervalo <- append(conteo_intervalo, list(llamadas_intervalo))
  }
  
  conteo_df <- as.data.frame(do.call(rbind, conteo_intervalo))
  
  colnames(conteo_df) <- paste0(horas_en_intervalo, "-", horas_en_intervalo + 1, "h")
  
  return(colMeans(conteo_df, na.rm = TRUE))
}



# Calcular medias para cada día y cada intervalo
dias <- c("lunes", "martes", "miercoles", "jueves", "domingo")
medias <- data.frame(Dia = character(), Hora = numeric(), Media_Llamadas = numeric())

for (dia in dias) {
  media_7_9 <- calcular_conteos(datos_7_9[[dia]], 7, 9)
  media_9_11 <- calcular_conteos(datos_9_11[[dia]], 9, 11)
  media_11_16 <- calcular_conteos(datos_11_16[[dia]], 11, 16)
  media_16_17 <- calcular_conteos(datos_16_17[[dia]], 16, 17)
  media_17_22 <- calcular_conteos(datos_17_22[[dia]], 17, 22)
  media_22_24 <- calcular_conteos(datos_22_24[[dia]], 22, 24)
  
  medias <- rbind(medias, 
                  data.frame(Dia = dia, 
                             Hora = c(seq(7, 8, by = 1), seq(9, 10, by = 1), seq(11, 15, by = 1), 
                                      seq(16, 16, by = 1), seq(17, 21, by = 1), seq(22, 23, by = 1)), 
                             Media_Llamadas = c(media_7_9, media_9_11, media_11_16, media_16_17, 
                                                media_17_22, media_22_24)))
}

medias <- rbind(medias, 
                data.frame(Dia = rep(dias, each = 1), Hora = rep(24, length(dias)), Media_Llamadas = rep(0, length(dias))))


medias$Dia <- factor(medias$Dia, levels = c("lunes", "martes", "miercoles", "jueves", "domingo"),
                     labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Domingo"))


ggplot(medias, aes(x = Hora, y = Media_Llamadas, color = Dia, group = Dia)) +
  geom_line(size = 0.5) +  # Línea más fina sin puntos
  labs( x = "Hora",
       y = "Nº de llamadas / hora") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(7, 24, by = 1)) +  
  scale_color_manual(values = c(
    "Lunes" = "blue",
    "Martes" = "green",
    "Miércoles" = "orange",
    "Jueves" = "purple",
    "Domingo" = "red"
  )) + 
  scale_y_continuous(breaks = seq(0, max(medias$Media_Llamadas), by = 20))
