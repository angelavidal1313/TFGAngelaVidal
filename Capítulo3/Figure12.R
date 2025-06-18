# EVOLUCIÓN MAP

library(ggplot2)
library(dplyr)

# Fijamos semilla para N(t) y procesos aleatorios
set.seed(23411)

time_points <- 0:10
states <- c("Estado 0", "Estado 1")
state_values <- c(-0.5, -1.5)

# 1. Generamos N(t) fijo (independiente de J(t))
lambda_base <- 0.5  # Tasa base para generar N(t)
N_t_fijo <- cumsum(c(0, rpois(length(time_points)-1, lambda_base)))

# 2. Cambios de estado
cambios_manuales <- c(1, 2, 4, 6, 7, 8, 9)  # Cambios en t=1,2,4,7,9

# 3. Simulamos J(t)
current_state <- 1  # Estado inicial (1 = Estado 0)
J_t_numeric <- rep(state_values[1], length(time_points))

for (i in 2:length(time_points)) {
  if (time_points[i] %in% cambios_manuales) {
    # Cambio forzado en tiempos manuales
    current_state <- ifelse(current_state == 1, 2, 1)
  } else if (N_t_fijo[i] > N_t_fijo[i-1]) {
    current_state <- current_state
  }
  
  J_t_numeric[i] <- state_values[current_state]
}

time_points_continuous <- seq(0, 10, by = 0.01)
J_t_continuous <- approx(time_points, J_t_numeric, 
                         xout = time_points_continuous, 
                         method = "constant")$y
N_t_continuous <- approx(time_points, N_t_fijo, 
                         xout = time_points_continuous, 
                         method = "constant")$y

# Data frame para gráfico
df <- data.frame(
  Time = time_points_continuous,
  J_t = J_t_continuous,
  N_t = N_t_continuous
)

# Puntos de evento para marcar cambios
event_points <- data.frame(
  Time = time_points,
  J_t = J_t_numeric,
  N_t = N_t_fijo
)

# Graficar
ggplot(df, aes(x = Time)) +
  geom_line(aes(y = N_t, color = "N(t)"), size = 1, linetype = "dashed") +
  geom_step(aes(y = J_t, color = "J(t)"), size = 1) +
  geom_point(data = event_points, aes(y = N_t, color = "N(t)"), size = 2) +
  geom_point(data = event_points, aes(y = J_t, color = "J(t)"), size = 2) +
  scale_color_manual(
    name = "Procesos",
    values = c("N(t)" = "blue", "J(t)" = "red"),
    labels = c(expression(J(t)), expression(N(t)))
  ) +
  labs(x = "Tiempo (t)", y = expression(N(t))) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.title.y.right = element_text(color = "red", vjust = 0.5)
  ) +
  scale_y_continuous(
    limits = c(-2, max(N_t_fijo)),
    sec.axis = sec_axis(
      trans = ~ .,
      name = expression(J(t)),
      breaks = state_values,
      labels = states
    )
  ) +
  scale_x_continuous(breaks = 0:10) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")