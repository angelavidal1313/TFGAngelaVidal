
# CÁLCULO DE LOS VALORES INICIALES X0 PARA LA INFERENCIA DE CADA SLOT

# SLOT G1

# En este caso, nuestra muestra son los datos del Anonymous bank.

g1 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_7_9h.xlsx",
                 sheet = 1, col_names = FALSE)
# Convertir cada columna a una lista, eliminando los NA
g1_list <- lapply(g1, function(col) as.numeric(na.omit(col)))

# Opcional: nombrar las muestras
names(g1_list) <- paste0("Muestra", seq_along(g1_list))


mom_emp_g1 <- calcular_descriptores_empiricos(g1_list)
mom_emp_g1

rho_emp_g1 <- mom_emp_g1$rho
mu1_emp_g1 <- mom_emp_g1$mu_1
mu2_emp_g1 <- mom_emp_g1$mu_2
mu3_emp_g1 <- mom_emp_g1$mu_3

par <- c(1,rho_emp_g1,mu1_emp_g1,mu2_emp_g1,mu3_emp_g1)

# Ahora, llevamos acabo la minimización de la eq 13 para hallar D0^ y D1^

# Añadiendo las restricciones de un MAP2

set.seed(123)
x1 <- -runif(1, 0.1, 10)      # x1 <= 0
x2 <- runif(1, 0, -x1)         # x2 >= 0 y x1 + x2 <= 0
x3 <- -runif(1, 0.1, 10)       # x3 <= 0
x4 <- runif(1, 0, -x3)         # x4 >= 0 y x3 + x4 <= 0
x0_random <- c(x1, x2, x3, x4)

ui <- rbind(
  c(0, 1, 0, 0),    # y ≥ 0
  c(0, 0, 0, 1),     # v ≥ 0
  c(-1, 0, 0, 0),     # x ≤ 0 → -x ≥ 0
  c(0, 0, -1, 0),     # u ≤ 0 → -u ≥ 0
  c(-1, -1, 0, 0),   # x + y ≤ 0 → -x - y ≥ 0
  c(0, 0, -1, -1)    # u + v ≤ 0 → -u - v ≥ 0
)

resultado_test <- constrOptim(theta=x0_random, 
                              f = function(x) myfun_ro1_canonica1(x, par),
                              ui=ui,
                              ci=c(0,0,0,0,0,0),
                              method = "Nelder-Mead")

x_rest <- resultado_test$par
x_rest

D0_est_rest <- matrix(c(x_rest[1], 0, x_rest[2], x_rest[3]), nrow = 2)
D1_est_rest <- matrix(c(-x_rest[1]-x_rest[2], x_rest[4], 0, -x_rest[3]-x_rest[4]), nrow = 2)
D0_est_rest
D1_est_rest

x0_g1 <- c(D0_est_rest[1,1],D0_est_rest[1,2],D0_est_rest[2,2],D1_est_rest[2,1])
x0_g1



# SLOT G2

g2 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_9_11h.xlsx",
                 sheet = 1, col_names = FALSE)
# Convertir cada columna a una lista, eliminando los NA
g2_list <- lapply(g2, function(col) as.numeric(na.omit(col)))

# Opcional: nombrar las muestras
names(g2_list) <- paste0("Muestra", seq_along(g2_list))


mom_emp_g2 <- calcular_descriptores_empiricos(g2_list)
mom_emp_g2

rho_emp_g2 <- mom_emp_g2$rho
mu1_emp_g2 <- mom_emp_g2$mu_1
mu2_emp_g2 <- mom_emp_g2$mu_2
mu3_emp_g2 <- mom_emp_g2$mu_3

par <- c(1,rho_emp_g2,mu1_emp_g2,mu2_emp_g2,mu3_emp_g2)

# Ahora, llevamos acabo la minimización de la eq 13 para hallar D0^ y D1^

# Añadiendo las restricciones de un MAP2

set.seed(123)
x1 <- -runif(1, 0.1, 10)      # x1 <= 0
x2 <- runif(1, 0, -x1)         # x2 >= 0 y x1 + x2 <= 0
x3 <- -runif(1, 0.1, 10)       # x3 <= 0
x4 <- runif(1, 0, -x3)         # x4 >= 0 y x3 + x4 <= 0
x0_random <- c(x1, x2, x3, x4)

ui <- rbind(
  c(0, 1, 0, 0),    # y ≥ 0
  c(0, 0, 0, 1),     # v ≥ 0
  c(-1, 0, 0, 0),     # x ≤ 0 → -x ≥ 0
  c(0, 0, -1, 0),     # u ≤ 0 → -u ≥ 0
  c(-1, -1, 0, 0),   # x + y ≤ 0 → -x - y ≥ 0
  c(0, 0, -1, -1)    # u + v ≤ 0 → -u - v ≥ 0
)

resultado_test <- constrOptim(theta=x0_random, 
                              f = function(x) myfun_ro1_canonica1(x, par),
                              ui=ui,
                              ci=c(0,0,0,0,0,0),
                              method = "Nelder-Mead")

x_rest <- resultado_test$par
x_rest

D0_est_rest <- matrix(c(x_rest[1], 0, x_rest[2], x_rest[3]), nrow = 2)
D1_est_rest <- matrix(c(-x_rest[1]-x_rest[2], x_rest[4], 0, -x_rest[3]-x_rest[4]), nrow = 2)
D0_est_rest
D1_est_rest

x0_g2 <- c(D0_est_rest[1,1],D0_est_rest[1,2],D0_est_rest[2,2],D1_est_rest[2,1])
x0_g2



# SLOT G3

g3 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_11_16h.xlsx",
                 sheet = 1, col_names = FALSE)
# Convertir cada columna a una lista, eliminando los NA
g3_list <- lapply(g3, function(col) as.numeric(na.omit(col)))

# Opcional: nombrar las muestras
names(g3_list) <- paste0("Muestra", seq_along(g3_list))


mom_emp_g3 <- calcular_descriptores_empiricos(g3_list)
mom_emp_g3

rho_emp_g3 <- mom_emp_g3$rho
mu1_emp_g3 <- mom_emp_g3$mu_1
mu2_emp_g3 <- mom_emp_g3$mu_2
mu3_emp_g3 <- mom_emp_g3$mu_3

par <- c(1,rho_emp_g3,mu1_emp_g3,mu2_emp_g3,mu3_emp_g3)

# Ahora, llevamos acabo la minimización de la eq 13 para hallar D0^ y D1^

# Añadiendo las restricciones de un MAP2

set.seed(123)
x1 <- -runif(1, 0.1, 10)      # x1 <= 0
x2 <- runif(1, 0, -x1)         # x2 >= 0 y x1 + x2 <= 0
x3 <- -runif(1, 0.1, 10)       # x3 <= 0
x4 <- runif(1, 0, -x3)         # x4 >= 0 y x3 + x4 <= 0
x0_random <- c(x1, x2, x3, x4)

ui <- rbind(
  c(0, 1, 0, 0),    # y ≥ 0
  c(0, 0, 0, 1),     # v ≥ 0
  c(-1, 0, 0, 0),     # x ≤ 0 → -x ≥ 0
  c(0, 0, -1, 0),     # u ≤ 0 → -u ≥ 0
  c(-1, -1, 0, 0),   # x + y ≤ 0 → -x - y ≥ 0
  c(0, 0, -1, -1)    # u + v ≤ 0 → -u - v ≥ 0
)

resultado_test <- constrOptim(theta=x0_random, 
                              f = function(x) myfun_ro1_canonica1(x, par),
                              ui=ui,
                              ci=c(0,0,0,0,0,0),
                              method = "Nelder-Mead")

x_rest <- resultado_test$par
x_rest

D0_est_rest <- matrix(c(x_rest[1], 0, x_rest[2], x_rest[3]), nrow = 2)
D1_est_rest <- matrix(c(-x_rest[1]-x_rest[2], x_rest[4], 0, -x_rest[3]-x_rest[4]), nrow = 2)
D0_est_rest
D1_est_rest

x0_g3 <- c(D0_est_rest[1,1],D0_est_rest[1,2],D0_est_rest[2,2],D1_est_rest[2,1])
x0_g3


# SLOT G4

g4 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_16_17h.xlsx",
                 sheet = 1, col_names = FALSE)
# Convertir cada columna a una lista, eliminando los NA
g4_list <- lapply(g4, function(col) as.numeric(na.omit(col)))

# Opcional: nombrar las muestras
names(g4_list) <- paste0("Muestra", seq_along(g4_list))


mom_emp_g4 <- calcular_descriptores_empiricos(g4_list)
mom_emp_g4

rho_emp_g4 <- mom_emp_g4$rho
mu1_emp_g4 <- mom_emp_g4$mu_1
mu2_emp_g4 <- mom_emp_g4$mu_2
mu3_emp_g4 <- mom_emp_g4$mu_3

par <- c(1,rho_emp_g4,mu1_emp_g4,mu2_emp_g4,mu3_emp_g4)

# Ahora, llevamos acabo la minimización de la eq 13 para hallar D0^ y D1^

# Añadiendo las restricciones de un MAP2

set.seed(123)
x1 <- -runif(1, 0.1, 10)      # x1 <= 0
x2 <- runif(1, 0, -x1)         # x2 >= 0 y x1 + x2 <= 0
x3 <- -runif(1, 0.1, 10)       # x3 <= 0
x4 <- runif(1, 0, -x3)         # x4 >= 0 y x3 + x4 <= 0
x0_random <- c(x1, x2, x3, x4)

ui <- rbind(
  c(0, 1, 0, 0),    # y ≥ 0
  c(0, 0, 0, 1),     # v ≥ 0
  c(-1, 0, 0, 0),     # x ≤ 0 → -x ≥ 0
  c(0, 0, -1, 0),     # u ≤ 0 → -u ≥ 0
  c(-1, -1, 0, 0),   # x + y ≤ 0 → -x - y ≥ 0
  c(0, 0, -1, -1)    # u + v ≤ 0 → -u - v ≥ 0
)

resultado_test <- constrOptim(theta=x0_random, 
                              f = function(x) myfun_ro1_canonica1(x, par),
                              ui=ui,
                              ci=c(0,0,0,0,0,0),
                              method = "Nelder-Mead")

x_rest <- resultado_test$par
x_rest

D0_est_rest <- matrix(c(x_rest[1], 0, x_rest[2], x_rest[3]), nrow = 2)
D1_est_rest <- matrix(c(-x_rest[1]-x_rest[2], x_rest[4], 0, -x_rest[3]-x_rest[4]), nrow = 2)
D0_est_rest
D1_est_rest

x0_g4 <- c(D0_est_rest[1,1],D0_est_rest[1,2],D0_est_rest[2,2],D1_est_rest[2,1])
x0_g4


# SLOT G5

g5 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_17_22h.xlsx",
                 sheet = 1, col_names = FALSE)
# Convertir cada columna a una lista, eliminando los NA
g5_list <- lapply(g5, function(col) as.numeric(na.omit(col)))

# Opcional: nombrar las muestras
names(g5_list) <- paste0("Muestra", seq_along(g5_list))


mom_emp_g5 <- calcular_descriptores_empiricos(g5_list)
mom_emp_g5

rho_emp_g5 <- mom_emp_g5$rho
mu1_emp_g5 <- mom_emp_g5$mu_1
mu2_emp_g5 <- mom_emp_g5$mu_2
mu3_emp_g5 <- mom_emp_g5$mu_3

par <- c(1,rho_emp_g5,mu1_emp_g5,mu2_emp_g5,mu3_emp_g5)

# Ahora, llevamos acabo la minimización de la eq 13 para hallar D0^ y D1^

# Añadiendo las restricciones de un MAP2

set.seed(123)
x1 <- -runif(1, 0.1, 10)      # x1 <= 0
x2 <- runif(1, 0, -x1)         # x2 >= 0 y x1 + x2 <= 0
x3 <- -runif(1, 0.1, 10)       # x3 <= 0
x4 <- runif(1, 0, -x3)         # x4 >= 0 y x3 + x4 <= 0
x0_random <- c(x1, x2, x3, x4)

ui <- rbind(
  c(0, 1, 0, 0),    # y ≥ 0
  c(0, 0, 0, 1),     # v ≥ 0
  c(-1, 0, 0, 0),     # x ≤ 0 → -x ≥ 0
  c(0, 0, -1, 0),     # u ≤ 0 → -u ≥ 0
  c(-1, -1, 0, 0),   # x + y ≤ 0 → -x - y ≥ 0
  c(0, 0, -1, -1)    # u + v ≤ 0 → -u - v ≥ 0
)

resultado_test <- constrOptim(theta=x0_random, 
                              f = function(x) myfun_ro1_canonica1(x, par),
                              ui=ui,
                              ci=c(0,0,0,0,0,0),
                              method = "Nelder-Mead")

x_rest <- resultado_test$par
x_rest

D0_est_rest <- matrix(c(x_rest[1], 0, x_rest[2], x_rest[3]), nrow = 2)
D1_est_rest <- matrix(c(-x_rest[1]-x_rest[2], x_rest[4], 0, -x_rest[3]-x_rest[4]), nrow = 2)
D0_est_rest
D1_est_rest

x0_g5 <- c(D0_est_rest[1,1],D0_est_rest[1,2],D0_est_rest[2,2],D1_est_rest[2,1])
x0_g5


# SLOT G6

g6 <- read_excel("/Users/angelavidalmonge/Desktop/TFG/Datos/tiempos_entre_llegadas_22_24h.xlsx",
                 sheet = 1, col_names = FALSE)
# Convertir cada columna a una lista, eliminando los NA
g6_list <- lapply(g6, function(col) as.numeric(na.omit(col)))

# Opcional: nombrar las muestras
names(g6_list) <- paste0("Muestra", seq_along(g6_list))


mom_emp_g6 <- calcular_descriptores_empiricos(g6_list)
mom_emp_g6

rho_emp_g6 <- mom_emp_g6$rho
mu1_emp_g6 <- mom_emp_g6$mu_1
mu2_emp_g6 <- mom_emp_g6$mu_2
mu3_emp_g6 <- mom_emp_g6$mu_3

par <- c(1,rho_emp_g6,mu1_emp_g6,mu2_emp_g6,mu3_emp_g6)

# Ahora, llevamos acabo la minimización de la eq 13 para hallar D0^ y D1^

# Añadiendo las restricciones de un MAP2

set.seed(123)
x1 <- -runif(1, 0.1, 10)      # x1 <= 0
x2 <- runif(1, 0, -x1)         # x2 >= 0 y x1 + x2 <= 0
x3 <- -runif(1, 0.1, 10)       # x3 <= 0
x4 <- runif(1, 0, -x3)         # x4 >= 0 y x3 + x4 <= 0
x0_random <- c(x1, x2, x3, x4)

ui <- rbind(
  c(0, 1, 0, 0),    # y ≥ 0
  c(0, 0, 0, 1),     # v ≥ 0
  c(-1, 0, 0, 0),     # x ≤ 0 → -x ≥ 0
  c(0, 0, -1, 0),     # u ≤ 0 → -u ≥ 0
  c(-1, -1, 0, 0),   # x + y ≤ 0 → -x - y ≥ 0
  c(0, 0, -1, -1)    # u + v ≤ 0 → -u - v ≥ 0
)

resultado_test <- constrOptim(theta=x0_random, 
                              f = function(x) myfun_ro1_canonica1(x, par),
                              ui=ui,
                              ci=c(0,0,0,0,0,0),
                              method = "Nelder-Mead")

x_rest <- resultado_test$par
x_rest

D0_est_rest <- matrix(c(x_rest[1], 0, x_rest[2], x_rest[3]), nrow = 2)
D1_est_rest <- matrix(c(-x_rest[1]-x_rest[2], x_rest[4], 0, -x_rest[3]-x_rest[4]), nrow = 2)
D0_est_rest
D1_est_rest

x0_g6 <- c(D0_est_rest[1,1],D0_est_rest[1,2],D0_est_rest[2,2],D1_est_rest[2,1])
x0_g6


x0_inferencia <- list(G1=x0_g1,G2=x0_g2,G3=x0_g3,G4=x0_g4,G5=x0_g5,G6=x0_g6)
