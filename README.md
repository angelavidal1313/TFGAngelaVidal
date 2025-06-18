# TFG Ángela Vidal Monge

A continuación se describe el contenido de cada uno de los códigos realizados en R empleados en el Trabajo de Fin de Grado: Modelo de sistemas de colas basado en procesos MAP. Para facilitar su manejo, los códigos han sido organizados por capítulos:

## Capítulo 2
- Figure1.R : Implementación de la Figura 1.
- Modelos.R : Esboza las Figuras 2-8, que representan las gráficas de las diferentes distribuciones de servicio e impaciencia tratadas en la sección 2.4.


## Capítulo 3
- Figure9.R :  Implementación de la Figura 1.
- Figure10y11.R :  Implementación de las Figura 10 y 11.
- Figure12.R : Implementación de la Figura 12.
- Table1y2 : Estudio para las Tablas 1 y 2.

### 3.2. Simulador
- SimuladorMAP2.R : Contiene las funciones Mrp2 y simulate_map2 para el simulador del proceso MAP_2.
- EjSimuladorMAP2.R : Implementación del ejemplo dado por las matrices de la ecuación (3.20).

### 3.3. Inferencia
- FuncionesInferencia.R : Contiene las funciones empleadas en el proceso de inferencia: delta, delta_beta, inference_new...
- EjemploInferencia.R : Implementación del ejemplo dado por las matrices de la ecuación (3.22), incluyendo las Figuras 16 y 17.
- Análisis G2.R y Análisis G3.R : Estudio de la inferencia para los intervalos G2 y G3 expuesto en la subsección 3.3.2., incluyendo las gráficas para la Figura 18 y las Tablas 9 y 10.
- x0Calculados : Ejecuta el primer método de inferencia, basado en la ecuación (3.21), para calcular los puntos iniciales x0 utilizados en el proceso de inferencia definitivo de cada intervalo G.
- InferenciaMAP2.R : Cálculo de las matrices D0 y D1 para cada intervalo G mediante el método de inferencia basado en la ecuación (3.23).


## Capítulo 4

### 4.1. Tiempos Servicio
- TiemposServicio.R : Desarrollo del análisis completo de los tiempos de servicio, incluyendo la Figura 19 y las Tablas 11 y 12.
- LogHistogramas.R : Implementación de la representación de los ajustes en los histogramas de los logaritmos, para las Figuras 20 y 21.
- CrossValidationTS.R : Implementación de la técnica de Validacion Cruzada, incluyendo la Tabla 13.
- ParamServicio.R : Guarda los parámetros para los tiempos de servicio de cada intervalo G que se utilizarán en el simulador de la cola.

### 4.2. Tiempos Impaciencia
- TiemposImpaciencia.R : Desarrollo del análisis completo de los tiempos de impaciencia, incluyendo la Figura 22.
- CrossValidationTI.R : Implementación de la técnica de Validacion Cruzada, incluyendo la Tabla 14.
- ParamImpaciencia.R : Guarda los parámetros para los tiempos de servicio de cada intervalo G que se utilizarán en el simulador de la cola.

### 4.3. Simulador Cola
- SimuladorCola.R : Incluye la función simulate_MAP_PH3_qeue, así como las funciones necesarias para ejecutarla.

### 4.4. Rendimiento Cola
- TasasAbandono.R : Estudio de las tasas de abandono para la Tabla 17.
- Figure24y25.R : Implementación de las Figura 24 y 25.
- Tabla18.R : Estudio para la Tabla 18. 
