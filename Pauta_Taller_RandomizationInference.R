# Cargar librerías necesarias
library(dplyr)
library(ri2)
library(haven)
library(ggplot2)
set.seed(1207)





#### P1 ####
data <- read.csv("muestra_stem.csv")


#### ajustamos base de datos ####
data$Z <- data$SLEP
data$post <- ifelse(data$AGNO >= 2018,1,0)

#### creamos funcion de DD que retorna el valor p y el valor del coeficiente de interes ####

dd_pvalue_beta <- function(data) {
  reg <- tryCatch(lm(ASISTENCIA ~ post + Z + post * Z, data = data), error = function(e) NULL)
  
  # Verificar si el modelo se ajustó correctamente y tiene suficientes coeficientes
  if (is.null(reg) || length(coef(reg)) < 4) {
    return(c(p_value = NA, beta = NA))
  }
  
  # Extrae p_value y beta, devolviendo NA si no están disponibles
  p_value <- ifelse(!is.na(summary(reg)$coefficients[4, 4]), summary(reg)$coefficients[4, 4], NA)
  beta <- ifelse(!is.na(coef(reg)[4]), coef(reg)[4], NA)
  
  return(c(p_value = p_value, beta = beta))
}



df_iteracion = data.frame(N = integer(), p_value = numeric(), beta = numeric(),stringsAsFactors = FALSE)
MRUN_unicos = data.frame(unique(data$MRUN))

#### creamos looop ####



for (i in 0:(77)){
  tamaño_muestra = 150 + 50*i
  muestra_mrun = sample_n(MRUN_unicos, tamaño_muestra)
  
  data_filtrada <- data %>%
    filter(MRUN %in% muestra_mrun$unique.data.MRUN.)
  
  resultado = dd_pvalue_beta(data_filtrada)
  
  df_iteracion <- rbind(df_iteracion, data.frame(N = tamaño_muestra, p_value = resultado[1], beta = resultado[2]))
}



#### analizamos impacto slep sobre asistencia ####



# Crear el gráfico base con ggplot2 y añadir la línea de kernel
ggplot(df_iteracion, aes(x = N, y = p_value)) +
  geom_point() +                          # Puntos individuales
  geom_smooth(method = "loess", color = "blue") +  # Línea de suavización kernel
  labs(title = "Relación entre N y p_value",
       x = "N",
       y = "p_value") +
  theme_minimal()





# Crear el gráfico base con ggplot2 y añadir la línea de kernel
ggplot(df_iteracion, aes(x = N, y = beta)) +
  geom_point() +                          # Puntos individuales
  geom_smooth(method = "loess", color = "orange") +  # Línea de suavización kernel
  labs(title = "Relación entre N y Beta3 con línea de suavización",
       x = "N",
       y = "p_value") +
  theme_minimal()


### RESPUESTA ###

#Una vez corrido el código, se analizan ambos gráficos. 
#En primer lugar, los p-values presentan una pequeña convergencia alrededor de las 3000 observaciones, 
#donde comienzan estar más cercano al valor observado. Esto podría indicar que habría que quizás el modelo 
#no cuenta con las observaciones suficientes para analizar su robustez mediante el p-value teórico.

# Para el caso del coeficiente y la relación entre la cantidad de observaciones, 
#alrededor de 2000 observaciones ya hay propiedades asintóticas. Esto valida el resultado obtenido

# Una forma de aumentar el p-value sin aumentar la cantidad de observaciones es mediante la inclusión de 
#variables significativas y relevantes al modelo. 
#Esto permite disminuir los errores volviendo el modelo más preciso y por ende con un p-value menor.


#### P2 ####

## usamos diferencias de diferencias
dd_fun_asist <- function(data) {
  coef(lm(ASISTENCIA ~ post + Z + post*Z, data = data))[4]
}



# Configurar el diseño de aleatorización
declare <- declare_ra(N = nrow(data))
sims <- 10000


# Realizar el análisis de inferencia aleatoria
ri_out <- conduct_ri(
  test_function = dd_fun_asist,
  declaration = declare,
  data = data,
  sims = sims
)

summary(ri_out)
plot(ri_out)

### RESPUESTA ###

# Realizamos los ajustes al código y corremos test de permutación.
# Para el caso del clsuter el p-value de dos cola fue de 0, en el caso del test de permutación también es 0.
# Está similitud en los p-values se puede deber a que no hay gran variabilidad si se asigna en el tratamiento de forma grupal o individual
# Además, el número de clusters no es muy grande por lo que también no genera variabilidad.

# Bajo ambos casos, el análisis entrega robustez al modelo y se puede indicar 
# que SLEP tuvo un impacto significativo sobre la ASISTENCIA.

#### P4 ####

data_hcc = read.csv("hcc.csv")

data_hcc$Y = data_hcc$X8.Cir # cirrosis
data_hcc$Z = data_hcc$X3.Alc # alcohol

ate_obs <- with(data_hcc, mean(Y[Z == 1]) - mean(Y[Z == 0]))







complete_dec = declare_ra(N = nrow(data_hcc))
sims = 1000

ri_out <-
  conduct_ri(
    model_1 = Y ~ Z + X1.Gen,
    model_2 = Y ~ Z + X1.Gen + Z*X1.Gen,
    declaration = complete_dec,
    sharp_hypothesis = ate_obs,
    data = data_hcc, 
    sims = sims
  )
summary(ri_out)
plot(ri_out)


### RESPUESTA ###

# Utilizando el F-estadístico para analizar la heterogeneidad del efecto del tratamiento entre hombre y mujeres
# se obtiene un F-estadístico bastante bajo y un p-value bastante alto, por lo que no hay suficiencia estadística
# para rechazar la hipotesis nula (Que el tratamiento afecta a hombres y mujeres por igual) por lo tanto
# estadísticamente el consumo alcohol impacta de la misma manera a ambos sexos en la probabilidad
# de sufrir cirrosis


