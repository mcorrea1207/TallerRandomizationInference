# Cargar librerías necesarias
library(dplyr)
library(ri2)

set.seed(1207)

# Definir los estados y el número de personas por estado
estados <- c("Estado1", "Estado2", "Estado3", "Estado4", "Estado5", "Estado6", "Estado7", "Estado8")
num_personas_por_estado <- 100  # Puedes ajustar este número

# Crear un DataFrame vacío
df <- data.frame()

# Generar observaciones para cada estado
for (estado in estados) {
  # Determinar si el estado recibe tratamiento
  recibe_tratamiento <- ifelse(estado %in% c("Estado1", "Estado2", "Estado3", "Estado4", "Estado5"), TRUE, FALSE)
  
  # Generar probabilidades únicas
  if (recibe_tratamiento) {
    # Generar valores únicos para tratamiento
    Probabilidad_Paro <- rnorm(num_personas_por_estado, mean = 0.3, sd = 0.3)
  } else {
    # Generar valores únicos para sin tratamiento
    Probabilidad_Paro <- rnorm(num_personas_por_estado, mean = 0.35, sd = 0.25)
  }
  
  # Crear el DataFrame de personas
  personas <- data.frame(
    ID_Persona = seq(1, num_personas_por_estado),  # Asegurarse de que los IDs sean únicos
    Estado = estado,
    Tratamiento = ifelse(recibe_tratamiento, 1, 0),
    Probabilidad_Paro = Probabilidad_Paro
  )
  
  # Agregar las personas al DataFrame general
  df <- rbind(df, personas)
}


df$Probabilidad_Paro <- pmin(pmax(df$Probabilidad_Paro, 0), 1)


# Mostrar el DataFrame
print(df)

reg <- lm(Probabilidad_Paro ~ Tratamiento, data = df)
summary(reg)


# Configurar el diseño de aleatorización
clustered_dec <- declare_ra(clusters = df$Estado)

df$Z <- df$Tratamiento
sims <- 100000

# Realizar el análisis de inferencia aleatoria
ri_out <- conduct_ri(
  Probabilidad_Paro ~ Z,
  declaration = clustered_dec,
  data = df,
  sims = sims
)

# Resumen y gráfico de los resultados
summary(ri_out)
plot(ri_out)



# test de balance


# obtenemos valor f
reg$f[1]

balance_fun <- function(data) {
  summary(lm(Probabilidad_Paro ~ Z, data = data))$f[1]
}

complete_dec <- declare_ra(N = nrow(df))
ri_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = complete_dec,
    data = df,
    sims = sims
  )

summary(ri_out)
plot(ri_out)
