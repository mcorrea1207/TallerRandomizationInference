# Cargar librerías necesarias
library(dplyr)
library(ri2)
library(haven)



data <- read.csv("muestra_stem.csv")



set.seed(1207)

#### analizamos impacto slep sobre asistencia ####

data$Z <- data$SLEP
data$post <- ifelse(data$AGNO >= 2018,1,0)


## usamos diferencias de diferencias
reg <- lm(ASISTENCIA ~ post + SLEP + post*SLEP, data = data)
summary(reg)

dd_fun_asist <- function(data) {
  coef(lm(ASISTENCIA ~ post + Z + post*Z, data = data))[4]
}



# Configurar el diseño de aleatorización
clustered_dec <- declare_ra(clusters = data$RBD)


sims <- 10000


# Realizar el análisis de inferencia aleatoria
ri_out <- conduct_ri(
  test_function = dd_fun_asist,
  declaration = clustered_dec,
  data = data,
  sims = sims
)

summary(ri_out)
plot(ri_out)

# Configurar el diseño de aleatorización
clustered_dec <- declare_ra(clusters = data$RBD)



#### analizamos impacto slep sobre notas ####


# transformamos a punto para poder trabajar

data$PROM_GRAL <- gsub(",", ".", data$PROM_GRAL)
data$PROM_GRAL <- as.numeric(data$PROM_GRAL)


# quitamos los NA

data_clean <- data[!is.na(data$PROM_GRAL),]

# corremos la regresión


reg <- lm(PROM_GRAL ~ post + Z + post*Z + ASISTENCIA, data = data_clean)
summary(reg)

# clusterizamos por colegio
clustered_dec <- declare_ra(clusters = data_clean$RBD)

dd_fun_prom <- function(data) {
  coef(lm(PROM_GRAL ~ post + Z + post*Z + ASISTENCIA, data = data))[5]
}


# Realizar el análisis de inferencia aleatoria
ri_out <- conduct_ri(
  test_function = dd_fun_prom,
  declaration = clustered_dec,
  data = data_clean,
  sims = sims
)

# Resumen y gráfico de los resultados
summary(ri_out)
plot(ri_out)



# test de balance


# obtenemos valor f

reg = lm(ASISTENCIA ~ post + Z + post*Z, data = data)
summary(reg)$f[1]

summary(reg)

balance_fun <- function(data) {
  summary(lm(ASISTENCIA ~ post + Z + post*Z, data = data))$f[1]
}

complete_dec <- declare_ra(N = nrow(data))
ri_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = complete_dec,
    data = data,
    sims = sims
  )

summary(ri_out)
plot(ri_out)

#### analisis ambos casos en conjunto







sims <- 10000  # cantidad simulaciones

# análisis para asistencia
ri_out_asist <- conduct_ri(
  test_function = dd_fun_asist,
  declaration = clustered_dec,
  data = data_clean,
  sims = sims
)

# análisis para notas
ri_out_prom <- conduct_ri(
  test_function = dd_fun_prom,
  declaration = clustered_dec,
  data = data_clean,
  sims = sims
)

# Extraer los resultados de las simulaciones
ri_results_asist <- ri_out_asist[["sims_df"]][["est_sim"]]
ri_results_prom <- ri_out_prom[["sims_df"]][["est_sim"]]

# Combinar los resultados para el gráfico
ri_results <- data.frame(
  coef_asist = ri_results_asist,
  coef_prom = ri_results_prom
)

# Crear el gráfico de dispersión
ggplot(ri_results, aes(x = coef_asist, y = coef_prom)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_point(aes(x = summary(lm(ASISTENCIA ~ post + Z + post*Z, data = data_clean))$coefficients[4, 1], 
                 y = summary(lm(PROM_GRAL ~ post + Z + post*Z, data = data_clean))$coefficients[4, 1]), 
             color = "red", shape = 4, size = 3) +  # Marca el valor original con una cruz
  labs(
    title = "Distribución de coeficientes de asistencia y notas Cluster Randomization",
    x = "Coeficiente de asistencia",
    y = "Coeficiente de notas"
  ) +
  theme_minimal()






#### analisis cantidad de colegios tratados ####


# Filtrar observaciones post 2018
data_post2018 <- data %>% filter(AGNO > 2018)

# Contar colegios con SLEP aplicado al menos a un estudiante
colegios_con_slep <- data_post2018 %>%
  group_by(RBD) %>%
  summarize(SLEP_aplicado = any(SLEP == 1), todos_SLEP = all(SLEP == 1))

# Número de colegios con SLEP aplicado al menos a uno de los estudiantes
n_colegios_con_slep <- colegios_con_slep %>% filter(SLEP_aplicado) %>% nrow()

# Colegios donde no se les aplicó SLEP a todos
colegios_no_todos_slep <- colegios_con_slep %>% filter(SLEP_aplicado & !todos_SLEP)

# Resultados
n_colegios_con_slep  # Cantidad de colegios con SLEP aplicado a al menos un estudiante
colegios_no_todos_slep  # Lista de colegios donde no se les aplicó SLEP a todos

# Contar colegios donde no se aplicó SLEP a ningún estudiante
n_colegios_sin_slep <- colegios_con_slep %>% filter(!SLEP_aplicado) %>% nrow()

# Resultado
n_colegios_sin_slep  # Cantidad de colegios sin SLEP aplicado a ningún estudiante


# Contar el número total de estudiantes únicos en el data frame original
total_alumnos <- data %>% summarize(total_estudiantes = n_distinct(MRUN))

# Resultado
total_alumnos$total_estudiantes  # Cantidad total de estudiantes

