# Cargar librerías necesarias
library(dplyr)
library(ri2)
library(haven)

set.seed(1207)

data <- read.csv("StudentPerformanceFactors.csv")
data$Male = ifelse(data$Gender == "Male", 1, 0)

data$Extracurricular_Activities = ifelse(data$Extracurricular_Activities == "Yes", 1, 0)
data$Internet_Access = ifelse(data$Internet_Access == "Yes", 1, 0)

reg = (lm(Exam_Score ~ Internet_Access + Male + Internet_Access*Male, data = data))
summary(reg)

# test de balance


# obtenemos valor f
data$Z = data$Internet_Access


balance_fun <- function(data) {
  summary(lm(Exam_Score ~ Z + Physical_Activity, data = data))$f[1]
}


sims = 1000

complete_dec <- declare_ra(N = nrow(data))
ri_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = complete_dec,
    data = data,
    sims = sims
  )

summary(ri_out)



data$Z = data$Internet_Access
data$Y = data$Exam_Score
ate_obs <- with(data, mean(Y[Z == 1]) - mean(Y[Z == 0]))

sims = 1000

ri_out <-
  conduct_ri(
    model_1 = Y ~ Z + Male,
    model_2 = Y ~ Z + Male + Z*Male,
    declaration = complete_dec,
    sharp_hypothesis = ate_obs,
    data = data, 
    sims = sims
  )
summary(ri_out)
plot(ri_out)


