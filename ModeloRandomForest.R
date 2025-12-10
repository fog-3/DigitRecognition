# --------- Entrenamiento usando RandomForest para DigitRecognition ------------

set.seed(10)
library(randomForest)
library(caret)
library(tictoc)
library(ggplot2)
library(tidyr)

# -----------------------------------------------
# Dataframe para imprimir curva de aprendizaje
# -----------------------------------------------

resultados_curva <- data.frame(
  tamano_Muestra = integer(),
  tiempo_Segundos = numeric(),
  accuracy = numeric()
)

tamanos_muestra <- c(1000, 2000, 3000, 4000, 5000)

# -----------------------------------------------
# División 80-20 entre train y test
# -----------------------------------------------
ind <- sample(1:nrow(train_reducido_df), 0.8 * nrow(train_reducido_df))
dtrain <- train_reducido_df[ind, ]
dtest  <- train_reducido_df[-ind, ]

# -----------------------------------------------
# Submuestra de 1000 para realizar tuning del Random Forest
# -----------------------------------------------
subsample_index <- sample(1:nrow(dtrain), 1000)
dtrain_subsample <- dtrain[subsample_index, ]
dtrain_subsample <- na.omit(dtrain_subsample)

# -----------------------------------------------
# TUNING: Búsqueda del mejor número de árboles
# (grid centrada alrededor de 100 como pediste)
# -----------------------------------------------

ntree_grid <- c(50, 100, 200, 300, 400, 500)
oob_errors <- c()

cat("Iniciando la búsqueda del mejor número de árboles...\n")

for (nt in ntree_grid) {
  cat("Probando ntree =", nt, "...\n")
  rf_temp <- randomForest(label ~ ., data = dtrain_subsample, ntree = nt)
  oob_errors <- c(oob_errors, rf_temp$err.rate[nt, "OOB"])
}

tuning_results <- data.frame(ntree = ntree_grid, OOB_Error = oob_errors)
print(tuning_results)

best_ntree <- tuning_results$ntree[which.min(tuning_results$OOB_Error)]

cat("\n--- Mejor número de árboles (ntree) ---\n")
cat("Mejor ntree =", best_ntree, "\n\n")

# -----------------------------------------------
# Entrenamiento con diferentes tamaños de muestra
# -----------------------------------------------

for (i in 1:length(tamanos_muestra)) {
  
  subsample_index <- sample(1:nrow(dtrain), tamanos_muestra[i])
  dtrain_subsample_model <- dtrain[subsample_index, ]
  dtrain_subsample_model <- na.omit(dtrain_subsample_model)
  
  tic("--------Entrenamiento del modelo Random Forest--------")
  rf <- randomForest(label ~ ., data = dtrain_subsample_model, ntree = best_ntree, proximity = TRUE)
  tiempo_entrenamiento <- toc()
  tiempo_en_segundos <- tiempo_entrenamiento$toc - tiempo_entrenamiento$tic
  
  # Predicciones
  pred <- predict(rf, newdata = dtest)
  matrizconfusion <- table(pred, dtest$label)
  accuracy <- sum(diag(matrizconfusion)) / sum(matrizconfusion)
  
  cat("Accuracy del modelo Random Forest:", accuracy * 100, "%\n")
  cat("Tiempo empleado:", tiempo_en_segundos, "s\n")
  
  resultados_curva[nrow(resultados_curva) + 1, ] <- c(
    nrow(dtrain_subsample_model),
    tiempo_en_segundos,
    accuracy
  )
}

print(resultados_curva)

# -----------------------------------------------
# Gráficas
# -----------------------------------------------
resultados_long <- resultados_curva %>%
  pivot_longer(
    cols = c("tiempo_Segundos", "accuracy"),
    names_to = "Parametros",
    values_to = "Valor"
  )

ggplot(resultados_long, aes(x = tamano_Muestra, y = Valor, color = Parametros)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Curva de Aprendizaje para el modelo Random Forest",
    x = "Tamaño del Conjunto de Entrenamiento (Muestras)",
    y = "",
    color = "Parámetros utilizados"
  ) +
  scale_color_manual(values = c("accuracy" = "blue", "tiempo_Segundos" = "red")) +
  theme_minimal()





saveRDS(randomForest, file = "modelo_randomForest_digitrecognition.rds")
cat("\nModelo guardado correctamente como 'modelo_randomForest_digitrecognition.rd'\n")
