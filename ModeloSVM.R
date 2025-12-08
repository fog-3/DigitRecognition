# --------- Entrenamiento usando un SVM para el problema de DigitRecognition ------------

#Seleccionamos una semilla y cargamos las librerias que vayamos a utilizar

set.seed(10)
library(e1071)
library(caret)
library(tictoc)
library(ggplot2)
library(tidyr)

# --------------------------------------------------------
#Defino un dataframe para imprimir la curva de aprendizaje
# --------------------------------------------------------
resultados_curva <- data.frame(
  tamano_Muestra = integer(),
  tiempo_Segundos = numeric(),
  accuracy = numeric()
)

#View(train_reducido_df)
tamanos_muestra <- c(1000, 2000, 3000, 4000, 5000)

# -------------------------------------------------------------------------
#Defino el conjunto de entrenamiento y test para aplicar validación cruzada
# -------------------------------------------------------------------------
ind <- sample(1:nrow(train_reducido_df), 0.8 * nrow(train_reducido_df))
dtrain <- train_reducido_df[ind,]
dtest  <- train_reducido_df[-ind,]


# ---------------------------------------------------------------------------
#Me defino una submuestra para encontrar los mejores hiperparámetros posibles
# ---------------------------------------------------------------------------
subsample_index <- sample(1:nrow(train_reducido_df), 1000)
dtrain_subsample <- dtrain[subsample_index, ]
dtrain_subsample <- na.omit(dtrain_subsample)

# ---------------------------------------------------------
#Definimos los valores de coste y gamma a probar en el tune
# ---------------------------------------------------------
coste_valores <- 10^(-1:2)    
gamma_valores <- 10^(-3:0)   
par_grid <- list(cost = coste_valores, gamma = gamma_valores)


# ----------------
#Iniciamos el tune
# ----------------
cat("Iniciando la búsqueda de hiperparámetros...\n")
svm_tune <- tune(
  svm,
  label ~ .,
  data = dtrain_subsample,
  kernel = "radial", #El kernel será siempre radial, es el que mejor funciona generalmente ante muestras de datos grandes
  ranges = par_grid,
  tunecontrol = tune.control(sampling = "fix"))

cat("Búsqueda de hiperparámetros finalizada.\n")

# --------------------------------------------------------
#Guardamos la mejor combinación de hiperparámetros del SVM
# --------------------------------------------------------
mejor_costo <- svm_tune$best.parameters$cost
mejor_gamma <- svm_tune$best.parameters$gamma

cat("\n--- Mejores Hiperparámetros ---\n")
cat("Mejor Costo (C):", mejor_costo, "\n")
cat("Mejor Gamma (γ):", mejor_gamma, "\n")

# -------------------------------------------------------------------
#Una vez tenemos los mejores hiperparámetros, entrenamos el svm final
# -------------------------------------------------------------------
for(i in 1:length(tamanos_muestra)){
  #Me defino otra submuestra para entrenar finalmente el modelo
  subsample_index <- sample(1:nrow(dtrain), tamanos_muestra[i])
  dtrain_subsample_model <- dtrain[subsample_index, ]
  dtrain_subsample_model <- na.omit(dtrain_subsample_model)
  
  
  tic("--------Entrenamiento del modelo SVM--------")
  svmDigit <- svm(label ~ ., data=dtrain_subsample_model, kernel = "radial", cost = mejor_costo, gamma = mejor_gamma)
  tiempo_entrenamiento <- toc()
  tiempo_en_segundos <- tiempo_entrenamiento$toc - tiempo_entrenamiento$tic
 
  #Calculamos el accuracy
  pred <- predict(svmDigit, dtest, type = "class")
  matrizconfusion <- table(pred, dtest$label)
  accuracy <- sum(diag(matrizconfusion)) / sum(matrizconfusion)
  
  cat("El accuracy obtenido por el modelo SVM con dataset MNIST es del:", accuracy*100,"%")
  cat("Tiempo empleado en entrenar el modelo:", tiempo_en_segundos ,"s \n")
  
  resultados_curva[nrow(resultados_curva) + 1, ] <- c(
    nrow(dtrain_subsample_model),
    tiempo_en_segundos,
    accuracy
  )
  
}


print(resultados_curva)

# --------------------------------
#Mostramos la curva de aprendizaje
# --------------------------------
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
    title = "Curva de Aprendizaje para el modelo SVM",
    x = "Tamaño del Conjunto de Entrenamiento (Muestras)",
    y = "",
    color = "Parámetros utilizados"
  ) +
  scale_color_manual(values = c("accuracy" = "blue", "tiempo_Segundos" = "red")) +
  theme_minimal()
