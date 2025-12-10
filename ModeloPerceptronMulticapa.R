# --------- Entrenamiento usando un perceptron multicapa para DigitRecognition ------------

set.seed(10)
library(nnet)
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

tamanos_muestra <- c(1000, 2000, 3000, 4000, 5000)


# -------------------------------------------------------------------------
#Defino el conjunto de entrenamiento y test (80% y 20%)
# -------------------------------------------------------------------------
ind <- sample(1:nrow(train_reducido_df), 0.8 * nrow(train_reducido_df))
dtrain <- train_reducido_df[ind,]
dtest  <- train_reducido_df[-ind,]
dtrain$label <- as.factor(dtrain$label)
levels(dtrain$label) <- make.names(levels(dtrain$label))

# ---------------------------------------------
#Me defino una submuestra para hacer el tuning
# ---------------------------------------------
subsample_index <- sample(1:nrow(train_reducido_df), 1000)
dtrain_subsample <- dtrain[subsample_index, ]
dtrain_subsample <- na.omit(dtrain_subsample)

# --------------------------------------------------
#Definimos los valores para optimizar usando tune
# --------------------------------------------------

size_valores <- c(5, 10, 25)
decay_valores <- c(5e-4, 5e-3, 5e-2) 

mlp_par_grid <- expand.grid(
  .size = size_valores,
  .decay = decay_valores
)

ctrl <- trainControl(
  method = "LGOCV",     
  p = 0.8,               
  number = 1,            
  savePredictions = "final",
  verboseIter = FALSE
)

# ----------------
#Iniciamos el tune
# ----------------

perceptron_tune <- train(
  label ~ .,
  data = dtrain_subsample,
  method = "nnet",        
  trControl = ctrl,      
  tuneGrid = mlp_par_grid,
  maxit = 50,           
  MaxNWts = 100000,   
  trace = FALSE,          
  linout = FALSE          
)

#------------------------------------------------------------
#Elegimos la combinación de valores que mejor accuracy aporte
#------------------------------------------------------------
print(perceptron_tune$results)

# -------------------------------------------------------------------
#Una vez tenemos los mejores hiperparámetros, entreno el Perceptrón
# -------------------------------------------------------------------

for(i in 1:length(tamanos_muestra)){
  
  subsample_index <- sample(1:nrow(dtrain), tamanos_muestra[i])
  dtrain_subsample_model <- dtrain[subsample_index, ]
  dtrain_subsample_model <- na.omit(dtrain_subsample_model)
  
  
  tic("--------Entrenamiento del modelo SVM--------")
  perceptron <- nnet(
    label ~ .,
    data   = dtrain_subsample_model,
    size   = 25,       # Número de neuronas en la capa oculta
    decay  = 5e-02,     # Parámetro de regularización
    maxit  = 50,      # Número de iteraciones máximas
    MaxNWts = 100000,  #MaxNWts lo mantenemos constante por si hay muchos pesos
    trace  = FALSE
  )
  tiempo_entrenamiento <- toc()
  tiempo_en_segundos <- tiempo_entrenamiento$toc - tiempo_entrenamiento$tic
  
  #Calculamos el accuracy
  pred <- predict(perceptron, dtest, type = "class")
  matrizconfusion <- table(pred, dtest$label)
  accuracy <- sum(diag(matrizconfusion)) / sum(matrizconfusion)
  
  cat("El accuracy obtenido por el perceptron es del:", accuracy*100,"%")
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


saveRDS(perceptron, file = "modelo_perceptron_digitrecognition.rds")
cat("\nModelo guardado correctamente como 'modelo_perceptron_digitrecognition.rd'\n")


