
# --------- Entrenamiento usando un SVM para el problema de DigitRecognition ------------

#Seleccionamos una semilla y cargamos las librerias que vayamos a utilizar
set.seed(10)
library(e1071)
library(caret)


#View(train_reducido_df)


#Defino el conjunto de entrenamiento y test para aplicar validación cruzada
ind <- sample(1:nrow(train_reducido_df), 0.8 * nrow(train_reducido_df))
dtrain <- train_reducido_df[ind,]
dtest  <- train_reducido_df[-ind,]

#Me defino una submuestra para encontrar los mejores hiperparámetros posibles
subsample_index <- sample(1:nrow(train_reducido_df), 1000)
dtrain_subsample <- dtrain[subsample_index, ]
dtrain_subsample <- na.omit(dtrain_subsample)

#Me defino otra submuestra para entrenar finalmente el modelo
subsample_index_2000 <- sample(1:nrow(dtrain), 2000)
dtrain_subsample_model <- dtrain[subsample_index_2000, ]
dtrain_subsample_model <- na.omit(dtrain_subsample_model)

#Me defino una submuestra para el conjunto de test
subsample_index_500 <- sample(1:nrow(dtest), 500)
dtest_subsample <- dtest[subsample_index_500, ]
dtest_subsample <- na.omit(dtest_subsample)


#Definimos los valores de coste y gamma a probar en el tune
coste_valores <- 10^(-1:2)    
gamma_valores <- 10^(-3:0)   
par_grid <- list(cost = coste_valores, gamma = gamma_valores)

#Iniciamos el tune
cat("Iniciando la búsqueda de hiperparámetros...\n")
svm_tune <- tune(
  svm,
  label ~ .,
  data = dtrain_subsample,
  kernel = "radial", #El kernel será siempre radial, es el que mejor funciona generalmente ante muestras de datos grandes
  ranges = par_grid,
  tunecontrol = tune.control(sampling = "fix"))

cat("Búsqueda de hiperparámetros finalizada.\n")


#Guardamos la mejor combinación de hiperparámetros del SVM
mejor_costo <- svm_tune$best.parameters$cost
mejor_gamma <- svm_tune$best.parameters$gamma

cat("\n--- Mejores Hiperparámetros ---\n")
cat("Mejor Costo (C):", mejor_costo, "\n")
cat("Mejor Gamma (γ):", mejor_gamma, "\n")

#Una vez tenemos los mejores hiperparámetros, entrenamos el svm final
svmDigit <- svm(label ~ ., data=dtrain_subsample_model, kernel = "radial", cost = mejor_costo, gamma = mejor_gamma)


#Calculo el accuracy del modelo 
pred <- predict(svmDigit,dtest_subsample,type="class")
matrizconfusion <- table(pred, dtest_subsample$label)

#Mostramos la matriz de confusión
print(matrizconfusion)

#Calculamos el accuracy
accuracy<-sum(diag(matrizconfusion)/sum(matrizconfusion))

cat("El accuracy obtenido por el modelo SVM con dataset DigitRecognition es del:", accuracy*100,"%")










