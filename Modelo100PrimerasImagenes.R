# --------- Entrenamiento usando un SVM para el problema de DigitRecognition para entrenar usando solo las 100 primeras imagenes ------------

#Seleccionamos una semilla y cargamos las librerias que vayamos a utilizar
set.seed(10)
library(e1071)
library(tictoc)


# -------------------------------------------------------------------------
#Defino el conjunto de entrenamiento y test para aplicar validación cruzada
# -------------------------------------------------------------------------
ind <- sample(1:nrow(train_reducido_df), 0.8 * nrow(train_reducido_df))
dtrain <- train_reducido_df[ind,]
dtest  <- train_reducido_df[-ind,]


#Me defino otra submuestra de 100 imagenes para entrenar finalmente el modelo
subsample_index  <- sample(1:nrow(dtrain), 100)
dtrain_subsample <- dtrain[subsample_index, ]
dtrain_subsample <- na.omit(dtrain_subsample)

#Defino los mejores hiper parámetros encontrados
mejor_costo <- svm_tune$best.parameters$cost
mejor_gamma <- svm_tune$best.parameters$gamma

# -------------------------------------------------------------------------------------
#Entreno el SVM utilizando solo las 100 primeras muestras del conjunto de entrenamiento
# -------------------------------------------------------------------------------------

tic("--------Entrenamiento del modelo SVM--------")
svmDigit100 <- svm(label ~ ., data=dtrain_subsample, kernel = "radial", cost = mejor_costo, gamma = mejor_gamma)
tiempo_entrenamiento <- toc()
tiempo_en_segundos <- tiempo_entrenamiento$toc - tiempo_entrenamiento$tic

#Calculamos el accuracy
pred <- predict(svmDigit100, dtest, type = "class")
matrizconfusion <- table(pred, dtest$label)
accuracy <- sum(diag(matrizconfusion)) / sum(matrizconfusion)

print(matrizconfusion)

cat("El accuracy obtenido por el modelo SVM con 100 muestras utilizando el dataset MNIST es del:", accuracy*100,"%")
cat("Tiempo empleado en entrenar el modelo:", tiempo_en_segundos ,"s \n")


#Guardamos el modelo en un fichero
saveRDS(svmDigit100, file = "modelo_svm100_digitrecognition.rds")
cat("\nModelo guardado correctamente como 'modelo_svm_digitrecognition100.rds'\n")



