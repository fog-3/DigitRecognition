## --------- Entrenamiento usando la técnica de stacking para el problema de DigitRecognition ------------


set.seed(10)
library(e1071)
library(nnet)
library(caret)
library(tictoc)
library(ggplot2)
library(tidyr)

# -----------------------------------------
#Defino el conjunto de entrenamiento y test 
# -----------------------------------------
ind <- sample(1:nrow(train_reducido_df), 0.8 * nrow(train_reducido_df))
dtrain <- train_reducido_df[ind,]
dtest  <- train_reducido_df[-ind,]


# --------------------------------------------------------
#Me creo dos datasets distintos provenientes de train:
#   -Train_base para entrenar modelos base
#   -Train_meta para entrenar el modelo final
# --------------------------------------------------------
ind_meta <- sample(1:nrow(dtrain), 0.7 * nrow(dtrain))
train_base <- dtrain[ind_meta, ]
train_meta <- dtrain[-ind_meta, ]

train_base$label <- as.factor(train_base$label)
train_meta$label <- as.factor(train_meta$label)

# ---------------------------------------------------
#Entreno ambos modelos y hago predicciones con ambos
# ---------------------------------------------------
subsample_index <- sample(1:nrow(train_base), 5000)
dtrain_subsample_model <- train_base[subsample_index, ]
dtrain_subsample_model <- na.omit(dtrain_subsample_model)
dtrain_subsample_model$label <- as.factor(dtrain_subsample_model$label)

svmDigit <- svm(label ~ ., data=dtrain_subsample_model, kernel = "radial", cost = 1, gamma = 0.1)
svm_predict <-predict(svmDigit,train_meta,type="class")

perceptronDigit <- perceptron <- nnet(label ~ .,
  data   = dtrain_subsample_model,
  size   = 25,       # Número de neuronas en la capa oculta
  decay  = 5e-02,     # Parámetro de regularización
  maxit  = 50,      # Número de iteraciones máximas
  MaxNWts = 100000,  #MaxNWts lo mantenemos constante por si hay muchos pesos
  trace  = FALSE
)
perceptron_predict <-predict(perceptronDigit,train_meta,type="class")

datasetFinal <- data.frame(
  svm_pred = as.factor(svm_predict),
  perceptron_pred = as.factor(perceptron_predict),
  label_real = train_meta$label
)


# ------------------------------------------------------------------
#Entreno el modelo final utilizando las predicciones como dataSet
# -----------------------------------------------------------------

svmFinal <- svm(label_real ~ ., data=datasetFinal, kernel = "radial", cost = 1, gamma = 0.1)

svm_test_pred <- predict(svmDigit, dtest, type="class")
perc_test_pred <- predict(perceptronDigit, dtest, type="class")

dtest_features_stacking <- data.frame(
  svm_pred = as.factor(svm_test_pred),
  perceptron_pred = as.factor(perc_test_pred)
)

# -------------------------------------
#Calculo el accuracy del modelo final
# --------------------------------------

pred <- predict(svmFinal, dtest_features_stacking, type = "class")
matrizconfusion <- table(pred, dtest$label)
accuracy <- sum(diag(matrizconfusion)) / sum(matrizconfusion)

cat("El accuracy obtenido por el Stacking con dataset MNIST es del:", accuracy*100,"%")

