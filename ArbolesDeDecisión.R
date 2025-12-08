# --------- Entrenamiento usando un Árbol de decisión ------------
library(rpart)
library(rpart.plot)

# Usamos 2000 elementos del dataset para entrenar y 300 para test
x_train <- X_reducida[1:2000, ]
y_train <- trainlst$y[1:2000]

# Ponemos datos nunca vistos por el modelo para test
x_test <- X_reducida[2001:2300, ] 
y_test <- trainlst$y[2001:2300]

# Combinar en Data Frames
train_data <- data.frame(y = y_train, x_train)
test_data <- data.frame(y = y_test, x_test)

# Asegurar que la variable objetivo sea Factor
train_data$y <- as.factor(train_data$y)
test_data$y <- as.factor(test_data$y)

# ENTRENAMIENTO
decision_tree <- rpart(
  y ~ ., # Predice 'y' basado en todos los demás atributos
  data = train_data,
  method = "class",
  # Para de iterar cuando la mejoría de cp sea menor o igual al 0.01
  control = rpart.control(cp = 0.01)
)

# EVALUACIÓN
predictions <- predict(decision_tree, test_data, type = "class")

conf_matrix <- table(Real = test_data$y, Predicho = predictions)

print(conf_matrix)

# Calcular Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

cat("Precisión del modelo (Accuracy):", round(accuracy * 100, 2), "%\n")

## Dibujo del arbol
mis_colores <- list("pink", "green", "cyan", "yellow", 
                 "orange", "wheat", "lightblue", "lightgray", 
                 "violet", "lightgreen")

par(mfrow=c(1,1))
rpart.plot(decision_tree,
           type = 3,       # Dibuja etiquetas separadas para izq/der (más fácil de leer)
           extra = 100,    # Muestra el porcentaje de observaciones en cada nodo
           under = TRUE,   # Pone la etiqueta de la clase debajo del cuadro
           faclen = 0,     # Muestra el nombre completo de las variables (ej: pixel340)
           cex = 0.7,      # Reduce el tamaño de letra para que quepa todo
           main = "Árbol de Decisión: Dígit recognicer",
           box.palette = mis_colores
)
