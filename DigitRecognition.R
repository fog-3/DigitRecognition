library(tidyverse)
suppressMessages(library(caret))


train <- read.table("C:/Users/TT CUSTOM/Desktop/DigitRecognition/train.csv",header=T, sep=",")
test <- read.table("C:/Users/TT CUSTOM/Desktop/DigitRecognition/test.csv",header=T, sep=",")

dim(train)
dim(test)
table(train$label) ## Número de dígitos que hay de cada tipo
labels <- train$label

# Función para visualizar un dígito específico
mostrar_digito <- function(datos, indice_fila, nrow = 28, ncol = 28, tiene_label = TRUE) {
  
  fila <- datos[indice_fila, ]
  
  if (tiene_label) {
    # Quitamos la primera columna (label)
    fila <- fila[-1]
  }
  
  fila_pixeles <- as.numeric(fila)
  
  matriz_imagen <- matrix(fila_pixeles, nrow = nrow, ncol = ncol, byrow = TRUE)
  
  matriz_rotada <- t(apply(matriz_imagen, 2, rev))
  
  image(matriz_rotada,
        col  = gray.colors(255, start = 0, end = 1),
        axes = FALSE)
}


# Visualizar el tercer dígito del dataset de entrenamiento (puedes cambiar el índice)
mostrar_digito(train, 7)

# Visualizar varios para probar (ej. los primeros 4)
par(mar = c(0.1, 0.1, 0.1, 0.1))
par(mfrow=c(3,10)) # Dividir ventana en 2x2
for(k in 1:30) {
  mostrar_digito(train, k)
}

# ----- Convolución ---------

# Train set
trainlst = list(
  n = nrow(train),
  x = train |> select(-label) |> as.data.frame() |> as.matrix(), 
  y = train |> pull(label) |> as.factor()
)

# -------- Convolución de la imagen  -------- #

# Crea una matriz que transforma vector de 28 a 14 promediando cada 2.
W <- diag(14)[rep(1:14, each = 2), ] / 2

# Usa el producto Kronecker para aplicar W tanto a filas como a columnas
Pool2x2 <- W %x% W 

# Asignar nombres a las nuevas columnas (pixel1...pixel196)
colnames(Pool2x2) <- paste0("p", 1:ncol(Pool2x2))

# Aplicamos la transformación a la matriz de imágenes 'x' que creamos antes
X_reducida <- trainlst$x %*% Pool2x2 
X_reducida <- X_reducida /255
train_reducido_df <- as.data.frame(X_reducida)
train_reducido_df$label <- trainlst$y


# Verificar la reducción
dim(X_reducida) 
# Debería salir: [42000, 196]. ¡Ha bajado de 784 a 196 columnas!

# Mostramos el digito con la imagen original junto con la convolución realizada
par(mar = c(1, 1, 1, 1))
par(mfrow=c(1,2))
mostrar_digito(trainlst$x, 7, tiene_label = FALSE)
mostrar_digito(X_reducida, 7, nrow = 14, ncol = 14, tiene_label = FALSE)


#------- Eliminación de bordes vacíos ---------#

library(caret)

# Identificar columnas con varianza cero (o casi cero) de las imagenes originales
nzv <- nearZeroVar(trainlst$x, saveMetrics = TRUE)

# Identificar columnas con varianza cero (o casi cero) de las imagenes convolucionadas
nzv2 <- nearZeroVar(X_reducida, saveMetrics = TRUE)

# Vemos cuántos píxeles son "inútiles" (bordes negros constantes)
print(paste("Píxeles a eliminar (Imagen original):", sum(nzv$zeroVar)))

# Vemos cuántos píxeles son "inútiles" (bordes negros constantes)
print(paste("Píxeles a eliminar (Imagen convolucionada):", sum(nzv2$zeroVar)))

## Viendo la poca reducción que supone no aplicaremos este método de preprocesamiento
## No merece la pena
