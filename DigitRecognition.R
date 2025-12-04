
train <- read.table("C:/Users/ferna/Desktop/Trabajo escolar/1.Universidad/Curso 2025-2026/1_Aprendizaje/Práctica en grupo/train.csv",header=T, sep=",")
test <- read.table("C:/Users/ferna/Desktop/Trabajo escolar/1.Universidad/Curso 2025-2026/1_Aprendizaje/Práctica en grupo/test.csv",header=T, sep=",")

dim(train)
dim(test)
table(train$label) ## Número de dígitos que hay de cada tipo
labels <- 

# Función para visualizar un dígito específico
mostrar_digito <- function(datos, indice_fila) {
  
  # 1. Extraemos solo los píxeles (quitamos la columna 'label')
  fila_pixeles <- as.numeric(datos[indice_fila, -1]) 
  
  # 2. Reconstruimos la matriz 28x28 usando la lógica de la fórmula x = i*28 + j
  # 'byrow = TRUE' asegura que llenamos la matriz fila a fila, tal como indica la fórmula.
  matriz_imagen <- matrix(fila_pixeles, nrow = 28, ncol = 28, byrow = TRUE)
  
  # 3. Visualización
  # La función image() rota la matriz 90 grados, así que corregimos el númro
  
  # Rotar la matriz para que se vea bien en el plot
  matriz_rotada <- t(apply(matriz_imagen, 2, rev))
  
  # Mostrar la imagen
  # col = gray.colors(255) usa escala de grises como indica el enunciado (0-255)
  image(matriz_rotada, col = gray.colors(255), axes = FALSE, 
        main = datos[indice_fila, 1])
}

props = colMeans(X > 0)                 # Compute P(X > 0)
use_pix = props > .15 & props < 1-.15   # Find pixels that are not near-zero-variance

# One-liner:
use_pix = colMeans(X > 0) |> between(0.15, 1-0.15)

# What is the number of pixels retained?
sum(use_pix)

# Visualizar el tercer dígito del dataset de entrenamiento (puedes cambiar el índice)
mostrar_digito(train, 7)

# Visualizar varios para probar (ej. los primeros 4)
par(mfrow=c(2,5)) # Dividir ventana en 2x2
for(k in 1:10) {
  mostrar_digito(train, k)
}
