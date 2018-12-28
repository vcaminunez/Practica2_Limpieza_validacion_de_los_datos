#librerías
library(C50)
library(plyr)
dir <- "C:/Users/victor/Dropbox/uoc/semestre2/tipologia y ciclo de vida de los datos/practica2"
setwd(dir)

data_0 <- read.csv("datos-discretizados.csv", header=TRUE)


data <- data.frame(data_0)

# OBTENER LOS CONJUNTOS DE TRAIN (80%) Y TEST (20%)
n      <- nrow(data)
ntrain <- round(n*0.8)           
tindex <- sample(n,ntrain)        
xtrain <- data[tindex,1:10]  
xtest  <- data[-tindex,1:10]  
ytrain <- data[tindex,11]     
ytrain <- as.factor(ytrain)
ytest  <- data[-tindex,11]    

# ENTRENAMOS EL MODELO CON LOS DATOS DE ENTRENAMIENTO
model <- C50::C5.0(xtrain, ytrain,rules = TRUE) 

# Aproximación del error y las reglas de clasificación
summary(model)

sink("reglas-discretizadas.txt")
print(summary(model))
sink()

# predecir las calidades de grupo de comprobación de calidad

p1 <- predict(model, xtest, type="class")

# CALCULAMOS LA EFICACIA DEL MODELO

accuracy <- sum(ytest==p1)/length(ytest)
error <- 1- accuracy
print(accuracy)
print(error)

table(ytest, Predicho =p1)


#validación cruzada con k = 10
#calculamos el modelo utilizando 9 de las 10 conjuntos para entrenar y el restante para evaluar
#se calcula el error de cada modelo y se suman todos los errores
conjuntos <- split(data, cut(sample(1:nrow(data)),10))

error_total <- 0

for (i in 1:length(conjuntos)) {
  
  test <- data.frame(conjuntos[i])
  colnames(test)[1] <- "fixed.acidity"
  colnames(test)[2] <- "volatile.acidity"
  colnames(test)[3] <- "citric.acid"
  colnames(test)[4] <- "chlorides"
  colnames(test)[5] <- "free.sulfur.dioxide"
  colnames(test)[6] <- "total.sulfur.dioxide"
  colnames(test)[7] <- "density"
  colnames(test)[8] <- "pH"
  colnames(test)[9] <- "sulphates"
  colnames(test)[10] <- "alcohol"
  colnames(test)[11] <- "quality"
  
  
  test_x <- test[1:10]
  test_y <- test$quality
  
  train <- ldply(conjuntos[-i], data.frame)
  train <- train[2:12]
  train <- data.frame(train)
  
  train_x <- train[1:10]
  train_y <- as.factor(train$quality)
  
  model2 <- C50::C5.0(train_x, train_y,rules = TRUE)
  
  p2 <- predict(model2, test_x, type="class")
  accuracy_2 <- sum(test_y==p2)/length(test_y)
  error2 <- 1- accuracy_2
  print(error2)
  error_total <- error_total + error2
}
#el error final es la media de los errores
error_total = error_total / 10.0
print(error_total)
