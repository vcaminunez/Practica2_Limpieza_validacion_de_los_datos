#librerías
library(arules)
library(nortest)

#lectura del fichero
#estabelcemso el directorio de trabajo que corresponda. Es un campo que se ha de cambiar dependiendo de donde
#se encuentre el fichero de lectura
dir <- "C:/Users/victor/Dropbox/uoc/semestre2/tipologia y ciclo de vida de los datos/practica2"
setwd(dir)


data_0 <- read.csv("winequality-red.csv", header=TRUE)

#valores estadísticos del dataset
summary(data_0)


#miramos el número de ceros de cada campo
colSums(data_0 == 0)

#miramos el número de nulos de cada campo
sapply(data_0,function(x) sum(is.na(x)))

#graficas de cajas donde se ven los valores extremos de todos los campos menos quality
par(mfrow=c(2,2)) 
boxplot(data_0$fixed.acidity,main='fixed.acidity',xlab="valores",horizontal = TRUE)
boxplot(data_0$volatile.acidity,main='volatile.acidity',xlab="valores",horizontal = TRUE)
boxplot(data_0$citric.acid,main='citric.acid',xlab="valores",horizontal = TRUE)
boxplot(data_0$residual.sugar,main='residual.sugar',xlab="valores",horizontal = TRUE)

par(mfrow=c(2,2)) 
boxplot(data_0$chlorides,main='chlorides',xlab="valores",horizontal = TRUE)
boxplot(data_0$free.sulfur.dioxide,main='free.sulfur.dioxide',xlab="valores",horizontal = TRUE)
boxplot(data_0$total.sulfur.dioxide,main='total.sulfur.dioxide',xlab="valores",horizontal = TRUE)
boxplot(data_0$density,main='density',xlab="valores",horizontal = TRUE)

par(mfrow=c(2,2)) 
boxplot(data_0$pH,main='pH',xlab="valores",horizontal = TRUE)
boxplot(data_0$sulphates,main='sulphates',xlab="valores",horizontal = TRUE)
boxplot(data_0$alcohol,main='alcohol',xlab="valores",horizontal = TRUE)
boxplot(data_0$quality,main='quality',xlab="valores",horizontal = TRUE)


#comprobamos si los datos siguen o no una distribución normal

for ( i in 1:12){
  print(colnames(data_0[i]))
  print(ad.test(data_0[,i]))
}


# histograma de cada unos de los campos
par(mfrow=c(2,2)) 
hist(data_0$fixed.acidity,main='fixed.acidity',xlab="valores")
hist(data_0$volatile.acidity,main='volatile.acidity',xlab="valores")
hist(data_0$citric.acid,main='citric.acid',xlab="valores")
hist(data_0$residual.sugar,main='residual.sugar',xlab="valores")

par(mfrow=c(2,2)) 
hist(data_0$chlorides,main='chlorides',xlab="valores")
hist(data_0$free.sulfur.dioxide,main='free.sulfur.dioxide',xlab="valores")
hist(data_0$total.sulfur.dioxide,main='total.sulfur.dioxide',xlab="valores")
hist(data_0$density,main='density',xlab="valores")

par(mfrow=c(2,2)) 
hist(data_0$pH,main='pH',xlab="valores")
hist(data_0$sulphates,main='sulphates',xlab="valores")
hist(data_0$alcohol,main='alcohol',xlab="valores")
hist(data_0$quality,main='quality',xlab="valores")



#comprobamos la homogeneidad de la varianza de los diferentes campos con respecto a quality
for ( i in 1:11){
  print(colnames(data_0[i]))
  print(fligner.test(data_0[,i] ~ quality, data = data_0))
}

# Calcular el paso de cada campo para el campo "quality"

for ( i in 1:11){
  print(colnames(data_0[i]))
  print(kruskal.test(data_0[,i] ~ quality, data = data_0))
}


#exportación de los datos discretizados para realizar el clasificador

datos1 <- data_0[-4]

datos1$quality[datos1$quality < 5] <- 'Baja'
datos1$quality[datos1$quality == 5] <- 'Normal'
datos1$quality[datos1$quality == 6] <- 'Normal'
datos1$quality[datos1$quality == 7] <- 'Alta'
datos1$quality[datos1$quality == 8] <- 'Alta'

write.csv(datos1, file = "datos-discretizados.csv",row.names=FALSE)

