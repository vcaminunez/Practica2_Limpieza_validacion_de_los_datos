#librerías
library(arules)

#lectura del fichero
#estabelcemso el directorio de trabajo que corresponda. Es un campo que se ha de cambiar dependiendo de donde
#se encuentre el fichero de lectura
dir <- "C:/Users/victor/Dropbox/uoc/semestre2/tipologia y ciclo de vida de los datos/practica2"
setwd(dir)


data_0 <- read.csv("winequality-red.csv", header=TRUE)

#miramos el número de ceros de cada campo
colSums(data_0 == 0)


#miramos el número de nulos de cada campo
sapply(data_0,function(x) sum(is.na(x)))

#valores estadísticos del dataset
summary(data_0)


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


# valores extremos de todos los campos menos quality
boxplot.stats(data_0$fixed.acidity)$out
boxplot.stats(data_0$volatile.acidity)$out
boxplot.stats(data_0$citric.acid)$out
boxplot.stats(data_0$residual.sugar)$out
boxplot.stats(data_0$chlorides)$out
boxplot.stats(data_0$free.sulfur.dioxide)$out
boxplot.stats(data_0$total.sulfur.dioxide)$out
boxplot.stats(data_0$density)$out
boxplot.stats(data_0$pH)$out
boxplot.stats(data_0$sulphates)$out
boxplot.stats(data_0$alcohol)$out
boxplot.stats(data_0$quality)$out


#miramos als correlaciones entre los diferentes elementos 
data_1 <- data_0[-12]
cor(data_1)


#ploteamos los diferentes datos correlacionados
plot(data_0$citric.acid, data_0$fixed.acidity, type = "p", col = "black", xlab="citric.acid",ylab="fixed.acidity" )
plot(data_0$citric.acid, data_0$pH, type = "p", col = "black", xlab="citric.acid",ylab="pH" )
plot(data_0$citric.acid, data_0$volatile.acidity, type = "p", col = "black", xlab="citric.acid",ylab="volatile.acidity" )
plot(data_0$citric.acid, data_0$density, type = "p", col = "black", xlab="citric.acid",ylab="density" )
plot(data_0$free.sulfur.dioxide, data_0$total.sulfur.dioxide, type = "p", col = "black", xlab="free.sulfur.dioxide",ylab="total.sulfur.dioxide")
plot(data_0$alcohol, data_0$density, type = "p", col = "black", xlab="alcohol",ylab="density" )

#añadimos la columna de bound sulfur dioxide y eliminamos Total sulfur dioxide 
bound <-  data_0$total.sulfur.dioxide - data_0$free.sulfur.dioxide
data_0 $total.sulfur.dioxide <- bound
colnames(data_0)[7] <- "bound.sulfur.dioxide"



#discretizacion de los diferentes campos 
#eliminamos el campo density

data_0 <- data_0[-8]
datos_fin <-data_0 

data_2 <- data_0$fixed.acidity
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$fixed.acidity[data_0$fixed.acidity < 6.58] <- 'Muy baja'
datos_fin$fixed.acidity[(data_0$fixed.acidity >= 6.58) & (data_0$fixed.acidity < 7.79)] <- 'Baja'
datos_fin$fixed.acidity[(data_0$fixed.acidity >= 7.79) & (data_0$fixed.acidity < 9.24)] <- 'Normal'
datos_fin$fixed.acidity[(data_0$fixed.acidity >= 9.24) & (data_0$fixed.acidity < 11.2)] <- 'Alta'
datos_fin$fixed.acidity[data_0$fixed.acidity >= 11.2] <- 'Muy alta'


data_2 <- data_0$volatile.acidity
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$volatile.acidity[data_0$volatile.acidity < 0.34] <- 'Muy baja'
datos_fin$volatile.acidity[(data_0$volatile.acidity >= 0.34) & (data_0$volatile.acidity < 0.476)] <- 'Baja'
datos_fin$volatile.acidity[(data_0$volatile.acidity >= 0.476) & (data_0$volatile.acidity < 0.618)] <- 'Normal'
datos_fin$volatile.acidity[(data_0$volatile.acidity >= 0.618) & (data_0$volatile.acidity < 0.809)] <- 'Alta'
datos_fin$volatile.acidity[data_0$volatile.acidity >= 0.809] <- 'Muy alta'


data_2 <- data_0$citric.acid
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$citric.acid[data_0$citric.acid < 0.0838] <- 'Muy baja'
datos_fin$citric.acid[(data_0$citric.acid >= 0.0838) & (data_0$citric.acid < 0.204)] <- 'Baja'
datos_fin$citric.acid[(data_0$citric.acid >= 0.204) & (data_0$citric.acid < 0.339)] <- 'Normal'
datos_fin$citric.acid[(data_0$citric.acid >= 0.339) & (data_0$citric.acid < 0.489)] <- 'Alta'
datos_fin$citric.acid[data_0$citric.acid >= 0.489] <- 'Muy alta'


data_2 <- data_0$residual.sugar
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$residual.sugar[data_0$residual.sugar < 2.62] <- 'Muy baja'
datos_fin$residual.sugar[(data_0$residual.sugar >= 2.62) & (data_0$residual.sugar < 4.33)] <- 'Baja'
datos_fin$residual.sugar[(data_0$residual.sugar >= 4.33) & (data_0$residual.sugar < 6.97)] <- 'Normal'
datos_fin$residual.sugar[(data_0$residual.sugar >= 6.97) & (data_0$residual.sugar < 11.4)] <- 'Alta'
datos_fin$residual.sugar[data_0$residual.sugar >= 11.4] <- 'Muy alta'


data_2 <- data_0$chlorides
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$chlorides[data_0$chlorides < 0.0695] <- 'Muy baja'
datos_fin$chlorides[(data_0$chlorides >= 0.0695) & (data_0$chlorides < 0.0919)] <- 'Baja'
datos_fin$chlorides[(data_0$chlorides >= 0.0919) & (data_0$chlorides < 0.15)] <- 'Normal'
datos_fin$chlorides[(data_0$chlorides >= 0.15) & (data_0$chlorides < 0.304)] <- 'Alta'
datos_fin$chlorides[data_0$chlorides >= 0.304] <- 'Muy alta'


data_2 <- data_0$free.sulfur.dioxide
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$free.sulfur.dioxide[data_0$free.sulfur.dioxide < 8.5] <- 'Muy baja'
datos_fin$free.sulfur.dioxide[(data_0$free.sulfur.dioxide >= 8.5) & (data_0$free.sulfur.dioxide < 14.5)] <- 'Baja'
datos_fin$free.sulfur.dioxide[(data_0$free.sulfur.dioxide >= 14.5) & (data_0$free.sulfur.dioxide < 22.9)] <- 'Normal'
datos_fin$free.sulfur.dioxide[(data_0$free.sulfur.dioxide >= 22.9) & (data_0$free.sulfur.dioxide < 35.9)] <- 'Alta'
datos_fin$free.sulfur.dioxide[data_0$free.sulfur.dioxide >= 35.9] <- 'Muy alta'


data_2 <- data_0$bound.sulfur.dioxide
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$bound.sulfur.dioxide[data_0$bound.sulfur.dioxide < 15.6] <- 'Muy baja'
datos_fin$bound.sulfur.dioxide[(data_0$bound.sulfur.dioxide >= 15.6) & (data_0$bound.sulfur.dioxide < 29.9)] <- 'Baja'
datos_fin$bound.sulfur.dioxide[(data_0$bound.sulfur.dioxide >= 29.9) & (data_0$bound.sulfur.dioxide < 52.9)] <- 'Normal'
datos_fin$bound.sulfur.dioxide[(data_0$bound.sulfur.dioxide >= 52.9) & (data_0$bound.sulfur.dioxide < 86.4)] <- 'Alta'
datos_fin$bound.sulfur.dioxide[data_0$bound.sulfur.dioxide >= 86.4] <- 'Muy alta'


data_2 <- data_0$pH
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$pH[data_0$pH < 3.18] <- 'Muy baja'
datos_fin$pH[(data_0$pH >= 3.18) & (data_0$pH < 3.32)] <- 'Baja'
datos_fin$pH[(data_0$pH >= 3.32) & (data_0$pH < 3.45)] <- 'Normal'
datos_fin$pH[(data_0$pH >= 3.45) & (data_0$pH < 3.62)] <- 'Alta'
datos_fin$pH[data_0$pH >= 3.62] <- 'Muy alta'

data_2 <- data_0$sulphates
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$sulphates[data_0$sulphates < 0.538] <- 'Muy baja'
datos_fin$sulphates[(data_0$sulphates >= 0.538) & (data_0$sulphates < 0.649)] <- 'Baja'
datos_fin$sulphates[(data_0$sulphates >= 0.649) & (data_0$sulphates < 0.799)] <- 'Normal'
datos_fin$sulphates[(data_0$sulphates >= 0.799) & (data_0$sulphates < 1.12)] <- 'Alta'
datos_fin$sulphates[data_0$sulphates >= 1.12] <- 'Muy alta'


data_2 <- data_0$alcohol
rangos <-discretize(data_2,method="cluster", breaks = 5)
print(rangos[1])
datos_fin$alcohol[data_0$alcohol < 9.45] <- 'Muy baja'
datos_fin$alcohol[(data_0$alcohol >= 9.45) & (data_0$alcohol < 9.98)] <- 'Baja'
datos_fin$alcohol[(data_0$alcohol >= 9.98) & (data_0$alcohol < 10.7)] <- 'Normal'
datos_fin$alcohol[(data_0$alcohol >= 10.7) & (data_0$alcohol < 11.8)] <- 'Alta'
datos_fin$alcohol[data_0$alcohol >= 11.8] <- 'Muy alta'

#exportación de los datos
write.csv(datos_fin, file = "datos-discretizados.csv",row.names=FALSE)
write.csv(data_0, file = "datos-no-discretizados.csv",row.names=FALSE)

















 