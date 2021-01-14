#importe de librerias.
library(rgl)
library(neuralnet)
#Generar datos
n = 999
a = 2.0 
b = 3.0
c = -3.0 
#matriz llena de 0, de 999 filas y 3 columnas.
datos = matrix(0,n,3)

#recorremos la matriz
for(i in 1:n){
  if (i >= 1 && i <= 333){
    datos[i,1] <- rnorm(1) + a #eje X traslacion de +2.0
    datos[i,2] <- rnorm(1) - a #eje Y traslacion de -2.0
    datos[i,3] <- rnorm(1) + a #eje Z traslacion de +2.0
  }
  
  if (i > 333 && i <= 666) {
    datos[i,1] <- rnorm(1) + b
    datos[i,2] <- rnorm(1) + b
    datos[i,3] <- rnorm(1) + b
  }
  
  if (i > 666 && i <= 999) {
    datos[i,1] <- rnorm(1) + c
    datos[i,2] <- rnorm(1) + c
    datos[i,3] <- rnorm(1) + c
  }
}
#Dos ventanas para visualizar el cubo en 3D y en el plano.
X11()
plot3d(datos[,1],datos[,2],datos[,3], col="blue", type ="s", radius = 0.15)
X11()
plot(datos[,1],datos[,2], col="blue", type ="p")

#Preparacion de datos para la ANN
t1 = rep(4,times=333) #replica el valor de 4, por 333 veces.
t2 = rep(5,times=333)
t3 = rep(6,times=333)

#Juntamos las 3 clases anteriores en un solo vector llamado target
target = c(t1,t2,t3) #target

#Creamos un data frame juntando los datos generados al inicio y el vector target.
#A su vez renombramos las columnas para tener mas control.
#i1 es de input1. como primera entrada.
dframe = data.frame(datos,target)
names(dframe)
colnames(dframe) <- c ("i1","i2","i3","target")
attach(dframe)

#Separamos las muestras, una para entrenamiento y otra para test.
#dftrain es entrenamiento con un 80% de la muestra.
#dftest es test con un 20% de la muestra.

dftrain_1<- dframe[1:266,] #26,6% de clase 1 para entrenamiento
dftrain_2<-dframe[334:600,] #26,6% de clase 2 para entrenamiento
dftrain_3<-dframe[668:933,] #26,6% de clase 3 para entrenamiento

dftest_1<-dframe[267:333,] #6,7% de clase 1 para test
dftest_2<-dframe[601:667,] #6,7% de clase 2 para test
dftest_3<-dframe[933:999,] #6,7% de clase 3 para test

dftrain<-rbind(dftrain_1,dftrain_2,dftrain_3)#80% de la muestra total
dftest<- rbind(dftest_1,dftest_2,dftest_3)#20% de la muestra total

#Creamos la ANN de 3-9-1
#Donde tenemos 3 entradas, 9 neuronas de capa oculta y 1 salida.
#Le pasamos como data el dataframe de entrenamiento.
#9 neuronas en la capa oculta.
#5 repeticiones, con un threshold de 0.01
#funcion de activacion tangente hiperbolica.
#error es la suma de los errores cuadraticos.
red1 <- neuralnet(target ~ i1 +i2 + i3,data=dftrain,
                  hidden=9,
                  rep=5,threshold=0.01,
                  act.fct = "tanh",
                  algorithm = "rprop+",lifesign = "minimal", err.fct = "sse")

#Mostramos la arquitectura de la red con la mejor repeticion.

#png(filename="C:/Users/Josafat/Desktop/ramos 2020/Inteligencia Artificial/unas.png")
plot(red1, rep="best")

#Realizamos el test con el 20% restante de los datos.
#creando un subset del conjunto de testing, removiendo la columna target(variable dependiente).
temp_test <- subset(dftest,select = c("i1","i2","i3"))
red1.resultadoC<-predict(red1,temp_test)
resultados<-data.frame(actual = dftest$target,prediccion = red1.resultadoC)

#error cuadratico
error = round(1/100*(dftest$target - red1.resultadoC)^2,4)
verError = data.frame(dftest$target, red1.resultadoC, error)
print(verError)

#la funcion compute esta descontinuada y es remplazada por predict.nn
#red1.resultadoC<-compute(red1,temp_test)
#resultados<- data.frame(actual = dftest$target,prediccion = red1.resultadoC$net.result)
