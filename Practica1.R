# 2. Modelos multivariantes
library(mvtnorm)

V<-matrix(NA,2,2)
V[1,]<-c(1,1/2)
V[2,]<-c(1/2,1)
mu<-c(0,0)
x<-c(1,1)
dmvnorm(x,mu,V) # valor de la densidad de una normal en x=(1,1) y media=(0,0)

pmvnorm(lower=-Inf,upper=x,mean=mu,sigma=V) # función de distribución

pmvnorm(lower=c(-1,-1),upper=x,mean=mu,sigma=V)

# gráfica de la función de densidad
f<-function(x,y)
  dmvnorm(data.frame(x,y),mu,sigma=V)
x<-seq(-3,3,length=50)
y<-seq(-3,3,length=50)
z<-outer(x,y,f)

# gráfica 3d de la función de densidad de f (campana de gauss)
persp(x, y, z, xlab='x', ylab='y', zlab='f(x,y)', col='red', theta=0)

# Simulación de 50 datos de este modelo
set.seed(123)
d<-rmvnorm(50,mu,V)

# Podemos representarlos junto con las curvas de nivel de f mediante:
plot(d, xlab="X1", ylab="X2", pch=20, xlim=c(-3,3), ylim=c(-3,3)) # puntos
contour(x, y, z, add=TRUE, col='red') # elipses de Mahalanobis

summary(d) # medidas descriptivas: min, primer cuartil, mediana, media, etc.
colMeans(d) # media muestral
cov(d) # matriz de cuasi-covarianzas (S)
cor(d) # matriz de correlaciones muestrales
# Tambien se pueden calcular con var(d[,1]) y var (d[,2]), y la cuasi-covarianza
var(d[,1])
var(d[,2])
cov(d[,1],d[,2]) # este es el método automático

# Las cuasi-covarianzas
mu1<-mean(d[,1])
mu2<-mean(d[,2])
(1/49)*sum( (d[,1]-mu1)*(d[,2]-mu2)) # este es el método manual
# Coincide con el codigo de cov(d[,1],d[,2]) con n=50

# Test de normalidad multivariante Shapiro-Wilk:
library(mvnormtest)
mshapiro.test((t(d))) # p-valor = 0.6014 > 0.05
# -> Aceptación de la hipótesis nula H0 

# Distancia de Mahalanobis: para sacar observaciones atípicas(outliers) o errores
dM1<-mahalanobis(d,mu,V)
dM2<-mahalanobis(d,colMeans(d),cov(d))
# Como en la práctica las medias y covarianzas serán desconocidas
# Para sacar la observación más rara haremos:
max(dM1) 
which.max(dM1) # más rara
max(dM2)
which.max(dM2) # más rara

# Añadir una etiqueta a la 49:
text(d[49,1],d[49,2]-0.15,'49',cex=0.75) 
# Punto 49 coordenada 1, p49 c2, valor que quiero, tamaño

# Ver todas las distancias y ordenarlas:
View(data.frame(d[,1],d[,2],dM1,dM2)) # -> abre una ventana aparte

# 3. Datos multivariantes:
d<-iris   # sobreescribimos la variable d
mu<-colMeans(d[,1:4]) # Vector de medias de todas las filas y columnas de la 1 a la 4
mu 
V<-cov(d[,1:4]) # Matriz de covarianzas muestrales
V
# Para calcular varias medidas descriptivas sobre todas las variables:
summary(d)

# Hay 3 poblaciones de tamaño 50 y las medias de la primera columna se calculan con:
tapply(d$Sepal.Length,d$Species,mean)
# Para el resto de medidas descriptivas y medias:
tapply(d$Sepal.Length,d$Species,summary)
tapply(d[,1:4],d$Species,colMeans)

# Las diferencias entre las distintas especies se pueden visualizar con los gráficos caja-bigote que
# representan los cuartiles (cajas) y los valores extremos (bigotes). Para ello haremos:
boxplot(d$Sepal.Length~d$Species,xlab='Especies',ylab='Long.sepalo')
# Comentarios: Las flores setosa tienen sépalos más cortos y las virginica más largos. En esta última especie
# se aprecia un posible dato atípico (outlier) que podría pertenecer a la especie setosa.

# Mejoramos los datos con tidyverse y obtenemos la tabla de la derecha
library(tidyverse)
d %>%
ggplot(aes(x=Species,y=Sepal.Length)) + geom_boxplot(aes(color=Species))

# Estos gráficos demuestran que en realidad tenemos k = 4 variables medidas
# en tres poblaciones (especies) distintas. Por esto el test de normalidad multivariante puede fallar
mshapiro.test(t(d[,1:4]))
# El p-valor es de 0.02342 -> rechazamos la normalidad de los datos conjuntos con las 3 especies

# Para separar los datos de las 3 especies podemos usar:
list <- split(d,d$Species)
list2env(list,.GlobalEnv)
# Se crean tres tablas de datos con los nombres de cada especie

# Si realizamos el test de normalidad de las 3 especies por separados solo la pasa la especie setosa
mshapiro.test(t(setosa[,1:4])) # p-valor = 0.07906
mshapiro.test(t(virginica[,1:4])) # p-valor = 0.007955
mshapiro.test(t(versicolor[,1:4])) # p-valor = 0.005739
# aunque al ser por poco a lo mejor tampoco es normal, por otra parte si eliminamos la observación atípica 
# de la especie virgínica a lo mejor esta es normal.

mshapiro.test(t(virginica[c(1:6,8:50),1:4])) # p-valor = 0.008909
# Sigue fallanndo la normalidad

# Las gráficas de puntos (scatter plots) se pueden obtener con: 
plot(d[,1:4])

# Para ver si estos grupos se deben a las distintas especies podemos hacer: 
plot(d$Sepal.Length,d$Sepal.Width,pch=as.integer(d$Species))
legend("bottomright",legend=c('setosa','versic.','virgin.'),pch=1:3,cex=0.8)

# Si preferimos diferenciar los grupos por colores podemos hacer:
plot(d$Sepal.Length,d$Sepal.Width,pch=20,xlab='Long. sépalo',ylab='Anchura sépalo')
points(versicolor$Sepal.Length,versicolor$Sepal.Width,pch=20,col='red')
points(virginica$Sepal.Length,virginica$Sepal.Width,pch=20,col='blue')
legend('bottomright',legend=c('setosa negro','versic. rojo','virgin. azul'),cex=0.5)

# En estos gráficos no se aprecian observaciones atípicas. Para detectarlas podemos usar la distancia de Mahalanobis a la media. Al usar datos reales tendremos que usar los vectores de medias y las
# matrices de covarianzas muestrales (ya que las teóricas son desconocidas)
Msetosa<-mahalanobis(setosa[,1:4],colMeans(setosa[,1:4]),cov(setosa[,1:4]))
sort(Msetosa) # -> Las lineas atípicas son las de las líneas 42 y 44
# Si queremos señalar a la flor 42 en el gráfico de la izquierda añadiremos el comando:
text(d[42,1],d[42,2],'42',cex=0.8, pos=3)

# La verosimilitud de un dato en un modelo estadístico será el valor de la función de densidad
# (función de probabilidad si es un modelo discreto) en ese punto
mu1<-colMeans(setosa[,1:4])
V1<-cov(setosa[,1:4])
dmvnorm(d[42,1:4],mu1,V1) # -> 0.03666644

# Calculo la verosimilitud de las otras especies: 
mu2<-colMeans(versicolor[,1:4])
V2<-cov(versicolor[,1:4])
dmvnorm(d[42,1:4],mu2,V2)

mu3<-colMeans(virginica[,1:4])
V3<-cov(virginica[,1:4])
dmvnorm(d[42,1:4],mu3,V3)
#  (1.428739 · 10−11 y 3.41577 · 10−27, respectivamente)

# 4. Guardar y leer objetos y datos:

# El programa tiene un directorio activo que es donde realizará todas las operaciones
# por defecto (si no se le indica otra cosa). Si queremos cambiarlo debemos seleccionar en el menú
# superior:
#   Session>Set Working Directory>Choose Directory
# También se puede hacer con el comando setwd(’c:/nombre’) indicando la “ruta” completa del disco c. 

# Si queremos guardar el objeto d (constante, vector, dataframe, etc.) en ese directorio usaremos
dump('d','DatosPractica1.R')
# Ahora podemos borrarlo con 
rm(d)
# Tecleando d vemos que este objeto ahora no contiene ningún dato. Para recuperarlo podemos usar:
source('DatosPractica1.R')

# Otra forma de hacer lo de arriba es la siguiente: "pinchar en la ventana inferior derecha sobre el archivo. De esta forma se abre una nueva ventana
# (script) y si marcamos el comando y lo ejecutamos con Run, se cargará ese objeto."

ls()  # Para ver el conjunto de objetos activos

# Si queremos guardarlos todos:
dump(ls(),'ObjetosP1.R')

# Podemos borrarlos todos sunado la escoba o
rm(list=ls())

# Se pueden recuperar con:
source('ObjetosP1.R')


# Guardar y leer los datos d como un fichero txt con:
write.table(d, file='Prueba.txt')
d1<-read.table(file='Prueba.txt', header=TRUE)

# Para leer ficheros .txt y .csv podemos utilizar las siguientes funciones:
# read.csv()
# read.csv2


" En estas opciones hay que indicar el fichero, header=TRUE o FALSE según si la primera fila contiene
o no encabezamientos (nombres de las columnas), sep= indicando cómo se separan los datos en
el fichero (espacios en blanco, tabulador, coma, punto y coma, etc.), dec= indicando qué carácter
se utiliza para la separación de los decimales y fill= indicando si el fichero tiene celdas en blanco
(TRUE)."

# Por ejemplo, podemos leer el fichero: binary.csv (descargar del aula virtual)
"Recuerde que primero debemos descargar este fichero en el directorio activo. Para cambiar el
directorio activo usar: Session>Set Working Directory>Choose Directory. Este fichero se usará en
prácticas posteriores."

datos<- read.csv('binary.csv',header=TRUE)

# Otra forma de leer ese fichero : datos <- read.csv("C:/Users/TU_USUARIO/Downloads/binary.csv", header = TRUE)
# Hay que poner la ruta donde esté el fichero

# Para leer ficheros con extensión .rda
load('decatlon.rda')

# También se pueden leer con: File > Open File.



# Pregunta examen: haz el test de normalidad quitando la fila 7:
# d[c(1:6,8:50), 1:4] -> con la 'c' seleccionas las columnas

