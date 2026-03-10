
# Ejercicio 5.3 
# 3. Aplicar un DA a los datos del fichero de R denominado iris.

data("iris")
d<-iris
View(d)

summary(d)  # Hay 3 especies

tapply(d$Sepal.Length,d$Species,summary)
'Virginica es la especie con mayor longitud de sepalo y setosa la menor'
tapply(d$Sepal.Width,d$Species,summary)
'Setosa es la de sepalo más ancho y virginica la que menos'
tapply(d$Petal.Length,d$Species,summary)
'Setosa petalo corto, virginica muy largo, versicolor algo largo'
tapply(d$Petal.Width,d$Species,summary)
'Setosa petalo fino, versicolor medio, virginica ancho'
help("iris")

# PCA, usamos correlaciones para estandarizar los datos
PCA<-princomp(d[,1:4],cor=TRUE)
PCA

biplot(PCA,pc.biplot=TRUE,xlabs=d$Species,cex=0.7) # El biplot es lo mejor
'Primera componente como de grandes son los petalos y de largos los sepalos
 Segunda componente como de anchos son los sepalos'

plot(PCA$scores[,1],PCA$scores[,2],xlab='Y1',ylab='Y2')
text(PCA$scores[,1],PCA$scores[,2],d$Species,cex=0.7,pos=4,col='red')

# ----------------------------------------

# Cargo la librería
library(MASS)
LDA<-lda(d[,1:4], d[,5], prior= c(1/3,1/3,1/3))
LDA
# Hago LDA$means y veo lo mismo que con todos los tapply !!!

P<-predict(LDA,d[,1:4])
P$class
'Vemos quiénes están en cada especie según la clasificación D-K'

Resumen<-P$class==d$Species
Resumen   # Hay 3 fallos: 71, 84, 134

# Otra opción: hacer la matriz de confusión inicial
table(d$Species, P$class) # Horizontal realidad | Vertical predicción
'Hay 50 de setosa; 48/50 de versicolor y 2 que asigna a virginica; 49/50 virginica y 1 que asigna a versicolor'

# Formula a posteriori
P$posterior # Básicamente vemos la probabilidad de que estén en cada grupo

# ----------------------------------------

# Hacemos LDA con validación cruzada
LDACV<-lda(d[,1:4], d[,5], prior= c(1/3,1/3,1/3), CV=TRUE)
LDACV
# Con validación cruzada, el LDACV es una especie de predict()

# Matriz de confusión realista
table(d$Species,LDACV$class)  # Misma fucking tabla
LDACV$class==d$Species # Segunda comprobación: 71, 84, 134 

sum(d$Species == LDACV$class) / 150   # 150 flores | Eficiencia = 0.98

# ------------- PREGUNTA TIPO EXAMEN ---------------

'¿Qué variable es la que mejor discrimina? Es decir cuál es la más importante para saber la especie'
ds<-scale(d[,1:4])
LDA_EST<-lda(ds[,1:4], d$Species, prior=c(1/3,1/3,1/3))  # Calculamos los coeficientes
LDA_EST$scaling
LDA_EST
# La mejor variable es la longitud del petalo

# ----------------------------------------

QDA<-qda(d[,1:4],d$Species,prior=c(1/3,1/3,1/3))
QDA$ldet
QDA$scaling

PQ<-predict(QDA,d[,1:4])  
PQ$class  # Misma predicción que antes con LDA

table(d$Species,PQ$class) # Misma fucking tabla

QDACV<-qda(d[,1:4],d$Species,prior=c(1/3,1/3,1/3),CV=TRUE)
QDACV
table(d$Species,QDACV$class)  # Hay 3 versicolor que se clasificarían en virginica y 1 virginica que se clasifica en versicolor

# ----------------------------------------

S1<-cov(d[1:50,1:4])            # La matriz de setosa
d1<-d[d$Species=='setosa',1:4]  # Separa los datos de setosa
cov(d1)

S2<-cov(d[51:100,1:4])            # La matriz de setosa
d2<-d[d$Species=='versicolor',1:4]  # Separa los datos de setosa
cov(d2)

S3<-cov(d[101:150,1:4])            # La matriz de setosa
d3<-d[d$Species=='virginica',1:4]  # Separa los datos de setosa
cov(d3)

'Son diferentes, no parecen estimaciones de una misma matriz V.
 Ahora vamos a mirar si los datos son normales. Cargamos "mvnormtest"'

library(mvnormtest)
mshapiro.test(t(d[1:50,1:4]))     # p-valor = 0.07  | Pasa el test de normalidad
mshapiro.test(t(d[51:100,1:4]))   # p-valor = 0.005 | No es normal
mshapiro.test(t(d[101:150,1:4]))  # p-valor = 0.007 | No es normal

plot(d[1:50, 1:4])
plot(d[51:100, 1:4])
plot(d[101:150, 1:4])

'S = 1 / (n1 + n2 + n3 - 3) · (n1 - 1)S1 + (n2 - 1)S2 + (n3 - 1)S3'
S<-(49*S1+49*S2+49*S3)/147
S

LDA$scaling

m1 <- colMeans(d1) # Centro de Setosa
m2 <- colMeans(d2) # Centro de Versicolor
m3 <- colMeans(d3) # Centro de Virginica

mahalanobis(d[84, 1:4], m1, S)  # 149.0303
mahalanobis(d[84, 1:4], m2, S)  # 8.439263
mahalanobis(d[84, 1:4], m3, S)  # 4.864465
# Se clasifica en la especie virginica 

# ----------------------------------------

'Hacemos el gráfico'
plot(P$x, col = d$Species, xlab = "LD1", ylab = "LD2", pch=20)
legend("topright", legend = levels(d$Species), col = 1:3, pch = 20, cex=0.4)  # cex=* para el tamaño

# ----------------------------------------

'f1 y f2 calculaban a altura de la campana de Gauss multivariante
 Cuando hacemos f1/(f1+f2) se calcula la probabilidad a posteriori a mano
 Y no la usamos porque ya tengo P$posterior '

# ----------------------------------------

'a2 no lo calculamos porque tenemos LDA$scaling, 
 con LD1 y LD2 tenemos los coeficientes, nadie lo hace a mano
 basicamente lo de los coeficientes de fisher y las funciones para calcular 
 la probabilidad a posteriori son una perdida de tiempo, solo sirven si te pregunta la formula teorica'


# --------------------------------------

# Ejercicio 6.1 
# 1. Aplicar un análisis cluster al fichero iris. Comprobar si los grupos obtenidos coinciden con los grupos 
# establecidos por las distintas especies (usar help para ver las descripciones de las variables).

'Aunque ya hemos echado un vistazo a los datos y hemos hecho PCA y DA
 Vamos a volver a hacer ciertas cosas para que tengamos una idea mejor'
set.seed(1234)
summary(d) # Escalas distintas

plot(d)
boxplot(d)

ds <- as.data.frame(scale(d[,1:4])) # Estandarizo los datos
plot(ds,pch=20)
boxplot(ds)
View(ds)

n<-150
PCA$loadings
'Vemos los loadings y decimos qué son las componentes principales
 Comp1: Como de larga es la flor y como de ancho el petalo
 Comp2: Como de ancho el sepalo'
S<-PCA$scores
Y1<-S[,1]
Y2<-S[,2]
plot(Y1,Y2,pch=20)  
text(Y1,Y2-0.2,labels=row.names(d),cex=0.5,col='red')
'Vemos que hay claramente dos grupos'

# Kmeans con K=3 porque hay 3 especies
CA1<-kmeans(ds,centers = 3,nstart = 10)
CA1
'Tres grupos de 53, 50 y 47 con centroides:
1  -0.05005221 -0.88042696    0.3465767   0.2805873
2  -1.01119138  0.85041372   -1.3006301  -1.2507035
3   1.13217737  0.08812645    0.9928284   1.0141287'         

# Centroides
C1<-CA1$centers[1,] 
C2<-CA1$centers[2,]
C3<-CA1$centers[3,]

Y<-CA1$cluster
ds_clasif<-data.frame(ds,Y)  # Datos estandarizados y la clasificación con cluster sobre esos datos
View(ds_clasif)

# Representación gráfica
plot(Y1,Y2,col=CA1$cluster,pch=20)  # Mejor plot (colores)
text(Y1,Y2-0.2,labels=row.names(d),cex=0.5) 

library(cluster)
clusplot(ds,CA1$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0)
'Clusplot muy bueno, componentes principales y circulos'

library(fpc)
plotcluster(ds,CA1$cluster)
'Este es con DA, basicamente se ve más fácil que hay dos grupos'

CA1$withinss

CA1$totss   # Suma de las distancias al cuadrado sin grupos 
1 - (CA1$tot.withinss)/(CA1$totss) # = 0.7669658
'Con tres grupos la “variabilidad” se reduce un 76.69658 %'

ds_clasif<-data.frame(ds_clasif,d$Species)
View(ds_clasif)
'Hay muchísimos cambios, se mezclan muchisimos de versicolor 
 y virginica que clasifican en la especie contraria'
table(d$Species,ds_clasif$Y)  # De setosa todos están bien clasficados


# Cluster jerarquizado
D <- dist(ds, method = 'euclidean') 
M<-as.matrix(D)[1:150,1:150]  
M[1:5,1:5]  
heatmap(M)
heatmap(M[1:5,1:5])

CA2<-hclust(D,method='complete')
grupos2<-cutree(CA2, k=3)
sum(Y==grupos2)
table(Y,grupos2)  
'Con el table(filas,columnas) -> vertical=filas y horizontal=columnas
 Luego 47/47 grupo3, 23/53 grupo2, 49/50 grupo1 
 Este método es bueno para clasificar la especie setosa pero falla con las otras porque mezcla especies'

plot(Y1,Y2,pch=as.integer(grupos2),col=grupos2,cex=0.7) # 2 grupos 
legend('topright',legend=c('G=1','G=2','G=3'),pch=1:3, col=1:3,cex=0.5)
text(Y1+0.15,Y2,1:n,cex=0.5)

plot(CA2,cex=0.8,main='Dendograma',ylab='Distancia',xlab='Observaciones',sub='')
abline(h=5,col='green') 
# Para coger 3 grupos


library(NbClust)
NbClust(ds,method='complete',index='all')$Best.nc
'Por la gráfica diríamos de coger una K=3
* Among all indices:                                                
* 2 proposed 2 as the best number of clusters 
* 17 proposed 3 as the best number of clusters 
* 1 proposed 5 as the best number of clusters 
* 1 proposed 12 as the best number of clusters 
* 2 proposed 15 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  3 '
# Correcto cogemos 3 propuesto por 17 métodos

library(cluster)
gap<-clusGap(ds,FUNcluster=kmeans,K.max=15)
gap

dev.off()
plot(gap$Tab[,3],type='b',xlab='k',ylab='GAP')
library(ggplot2)
library(factoextra)
fviz_gap_stat(gap)
'Según esto tal vez deberíamos coger K=2, no obstante, GAP es un método más
 y teniendo en cuenta que dice que hay 17 métodos a favor de K=3 cogeremos esa'









