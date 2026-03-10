
load("decatlon.rda")
summary(d)  # Escalas distintas

boxplot(d)
View(d)

ds <- as.data.frame(scale(d[,1:11]))
boxplot(ds)

n<-34
PCA<-princomp(d[,1:11],cor=TRUE)
PCA$loadings

'1º comp: malos atletas en general
 2º comp: atletas fuertes '

S<-PCA$scores 
Y1<-S[,1]
Y2<-S[,2]

biplot(PCA,pc.biplot=TRUE,xlabs=1:n,cex=0.5)  # Este está bien
plot(Y1,Y2,pch=20)  # Este plot no me gusta

# -------------- K-means ----------------

set.seed(1234)
CA1<-kmeans(ds,centers = 2,nstart = 10)
CA1
'Grupos de 7 y 27 atletas'

C1<-CA1$centers[1,]
C2<-CA1$centers[2,]

Y<-CA1$cluster
d1<-data.frame(ds,Y)
View(d1)

plot(Y1,Y2,col=CA1$cluster,pch=20)  # Grupos por colores
text(Y1,Y2-0.2,labels=row.names(d),cex=0.5)

library(cluster)
clusplot(ds,CA1$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0) # Máxima varianza

library(fpc)
plotcluster(ds,CA1$cluster) # Esto es feo y no le gusta al tio

CA1$withinss

1 - (CA1$tot.withinss)/(CA1$totss)
'Con dos grupos la “variabilidad” se reduce un 29.36486 %'

# ------------------ Jerárquico ----------------

D <- dist(ds, method = 'euclidean') 
M<-as.matrix(D)[1:n,1:n]

CA2<- hclust(D,method='complete')
grupos<-cutree(CA2, k=2)
sum(Y==grupos)
table(Y,grupos)

d2<-data.frame(ds,grupos)
View(d2)

plot(Y1,Y2,pch=as.integer(grupos),col=grupos,cex=0.7)

plot(CA2,cex=0.8,main='Dendograma',ylab='Distancia',xlab='Observaciones',sub='')
abline(h=10,col='red')
'Estados más similares: 12 y 15'

library(NbClust)
NbClust(ds,method='complete',index='all')$Best.nc
'* Among all indices:                                                
* 12 proposed 2 as the best number of clusters 
* 4 proposed 3 as the best number of clusters 
* 3 proposed 4 as the best number of clusters 
* 1 proposed 5 as the best number of clusters 
* 2 proposed 10 as the best number of clusters 
* 1 proposed 12 as the best number of clusters 
* 1 proposed 15 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  2'


library(cluster)
gap<-clusGap(ds,FUNcluster=kmeans,K.max=15)
gap

dev.off()
plot(gap$Tab[,3],type='b',xlab='k',ylab='GAP')
library(ggplot2)
library(factoextra)
fviz_gap_stat(gap)
'Sale K=1 por mucho, no obstante como en el NbClust nos aparece K=2 respaldado por varios métodos,
 nos quedamos con dos grupos para el conjunto.'

# ----------- Análisis discriminante lineal ---------------

'Repetimos el summary para ver como son los datos'
summary(d)

View(d1)

library(MASS)
LDA<-lda(d1[,1:11], d1$Y, prior= c(0.5,0.5))
LDA

P<-predict(LDA,d1[,1:11])
P$class

Resumen<-P$class==d1$Y  # Comparación del cluster con el LDA
Resumen   # El atleta 26 es el único mal clasificado

table(d1$Y, P$class) # Matriz de confusión (no sirve pero la hacemos)
# Comprobamos que efectivamente solo hay un fallo

LDACV<-lda(d1[,1:11], d1$Y, prior= c(0.5,0.5), CV=TRUE)
LDACV

# Matriz de confusión realista
table(d1$Y,LDACV$class)  
LDACV$class==d1$Y   # 5 errores, esta es la que tomamos como verdadera

sum(d1$Y == LDACV$class) / 34 # Eficiencia del 85.29412 % 

d1<-data.frame(d1,LDACV$class)
View(d1)

'Hay discrepancias entre el LDA y el cluster de 5 atletas que clasifican
 en distintos grupos con el método de Kmeans y el LDA. '

# ------------ Análisis discriminante cuadrático -------------

QDA<-qda(d1[,1:11],d1$Y,prior=c(0.5,0.5))
QDA$ldet
QDA$scaling

PQ<-predict(QDA,d1[,1:11])  
PQ$class  

table(d1$Y,PQ$class) 

QDACV<-qda(d1[,1:11],d1$Y,prior=c(0.5,0.5),CV=TRUE)
QDACV
table(d1$Y,QDACV$class)  

'El QDA te exige que tanto el Grupo 1 como el Grupo 2 tengan al menos 12 atletas cada uno,
 porque tenemos 11 variables y los grupos deben ser mayores de ese número.
 Dado que con el cluster nos ha salido que los grupos tienen 7 y 27 atletas,
 podemos concluir que la razón de que no podamos ejecutar el método es esa.'

# ------------ COMPROBACIÓN DEL LDA CON MAHALANOBIS -------------

'Calculamos las matrices de covarianzas de cada grupo'
d1_G1 <- d1[d1$Y == 1, 1:11] # Datos del grupo 1 (7 atletas)
d1_G2 <- d1[d1$Y == 2, 1:11] # Datos del grupo 2 (27 atletas)

S1 <- cov(d1_G1)
S2 <- cov(d1_G2)

'Fórmula: ((n1 - 1)*S1 + (n2 - 1)*S2) / (n1 + n2 - 2)'
S <- (6 * S1 + 26 * S2) / 32

m1 <- colMeans(d1_G1)
m2 <- colMeans(d1_G2)

'Investigamos a un atleta mal clasificado en la Validación Cruzada'
'Por ejemplo, veamos a qué distancia está el atleta de la fila 3 de cada centro'
D1_atleta <- mahalanobis(d1[, 1:11], m1, S) 
D2_atleta <- mahalanobis(d1[, 1:11], m2, S) 
D1_atleta
D2_atleta

d1<-data.frame(d1,D1_atleta,D2_atleta)
View(d1)

c(Dist_Grupo1 = D1_atleta, Dist_Grupo2 = D2_atleta)
'Aquí verás que la distancia matemática al grupo erróneo es menor que al suyo propio, 
 justificando el error del modelo.'

# ----------------------------------------

'Hacemos el gráfico de la proyección LDA'
'Como hay 2 grupos, el LDA proyecta en K-1 = 1 dimensión (una línea recta)'
plot(P$x, d1$Y, col = d1$Y, xlab = "LD1 (Coordenada Discriminante)", ylab = "Grupo", pch=20)
legend("topright", legend = c("Grupo 1", "Grupo 2"), col = 1:2, pch = 20, cex=0.7)



