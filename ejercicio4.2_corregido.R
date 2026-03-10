
# Ejercicio 6.2
'2. Aplicar un análisis cluster al fichero heptathlon incluido en el paquete MVA clasificando a las atletas
 en dos grupos (usar help para ver las descripciones de las variables).'

library(MVA)
data("heptathlon")
d <- heptathlon

d$hurdles <- max(d$hurdles) - d$hurdles
d$run200m <- max(d$run200m) - d$run200m
d$run800m <- max(d$run800m) - d$run800m

d <- d[, 1:7]
View(d) # Ya está arreglado

'Vamos a aplicar un cluster al paquete pero antes vamos a hacer un PCA para ver los datos'
summary(d)  # Distintas escalas
boxplot(d)

ds <- as.data.frame(scale(d)) # Estandarizo datos
plot(ds,pch=20)
boxplot(ds)
View(ds)

n<-25   # No se que n tiene, lo he contado y hay 25 países
PCA<-princomp(d,cor=TRUE)
PCA$loadings
'Veo los loadings para ver los Comp1 y Comp2
 Veo que la primera componente son los deportes de correr y saltar
 y la componente 2 son los de tiro sobre todo el tiro con jabalina'

S<-PCA$scores   # Guardo las componentes principales
Y1<-S[,1]
Y2<-S[,2]

plot(Y1,Y2,pch=20)  # Este plot no me gusta
text(Y1,Y2-0.2,labels=row.names(d),cex=0.5,col='red') 

'Dado que he hecho un cambio de los valores iniciales, ahora las componentes principales cambian 
 y por tanto también cambian los valores de la gráfica'



set.seed(1234)  
CA1<-kmeans(ds,centers = 2,nstart = 10)
CA1
'2 grupos de 18 y 7, con centroides:
1 -0.4442044  0.4447735  0.3766005 -0.4111614  0.454180  0.2042687 -0.2466638  0.4705811
2  1.1422400 -1.1437034 -0.9684014  1.0572721 -1.167892 -0.5252624  0.6342783 -1.2100657'

C1<-CA1$centers[1,]
C2<-CA1$centers[2,]

Y<-CA1$cluster
d1<-data.frame(ds,Y)
View(d1)

plot(Y1,Y2,col=CA1$cluster,pch=20)  # Con este se ven los grupos por colores
text(Y1,Y2-0.2,labels=row.names(d),cex=0.5)

library(cluster)
clusplot(ds,CA1$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0) # Máxima varianza

library(fpc)
plotcluster(ds,CA1$cluster) 

dE2<-function(x,C) (sum((x-C)*(x-C)))
dC1<-1:n
for (i in 1:n) dC1[i]<-dE2(ds[i,],C1)
sum(dC1*(Y==1))
dC2<-1:n
for (i in 1:n) dC2[i]<-dE2(ds[i,],C2)
sum(dC2*(Y==2))

CA1$withinss  # Sale bien, puedo guardarlo en el dataframe

d1<-data.frame(d1,dC1,dC2)
View(d1)

1 - (CA1$tot.withinss)/(CA1$totss) # = 0.4751918
'Frase examen: con dos grupos la “variabilidad” se reduce un 38.83435 %'



D <- dist(ds, method = 'euclidean') 
M<-as.matrix(D)[1:25,1:25] 
M[1:5,1:5] 
heatmap(M)

CA2<- hclust(D,method='complete')
grupos2<-cutree(CA2, k=2)
sum(Y==grupos2)
table(Y,grupos2)

d2<-data.frame(ds,grupos2)
View(d2)

plot(Y1,Y2,pch=as.integer(grupos2),col=grupos2,cex=0.7)

plot(CA2,cex=0.8,main='Dendograma',ylab='Distancia',xlab='Observaciones',sub='')
abline(h=10,col='red')
'Atletas más similares: Fleming (AUS) - Dimitrova (BUL)'

'Sale todo igual, salvo que al revés, los atletas más similares si coinciden.
 Basicamente solo cambia en los plots.'



library(NbClust)
NbClust(ds,method='complete',index='all')$Best.nc
'Por la gráfica parece que va a salir K=2 o K=4.
* Among all indices:                                                
* 9 proposed 2 as the best number of clusters 
* 2 proposed 3 as the best number of clusters 
* 7 proposed 4 as the best number of clusters 
* 2 proposed 6 as the best number of clusters 
* 1 proposed 10 as the best number of clusters 
* 1 proposed 11 as the best number of clusters 
* 2 proposed 15 as the best number of clusters 

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  2 
 
 Efectivamente, parece que esos dos valores son los grupos que podemos usar'

library(cluster)
gap<-clusGap(ds,FUNcluster=kmeans,K.max=15)
gap

dev.off()
plot(gap$Tab[,3],type='b',xlab='k',ylab='GAP')
library(ggplot2)
library(factoextra)
fviz_gap_stat(gap)
'De nuevo, K=2 o K=4'



# Arreglo quitando el atleta Launa (PNG)

'Veamos hay un valor atípico: Launa(PNG) está lejísimos de todos los valores, 
 está en la esquina de la gráfica de las componentes principales, por tanto,
 es lenta y tiene buen salto, por lo visto también es buena con la jabalina.
 Probamos con K=4'

set.seed(1234)  
CA1<-kmeans(ds,centers = 4,nstart = 10)
CA1
'4 grupos de 1, 6, 5 y 13 -> Mismos grupos'

C1<-CA1$centers[1,]
C2<-CA1$centers[2,]
C3<-CA1$centers[3,]
C4<-CA1$centers[4,]

Y<-CA1$cluster
d1<-data.frame(ds,Y)
View(d1)

plot(Y1,Y2,col=CA1$cluster,pch=20)  # Con este se ven los grupos por colores
text(Y1,Y2-0.2,labels=row.names(d),cex=0.5)

library(cluster)
clusplot(ds,CA1$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0) # Máxima varianza

library(fpc)
plotcluster(ds,CA1$cluster) 

dE2<-function(x,C) (sum((x-C)*(x-C)))
dC1<-1:n
for (i in 1:n) dC1[i]<-dE2(ds[i,],C1)
sum(dC1*(Y==1))
dC2<-1:n
for (i in 1:n) dC2[i]<-dE2(ds[i,],C2)
sum(dC2*(Y==2))
'Paso de hacer el resto'

CA1$withinss  # Sale bien

1 - (CA1$tot.withinss)/(CA1$totss)
'Frase examen: con dos grupos la “variabilidad” se reduce un 70.23248 %'


CA2<- hclust(D,method='complete')
grupos2<-cutree(CA2, k=4)
sum(Y==grupos2)
table(Y,grupos2)

plot(Y1,Y2,pch=as.integer(grupos2),col=grupos2,cex=0.7) 

'Da distinto al Kmeans en 3 elementos del grupo 3.
 Es posible que si elimino al dato atípico tengamos la solución'

# ---------------------------------

boxplot(d)
boxplot(ds)
boxplot(ds[1:24,]) 
'Claramente si eliminamos el valor atípico de Launa(PNG) entonces podemos 
 ver que los grupos son mayores'

'Voy a ver quitando ese valor cuantos grupos debo coger'
library(NbClust)
NbClust(ds[1:24,],method='complete',index='all')$Best.nc

'* Among all indices:                                                
* 5 proposed 2 as the best number of clusters 
* 9 proposed 3 as the best number of clusters 
* 1 proposed 4 as the best number of clusters 
* 1 proposed 5 as the best number of clusters 
* 1 proposed 9 as the best number of clusters 
* 1 proposed 11 as the best number of clusters 
* 5 proposed 15 as the best number of clusters  

                   ***** Conclusion *****                            
 
* According to the majority rule, the best number of clusters is  3 

Al eliminarlo vemos que se necesitan K=3 grupos'

set.seed(1234)  
CA1<-kmeans(ds[1:24,],centers = 3,nstart = 10)
CA1
'3 grupos de 13, 6, 5'

C1<-CA1$centers[1,]
C2<-CA1$centers[2,]
C3<-CA1$centers[3,]

Y<-CA1$cluster
d1<-data.frame(ds[1:24,],Y)
View(d1)

plot(Y1[1:24],Y2[1:24],col=CA1$cluster,pch=20)  # Con este se ven los grupos por colores
text(Y1[1:24],Y2[1:24]-0.2,labels=row.names(d),cex=0.5)

library(cluster)
clusplot(ds[1:24,],CA1$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0) # Máxima varianza

library(fpc)
plotcluster(ds[1:24,],CA1$cluster) 

n<-24
dE2<-function(x,C) (sum((x-C)*(x-C)))
dC1<-1:n
for (i in 1:n) dC1[i]<-dE2(ds[i,],C1)
sum(dC1*(Y==1))
dC2<-1:n
for (i in 1:n) dC2[i]<-dE2(ds[i,],C2)
sum(dC2*(Y==2))
'Paso de hacer el resto'

CA1$withinss  # Sale bien

1 - (CA1$tot.withinss)/(CA1$totss)
'Frase examen: con dos grupos la “variabilidad” se reduce un 57.39401 %'


D <- dist(ds[1:24,], method = 'euclidean') # Es importante fijarse en qué distancia se usa
M<-as.matrix(D)[1:24,1:24]

CA2<- hclust(D,method='complete')
grupos2<-cutree(CA2, k=3)
sum(Y==grupos2)
table(Y,grupos2)

plot(Y1[1:24],Y2[1:24],pch=as.integer(grupos2),col=grupos2,cex=0.7)

plot(CA2,cex=0.8,main='Dendograma',ylab='Distancia',xlab='Observaciones',sub='')
abline(h=5,col='red')

# Sale casi el mismo dendograma, es un buen cambio para eliminar un grupo












