# -----------------------------------------------------------------------------
# PRÁCTICA 6: ANÁLISIS CLUSTER
# -----------------------------------------------------------------------------

# 1. Estudio inicial de los datos

d<-USArrests
help("USArrests")
View(d)

summary(d)  # Escalas diferentes (hay que estandarizar los datos)
cov(d)      # Las cuasi-varianzas son la diagonal de la matriz
colMeans(d) # Vector media muestral

plot(d)
boxplot(d)  # El único distinto, y en el que aparecen los valores atípicos

require(graphics)
pairs(d, panel = panel.smooth, main = 'USArrests',pch=20) # Literalmente igual que plot

ds <- as.data.frame(scale(d))
plot(ds,pch=20) # Otra vez identico

View(ds) # Busco los valores atípicos (un poco coñazo)

n<-50
PCA<-princomp(d,cor=TRUE)
L<-PCA$loadings
'Vemos los loadings y decimos qué son las componentes principales
 Comp1: La criminalidad de los estados (positivo)
 Comp2: Lo urbano que es la ciudad (+urbano = -puntuacion)'
S<-PCA$scores
Y1<-S[,1]
Y2<-S[,2]
biplot(PCA,pc.biplot=TRUE,cex=0.5)  # Nombres de estados 
biplot(PCA,pc.biplot=TRUE,xlabs=1:n,cex=0.5)  # Nos da números
plot(Y1,Y2,pch=20)  # Nos da puntos | Están estandarizados porque el PCA es con correlaciones que estandariza los datos
text(Y1,Y2-0.2,labels=row.names(d),cex=0.5,col='red') # Ponemos nombre a los puntos
'Se ven dos nubes de puntos en base a un mayor número de arrestos'

# -----------------------------------------

# 2. Análisis cluster con Kmeans
'Este método de CA pide indicar K (nº de cluster). Tomamos K=2 para detectar más detenciones.'

set.seed(1234)  # Lo aplicamos a los datos estandarizados:
CA1<-kmeans(ds,centers = 2,nstart = 10)
'Hemos indicado que calcule dos grupos y realice el algoritmo 10 veces al azar'
CA1
' Dos grupos de 20 y 30 con centroides:
1  1.004934  1.0138274  0.1975853  0.8469650
2 -0.669956 -0.6758849 -0.1317235 -0.5646433
Observa que los del primer grupo tienen más arrestos que los del segundo'

C1<-CA1$centers[1,] # Guardo los centroides
C2<-CA1$centers[2,]

Y<-CA1$cluster   # Nos da la clasificación
d1<-data.frame(ds,Y)  # Datos estandarizados y la clasificación con cluster sobre esos datos
View(d1)

# Representación de los grupos
plot(ds[,1:2],col=CA1$cluster,pch=20)   # Da lo mismo usar ds que d1 (solo hay una columna de diferencia)
plot(ds[,3:4],col=CA1$cluster,pch=20)
plot(ds,col=CA1$cluster,pch=20)         # Vemos todos los plots
plot(Y1,Y2,col=CA1$cluster,pch=20)      # Este es el plot de las componentes principales (es el mejor plot)
text(Y1,Y2-0.2,labels=row.names(d),cex=0.5)
'Las variables representando arrestos tienen valores grandes en el grupo primero 
 pero que la variable población urbana no cambia mucho en cada grupo.
 La primera componente sí sirve para discriminar'

plot(Y1,Y2,pch=as.integer(CA1$cluster),col=CA1$cluster)  # Igual pero con figuras
legend('topleft',legend=c('cluster1','cluster2'),pch=1:2,cex=0.6)

# Añadir centroides al gráfico: (Es útil para verlo, pero vamos que prefiero componentes)
plot(ds[,1:2],col=CA1$cluster,pch=20)
points(C1[1],C1[2],col='green')
text(C1[1],C1[2]+0.2,'C1',col='green')
points(C2[1],C2[2],col='green')
text(C2[1],C2[2]+0.2,'C2',col='green')
plot(ds[,3:4],col=CA1$cluster,pch=20)
points(C1[3],C1[4],col='green')
text(C1[3],C1[4]+0.2,'C1',col='green')
points(C2[3],C2[4],col='green')
text(C2[3],C2[4]+0.2,'C2',col='green')

# Queremos uno con las componentes principales, además añade los círculos
library(cluster)
clusplot(ds,CA1$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0) # Máxima varianza

# También podemos hacer uno con las funciones discriminantes
library(fpc)
plotcluster(ds,CA1$cluster) # Máxima separación
'Coge esas etiquetas recién creadas con Kmeans (CA1$cluster) y se las cree.
 Aplica el LDA y busca la recta que maximiza la separación exacta
 El PCA muestra la forma natural, el LDA muestra la perspectiva para ver más separación'

CA1$withinss  # Suma de las distancias al cuadrado de todos los puntos a los centroides

dE2<-function(x,C) (sum((x-C)*(x-C)))
n<-50
dC1<-1:n
for (i in 1:n) dC1[i]<-dE2(ds[i,],C1)
sum(dC1*(Y==1))
dC2<-1:n
for (i in 1:n) dC2[i]<-dE2(ds[i,],C2)
sum(dC2*(Y==2))
# Con esto comprobamos que da el mismo resultado (necesario para ver la distancia a los centroides)

d1<-data.frame(d1,dC1,dC2)
# Ahora el dataframe tiene los datos estandarizados, con el grupo, y la distancia al centroide
View(d1)  # Podemos ver que se han clasificado correctamente con las distancias a los centroides

CA1$tot.withinss # La suma de CA1$withinss

CA1$totss   # Suma de las distancias al cuadrado sin grupos 
1 - (CA1$tot.withinss)/(CA1$totss) # = 0.4751918
'Frase examen: con dos grupos la “variabilidad” se reduce un 47.51918 %'

# ----------------- TAREA -------------------
'Haz un estudio similar con K=3 o K=4'

CA_K3<-kmeans(ds,centers = 3,nstart = 10)
CA_K3   # 3 grupos: 13, 17 y 20 | Los centroides son:
'1 -0.9615407 -1.1066010 -0.9301069 -0.9667633
 2 -0.4469795 -0.3465138  0.4788049 -0.2571398
 3  1.0049340  1.0138274  0.1975853  0.8469650'

C1_K3<-CA_K3$centers[1,]
C2_K3<-CA_K3$centers[2,]
C3_K3<-CA_K3$centers[3,]

Y_K3<-CA_K3$cluster   # Clasificación

d1_K3<-data.frame(ds,Y_K3)
View(d1_K3)

plot(d1_K3,col=CA_K3$cluster,pch=20)  # Todos los plots (renta pero son muchos)
plot(Y1,Y2,col=CA_K3$cluster,pch=20)  # Con componentes principales renta mucho más
'G1: menos detenciones y rural, G2: detenciones medias y urbanizado, G3: muchos crímenes'

clusplot(ds,CA_K3$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0) # Máxima varianza
'Se cruzan dos de las elipses'

plotcluster(ds,CA_K3$cluster) # Aquí se ven los grupos más separados
'El G3 es muy distinto a los otros dos, el G1 y G2 tienen cosas en común pero se diferencian en algo secundario
 Esto ya lo sabemos por el plot de componentes principales, G3 mas crimenes
 G1 y G2 menos crímenes, ¿diferencia? -> obviamente que G1 es más rural y G2 urbano'

CA_K3$centers # La mejor forma de diferenciar grupos !!!
'La otra interpretación que puedo hacer es que mire los centroides de cada grupo
 Y de esta forma ver qué diferencia un grupo de otro'

CA_K3$withinss

# Esto es un poco porro pero es para calcular las distancias de todos los estados al centroide

# dE2<-function(x,C) (sum((x-C)*(x-C)))
n<-50
dC1_K3<-1:n
for (i in 1:n) dC1_K3[i]<-dE2(ds[i,],C1_K3)
sum(dC1_K3*(Y_K3==1))
dC2_K3<-1:n
for (i in 1:n) dC2_K3[i]<-dE2(ds[i,],C2_K3)
sum(dC2_K3*(Y_K3==2))
dC3_K3<-1:n
for (i in 1:n) dC3_K3[i]<-dE2(ds[i,],C3_K3)
sum(dC3_K3*(Y_K3==3))
# Con esto comprobamos que da el mismo resultado que CA_K3$withinss !!!

d1_K3<-data.frame(d1_K3,dC1_K3,dC2_K3, dC3_K3)  # Datos estandarizados, con el grupo, y la distancia al centroide
View(d1_K3)  # Podemos ver que se han clasificado correctamente con las distancias a los centroides

CA_K3$tot.withinss
CA_K3$totss 
1 - (CA_K3$tot.withinss)/(CA_K3$totss) # = 0.6003915
'Frase examen: con dos grupos la “variabilidad” se reduce un 60.03915 %'

'Resumen del dataframe d1_K3: NO LO QUEREMOS PARA NADA
 Solo se usa para comprobar, es decir, no queremos hacer esto, solo es útil 
 para hacer comprobaciones o si es que preguntan distancia a los centroides'

# -------------------------------------------

# 3. Análisis cluster jerarquizado
'La ventaja es que no indicamos un nº de clusters. El dendograma 
 nos muestra los grupos y nos ayuda a decidir con cuántos nos quedamos.'

D <- dist(ds, method = 'euclidean') # Es importante fijarse en qué distancia se usa
M<-as.matrix(D)[1:50,1:50]  # Lo vemos como una matriz
M[1:5,1:5]  # Distancias y representamos con mapa de calor:
heatmap(M)
heatmap(M[1:5,1:5]) # Más distancia, más oscuridad
# Los más similares son Arkansa y Alabama, y son los primeros en unirse
# Después Arizona y California se unirán

# Comprobar que todos los estados se clasifican igual que 
# en el algoritmo Kmeans excepto Missouri
CA2<- hclust(D,method='complete')
grupos2<-cutree(CA2, k=2)
sum(Y==grupos2)
table(Y,grupos2)  # Especie de tabla de confusión

grupos<-cutree(CA2, k=4)  # Cogemos 4 grupos
d2<-data.frame(ds,grupos) # Guardamos esa columna en el dataframe

plot(Y1,Y2,pch=as.integer(grupos),col=grupos,cex=0.7)   # 4 grupos
legend('topright',legend=c('Y=1','Y=2','Y=3','Y=4'),pch=1:4,cex=0.5)
text(Y1+0.15,Y2,1:n,cex=0.5)

plot(Y1,Y2,pch=as.integer(grupos2),col=grupos2,cex=0.7) # 2 grupos 
legend('topright',legend=c('Y=1','Y=2'),pch=1:2,cex=0.5)
text(Y1+0.15,Y2,1:n,cex=0.5)

# Dendograma
plot(CA2,cex=0.8,main='Dendograma',ylab='Distancia',xlab='Observaciones',sub='')
abline(h=4,col='red')   # Si corto aquí cojo 4 grupos
abline(h=5,col='green') # Si corto aquí cojo 2 grupos
'Preguntas examen: ¿Donde cortar para coger x grupos? h = y
 ¿Cuáles son los primeros en unirse? Iowa - New Hampshire
 Los dos estados más similares (y que primero se unen) son Iowa y New
 Hampshire, los siguientes Illinois y New York.
 La parte derecha del dendograma se sitúan los grupos de estados
 con más arrestos'

# ------------------------------------------

# 4. Número de grupos
'No existe un nº óptimo de grupos porque esto depende de factores subjetivos
 Existen índices para ayudarnos:'

library(NbClust)
NbClust(ds,method='complete',index='all')$Best.nc

'Esto devuelve:
-----
Among all indices:
* 9 proposed 2 as the best number of clusters
* 3 proposed 3 as the best number of clusters
* 6 proposed 4 as the best number of clusters
* 2 proposed 5 as the best number of clusters
* 3 proposed 15 as the best number of clusters
***** Conclusion *****
* According to the majority rule, the best number of clusters is 2
-----
Literalmente solo hay que fijarse en el nº de clusters más propuesto
Hay que buscar los picos (codo) y nos conducen a K=4'

library(cluster)
gap<-clusGap(ds,FUNcluster=kmeans,K.max=15)
gap

'GAP compara el total de las distancias intra-cluster en Kmeans
 para diferentes valores de K con las que se obtendrían con datos al azar'

dev.off()
plot(gap$Tab[,3],type='b',xlab='k',ylab='GAP')
library(ggplot2)
library(factoextra)
fviz_gap_stat(gap)
'Dos gráficas que indican el número óptimo de clusters, obvio es K=4'
'En resumen: NbClust es como preguntar y quedarte con el número más votado (o buscar el "codo" en su gráfica). 
 El estadístico GAP mide si tus grupos son mejores que el puro azar, y te quedas con el pico más alto.'

# ------------------------------------------

# Preguntas examen:
'Se usa validacion cruzada o no al clasificar un individuo? NO
 Se usa para quitar un individuo que ya sabe su grupo y ver si el método acierta.
 Cuando se clasifique uno no hace falta validación cruzada.
 En el examen habra que clasificar alguno

 Si no usamos la validacion cruzada al hacer tablas de confusion estamos haciendo trampas

 Hay 2 opciones usar discriminante y despues cluster y comparar, o bien,
 coger un fichero sin grupos, hacer grupos con cluster y aplicar discriminante a los grupos.
 
 '













