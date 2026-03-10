
data("iris")
d <- iris
View(d)

summary(d)
# 1. Graficas
boxplot(d[, 2] ~ d$Species)
'Respuesta 1.1: Setosa tiene la mediana más alta, las cajas de 
 Versicolor y Virginica se solapan bastante. Por tanto no sirve para separar bien.'


plot(d[,1], d[,2], pch=as.integer(d$Species), col=d$Species)
legend("topright", legend=levels(d$Species), pch=1:3)
'Respuesta 1.2: Setosa está separada, pero las flores 
 Versicolor y Virginica están mezcladas'


# Diferencia de especies
'Respuesta 1.3: La Setosa se tiene sépalo corto pero ancho. 
 Virginica y Versicolor están juntas en el centro, es decir, tamaño medio de sépalo,
 aunque Virginica tiende a sépalos más largos.'


points(6,3) # Flor con valores x=6 y=3
'Respuesta 1.4: Viendo el gráfico, el punto (6, 3) cae donde se 
 mezclan Versicolor y Virginica. De hecho coincide con una flor de Virginica
 ¿Fiable? NO, la clasificación a ojo aquí es incierta por el solapamiento.'


PCA<-princomp(d[,1:4],cor=TRUE)
PCA
biplot(PCA,pc.biplot=TRUE,xlabs=d$Species,cex=0.7)
'Respuesta 1.5: Si se separan mejor con las cuatro variables. La componente 1 separa a la 
 Setosa de las otras dos. La componente 2 (eje Y) ayuda a desempatar un poco 
 entre Versicolor y Virginica, aunque sigue habiendo una pequeña frontera difusa.'


'Según el gráfico las Setosa serán de un pétalo mucho menor que las otras, 
 y las virginica tienen un sepalo algo más ancho que las versicolor.'



# 2. DA

# 2.1 LDA
library(MASS)
LDA<-lda(d[,1:4], d[,5], prior= c(1/3,1/3,1/3))
LDA

P<-predict(LDA,d[,1:4])
P$class

z<-c(2,1,2,1)
predict(LDA,z)
'Se clasifica como versicolor y las probs. a posteriori son:
 $posterior
           setosa   versicolor    virginica
 [1,] 9.889334e-22  0.9999913   8.696414e-06

 ¿Es fiable? Logicamente si, hay una probabilidad del 99.99%. (>90%)'


# 2.2 LDACV
LDACV<-lda(d[,1:4], d[,5], prior= c(1/3,1/3,1/3), CV=TRUE)
LDACV

# Matriz de confusión
t<-table(d$Species,LDACV$class)  # (Real, Prediccion)
t

sum(d$Species == LDACV$class) / 150
'Probabilidad global de acierto (eficiencia): 0.98'

'Prob. de acierto para una flor virgínica:'
t["virginica", "virginica"] / sum(t["virginica", ]) 
# 49 / 50 = 0.98 (98%)

'Prob. de acierto para una flor CLASIFICADA como virgínica:'
t["virginica", "virginica"] / sum(t[, "virginica"]) 
# 49 / 51 = 0.9607843 (96%)


# 2.3 Flores mal clasificadas LDA
LDACV$class==d$Species
'Flor 71, 84 y 134 mal clasificadas'
mal_clasif_lda<-c(71,84,134)
d$Species[mal_clasif_lda]   # Real: versicolor versicolor virginica
LDACV$class[mal_clasif_lda] # Clasif: virginica  virginica  versicolor 


# 2.4 QDA
QDA<-qda(d[,1:4],d$Species,prior=c(1/3,1/3,1/3))
QDA

PQ<-predict(QDA,d[,1:4])  
PQ$class  

predict(QDA,z)  
'Clasifica como virginica y las probs. a posteriori:
 $posterior
           setosa   versicolor    virginica
 [1,] 2.363374e-25  1.971855e-05  0.9999803'


# 2.5 QDACV
QDACV<-qda(d[,1:4],d$Species,prior=c(1/3,1/3,1/3),CV=TRUE)
QDACV

tq<-table(d$Species,QDACV$class) 
tq

sum(d$Species == QDACV$class) / 150
'Probabilidad global de acierto (eficiencia): 0.9733333'

'Prob. de acierto para una flor virgínica:'
tq["virginica", "virginica"] / sum(tq["virginica", ]) 
# 49 / 50 = 0.98 (98%)

'Prob. de acierto para una flor CLASIFICADA como virgínica:'
tq["virginica", "virginica"] / sum(tq[, "virginica"]) 
# 49 / 52 = 0.9423077 (94%)


# 2.6 Flores mal clasificadas QDA
QDACV$class==d$Species
'Flor 69, 71, 84 y 134 mal clasificadas'
mal_clasif_qda<-c(69,71,84,134)
d$Species[mal_clasif_qda]   # Real: versicolor versicolor versicolor virginica 
QDACV$class[mal_clasif_qda] # Clasif: virginica virginica  virginica  versicolor


# 2.7 Grafico de las proyecciones canónicas en LDA
P
plot(P$x, col=d$Species, pch=20, ylim=c(-5, 5))
'No pongo y = d$Species porque son >2 grupos.
 Y hay que borrar lo de xlim e ylim, lo uso porque la z está fuera de los límites'

legend("topright", legend=levels(d$Species), col=1:3, pch=20)
'Los grupos se sitúan: Setosa aislada a la derecha, Versicolor en el centro, Virginica a la izquierda.
 Es decir, setosa muy por encima de la media'

# Añadir el punto z al gráfico
P_z<-predict(LDA,z)
P_z$x
points(P_z$x[1,1], P_z$x[1,2])
# Si no funciona pongo los números directamente:
# points(-1.914549,3.473402)
'El punto Z se sitúa en el medio izquierda, justo encima del grupo  
 de la especie Versicolor. '



# 3. Cluster

# 3.1 Datos estandarizados y kmeans
ds <- as.data.frame(scale(d[,1:4]))
set.seed(7654)
CA1<-kmeans(ds,centers = 3,nstart = 10)
CA1
'Grupos de tamaño 53, 50 y 47'


# 3.2 Grupos de las flores 2 y 122
Y<-CA1$cluster
ds_clasif<-data.frame(ds,Y)
View(ds_clasif)
Y[2]    # Grupo 2 que se corresponderia con la especie setosa
Y[122]  # Grupo 1 que se corresponderia con versicolor

table(d$Species, CA1$cluster) 
'Con esto saco que es cada grupo, el 1 los versicolor, el 2 setosa, el 3 virginica'


# 3.3 Reducción de variabilidad
CA1$tot.withinss # Intra-grupos -> 138.8884
CA1$totss   # Sin grupos -> 596
1 - (CA1$tot.withinss)/(CA1$totss) # = 0.7669658
'Con tres grupos la “variabilidad” se reduce un 76.69658 %'


# 3.4 Grafico componentes principales
library(cluster)
clusplot(ds,CA1$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0)
PCA$loadings
'Vemos con loadings: Comp1 petalo grande y sepalo largo
                     Comp2 sepalo ancho'
# Otra opción: plot(PCA$scores[,1], PCA$scores[,2], col=CA1$cluster, pch=20)
'Setosa flores pequeñas, versicolor tamaño medio y sepalo fino, virginica flor grande y sepalo ancho'
View(ds_clasif)


# 3.5 Cluster vs reales
table(d$Species,ds_clasif$Y)
'El que mas se parece a versicolor es el grupo 1'
'Flores en comun: 39/50'
ds_clasif<-data.frame(ds_clasif,d$Species)
View(ds_clasif)
'La flor 51, 52 y 53 están clasificadas en otro grupo y realmente son versicolor'







