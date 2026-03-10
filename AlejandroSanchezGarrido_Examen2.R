
# Examen prácticas 5 y 6 - Alejandro Sánchez Garrido

load("nota.rda")
View(d)

summary(d)

plot(d[,4], d[,5], pch=20)


PCA<-princomp(d,cor=TRUE)
PCA
biplot(PCA,pc.biplot=TRUE,cex=0.7)

PCA$loadings
Y1<-PCA$scores[,1]
Y2<-PCA$scores[,2]

# Cluster
ds <- as.data.frame(scale(d))
set.seed(7777)  
CA1<-kmeans(ds,centers = 3,nstart = 10)
CA1

Y<-CA1$cluster
ds_clasif<-data.frame(ds,Y)
View(ds_clasif)
Y[1]
Y[71]


CA1$tot.withinss # Intra-grupos -> 209.3602
CA1$totss   # Sin grupos -> 435
1 - (CA1$tot.withinss)/(CA1$totss) # = 0.5187122
'Con tres grupos la “variabilidad” se reduce un 51.87122 %'

library(cluster)
clusplot(ds,CA1$cluster,color=T,shade=T,labels=2,cex=0.5,lines=0) # Máxima varianza



# Jerarquico
D <- dist(ds, method = 'euclidean') 
M<-as.matrix(D)[1:50,1:50]
M[1:2,1:23] 
heatmap(M)
heatmap(M[1:5,1:5]) 


CA2<- hclust(D,method='complete')
grupos<-cutree(CA2, k=3)
sum(Y==grupos)
table(Y,grupos) 
grupos

sum(grupos == 1)
sum(grupos == 2)
sum(grupos == 3)
 
ds_clasif<-data.frame(ds_clasif,grupos) 
View(ds_clasif)

plot(Y1,Y2,pch=as.integer(grupos),col=grupos,cex=0.7)   


# DA
d1<-data.frame(d,Y)
d1
library(MASS)
LDA<-lda(d1[,1:5], d1$Y, prior= c(1/3,1/3,1/3))
LDA

P<-predict(LDA,d1[,1:5])
P$class

z<-c(55,74,51,32,27)
z
predict(LDA,z)



LDACV<-lda(d1[,1:5], d1$Y, prior= c(1/3,1/3,1/3), CV=TRUE)
LDACV

t<-table(d1$Y,LDACV$class)  # (Real, Prediccion)
t

sum(d1$Y == LDACV$class) / 88
'Probabilidad global de acierto (eficiencia): 0.98'

t["1", "1"] / sum(t["1", ]) 
t["1", "1"] / sum(t[, "1"]) 

View(d1)


LDACV$class==d1$Y
'Alumnos 12, 63, 64 mal clasificados'
mal_clasif_lda<-c(12,63,64)
d1$Y[mal_clasif_lda]   # Cluster: 2, 3 y 3
LDACV$class[mal_clasif_lda] # Clasif: 1, 2 y 2



QDA<-qda(d1[,1:5],d1$Y,prior=c(1/3,1/3,1/3))
QDA

PQ<-predict(QDA,d1[,1:5])  
PQ$class  

predict(QDA,z)  


QDACV<-qda(d1[,1:5],d1$Y,prior=c(1/3,1/3,1/3),CV=TRUE)
QDACV

tq<-table(d1$Y,QDACV$class) 
tq

sum(d1$Y == QDACV$class) / 88

tq["2", "2"] / sum(tq["2", ]) 
tq["2", "2"] / sum(tq[, "2"]) 


QDACV$class==d1$Y
'Flor 5, 12, 45, 58 mal clasificadas'
mal_clasif_qda<-c(5,12,45,58)
d1$Y[mal_clasif_qda]   # Real: 1,2,2,2
QDACV$class[mal_clasif_qda] # Clasif: 2,1,3,3


P
plot(P$x, col=d1$Y, pch=20)

predict(LDA,z)
points(0.3514878, 0.6699135)












