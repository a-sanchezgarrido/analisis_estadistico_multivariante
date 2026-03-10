load('nota.rda')
View(d)


# EJERCICIO 1 (Exploración + PCA)

summary(d)
# 1.1 Boxplot de todas las variables (notas)
boxplot(d, pch=20, main="Boxplot de notas por asignatura")

# 1.2 Scatter de dos variables (ejemplo: Mecánica vs Vectores)
plot(d$Mecanica, d$Vectores, pch=20)

# 1.5 Biplot PCA (como en iris, con cor=TRUE)
PCA <- princomp(d, cor=TRUE)
PCA$loadings
biplot(PCA, pc.biplot=TRUE, cex=0.7)
'Notas buenas en general y la segunda componente + si destaca en mecanica y vectores, - si destaca en estadistica y analisis'


# EJERCICIO 3 (Cluster: kmeans sobre datos estandarizados)


ds <- scale(d)                  # estandarizar
set.seed(2222)                 
CA1 <- kmeans(ds, centers=4, nstart=10)  
CA1

# 3.1 Tamaños
CA1$size

# 3.3 Reducción de variabilidad
CA1$tot.withinss
CA1$totss
1 - CA1$tot.withinss / CA1$totss

# 3.4 Gráfico clusters (como en prácticas)
library(cluster)
clusplot(ds, CA1$cluster, color=TRUE, shade=TRUE, labels=2, cex=0.6, lines=0)


# METER GRUPO EN TABLA
Y <- CA1$cluster
d1 <- data.frame(ds, Y)    
View(d1)
# d1$Y <- as.factor(d1$Y) 


# EJERCICIO 2 (Discriminante)

library(MASS)

# X: variables (las 5 estandarizadas) y g: el grupo
# X <- d1[,1:5]
# g <- as.factor(d1$Y)

# 2.1 LDA + clasificar z
LDA <- lda(d1[,1:5], d1$Y, prior=rep(1/4,4))

z <- c(40, 44, 51, 25, 72)

pred <- predict(LDA, z)
pred
pred$class
pred$posterior

# 2.2 LDA con CV + tabla
LDACV <- lda(d1[,1:5], d1$Y, prior=rep(1/4,4), CV=TRUE)
tablaL <- table(Real=d1$Y, Clasificada=LDACV$class)
tablaL

# Probabilidad global de acierto
sum(diag(tablaL)) / sum(tablaL)

# 2.3 Mal clasificadas (LDA)

# Usar mi forma
'acierto <- (LDACV$class == d1$Y)
acierto
mal <- which(!acierto)

cbind(Fila = mal,
      Real = as.character(d1$Y[mal]),
      Clasificada = as.character(LDACV$class[mal]))
'

# 2.4 QDA + z
QDA <- qda(d1[,1:5], d1$Y, prior=rep(1/4,4))
pred_qda <- predict(QDA, z)
pred_qda
pred_qda$class
pred_qda$posterior

# 2.5 QDA con CV + tabla
QDACV <- qda(d1[,1:5], d1$Y, prior=rep(1/4,4), CV=TRUE)
tablaQ <- table(Real=d1$Y, Clasificada=QDACV$class)
tablaQ
sum(diag(tablaQ)) / sum(tablaQ)

# 2.6 Mal clasificadas (QDA)

# Mi forma mejor
' 
acierto <- (QDACV$class == d1$Y)
acierto
mal <- which(!acierto)

cbind(Fila = mal,
      Real = as.character(d1$Y[mal]),
      Clasificada = as.character(QDACV$class[mal]))
'

# 2.7 Proyecciones canónicas LDA + z
P <- predict(LDA,d)$x
P
proy_z <- predict(LDA, z)$x

plot(proy[,1], proy[,2], pch=as.integer(d1$Y),
     xlab="LD1", ylab="LD2", main="Proyecciones canónicas (LDA)",
     xlim=range(c(proy[,1], proy_z[1])),
     ylim=range(c(proy[,2], proy_z[2])))

legend("bottomleft", legend=levels(d1$Y), pch=1:length(levels(d1$Y)), bty="n")

points(proy_z[1], proy_z[2], pch=19, cex=1.8)
text(proy_z[1], proy_z[2], labels="z", pos=4)

proy_z

