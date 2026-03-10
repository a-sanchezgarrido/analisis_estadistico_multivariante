# -----------------------------------------------------------------------------
# PRÁCTICA 5: ANÁLISIS DISCRIMINANTE
# -----------------------------------------------------------------------------

# 1. Estudio inicial de los datos
'Siempre es conveniente estudiar los datos antes de aplicar cualquier técnica.
 Estudiamos las diferencias para ver si serán de utilidad a la hora de clasificar (discriminar)'

load('escarabajos.rda')
View(d)
'Muestras de 40 escarabajos de especies HO / HC -> 1 / 2
 Pero el 40 no tiene grupo asignado'
d[,1] # d$surco

tapply(d$surco,d$especie,summary) # Estadísticos básicos por grupo (especie)

'La media de la variable surco es más grande en la especie HO (194.5) que
 en la HC (179.6) y que su valor en el escarabajo 40 (182.2) 
 está más cerca de la media de la especie HC'

plot(d$surco,d$codigo,ylim=c(0,3),pch=20) # Gráfico
points(d$surco[40],1.5,pch=20)
text(d$surco[40]+2.5,1.5, labels ='e40',cex=0.8)

plot(d$surco,d$especie,pch=20) # Etiqueta por orden alfabético: *=1, HC=2 y HO=3

# Podemos usar los gráficos caja-bigote por grupos:
boxplot(d$surco~d$especie,pch=20) # Si solo usaramos esta variable se va a HC
boxplot(d$long~d$especie,pch=20)  # Si usaramos esta se va a HO

# Para analizarlo por parejas usamos:
plot(d$surco,d$long,pch=as.integer(d$especie))  # Formas
legend('bottomright',legend=c('e40','HC','HO'),pch=1:3, cex=0.5)

plot(d$surco, d$long,pch=20)  # Escrito al lado
text(d$surco,d$long,d$especie,cex=0.7,pos=4,col='red')

'Podemos hacer un gráfico usando las dos primeras componentes principales'
pca<-princomp(d[,1:4],cor=TRUE) # Correlaciones porque las escalas son distintas (aunque no mucho)

biplot(pca,pc.biplot=TRUE,xlabs=d$especie,cex=0.7)
'Los grupos se separan bastante bien, y el e40 aparece cerca de las fronteras.
 Las dos primeras componentes principales no son necesariamente mejores variables'

plot(pca$scores[,1],pca$scores[,2],xlab='Y1',ylab='Y2') # Gráfico de las puntuaciones
text(pca$scores[,1],pca$scores[,2],d$especie,cex=0.7,pos=4,col='red')
'Diferencia entre usar puntuaciones o el biplot: 
 Basicamente es lo mismo pero el biplot está estandarizado y las puntuaciones'

# -------------------------------------------

# 2. Análisis Discriminante Lineal (LDA)
'FDL de Fisher para distinguir entre dos grupos supongo que las matrices de covarianzas son iguales.
 L = L(Z) = a´Z donde Z son las medidas a clasificar y los coeficientes se calculan con:
 a´= lambda · (mu_x - mu_y)´V^-1.'
# Para calcular el vector a, cargamos MASS
library('MASS')
LDA<-lda(d[1:39,1:4],d[1:39,6],prior=c(0.5,0.5))
LDA
'Da las probabilidades de pertenencia a priori y los vectores de medias de los grupos:
 Pertenencia a priori: 0.5 | X^ = 194.4737 267.0526 137.3684 185.9474
                           | Y^ = 179.5500 290.8000 157.2000 209.2500
 !! Importante: ver que hemos fijado las pertenencias a 0.5 ambas para usar la función de Fisher
 Los coeficientes estimados son a = -0.09327642 0.03522706 0.02875538 0.03872998'
a<-LDA$scaling # Guardamos los coeficientes

'Para clasificar a un individuo con medidas z, calculamos L(z) = a´z 
 y las proyecciones de las medias L(X^) y L(Y^).
 La frontera de las regiones es la media de estas: K = (L(X^) y L(Y^))/2'
L<-function(z) sum(a*z)
mHO<- L(LDA$means[1,])  # La proyección de la media L(X) de la especie HO
mHO # L(X^) 
mHC<- L(LDA$means[2,])
mHC # L(Y^)

K<-(mHC+mHO)/2
K 
'La regla de decisión óptima: Si L(z) > K es HC y si L(z) < K es HO
 Hacemos las proyecciones de los 40:'
D<-1:40
for (i in 1:40) D[i]<-L(d[i,1:4]) 
# Bucle para 40 en el que se guarda la función de Fisher en D[i] usando las 4 variables
D
'Por ejemplo aquí el escarabajo 1 tiene D[1]=1.253859 < K -> es del grupo HO
 Y el escarabajo 40 tiene D[40]=3.968782 < K -> grupo HO'

plot(D,d$codigo,ylim=c(1,2.1),pch=20)   # Puntuaciones discriminantes
text(D,d$codigo,cex=0.7,pos=1,col='red')

# Incluimos el escarabajo K
points(D[40],1.5,pch=20)
text(D[40],1.55, labels ='40',cex=0.7,col='red')
abline(v=K,lty=2)
text(K+0.1,1.5,labels='K',cex=0.7,pos=3,col='red')
# Se ve que el escarabajo 27 se clasifica mal y que el 40 se clasifica en HO

'También se pueden calcular las puntuaciones con D-K y ver si son >0 ó <0'
P<-predict(LDA,d[,1:4]) # Es automático, usamos P ó P$x
P

'Se puede hacer en forma de histograma, es fácil de apreciar la información:
 El escarabajo 40 debería clasificarse en HO, y hay un escarabajo mal clasificado'
ldahist(P$x,g=d$especie)  # Es enorme, hacerlo grande para que salga
P$class # Nos da los grupos a los que pertenece cada escarabajo

'Vemos si la clasificación es correcta'
Resumen<-P$class==d[,6]
Resumen   # Todos coinciden menos el 27 'FALSE' y 40 'NA'

'Podemos hacer el recuento de los resultados con "la matriz de confusión" 
 Se usa para ver si esta clasificación es buena'
table(d[,6],P$class)  # 1ª fila -> 19 bien | 2ª fila -> 1 mal y 19 bien

'Fila 1 (Grupo verdadero: 1 HO): Hay 19 escarabajos que pertenecen a la especie HO
 Fila 2 (Grupo verdadero: 2 HC): En tu muestra real hay 20 escarabajos que pertenecen a la especie HC
 Con las columnas vemos si hay alguno que deba estar en otro grupo, 1x1->19 2x1->1 debería estar en HO
 P(acierto): 38/39 = 0.974359'

P$posterior # Probabilidades a porteriori (formula en el pdf)
P$posterior[40,]  # El escarabajo 40 tiene un 75% de estar en el HO
'Nos muestran que para un individuo de estas medidas la clasificación no es muy fiable'

'La función predict se puede usar para clasificar nuevos individuos'
z<-c(185,280,150,200)
predict(LDA,z) # Se clasifica en el grupo 2


# Técnica de validación cruzada
'Al clasificar a los individuos de los que se conoce su grupo, el 
 individuo a clasificar no se usa en el procedimiento de clasificación (se tacha)'
LDACV<-lda(d[1:39,1:4],d[1:39,6],prior=c(0.5,0.5),CV=TRUE)
table(d[1:39,6],LDACV$class)
LDACV$class==d[1:39,6]
'Hay 3 escarabajos del grupo 2 que se clasifican mal. P(acierto): p = (19 + 17)/39 = 0.9230769
 La tabla 1 es la que hace trampas, esta tabla es la realidad'

'Mirar página 7/12 para la tabla:
 Sensibilidad = 1 = 19/19 -> No se escapa un escarabajo HO (detectar positivos)
 Especificidad = 0.85 = 17/20 -> De los 20 de HC, se clasifican mal 3 (detectar negativos)
 Eficiencia = 0.9231 = 36/39 -> Porcentaje de acertar la especie (% de predecir resultados)'

# -------------------------------------------

# 3. Análisis Discriminante Cuadrático (QDA)
'Cuando las variables usadas para clasificar sean normales en cada grupo 
 pero sus matrices de covarianzas (teóricas) no sean iguales, 
 para clasificar comparamos sus funciones de densidad (fórmula en pdf).
 Antes usabamos la estimación de la matriz de varianzas común S,
 ahora se estiman usando solo los datos de ese grupo con S1 y S2'

'Mientras que con las funciones f(z) buscabas el valor máximo, 
 ahora se clasifica al individuo en el grupo donde el valor de QDF sea mínimo.
 Cuando los determinantes sean iguales, el método será equivalente
 al de la distancia de Mahalanobis mínima'
QDA<-qda(d[1:39,1:4],d[1:39,6],prior=c(0.5,0.5))
QDA

# No aparecen los coeficientes de las QDF, para eso hacemos:
QDA$scaling   # Matrices triangulares U1 y U2
QDA$ldet      # Determinantes (constantes que buscamos)

'Scaling nos da matrices triangulares Ui tales que Ui·Ui´ = S^−1.
 Ui´z convierte el óvalo en un esfera perfecta, y su matriz de Cov(Ui´z)=I'

P<-predict(QDA,d[,1:4])
P   # Solo está mal clasificado el 27, el 40 pertenece al grupo 1
'Según el pdf esta clasificación no es nada fiable.
 No es que el modelo sea malo, es bueno, el problema es que el escarabajo 40 está justo en medio'

table(P$class,d$codigo) # Vertical realidad | Horizontal predicción
# 1 mal clasificado, aparece en 2 pero la predicción nos da el 1

QDACV<-qda(d[1:39,1:4],d[1:39,6],prior=c(0.5,0.5),CV=TRUE)
table(d[1:39,6],QDACV$class)  # Tabla con validación cruzada

# Resultados parecidos al LDA , probabilidad de acierto p = 35/39 = 0.8974359.
z<-c(185,280,150,200)
predict(QDA,z)  # Para predecir nuevos escarabajos | z es del grupo 2 (fiable)
# Solo es válido si siguen una distribución normal


# -------------------------------------------

# 4. Comprobaciones
'¿Qué es mejor aplicar LDA o QDA?
 LDA funciona si las matrices de covarianzas teóricas son iguales
 QDA funciona si los datos son normales en cada grupo (campana de Gauss)
 El método de validación cruzada proporciona estimaciones de probabilidades de acierto'

S1<-cov(d[1:19,1:4])  # La matriz del grupo 1
d1<-d[d$especie=='HO',1:4]  # Separa los datos del grupo 1
cov(d1)

S2<-cov(d[20:39,1:4])  # La matriz del grupo 2
d2<-d[d$especie=='HC',1:4]  # Separa los datos del grupo 2
cov(d2)

'Son diferentes, no parecen estimaciones de una misma matriz V.
 Ahora vamos a mirar si los datos son normales. Cargamos "mvnormtest"'

library(mvnormtest)
# Tomamos alfa=0.05
mshapiro.test(t(d[1:19,1:4])) # p-valor = 0.2013 -> Pasa el test de normalidad
mshapiro.test(t(d[20:39,1:4]))# p-valor = 0.05769 -> Pasa el test de normalidad

plot(d[20:39,1:4])  # Datos del grupo 2

'Ver cálculo de la matriz de covarianzas común V en el pdf
 Basicamente calculamos una matriz conjunta para los dos grupos'
S<-(18*S1+19*S2)/37
S
'No depende de las probabilidades a priori'
In<-solve(S)  # Inversa de S
In

m1<-LDA$means[1,] # Medias del grupo 1  | También puedo hacer 'colMeans(d1)'
m2<-LDA$means[2,] # Medias del grupo 2

a2<-(m1-m2)%*%In  # Coeficientes de Fisher | %*% es producto de matrices en R
a2

'Coeficientes automáticos entre los manuales'
LDA$scaling/t(a2) # Scaling / Traspuesto de a2
# Lambda = -0.2701715 | Son proporcionales unos de otros

'¿Qué variable es la más importante?
 No puedes fiarte de los coeficientes originales por tener distintas escalas'
ds<-scale(d[,1:4])  # Estandarizamos los datos 
'Esto no destroza los datos como en el PCA, en LDA y QDA no cambian'
ds

lda(ds[1:39,1:4],d[1:39,6],prior=c(0.5,0.5)) # Calculamos los coeficientes
# La variable más discriminante es surco y la que menos es base2

'Lo que podemos hacer es eliminar alguna varible, para eso usamos los métodos de validación cruzada 
 y estudiar cuál proporciona los mejores resultados. Lo mejor es siempre usar todas'
lda(d[1:39,2:4],d[1:39,6],prior=c(0.5,0.5),CV=TRUE) # Eliminamos surco
# 7 escarabajos mal clasificados

# Comprobamos el resto:
lda(d[1:39,c(1,3,4)],d[1:39,6],prior=c(0.5,0.5),CV=TRUE)  # Sin long  | 2 fallos
lda(d[1:39,c(1,2,4)],d[1:39,6],prior=c(0.5,0.5),CV=TRUE)  # Sin base2 | 2 fallos
lda(d[1:39,1:3],d[1:39,6],prior=c(0.5,0.5),CV=TRUE)       # Sin base3 | 3 fallos

# Este proceso se puede hacer también por parejas. Y también se puede hacer en el QDA

library(mvtnorm)  # Calcular probabilidades a posteriori
f1<-dmvnorm(d[40,1:4],m1,S) # Como en LDA se asume misma dispersión usamos S
f2<-dmvnorm(d[40,1:4],m2,S)
f1/(f1+f2)  # Probabilidad a posteriori del escarabajo 40 en el grupo 1 (con prob. a priori iguales)
f2/(f1+f2)

19*f1/(19*f1+20*f2) # Con las a priori proporcionadas por los grupos
20*f2/(19*f1+20*f2)

# Lo hacemos con QDA, usamos S1 y S2
f1_qda <- dmvnorm(d[40, 1:4], m1, S1)
f2_qda <- dmvnorm(d[40, 1:4], m2, S2)
f1_qda / (f1_qda + f2_qda)  # A priori iguales
f2_qda / (f1_qda + f2_qda)

19*f1_qda/(19*f1_qda+20*f2_qda) # A priori proporcionadas
20*f2_qda/(19*f1_qda+20*f2_qda)

'Si hay más de dos grupos en un LDA, podemos calcular funciones discriminantes lineales:
 Li(z) = z′S^−1·mi − mi′·S^−1·mi/2 '
solve(S) %*% m1   # Parte izquierda (suma de coeficientes ·zi)
solve(S) %*% m2   #
-0.5*t(m1) %*% solve(S) %*% m1  # Parte derecha (resta)
-0.5*t(m2) %*% solve(S) %*% m2  #
'L1(z) = 0.9557217z1 − 0.0208622z2 + 0.6842504z3 + 0.4353125z4 − 177.6155
 L2(z) = 0.6104728z1 + 0.1095255z2 + 0.7906842z3 + 0.5786658z4 − 193.4209
 Esto se repetiría con el nº de grupos que tuviesemos, ahora al coger un escarabajo nuevo
 lo añadiríamos a las ecuaciones y aquella que tuviese la mayor puntuación sería la elegida.
 Hacer esto es igual que hacer lo de la parte 2, L1(z) − L2(z) = a′z − K
 En el QDA el individuo se clasifica en el grupo que le dé el valor mínimo'

e40<-d[40,1:4]
# L1(e40) y L2(e40) nos da estos resultados: Cogemos el grupo 1 por ser mayor
0.9557217*e40[1]-0.0208622*e40[2]+0.6842504*e40[3]+0.4353125*e40[4]-177.6155
0.6104728*e40[1]+0.1095255*e40[2]+0.7906842*e40[3]+0.5786658*e40[4]-193.4209

'Estos métodos son equivalentes a usar el de máxima verosimilitud (prob a posteriori)
 con matrices de covarianzas iguales (LDA) y distintas (QDA)'

'La puntuación de QDA está compuesta por dos trozos, la distancia de Mahalanobis
 que mide la distancia del escarabajo al centro del grupo y el determinante que 
 es una penalización y viene en QDA$ldet: QDF = Dist.Mahalanobis + log|Si|'

M1<-mahalanobis(e40,m1,S1)
M2<-mahalanobis(e40,m2,S2)
QDA$ldet+c(M1,M2) # Da lo mismo que QDF1 y QDF2, coges el menor -> Grupo 1
'Distancia de Mahalanobis original = Distancia Euclídea de los datos transformados esféricamente'

'También puedes clasificar los escarabajos solo basándote en la distancia de Mahalanobis
 pero suele fallar si ambas especies NO tienen nubes del mismo tamaño'

'Si cogemos las matrices triangulares para transformar los datos, coge la nube de puntos
 y transforma el óvalo en una esfera perfecta, para que la distancia de Mahalanobis sea
 la distancia Euclidea.'
M1  # -> da el mismo número que si hacemos la transformación: 3.35154
M2  # -> 3.860477

'Para obtener estas distancias en LDA hay que sustituir S1 y S2 por S'
mahalanobis(e40,m1,S) # -> 2.801345 y se clasifica en el grupo 1 por ser el más cercano
mahalanobis(e40,m2,S) # -> 5.032389


'Podemos obtener los valores transformados esféricamente y dibujarlos con plot. 
 Esto es crucial para los ejercicios con 3 o más grupos'
plot(P$x, d$codigo, col = d$codigo, xlab = "Transformados esféricos", ylab = "Especie (1=HO, 2=HC)")
# plot(P$x, col = d$codigo) -> para 3 o más grupos
'Como el LDA proyecta los datos en un número de dimensiones = nº grupos - 1 es una línea'









# ------------------------------------------------------
# Preguntas examen: 
'Si hay 3 grupos, no puedes poner 0.33 en la pertenencia a priori ya que NO suman 1
 La tabla de confusión/aciertos con validación cruzada siempre cae'




