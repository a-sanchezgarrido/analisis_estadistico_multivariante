
'1. Aplicar un PCA a los datos del fichero: USArrests incluido en R que contiene datos sobre los
arrestos por cada 100000 residentes por asesinato, asalto o violación en cada uno de los 50
estados de USA en 1973. También se incluye el porcentaje de población que vive en las áreas
urbanas. Fuente: help(USArrests).'

data("USArrests")
d <- USArrests
help("USArrests")


# Miramos los datos rapidamente
summary(d)
'Hay escalas bastante distintas, murder pequeño y assault muy grande'
str(d) # Ves en consola el documento

plot(d, pch=20, cex=0.8)  # Las variables están relacionadas se puede hacer PCA
'Correlacion entre asesinos y asaltos
Poca correlacion entre asesinos/asaltos y urbanos -> lo urbano va por separado'


# Componentes principales
'Como las escalas son tan distintas usamos correlaciones'
PCA <- princomp(d, cor=TRUE)
summary(PCA)
'Comp1 62%, Comp2 86%, Comp3 95% -> Podemos coger 2 componentes (m=2)'

PCA$loadings # Vemos los loadings (qué significa)
'Comp1: Interpretamos que si un estado está más a la derecha tiene más crímenes
Comp2: Mide lo rural frente a lo urbano, más negativo más urbano'

PCA$scores   # Vemos los scores (dónde está cada uno)
'Comp1: Score alto mucha criminalidad
Comp2: Score bajo muy urbano'
head(PCA$scores)
sort(PCA$scores[,1])  # Criminalidad
sort(PCA$scores[,2])  # Urbanizados

plot(PCA$scores[,1], PCA$scores[,2], xlab="Criminalidad", ylab="Urbanización") # Veo ambas componentes en el gráfico
text(PCA$scores[,1], PCA$scores[,2], labels=rownames(d), cex=0.7)
abline(h=0, v=0, col="red")


# Hacemos un biplot para ver el mapa (casi igual que el anterior)
biplot(PCA, cex=0.7)
'Como UrbanPop es casi perpendicular a las otras flechas esto nos chiva que son incorreladas'


# Por último decidimos cuantas componentes coger
screeplot(PCA)
plot(eigen(cor(d))$values,type='l',ylab='valores propios')
'Usando la regla del codo es más que evidente que estamos entre 2 y 3
Seguramente 2 ya que la bajada es más brusca de 1 a 2
Para asegurarnos usamos la regla de Rao'
summary(PCA)
'Al usar correlaciones, las varianzas son 1, por tanto la unica que lo cumple es m=1
pero la varianza de la componente 2 es 0.99 es prácticamente 1 y pasar de 62% a 87% es un gran salto
por eso tomamos m=2'

# Verificamos que no se rompa ninguna variable con las comunalidades
'Las saturaciones ayudan a poner nombre a los ejes,
los loadings son los pesos, las saturaciones son la correlación real'
SAT<-cor(d,PCA$scores)
SAT   # Valores altos en Comp1 de crimenes y altos en Comp2 de urbanización

'Nos dice qué porcentaje de información se ha salvado al coger las 2 componentes'
COM2<-SAT[,1]^2+ SAT[,2]^2
COM2  # se ha salvado muchísima información
'La calidad de representación es excelente para todas las variables individualmente'



