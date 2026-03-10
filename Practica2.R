# -----------------------------------------------------------------------------
# PRÁCTICA 4: ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# -----------------------------------------------------------------------------

# 1. Estudio inicial de los datos
# Miramos todos los datasets que tiene R
data()
d<-LifeCycleSavings # guardamos este dataset como d
View(d) # observamos que tiene 5 columnas con datos
help(LifeCycleSavings) # con esto lo que tenemos es información acerca de los detalles del dataset

# Resumen estadístico: Min, 1st Qu., Mediana, 3rd Qu., Max
summary(d)
# ¿Qué se aprecia? -> Las variables tienen escalas muy diferentes
str(d) # Basicamente muestra los mismo que una View pero en la consola

# Estudiamos las correlaciones con:
plot(d, pch=20, cex=0.8) # -> se ve que tiene correlaciones positivas, negativas y variables independientes
# Te dan las gráficas xy de todas las combinaciones de parejas posibles,
# Por ejemplo, con las graficas de pop15 y pop75

# Vector de medias pero no podemos usar mean(d)
mu<-colMeans(d)
sapply(d,mean) # otra opción pero mejor la de colMeans
# Matrices de covarianzas y correlaciones
cov(d) # -> muy distintas
M<-cor(d) # -> existen variables con correlaciones lineales positivas, negativas y casi nulas

# Las escalas, simetría y existencia de valores atípicos se puede estudiar con los caja-bigote:
boxplot(d) # boxplot(d[,i])
boxplot(d[,1])
boxplot(d[,2])
boxplot(d[,3])
boxplot(d[,4])
boxplot(d[,5])
# Incluyen la mediana, los cuartiles 1 y 3 (caja), min y max (bigotes)
# Los valores atípicos son los puntos que están fuera de los bigotes

# Resultados: La mayoría falta de simetría (menos el 3)
# En el 5 hay dos valores atípicos (Libia y Jamaica, 49 y 47) que presentan ddpi muy grandes
# Para detectar los países con valores extremos altos:
which.max(d[,5])  # -> donde está el valor más alto (índice)
sort(d[,5])       # -> valores ordenados de menor a mayor
order(d[,5])      # -> índices ordenados
# Si no encuentro lo que quiero o no lo entiendo, directamente:
View(d)

# Podemos hacer histogramas para estudiar simetrías y normalidad
hist(d[,1])   # hist(d$sr)
# Para añadir la curva normal a estos gráficos podemos hacer
hist(d$sr,probability=T,xlab='sr',ylab='f',main='Histograma sr')
# Pone la curva que tendría si existiese una distribución normal igual que esta pero con la normalidad perfecta
curve(dnorm(x,mean(d$sr),sd(d$sr)),add=T,col='red')
# Se aprecia asimetría y falta de normalidad

# Las distancias de Mahalanobis a la media se pueden usar ver 
# los países más raros y para detectar posibles valores atípicos
md<-mahalanobis(d,colMeans(d),cov(d)) # la calcula
md
plot(md,ylab='Distancias de Mahalanobis') # la representa
# Proporciona las distancias al cuadrado y las representa
# Más raros (lejanos de la media) son USA (15.46) y Libia (25.66)

# -------------------------------------------

# 2. Cálculo de las componentes principales
# Debemos decidir en primer lugar si usamos la matriz de covarianzas o la de correlaciones

# Covarianzas ->  las variables no se cambian de escala
# y tendrán más importancia aquellas variables que tengan varianzas mayores
# La usamos  cuando las unidades son iguales y queremos mantener 
# las escalas (la variables que varían poco tienen poca importancia)

# Correlaciones ->  las variables originales se estandarizan y todas 
# tienen a priori la misma importancia (varianza uno)
# Se usa cuando las variables se miden en unidades diferentes 
# (al estandarizar, las unidades desaparecen) o tienen rangos (varianzas) muy diferentes

# En este caso usamos la de correlaciones
# Para calcular las componentes principales: princomp y prcomp
PCA<-princomp(d,cor=TRUE)
summary(PCA,loadings=TRUE)  # características principales

# Importancia de las componentes:
# Se mide con sus variaciones estándar, la proporción y las proporciones acumuladas
# Tengo 5 variables, y al coger la matriz de correlaciones, R da valor a las varianzas = 1
# 1+1+1+1+1=5 -> la primera varianza es 2.822078 porque es 1.6799041^2 (desviación estándar^2)
# La proporción es 0.5644156 y se calcula como 1.6799041^2/5
# Las proporciones acumuladas se calculan sumando las proporciones anteriores
# ¿Qué nos dice esto? -> La primera componente tiene un 56.44156% de la información

# Los loadings(cargas) son los vectores de propios unitarios  de los valores propios anteriores (el primero 2.822078)
# Los valores ausentes son números pequeños 
PCA$loadings->L
L[,1] # -> primer vector propio UNITARIO para calcular la primera componente principal
# Este vector es el mismo que proporciona la tabla de los loadings pero son vectores columna

# La primera componente principal se calcula como:
# Y_1 = 0.308·X1* - 0.571·X2* + 0.560·X3* + 0.514·X4* + 0.0379·X5*
# Xi* = (Xi - Xi^)/Si variable i-esima estandarizada (muestral) 
# A cada dato le resta su media y lo divide por su desviación típica
# Las puntuaciones (scores), los valores que se obtienen en las componentes principales:
PCA$scores->S
S[,1] # vemos las puntuaciones en la primera componente principal

# Para comprobar si los datos de princomp son correctos usamos:
eigen(M) # Obtenemos los valores propios ordenados y los vectores propios
# Para comprobar los valores de las puntuaciones hay que estandarizar (no hace falta con covarianzas)
z<-scale(d)
# Ahora  para calcular las puntuaciones de la primera componente:
y1<-0.30846174*z[,1]-0.57065322*z[,2]+0.56043119*z[,3]+0.5135064*z[,4]+0.03787232*z[,5]
# Si vemos y1 hay pequeñas diferencias con los datos ya que princomp usa varianzas y no cuasi-varianzas
# S[,1] se obtiene con:
y1*sqrt(50/49)
# S[,i] = yi · raiz(n/n-1) -> por tanto es MÁS FÁCIL usar el calculo de princomp

# Las componentes principales se pueden calcular aunque no se tengan los datos completos:
princomp(covmat=M) # Sustituyendo M por la matriz de correlación (o covarianzas)
# R puede calcular las cargas pero nos las puntuaciones (loadings si, scores no)

# Para calcular las comp. princ. usamos:
PCAbis<-prcomp(d,scale=TRUE)
summary(PCAbis) # -> obtenemos la importancia de las componentes
PCAbis$rotation # cargas (loadings)
PCAbis$x # puntuaciones (scores)
# Se ve obviamente que los valores están cambiados de signo
princomp(d) # DATOS ERRÓNEOS por usar la matriz de covarianzas

# -------------------------------------------

# 3. Análisis de componentes principales (PCA)
# Debemos fijarnos en la importancia de cada una
# En este caso la información proporcionada por la primera será en general el doble de importante que la que proporciona la segunda, etc.
# Después miramos las cargas para dar un significado a estas variables nuevas
# En Y1, las variables estandarizadas, podemos afirmar que las variables más influyentes son: sr(+), pop15(-), pop75(+) y dpi(+)
# Es decir Y1 toma valores grandes en los países con valores pequeños en pop15 y grande de las otras 3
# Y1 nos muestra los países con poblaciones envejecidas y ricos

# Ahora podemos analizar las scores para decir cómo serán los individuos de la muestra según esa componente
summary(S[,1])  # Y1 € (-2.258755, 2.787708)
plot(S[,1],ylab='Y1')
sort(S[,1])     # otra forma de verlo
which.max(S[,1])# vemos qué país es el que mayor valor tiene -> Suecia
which.min(S[,1])# vemos el que menor valor tiene -> Malasia
# En el gráfico casi no hay valores entre 0 y -1, 
# por lo que la mayoría de los países se podrían clasificar como del tercer o del primer mundo 

# Las componentes son incorreladas (no nos dan información una de la otra)
# Incorreladas -> sin relación lineal | Independientes -> sin relación de ningún tipo (si son normales son independientes)
# Las podemos estudiar por separado 
biplot(PCA,pc.biplot=TRUE,cex=0.5) # a veces es conveniente representarlas por parejas
# Las cargas son los vectores en rojo y las puntuaciones estandarizadas son los nombres de los países
# Las variables que tengan vectores cortos estarán mal representadas
# Las puntuaciones se usarán para decir cómo serán los individuos de la muestra
sort(d[,1]) # Japón es el que mayor sr tiene

biplot(PCA,pc.biplot=TRUE,xlabs=1:50) # -> cambio de nombre a números

biplot(PCA,pc.biplot=TRUE,choices=c(3,4),xlabs=1:50) # Gráfico de las componentes tercera y cuarta 
# |
# v
eigen(M) # vemos vectores propios para ver la importancia de las varibles
S[,3]   # sr(+), pop15(+), pop75(-), dpi(-) y ddpi(-) -> ahorros altos, algo de natalidad, pocos mayores, pocos ingresos (jóvenes y ahorradores)
S[,4]   # sr(-), pop15(-), pop75(+), dpi(-) y ddpi(-) -> poco ahorro, poca natalidad, muchos mayores, poquísimos ingresos (envejecidos y pobres)

# Gráfico solo de las puntuaciones (sin estandarizar) de las dos primeras componentes
plot(S[,1],S[,2],xlab='Y1',ylab='Y2')

# Para encontrar un individuo (país) en este gráfico bastar mirar sus puntuaciones (scores) en S
text(S[38,1]+0.3,S[38,2],labels='Esp') # Busco España(38) y le ponemos su número
text(S[,1],S[,2]-0.2,labels=row.names(d),cex=0.6) # Etiquetas para todos

# Esto es distinto a lo que hace biplot -> que basicamente divide entre las desviaciones típicas
plot(S[,1]/1.6799,S[,2]/1.1207,xlab='Y1*',ylab='Y2*',pch=20)
text(S[,1]/1.6799,S[,2]/1.1207-0.2,labels=row.names(d),cex=0.6)

# La mayor dispersión de la primera componente nos indica que esta componente es más importante
pairs(PCA$scores[,1:3]) # gráficos 3D (poco habitual)

# -------------------------------------------

# 4. Saturaciones
# Para medir las relaciones lineales entre las variables iniciales y las componentes principales usamos
# la matriz de correlaciones conocida como matriz de saturaciones
# Se calculan como Aij = Corr(Xi,Yj) = t_ij/sigma_i · lambda_j^1/2

# Si hemos usado correlaciones: multiplicamos cargas por la raiz del valor propio
# Nos indicarán cuánta información tendrá de cada componente de cada variable
S1<-L[,1]*1.6799041 # saturaciones de la primera componente
# Y las saturaciones al cuadrado (información) con 
S1
S1^2 # -> la variable mejor representada Y1 es pop15 con 91.8994802%
# -> la peor representada es ddpi (no tiene casi información)

SAT<-cor(d,S) # -> Cálculo de saturaciones (usemos o no correlaciones)
SAT 

# Al ser incorreladas, las correlaciones múltiples al cuadrado son la suma de las correlaciones al cuadrado
# Estos valores nos indicarán la información que mantienen las p primeras componentes sobre cada variable Xi
COM2<-SAT[,1]^2+ SAT[,2]^2  # 2 primeras componentes principales
COM2 # Esto es la comunalidad (correlaciones múltiples)
# La mejor representada en las 2 primeras es pop15 y la peor es sr
summary(PCA) # si miramos la media de los valores de la última columna coincide con la información que mantienen Y1 y Y2

# El promedio de la comunalidad (correlaciones múltiples) de las dos primeras componentes 
# Es igual a la proporción acumulada de las dos primeras variables (summary)
( 0.6543657 + 0.9191976 + 0.8991785 + 0.8332266 + 0.7721760 ) / 5

# Como ejercicio hago el caso de solo la primera componente
SAT[,1]^2
( 0.268516885 + 0.918995810 + 0.886366987 + 0.744150387 + 0.004047742 ) / 5
summary(PCA)

# También comprobar que las sumas de los valores de cada columna nos dan los 
# valores propios (informaciones) de cada componente principal (si usamos correlaciones)
0.268516885 + 0.918995810 + 0.886366987 + 0.744150387 + 0.004047742
# Da 2.822078, el mayor valor propio

'La correlación múltiple al cuadrado es el máximo de las correlaciones
que se pueden obtener con combinaciones lineales de las componentes.
Además el máximo de esas correlaciones se obtiene con los coeficientes de la matriz de cargas (L)'

'La mejor combinación lineal de las dos primeras componentes para estimar (linealmente) sr es la
que se obtiene cortando L[1,], es decir, Z1 = 0.3084617 ∗ Y1 + 0.5542456 ∗ Y2.'
L[,1] # matriz de cargas (loadings)
L[,2]
Z1<-0.3084617*S[,1]+ 0.5542456*S[,2] # siendo S la matriz de puntuaciones (scores) de las componentes principales
cor(d[,1],Z1)^2 # coincide con la información que mantienen esas dos componentes sobre sr

# Z1 se podría usar para predecir sr usando las técnicas de regresión lineal
lm(d$sr~Z1) # Búscame la ecuación de la recta que mejor transforme mi estimación Z1 en los datos reales de sr
plot(Z1,d$sr,pch=20,ylab='sr')  # Datos
abline(lm(d$sr~Z1),col='red') # más cerca de la línea = mejor modelo
plot(d$sr-9.671-4.435*Z1,pch=20,ylab='Residuos') # Residuos (diferencias) o errores
# La mejor manera de recuperar sr usando Z1 es mediante sr≈9.671+4.435·Z1. (la desestandarizamos)
# Z1 predice sr estandarizada (ya que ambas tienen media cero) 
# y que 9.671 y 4.435 son la media y desviación estándar muestrales de sr.
# Al usar 2 componentes en vez de las 5 se pierde información
# Si p=k=5 no hay residuo

# -------------------------------------------

# 5. Número de componentes
'Al hacer el PCA podemos escoger con qué componentes principales quedarnos.
El número de componentes es m y se tomarán las m primeras, las más importantes'

# 5.1 Fijar un número concreto de componentes:
'Se basa en escoger en base a lo que necesites, es subjetiva.
Se suele escoger m=2, así podemos hacer un gráfico bidimensional.'

summary(PCA) # En este ejemplo si cogemos las dos primeras tenemos un 81% de la información
SAT   # Para los porcentajes de información se calculaba la matriz de saturaciones
COM2  # COM2 <- SAT[,1]^2 + SAT[,2]^2 esto nos da la información con las dos primeras componentes

'También habría que ver lo de las comunalidades, la peor representada es sr 
con un 65% de la información, por tanto todas están bien representadas.
En otros ejemplos, si no están bien representadas hay que señalarlo y aumentar m'

# 5.2 Fijar un porcentaje mínimo de información obtenida:
'Si queremos mantener un porcentaje p% debemos quedarnos con las primeras que verifiquen ser mayores a p%'

summary(PCA) # En el ejemplo habría que hacerlo al menos con 3 componentes para un 90%
COM2  # La comunalidad es mayor que 0.5 en todas, OK
SAT[,1]^2 # -> La comunalidad no es mayor en todas, m=1 no sirve

'Otra opción es fijar porcentaje para las comunalidades, así aseguramos que las originales están representadas.
En el ejemplo, con comunalidades mayores que 0.5 (50% de las variables)
tomamos m=2. Con esta regla, m=1 tiene un 56% pero nunca se cumpliría'

# 5.3 Regla de Rao:
'Solo serán relevantes las componentes con variabilidad (varianza o valor propio)
mayor que la variabilidad mínima. S^2 son las cuasivarianzas muestrales
Si usamos correlaciones, es como usar varianzas estandarizadas por tanto solo cogemos
las que tienen valor propio (varianzas) mayor que 1'

summary(PCA)  # En el ejemplo nos lleva a coger m=2 (la primera y la segunda)

'Si usamos covarianzas, el mínimo de las cuasivarianzas corresponde a pop75
que 1.66, luego habria que comprobar que es mayor que ese valor y tomaríamos m=4'

cov(d)        # Haríamos la matriz de cov y miramos la diagonal para saber el mínimo
diag(cov(d))  # Otra forma, evidentemente sería pop75 con 1.66609082
princomp(d)         # Obtenemos los valores propios y elegiríamos hasta la 4
eigen(cov(d))       # También nos daría los valores propios y los vectores

# 5.4 Regla de Kaiser:
'Solo serán relevantes las que tengan mayor variabilidad que la media.
Con correlaciones, las varianzas iniciales son 1, nos quedamos con las mayores de 1 -> m=2.
Si usamos covarianzas, solo nos quedaríamos con la primera componente 981871.2 > 196387 -> m=1.'

summary(PCA) # Las dos primeras sirven

mean(diag(cov(d))) # Sacamos la media = 196387
eigen((cov(d))) # Mirando los valores propios obtendríamos m=1

# 5.5 Regla del codo o del gráfico de sedimentación: 
'Consiste en representar j(eje x) frente a los valores propios estimados lambda_j.
El gráfico debe parecerse a algo como una montaña o un codo.
Serán representativas las componentes que están antes de este codo'

screeplot(PCA)  # Gráfico directo (diagrama de cajas)
plot(eigen(cor(d))$values,type='l',ylab='valores propios')  # Gráfico de líneas

'Aunque no está muy claro, parece que el codo se encuentra
en j=3, por lo que tomaríamos las dos primeras componentes (m=2).
Pero m=1 y m=3 también serían aceptables porque el codo no es tan claro'

# 5.6 Prueba de esfericidad:
'Me he quedado con m=2, me sobran 3. Con esto miramos si hay algo que 
haga falta de las otras componentes.
Hipótesis: todas las sobrantes son iguales. 
Si las varianzas son iguales se formará una esfera.
Si no lo son, puedo sacar más componentes.
Si son iguales la media aritmética y geométrica son iguales, si no T crece'

eigen(cor(d))$values->Lambda
mean(Lambda[3:5])->ma
exp(mean(log(Lambda[3:5])))->mg
(50-(2*5+11)/6)*(5-2)*log(ma/mg)->TB
0.5*(5-2-1)*(5-2+2)->gl
1-pchisq(TB,gl) # El p-valor obtenido es 2.27505·10^-8 < 0.05 -> rechazo H0.

qchisq(0.95,5)
curve(dchisq(x,5),0,50)

'Te da permiso para coger más componentes pero no te obliga.
Es útil si sale esferidad, ahí si que no habría que coger una m mayor'

# -------------------------------------------

'HECHO'

# -------------------------------------------
# Preguntas examen:
# Decir quien es Y = a1X1 + ... + akXk y poner esta fórmula
# No se puede hacer con la matriz de covarianzas tiene que ser con la de correlaciones
# Cov(X) NO - Cor(X) SI
# La matriz de correlaciones es la que se diagonaliza para sacar a1*
# No se ponen Z se ponen X* porque no conozco la media ni la varianza
# Se pone Xi* = (Xi - -xi)/Si  con -xi como x barra y Si = raiz(1/n-1 * Sumatorio) SCALE
# LOADING ai*
# SCORES con los Xi de un dato, se saca Yi que son las puntuaciones (con la matriz de covarianzas)
# Con la de correlaciones tiene más sentido
# Al final de la práctica hay varios métodos, en el examen me pedirán 3 métodos
# h1 = S1^2 + S2^2
# Las gráficas se ponen con colores, no con 0 y 1, y con las letras da error
# Las saturaciones son las correlaciones

