library("MVA")
data("heptathlon")
d <- heptathlon[, 1:7]

# 1.1 Calcular Y1 y Y2
summary(d)  # Veo que debo de seguir
'Como hay escalas distintas usaremos correlaciones'
plot(d, pch=20, cex=0.8)

PCA <- princomp(d, cor=TRUE)
summary(PCA) # Para ver importancia
PCA$loadings # Para ver los coeficientes
L<-PCA$loadings
'Nos dice que digamos como se calculan las primeras dos componentes'
'Las componentes principales se calculan como:
 Y_1 = 0.453·X1* - 0.377·X2* - 0.363·X3* + 0.408·X4* - 0.456·X5* - 0.075·X6* + 0.374·X7*'
L[,1]
'Del mismo modo calculamos Y_2'

# 1.2 y 1.3 Interpretar Y1 y Y2
'Como estamos en una carrera, menos tiempo es mejor, y los lanzamientos y saltos cuanto mas mejor'
'Las carreras son + y los saltos -, en las carreras, cuanto mayor es el valor mas tardan
en los saltos cuanto menor valor mas saltan.
La comp1 representa el rendimiento atletico global invertido, valores bajos representan mejores atletas'
'La comp2 es espcifica de lanzamiento, mayor valor mejor lanzamiento frente a velocidad o salto'

# 1.4 Puntuaciones del atleta 23
PCA$scores
'Hui-Ing (TAI)        2.93969248 -0.67514662'
'Tambien puedo filtrarlo con PCA$scores[23, 1:2]'

# 1.5 Cálculo de puntuaciones del atleta 23 manualmente
z<-scale(d)
z[23,]
'Nos dará estos resultados:
 1.3710758 -1.3086606 -2.0897062  0.5990364 -1.4390178 -0.6606394  0.1502818
 Si añadimos esto a la formula de la comp. princ. donde van las X: 
 Y_1 = 0.453·X1* - 0.377·X2* - 0.363·X3* + 0.408·X4* - 0.456·X5* - 0.075·X6* + 0.374·X7*;
 Cuando lo tengamos hay que multiplicar yi · sqrt(n/n-1)'
'En este caso, al sumar todo multiplicamos por sqrt(25/24)'

# ----------------------------------

# 2.1 Saturaciones de X1 con Y1 e Y2
SAT<-cor(d,PCA$scores)
SAT
'hurdles   0.9564348  0.17258347
Indica la relación lineal con las componentes principales,
en este caso es importante para explicar el rendimiento general y
para la componente 2 hay poco correlación por tanto es irrelevante'

# 2.2 Porcentaje de información que se mantendría en el biplot
COM2<- SAT[,1]^2 + SAT[,2]^2
COM2
'Obtenemos un porcentaje del 94.45526%, es decir mucha información'

# 2.3 Variables mejor y peor representadas:
'Mejor representada: hurdles  Información: 94.45526%
 Peor representada: run800m   Información: 68.72789%'

# ----------------------------------

# 3.1 Representar gráficamente (biplot) y posición del atleta 23:
biplot(PCA, cex=0.7)
PCA$scores[23,1:2] 
'Estas son las coordenadas en el gráfico de las dos componentes:
 X=2.9396925 Y=-0.6751466'

# 3.2 Interpretar como serán los datos en base a sus coordenadas
'La atleta es una corredora lenta ya que se encuentra con valores altos 
 de tiempo al correr, como además está lejos de las flechas de salto, salta poco'

# 3.3 Dar puntuaciones en Y1 e Y2 estandarizadas del atleta 23
'Las componentes principales tienen media 0 y varianzas = valor propio
 Por tanto, Hui-Ing (TAI)        2.93969248 -0.67514662
 hay que dividirlo entre sqrt(valor propio)'
PCA$scores/PCA$sdev[1]  # 1.391941798 -> peor atleta por encima de la media en tiempo
PCA$scores/PCA$sdev[2]  # -0.61778540 -> peor atleta por debajo de la media de lanzamientos

# 3.4 Mejor y peor atleta y en qué destacan según el gráfico
'Buscamos en las scores las que tengan los mayores valores'
which.max(PCA$scores[,1]) # Peor atleta Launa (PNG)
which.min(PCA$scores[,1]) # Mejor atleta Joyner-Kersee (USA)
'Destacan en carreras y javalina, Launa es la más lenta y Joyner-Kersee el más rápido,
pero Launa es la mejor en javalina y Joyner-Kersee la sigue de cerca'
PCA$scores

# 3.5 Mejores atletas en lanzamientos
which.max(PCA$scores[,2]) # Launa (PNG) destaca en javalina
order(PCA$scores[,2]) # Como es ascendente los mejores son 25, 5 y 1
'Es decir los mejores son Launa (PNG), Choubenkova (URS) y Joyner-Kersee (USA)'

# ----------------------------------

# 4.1 Valores propios, gráfico del codo y decir componentes
'Hacemos eigen siempre para ver valores y vectores propios'
eigen(cor(d))
'Valores propios: 4.46027516 1.19432056 0.52101413 0.45716683 0.24526674 0.07295558 0.04900101'

screeplot(PCA)  
plot(eigen(cor(d))$values,type='l',ylab='valores propios')
'Según la regla del codo nos quedaríamos con m=2'

summary(PCA) # Con dos componentes retenemos el 80.77994% de la información

# 4.2 Regla de Rao
'según la Regla de Rao como tenemos una matriz de correlaciones, las varianzas están
estandarizadas y valen 1, por tanto nos quedamos con las componentes que tengan mayor
varianza que 1'
summary(PCA)
'Nos quedamos con las dos primeras que son mayores que 1, m=2'

# 4.3 Prueba de esfericidad para m=2
'Miramos si algo hace falta de las otras componentes
Hipotesis: todas las sobrantes son iguales'
eigen(cor(d))$values->Lambda
mean(Lambda[3:7])->ma
exp(mean(log(Lambda[3:7])))->mg
(25-(2*7+11)/6)*(7-2)*log(ma/mg)->TB
0.5*(7-2-1)*(7-2+2)->gl
1-pchisq(TB,gl)
'ma=0.2690809   mg=0.1836392  T=39.79575  gl=14   p-valor=0.0002745175'
ma
mg
TB
gl
'Rechazo la hipótesis nula, no son todas iguales. Queda información en las sobrantes'

# 4.4 Decisión final
'Considerando que Rao sugiere m=2, el codo sugiere m=2 
y el porcentaje de varianza acumulada es alto (80%), 
la decisión final es quedarse con 2 componentes'













