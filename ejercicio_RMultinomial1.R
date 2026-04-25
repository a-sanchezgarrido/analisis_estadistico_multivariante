
# Ejercicios RMultinomial
# Problema 1:

# --------------------------------------------

'El fichero iris de R, contiene los datos correspondientes a medidas de los pétalos y sépalos
 de tres variedades de flor de iris (setosa, virginica y versicolor).
 Se desea realizar un análisis de Regresión Multinomial con el fin de predecir la variedad
 de la flor en función de las magnitudes de sus pétalos y sépalos. Se pide: '

'1) Recuperar los datos y realizar un estudio descriptivo previo atendiendo a nuestro
objetivo. '

library("tidyverse")
d <- iris
summary(d)
View(d)

plot(iris[, 1:4], col = iris$Species, main = "Matriz de dispersión Iris")

# --------------------------------------------

'2) Dividir el conjunto de datos en entrenamiento y prueba (70% entrenamiento,
 30% prueba). Tomar semilla 123.'

set.seed(123)

ntotal<-nrow(d)
n_entrenamiento<-round(0.7*ntotal)

filas_entrenamiento <- sample(1:ntotal, size = n_entrenamiento)
filas_entrenamiento

d_entrenamiento <- d[filas_entrenamiento, ] # 70%
d_test <- d[-filas_entrenamiento, ]         # 30% 
View(d_test)

# --------------------------------------------

'3) Con los datos de entrenamiento, obtener el modelo ajustado de Regresión
 Multinomial usando todos los predictores. '

'En la regresión multinomial, se coge una categoría para usarla como pivote,
 por defecto se coge por orden alfabético. Se coge la especie Setosa:
 ¿Qué probs hay de que sea Versicolor/Virginica en vez de Setosa?'

library("nnet")
mymultinom <- multinom(Species ~ ., data = d_entrenamiento)
summary(mymultinom)
'Coefficients:
            (Intercept) Sepal.Length Sepal.Width Petal.Length Petal.Width
 versicolor     63.7972    -27.80712   -27.99961      71.5816    18.78823
 virginica    -107.2881    -56.45906   -61.59348     140.6447    82.34126

 Versicolor -> ln(P(versi)/P(setosa)) = 63.7972 - 27.80712·Sepal.L - 27.99961·Sepal.W + ...

 Residual Deviance: 1.218832 -> cantidad de "error" que no se ha explicado
 AIC: 21.21883 -> equilibrio entre aciertos y ser simple'

# --------------------------------------------

'4) Con los datos de entrenamiento, aplicar los métodos de selección de regresores
 para comprobar si el modelo completo es reducible.'

modelo_backward <- step(mymultinom, direction = "backward")

modelo_nulo <- multinom(Species ~ 1, data = d)
modelo_forward <- step(modelo_nulo, scope = formula(mymultinom),
                       direction = "forward")

modelo_stepwise <- step(modelo_nulo, scope = formula(mymultinom),
                        direction = "both")

summary(modelo_backward)
'               Df      AIC
 <none>         10 21.21883
 El modelo es irreducible'

summary(modelo_forward)
'                Df      AIC
 <none>           8 29.26653
 + +Sepal.Length 10 31.89973
 El forward nos dice que si es reducible, se queda con todas menos Sepal.Length'

summary(modelo_stepwise)
'                Df      AIC
 <none>           8 29.26653
 + +Sepal.Length 10 31.89973
 - Sepal.Width    6 32.57901
 - Petal.Length   6 39.39931
 - Petal.Width    6 43.51576
 El stepwise nos dice de nuevo que es reducible, se queda todas menos Sepal.Length'

'Como el AIC del modelo_backward es el más bajo (21.21883), nos quedamos con él:
 Todas las variables son nuestro modelo, tomamos el modelo completo.'

# --------------------------------------------

'5) Con el modelo resultante del apartado anterior, obtener medidas de 
 bondad del ajuste e indicar si el modelo es significativo.'

mymultinom$AIC # Valor AIC del modelo: 21.21883
mymultinom$deviance #Valor de la devianza: 1.218832

'Significación del modelo comparando devianza del modelo completo frente al nulo'
diferencia_devianzas <- modelo_nulo$deviance - mymultinom$deviance
n = nrow(d)
c = nlevels(d$Species)
l = length(mymultinom$coefnames)
df_nulo = n - 1*(c-1)
df_mymultinom = n- l*(c-1)
grados_libertad <- df_nulo - df_mymultinom
p_valor <- pchisq(diferencia_devianzas, df = grados_libertad, lower.tail = FALSE)
p_valor
# el p-valor (3.734788e-66 < 0.05) permite concluir que el modelo completo es significativo.

'La signficación individual de cada predictor se obtiene con el test de Wald y el estadístico Z.'
valores_z <- summary(mymultinom)$coefficients/summary(mymultinom)$standard.errors
valores_z

p_valores <- 2*(1 - pnorm(abs(valores_z), 0, 1))
p_valores
'           (Intercept) Sepal.Length Sepal.Width Petal.Length Petal.Width
 versicolor   0.5936673    0.5031914  0.34227066  0.114124182 0.534419256
 virginica    0.3695915    0.1740520  0.03669608  0.001907569 0.006473231
 
 Para diferenciar a la Virginica de la Setosa: Sepal.Width, Petal.Length, Petal.Width
 son significativas (< 0.1); mientras que para diferenciar Versicolor frente a Setosa
 ninguno de los predictores es significativo (todos los p-valores > 0.1).
 Esto es porque hay una separación perfecta entre ambas especies y por eso
 aunque el modelo sea significativo nos da p-valores altos para estas variables.'

# --------------------------------------------

'6) Veamos ahora el problema de Regresión Multinomial como un problema de
 clasificación. Obtener la clase predicha para los datos del conjunto de prueba.'

d_test_predict <- predict(mymultinom, newdata = d_test, "class")
d_predict <- cbind(d_test, d_test_predict)
d_predict  # Predicción y datos originales juntos

'84   6.0   2.7   5.1   1.6   versicolor    virginica 
 -> Es versicolor y se ha clasificado como virginica, es el único error del modelo'

# --------------------------------------------

'7) Obtener la matriz de confusión para el conjunto de prueba y medir la eficiencia
 del clasificador con el “accuracy” (número de aciertos en la clasificación dividido
 entre número de datos totales).'

matriz_confusion <- table(d_predict$Species, d_predict$d_test_predict,
                          dnn = c("real", "predicho"))
matriz_confusion
'            predicho
 real         setosa versicolor virginica
   setosa         14          0         0
   versicolor      0         17         1
   virginica       0          0        13

 Como hemos resaltado antes, el 84 es el único error del modelo al predecirse'

accuracy <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
accuracy
'Obtenemos un accuracy de 0.9777778 (97.78%). Esto indica que la eficiencia 
 global del clasificador es excelente, siendo capaz de generalizar muy bien 
 ante datos nuevos. El único fallo cometido refleja la dificultad natural de 
 separar las especies Versicolor y Virginica en sus casos frontera.'











