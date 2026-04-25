# -----------------------------------------------------------------------------
# PRÁCTICA 3A: Regresión Logística
# -----------------------------------------------------------------------------

# 1. Conjunto de datos y análisis descriptivo previo
'Analizar cómo afectan las variables gre, gpa y rank en la admisión de los alumnos.
 Disponemos de tres predictores, 2 de ellos de tipo continuo (gre y gpa) y 1 categórico (rank),
 y que la variable respuesta (admit) es binaria'

library("tidyverse")
d <- read.csv("Datos/binary.csv")
summary(d)

# Pasamos las variables admit y rank a tipo factor (agrupa y te dice cuantos con ese valor)
d$rank <- factor(d$rank)
d$admit <- factor(d$admit)
summary(d)

# Diagramas de caja comparativos
d %>%
  ggplot(aes(x = admit, y = gre)) + geom_boxplot(aes(color = admit))

d %>%
  ggplot(aes(x = admit, y = gpa)) + geom_boxplot(aes(color = admit))
'Se pueden hace igual con boxplot(d$gre ~ d$admit)
                          boxplot(d$gpa ~ d$admit)'

# Separamos boxplot para cada nivel de la categoría rank
d %>%
  ggplot(aes(x = admit, y = gre)) + geom_boxplot(aes(fill = admit)) + facet_grid(.~ rank)
'Hay 4 niveles, pues van a salir 4 boxplots comparativos iguales que el primero'

d %>%
  ggplot(aes(x = admit, y = gpa)) + geom_boxplot(aes(fill = admit)) + facet_grid (.~ rank)

# Separados los 4 admitidos y los 4 no admitidos
ggplot(data = d, aes(x = admit, y = gre, color = rank)) +
  geom_boxplot()+
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data = d, aes(x = admit, y = gpa, color = rank)) +
  geom_boxplot()+
  theme_bw() +
  theme(legend.position = "bottom")
'Observamos algunos valores atípicos tanto en gpa como en gre que merecerían una
 interpretación más detallada. Por ejemplo:
 Observación atípica con una nota muy baja en gpa, procedente de un centro tipo 4 y que no ha sido admitido
 y otro con nota muy baja en gpa, procedente de un centro tipo 1 pero que sí ha sido admitido.'

'No se detectan en un plot normal, ni hay correlación entre ellos'
plot(d$gpa, d$gre, pch = as.integer(d$admit))
legend('bottomright', legend=c('0','1'), pch=1:2)


'Si hay celdas vacías o con pocos casos en las tablas de contingencia, 
 el modelo RLogistica puede ser inestable y poco fiable. '
addmargins(table(d$admit, d$rank, dnn = c("admit", "rank")))  # Tabla de contingencia

'Las puntuaciones en gre y gpa son más altas para los alumnos admitidos en la universidad que para los no
 admitidos. Y mirando la tabla de contingencia anterior, observamos que la tasa de admitidos frente a no
 admitidos es mayor para los centros educativos de procedencia con más prestigio (rank = 1).'

# --------------------------------------------

# 2. Estimación de los parámetros del modelo RLogistica y métodos de selección de regresores
'El modelo teórico de Regresión Logística para estos datos viene dado por:
 log(p/(1 − p)) = θ0 + θ1 · gre + θ2 · gpa + θ3 · rank2 + θ4 · rank3 + θ5 · rank4'

'Estimamos el modelo logístico usando todos los predictores. Para ello, 
 usamos la función glm() de R que sirve para estimar los modelos lineales generalizados'

logit <- glm(admit ~ gre + gpa + rank, data = d, family = "binomial")
summary(logit)
'Obtenemos que el modelo lineal ajustado viene dado por: (col. estimate)
 −3.989979 + 0.002264 · gre + 0.804038 · gpa − 0.675443 · rank2 − 1.340204 · rank3 − 1.551464 · rank4
 La interpretación de los coefientes: 
 - por cada gre, el log de admitido frente no admitido aumenta 0.002264
 - por cada gpa, el log de admitido frente no admitido aumenta 0.804038
 - asistir a un centro de rank2, hace que disminuya el log en 0.675443'

'Obtenemos también p-valores, son significativos (<0.05) por tanto, 
 sospechamos que el modelo no va a ser reducible.'


# Aplicamos métodos de selección de regresores:
modelo_backward <- step(logit, direction = "backward")   # Modelo no reducible

modelo_nulo <- glm(admit ~ 1, data = d, family = "binomial")
modelo_forward <- step(modelo_nulo, scope = formula(logit), direction = "forward")

modelo_stepwise <- step(modelo_nulo, scope = formula(logit), direction = "both")

'Mismos resultados:
# library("MASS")
# modelo_backward2 <- stepAIC(mylogit, direction = "backward")
# modelo_forward2 <- stepAIC(modelo_nulo, scope = formula(mylogit), direction = "forward")
# modelo_stepwise2 <- stepAIC(modelo_nulo, scope = formula(mylogit), direction = "both")'


'La bondad del ajuste del modelo se puede medir a través del AIC (criterio de Akaike), siendo mejor el
 ajuste cuanto menor sea el AIC. También se puede usar la devianza residual, siendo mejor el ajuste cuanto
 menor sea la devianza.'
logit$aic       # Valor AIC del modelo
logit$deviance  # Valor de la devianza
-2*logLik(logit)# Coincide con el valor de la devianza

# --------------------------------------------

# 3. Inferencias en el modelo RLogistica. Predicciones
'Tenemos garantías para realizar inferencias con dicho modelo.'

# Intervalos de confianza para los parámetros de regresión:
confint(logit, level = 0.95)

'Tomando exponenciales sobre los coeficientes y sobre los extremos del
 intervalo de confianza, medimos las variaciones producidas sobre los odds directamente'
# odds ratios
exp(coef(logit))
# odds ratios con sus Intervalos de Confianza
exp(cbind(OR = coef(logit), confint(logit, level = 0.95)))
'OR > 1 -> factor de éxito (aumenta prob)
 OR < 1 -> factor de fracaso (disminuye prob)'

# Significación del modelo comparando la devianza del completo frente al nulo
diferencia_devianzas <- logit$null.deviance - logit$deviance
grados_libertad <- logit$df.null - logit$df.residual
p_valor <- pchisq(diferencia_devianzas, df = grados_libertad, lower.tail = FALSE)
p_valor   # 7.578194e-08 < 0.05 -> el modelo completo es significativo

# Estudio de la significación conjunta de la variable rank
library("aod")
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 4:6)
'Los términos 4, 5 y 6 del resumen del modelo son rank2, rank3 y rank4'
summary(logit)
'Le digo coge los terminos 4, 5 y 6 y juzgalos a la vez, el p-valor permite concluir 
 que la variable rank es significativa (0.00011 < 0.05)'

'Podemos comparar si hay diferencia entre los coeficientes de dos niveles de rank.
 Hay que mirar la combinación, por ejemplo para rank2 y rank3:'
combinacion <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(logit), Sigma = vcov(logit), L = combinacion)
'El p-valor 0.019 concluye que hay diferencia significativa entre ambas'

'Predicción de alumno nuevo, type="response" para variables respuesta y 
 type="link" para el modelo lineal ajustado'
nuevos_1 <- data.frame(gre = 750, gpa = 3.5, rank = as.factor(2))
predict(logit, newdata = nuevos_1)
predict(logit, newdata = nuevos_1, type = "response")
'La probabilidad de ser admitido es de 0.4618316. log(p/1-p)=-0.1529712'

'Para ver el efecto del prestigio del centro de procedencia en la admisión, 
 podemos fijar gre y gpa en sus valores medios y variar rank'
nuevos_2 <- with(d, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
nuevos_2$rank_predic <- predict(logit, newdata = nuevos_2, type = "response")
nuevos_2
'Comprobamos que la variable rank es muy relevante, el alumno será admitido si rank=1
 (probabilidad superior a 0.5) y no será admitido en los otros casos'

'Fijamos gpa en su media, movemos rank en sus 4 niveles y hacemos un grid con el predictor gre. 
 Así vemos el efecto de la nota del examen de acceso (predictor gre) en la admisión'
nuevos_3 <- with(d,
                 data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                      4),
                            gpa = mean(gpa),
                            rank = factor(rep(1:4, each = 100))))
nuevos_3$gre_predic <- predict(logit, newdata = nuevos_3, type = "response")
nuevos_3
'Devuelve directamente la probabilidad real de que la universidad admita a cada uno.'

# -----------------------------------------

# 4. Validación del modelo
'Para que las inferencias sean válidas hay que verificar:
 Linealidad, Independencia, NO Multicolinealidad, NO Influyentes, Matriz representativa:
 esto es no hay celdas vacías y no hay problema de separación perfecta'

# Linealidad
comp_res_gre <- coef(logit)["gre"]*d$gre + logit$residuals
comp_res_gpa <- coef(logit)["gpa"]*d$gpa + logit$residuals
d2 <- d
View(d)
d2$comp_res_gre <- comp_res_gre
d2$comp_res_gpa <- comp_res_gpa

# Buscamos una relación lineal con el Log-Odds
ggplot(data = d2, aes(x = gre, y = comp_res_gre)) +
  geom_point() +
  geom_smooth(color = "red", method = "lm", linetype = 2, se = F) +
  geom_smooth(se = F)

ggplot(data = d2, aes(x = gpa, y = comp_res_gpa)) +
  geom_point() +
  geom_smooth(color = "red", method = "lm", linetype = 2, se = F) +
  geom_smooth(se = F)
'OK. Abraza la línea roja discontinua que es la relación lineal perfecta'

# Independencia (no hay patrones) 
ts.plot(logit$residuals)
'OK'

# Multicolinealidad
library("car")
vif(logit)
'OK. Todos cerca de 1, hay que mirar el GVIF'

# Valores influyentes
'Medidas para cuantificar cómo cambiaría nuestro modelo estimado si
 excluyéramos una observación; DFFITS: predicción | DFBETAS: coeficiente de regresión'
valores_dffits <- dffits(logit)
valores_dfbetas <- as.data.frame(dfbetas(logit))
plot(valores_dffits)
plot(valores_dfbetas$'(Intercept)')
plot(valores_dfbetas$gre)
plot(valores_dfbetas$gpa)
plot(valores_dfbetas$rank2)
plot(valores_dfbetas$rank3)
plot(valores_dfbetas$rank4)
'No se identifica ninguna observación especialmente alejada del resto,
 de manera que concluimos que no existen observaciones influyentes'

# -----------------------------------------

# 5. La Regresión Logística como un problema de clasificación
'En RLogística la variable respuesta es binaria, con Y=0 (no admitidos)
 y con Y=1 (sí admitidos). La predicción representa la probabilidad
 de que Y=1, si es >= 0.5 la clasificamos en el grupo Y=1.'

predic_grupos <- if_else(condition = logit$fitted.values >= 0.5,
                         true = 1, false = 0)
'La matriz de confusión comparando los valores de la muestra es:'
matriz_confusion <- table(d$admit, predic_grupos,
                          dnn = c("observado", "predicciones"))
matriz_confusion
'> matriz_confusion
         predicciones
observado   0   1
        0 254  19
        1  97  30
        
Realidad: 254+19 = 273 no admitidos
          97+30 = 127 admitidos
Predicción: 254+97 = 351 no admitidos
            19+30 = 49 admitidos

         predicciones
observado   0   1
        0  VN  FP
        1  FN  VP'

VP <- matriz_confusion[2, 2]
FN <- matriz_confusion[2, 1]
VN <- matriz_confusion[1, 1]
FP <- matriz_confusion[1, 2]
sensibilidad <- VP/(VP+FN)
sensibilidad

especificidad <- VN/(VN+FP)
especificidad

accuracy <- (VP+VN)/(VP+FP+VN+FN)
accuracy

# Otra forma de calcular el accuracy es dividiendo aciertos entre total de datos
sum(diag(matriz_confusion)) / sum(matriz_confusion)

'Si queremos aumentar la sensibilidad (individuos grupo Y=1) hay que bajar el umbral.'
# Umbral fijado 0.3
predic_grupos2 <- if_else(condition = logit$fitted.values >= 0.3,
                          true = 1, false = 0)
matriz_confusion2 <- table(d$admit, predic_grupos2,
                           dnn = c("observado", "predicciones"))
matriz_confusion2

VP2 <- matriz_confusion2[2, 2]
FN2 <- matriz_confusion2[2, 1]
VN2 <- matriz_confusion2[1, 1]
FP2 <- matriz_confusion2[1, 2]
sensibilidad2 <- VP2/(VP2+FN2)
sensibilidad2
especificidad2 <- VN2/(VN2+FP2)
especificidad2
accuracy2 <- (VP2+VN2)/(VP2+FP2+VN2+FN2)
accuracy2


'La curva ROC representa la sensibilidad frente a 1-especifidad, osea la
 proporción de VP frente a falsas alarmas. Se obtiene modificando el umbral.'
coordenada_ROC <- c(1 - especificidad, sensibilidad)
coordenada_ROC

coordenada_ROC2 <- c(1 - especificidad2, sensibilidad2)
coordenada_ROC2

# Representamos los puntos de la curva ROC
plot(coordenada_ROC[1], coordenada_ROC[2], xlim = c(0, 1), ylim = c(0, 1),
     col = "blue", xlab = "1-especificidad", ylab = "sensibilidad")
par(new = TRUE)
plot(coordenada_ROC2[1], coordenada_ROC2[2], xlim = c(0, 1), ylim = c(0, 1),
     col = "red", xlab = "1-especificidad", ylab = "sensibilidad")
curve(1*x, 0, 1, add = TRUE)  # Línea del azar
'El punto azul es la coordenada ROC para el umbral 0.5, el rojo 
 es para el umbral 0.3. En ambos, los puntos están por encima de la recta
 por tanto la tasa de aciertos (sensibilidad) es mejor que la de falsas alarmas.
 Es decir, el modelo es mejor que el azar.'

# Obtenemos la curva ROC:
library("pROC")
roc(d$admit, logit$fitted.values, plot = TRUE,
    legacy.axes = TRUE, percent = FALSE,
    xlab = "1-especificidad", ylab = "sensibilidad",
    col = "blue", lwd = 2, print.auc = TRUE)

# AUC cercano a 0.7 -> rendimiento aproximadamente del 70% (mejor que el azar)

'En este ejemplo no hemos dividido el conjunto de datos en entrenamiento y test. Por
 tanto, se puede dar sobreajuste (overfitting) al calcular la matriz de confusión 
 y las medidas de bondad del ajuste'

# -----------------------------------------

# 6. Consideraciones adicionales
'Necesitamos ajustar modelos con alguna o varias variables predictoras elevadas 
 a una potencia, así como considerar interacciones entre las variables predictoras.'

mylogit_nolineal <- glm(admit ~ gre + gpa + rank + I(gre^2) + I(gpa^2) + I(gre*gpa),
                        data = d, family = "binomial")
summary(mylogit_nolineal)
'Las potencias I(x^2) nos da permiso para dibujar parábolas.
 La interacción I(x*y), hacemos una sinergia y multiplicarlos
 permite que el modelo entienda la relación de dependencia.'

'En este caso, el modelo resultante es reducible al haber p-valores > 0.1.
 Variable con estrellas -> variable buena (p-valor<0.1).
 La variables nuevas tienen un p-valor alto y no sirven de nada,
 esto significa que la relación entre gre y gpa y la admisión ya 
 era bastante lineal, ahora al añadir los otros hay multicolinealidad.
 Por tanto tendría que usar los modelos de selección para coger variables.'

'La Regresión Logística también puede realizarse con la función lrm() del paquete rms.'
# library("rms")
# mylogit_new <- lrm(admit ~ gre + gpa + rank, data = d)
# summary(mylogit_new)
















