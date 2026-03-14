# -----------------------------------------------------------------------------
# PRÁCTICA 2A: Regresión Lineal Múltiple (RLM)
# -----------------------------------------------------------------------------

# 1. Conjunto de datos y análisis descriptivo previo
'Qué variables influyen principalmente en la cantidad de oxígeno consumida y
 proporcionar una ecuación que modelice la relación con el fin de realizar predicciones.
 El OXIGENO es la variable respuesta, mientras que el resto son los regresores o predictores del modelo'

'En una situación real, no dispondremos de la ecuación que relaciona las variables en estudio sino que debemos
 determinar qué variables influyen y estimar la ecuación que las relaciona.'

'El análisis descriptivo inicial debe contemplar, entre otros, diagramas de caja para cada variable por
 separado, con el fin de identificar outliers, y diagramas de dispersión por pares, con el fin de identificar
 relaciones lineales entre predictores o con la variable respuesta. También debemos hacer pruebas de normalidad
 de la variable respuesta para determinar si es necesario una transformación en los datos.'

library("readxl")
d<-read_xlsx('Datos/moore_simulado_def.xlsx')
View(d)

# En realidad se han simulado 3 modelos diferentes (uno lineal y dos no lineales)
d_lin <- d[, -c(7,8)] # Todas las columnas menos la 7 y la 8
d_cub <- d[, -c(6,8)]
d_exp <- d[, -c(6,7)]

'Empezamos con la variable respuesta OXIGENO_lineal, que no requiere ninguna
 transformación previa en los datos'
boxplot(d_lin)
boxplot(d_lin$OXIGENO_lineal, xlab = "OXIGENO_lineal")  # Variable respuesta
which.max(d_lin$OXIGENO_lineal) # Busco la observación atípica
plot(d_lin) # Relación clara entre TS y TVS (predictoras), el resto independientes
'Baja relación lineal con la variable respuesta de manera individual'

shapiro.test(d_lin$OXIGENO_lineal)  # p-valor = 0.03882 < 0.05 rechaza H0, no sigue una distribución normal
qqnorm(d_lin$OXIGENO_lineal)
qqline(d_lin$OXIGENO_lineal)

'En general, los datos atípicos no deben eliminarse de forma automática, sino que debemos
 realizar el análisis con y sin dichos datos para evaluar su efecto sobre el modelo, 
 si se trata verdaderamente de observaciones influyentes o no'

d_lin <- d_lin[-33, ]
boxplot(d_lin$OXIGENO_lineal)

shapiro.test(d_lin$OXIGENO_lineal) # p-valor = 0.3628 < 0.05, sigue sin tener normalidad
qqnorm(d_lin$OXIGENO_lineal)
qqline(d_lin$OXIGENO_lineal)

View(d_lin)
plot(d_lin)  # Comprobar que ya no hay atípicos

# --------------------------------------------

# 2. Estimación de los parámetros del modelo RLM y métodos de selección de regresores
'Realizar la estimación de los coeficientes del modelo RLM'

modelo_completo <- lm(OXIGENO_lineal ~ BOD + TKN + TS + COD + TVS ,
                      data = d_lin)
summary(modelo_completo)
# modelo_completo <- lm(OXIGENO_lineal ~. , data = datos_lin)
# Para usar todas las variables como regresores

'La variable respuesta se predice con la fórmula: 
 OXIGENO−lineal = −5.28448 + 0.01494·BOD + 0.05676·TKN + 0.02326·TS + 0.02668·COD + 6.65127·TVS
 Miro en la parte de coeficientes la columna de Estimate'
'La bondad del ajuste se mide con el valor de R-cuadrado (multiple R-squared) o el 
 R-cuadrado ajustado (adjusted R-squared), que al ser superiores a 0.8 nos indican un buen ajuste.
 Mejor ajustado para comparar la bondad del ajuste de modelos con distinto número de predictores (regresores).'
'p-valores altos (superiores a 0.10), indicativo de que el modelo es reducible'


# Modelo de selección de regresores: backward, fordward y stepwise
modelo_backward <- step(modelo_completo, direction = "backward")
'Primero se elimina la variable TKN (la que mayor p-valor tenía), después BOD y finalmente
 TS, quedándose con solo dos variables COD y TVS'
modelo_backward$coefficients


#Ajuste usando solo la cte
modelo_cte <- lm(OXIGENO_lineal ~ 1 , data = d_lin)

modelo_forward <- step(modelo_cte, direction = "forward",
                       scope = formula(modelo_completo))
modelo_forward$coefficients   # mismo modelo que se obtuvo con backward


modelo_stepwise <- step(modelo_cte, direction = "both",
                        scope = formula(modelo_completo))
modelo_stepwise$coefficients
'Por la relación lineal entre los regresores TS y TVS, propondremos como 
 alternativa el modelo que contempla los predictores TS y COD'

modelo_final_1 <- lm(OXIGENO_lineal ~ TVS + COD, data = d_lin)
summary(modelo_final_1)

modelo_final_2 <- lm(OXIGENO_lineal ~ TS + COD, data = d_lin)
summary(modelo_final_2)
'Todos los regresores del modelo final son significativos, 
 la significación (p-valor del contraste) correspondiente 
 a las variables TVS y COD es prácticamente cero, luego no podemos reducir más el modelo.'
'Bondades del ajuste altas en ambos casos, es decir, los modelos de regresión obtenidos explican aproximadamente el 84%
 de la variabilidad del oxígeno consumido por los microorganismos'

# -----------------------------------------

# 3. Inferencias en el modelo RLM. Predicciones
'Tenemos garantías para realizar inferencias con dicho modelo. 
 Estas inferencias incluyen la obtención de intervalos de confianza para
 los parámetros de regresión, intervalos de confianza para la media de 
 la variable respuesta e intervalos de predicción para la respuesta.'

confint(modelo_final_1, level=0.95)
confint(modelo_final_2, level=0.99)

nuevos <- data.frame(TVS = 0.85, COD = 70)
predict(modelo_final_1, newdata = nuevos, interval = "confidence", level = 0.95)
predict(modelo_final_1, newdata = nuevos, interval = "prediction", level = 0.95)
'El valor esperado para el oxígeno (cuando el agua tiene esos valores de TVS y COD) 
 estaría entre 3.92644 y 4.364941, y su predicción entre 3.338115 4.953266, 
 con una confianza del 95%'

# -----------------------------------------

# 4. Validación del modelo
'Hay que verificar: - Hipótesis de Normalidad (residuos siguiendo una distribución Normal)
                    - Hipótesis de Homocedasticidad (varianza cte en los errores(residuos))
                    - Hipótesis de Independencia (residuos indendientes)'

# Normalidad
shapiro.test(modelo_final_1$residuals)  # p-valor > 0.05 siguen distribución normal
qqnorm(modelo_final_1$residuals)
qqline(modelo_final_1$residuals)  # Se ve claramente aquí

plot(modelo_final_1$residuals)  # Ver residuos

# Homocedasticidad
plot(modelo_final_1$fitted.values, modelo_final_1$residuals)
'fitted.values son las predicciones del modelo (valores ajustados)
 residuals son los errores (lo eliminado respecto a la realidad)
 Por tanto, los residuos no dependen de la magnitud de la variable predicha,
 es decir, buscamos que haya una nube aleatoria, no un patrón'

# Independencia
ts.plot(modelo_final_1$residuals)
library("lmtest")
dwtest(modelo_final_1, alternative="two.sided")
'Los residuos no dependen de la fila y el p-valor = 0.9628 (> 0.10) 
 nos indica que se puede suponer la independencia'

# Multicolinealidad
'Calculo el factor de varianza inflada (VIF) para cada predictor. 
 Comprobamos que no hay multicolinealidad en el modelo final calculando los VIFs'
# library("rms")  NO FUNCIONA
library("car")
vif(modelo_final_1) # Ambos cerca de 1 (genial)

'Sí existe multicolinealidad en el modelo completo
 (recordar que TS y TVS tienen fuerte relación lineal)'
vif(modelo_completo)
'Si el VIF está cerca de 1, la variable aporta información única y fresca.
 Si el VIF supera el valor de 7, significa alarma roja: esa variable 
 está repetida o súper correlacionada con otra.'

# Última prueba, distancia de Cook
cook <- cooks.distance(modelo_final_1)
plot(cook)
'Si la Distancia de Cook supera el valor de 1, está distorsionando tus resultados.
 Con el gráfico observamos que todos están debajo de 1, no hay ninguna observación influyente.
 Si no hubiesemos borrado la observación 33 hubiesemos tenido una influyente.'

# -----------------------------------------

# 5. Transformaciones de los datos (familia Box-Cox)
'Identificar la transformación más adecuada sobre la variable respuesta.
 Consiste en elevar la variable respuesta a un exponente λ > 0, 
 o bien tomar logaritmos neperianos si resulta λ = 0.'

d_cub <- d_cub[-33, ] # Quitamos el 33 para analizar los datos
d_exp <- d_exp[-33, ]

library("MASS")
boxcox(lm(OXIGENO_lineal ~ 1, data = d_lin), lambda = seq(-3, 3, 1/10))
boxcox(lm(OXIGENO_lineal ~ 1, data = d_lin), lambda = seq(-3, 3, 1/10), plotit = FALSE)
'La trasformacion identidad es adecuada (para lambda igual a 1 se alcanza la máxima 
 verosimilitud). Por tanto, no hay que transformar la variable respuesta OXIGENO_lineal.'


boxcox(lm(OXIGENO_cubica ~ 1, data = d_cub), lambda = seq(-3, 3, 1/10))
boxcox(lm(OXIGENO_cubica ~ 1, data = d_cub), lambda = seq(-3, 3, 1/10), plotit = FALSE)
'La trasformacion adecuada es tomar raíz cúbica sobre la variable respuesta (para
 lambda igual a 1/3 aproximadamente se alcanza la máxima verosimilitud).
 Tras esta transformación se obtiene el mismo modelo del caso lineal anterior'

d_cub$Y <- d_cub$OXIGENO_cubica^(1/3)
modelo_cubico <- lm(Y ~ TVS + COD, data = d_cub)
summary(modelo_cubico)  # Los coeficientes coinciden con el modelo lineal
summary(modelo_final_1) # OK


boxcox(lm(OXIGENO_exponencial ~ 1, data = d_exp), lambda = seq(-3, 3, 1/10))
boxcox(lm(OXIGENO_exponencial ~ 1, data = d_exp), lambda = seq(-3, 3, 1/10), plotit = FALSE)
'La trasformacion adecuada es tomar logaritmos neperianos sobre la variable
 respuesta (para lambda igual a cero aproximadamente se alcanza la máxima verosimilitud)'

d_exp$Y <- log(d_exp$OXIGENO_exponencial)
modelo_exponencial <- lm(Y ~ TVS + COD, data = d_exp)
summary(modelo_exponencial)
summary(modelo_final_1) # OK

# -----------------------------------------

# 6. Consideraciones adicionales
'Necesitamos ajustar modelos con alguna o varias variables predictoras elevadas 
 a una potencia y considerar interacciones entre las variables predictoras.'

# Modelo con predictores elevados a potencias
# modelo_polinomial <- lm(OXIGENO_lineal ~ I(COD^3) + COD + pol(TVS,3), data = d_lin)
# La función pol() no funciona porque no tenemos la librería rms
modelo_polinomial <- lm(OXIGENO_lineal ~ I(COD^3) + COD + poly(TVS,3, raw = TRUE), data = d_lin)
summary(modelo_polinomial)


# Modelo con interacción | Con COD:TVS estamos indicando que se incluya interacción
modelo_interaccion <- lm(OXIGENO_lineal ~ COD + TVS + COD:TVS, data = d_lin)
summary(modelo_interaccion)


'Obsérvese que la introducción de varias potencias de un mismo regresor
 puede dar lugar a problemas de multicolinealidad.'
vif(modelo_polinomial)
'Sale distinto porque no he usado pol(), he usado poly(), da mal igualmente'











