# Para saber cual es la variable respuesta:
'Ahí pone que en motor.dat se midieron las variables:
VRP, VRS, Presion, Temp_Esc, Temp_Amb, LN_RFC, Empuje

y añade que se quiere proponer un modelo para predecir el “Empuje del motor” en función del resto de variables.
De ahí se deduce:
variable respuesta: Empuje
regresores: VRP, VRS, Presion, Temp_Esc, Temp_Amb, LN_RFC'

datos <- read.table("Datos/motor.dat", header = TRUE, sep = "\t", dec = ",")

View(datos)
str(datos)
summary(datos)
names(datos)

# 1) Descriptivo y normalidad
boxplot(datos)
boxplot(datos$EMPUJE) # Ahí hay uno
plot(datos)

shapiro.test(datos$EMPUJE)
qqnorm(datos$EMPUJE)
qqline(datos$EMPUJE)

# Si hace falta transformación logarítmica:
datos$Ynew <- log(datos$EMPUJE)

shapiro.test(datos$Ynew)
qqnorm(datos$Ynew)
qqline(datos$Ynew)

# La variable respuesta es EMPUJE y los regresores son VRP, VRS, PRESION, TEMP_ESC, TEMP_AMB y LN_RFC
# Con los diagramas de caja aparece al menos una observación atípica en la variable respuesta,
# pero el enunciado indica que no eliminemos ningún dato.
# La variable respuesta EMPUJE no sigue una distribución normal, ya que el p-valor del test
# de Shapiro-Wilk es 0.00345, menor que 0.05.
# Por tanto, no podemos asumir normalidad para EMPUJE.
# Sin embargo, la transformación logarítmica sí resulta adecuada, ya que para
# Ynew = log(EMPUJE) el p-valor del test de Shapiro-Wilk es 0.4463, mayor que 0.05.
# En consecuencia, a partir de aquí trabajamos con la variable transformada Ynew.

# 2) Correlaciones
cor(datos)
cor(datos[, c("VRP", "VRS", "PRESION", "TEMP_ESC", "TEMP_AMB", "LN_RFC", "Ynew")])

# La matriz de correlaciones informa de la intensidad y signo de la relación lineal entre pares de variables.
# La variable con mayor relación lineal con la respuesta transformada Ynew es PRESION:
# corr(PRESION, Ynew) = 0.8439
# Otras correlaciones con Ynew son:
# corr(LN_RFC, Ynew) = -0.2411
# corr(TEMP_ESC, Ynew) = 0.1483
# corr(TEMP_AMB, Ynew) = -0.1557
# corr(VRS, Ynew) = 0.0642
# corr(VRP, Ynew) = 0.0224
# No existen regresores altamente correlados dos a dos.
# La primera variable que debería entrar en el modelo es PRESION.

# 3) Modelo completo
modelo_completo <- lm(Ynew ~ VRP + VRS + PRESION + TEMP_ESC + TEMP_AMB + LN_RFC,
                      data = datos)
summary(modelo_completo)

# Selección backward
modelo_backward <- step(modelo_completo, direction = "backward")

summary(modelo_backward)
modelo_backward$coefficients

# Orden de salida:
# 1. VRS
# 2. TEMP_AMB
# 3. VRP

# Selección forward
modelo_cte <- lm(Ynew ~ 1, data = datos)
modelo_forward <- step(modelo_cte, direction = "forward",
                       scope = formula(modelo_completo))
summary(modelo_forward)
modelo_forward$coefficients

# Orden de entrada:
# 1. PRESION
# 2. TEMP_ESC
# 3. LN_RFC

# Selección stepwise
modelo_stepwise <- step(modelo_cte, direction = "both",
                        scope = formula(modelo_completo))
summary(modelo_stepwise)
modelo_stepwise$coefficients

# Tanto backward, forward como stepwise conducen al mismo modelo final:
# Ynew = 24.9610 + 0.004958*PRESION + 0.000472*TEMP_ESC - 1.7705*LN_RFC
# Bondad de ajuste:
# multiple R-squared: 0.7823
# adjusted R-squared: 0.7789

# 4) Multicolinealidad
library("car")
vif(modelo_backward)
vif(modelo_forward)
vif(modelo_stepwise)

# VIF(PRESION) = 1.02
# VIF(TEMP_ESC) = 1.01
# VIF(LN_RFC) = 1.02
# No hay multicolinealidad, ya que ningún VIF supera 7.



# 5) Modelo final propuesto
modelo_final <- modelo_backward
summary(modelo_final)

# Se propone un único modelo:
# Ynew = 24.9610 + 0.004958*PRESION + 0.000472*TEMP_ESC - 1.7705*LN_RFC
# porque los tres métodos de selección llevan al mismo resultado,
# no hay multicolinealidad y todos los coeficientes son significativos.
# El modelo explica aproximadamente el 78% de la variabilidad de log(EMPUJE).

# 6) Validación del modelo final

# Normalidad de residuos
shapiro.test(modelo_final$residuals)
qqnorm(modelo_final$residuals)
qqline(modelo_final$residuals)

# Homocedasticidad
plot(modelo_final$fitted.values, modelo_final$residuals)

# Independencia
ts.plot(modelo_final$residuals)
library("lmtest")
dwtest(modelo_final, alternative = "two.sided")

# Observaciones influyentes
cook <- cooks.distance(modelo_final)
cook
plot(cook)

# Normalidad:
# Los residuos son compatibles con normalidad, p-valor = 0.6204
#
# Homocedasticidad:
# El gráfico residuos frente a ajustados no muestra patrón claro ni ensanchamiento apreciable.
#
# Independencia:
# Los residuos oscilan alrededor de 0 y no presentan una tendencia clara.
# El estadístico de Durbin-Watson es 1.8130, próximo a 2, por lo que no hay
# evidencia importante de autocorrelación.
#
# Observaciones influyentes:
# La máxima distancia de Cook es 0.0459, muy por debajo de 1.
# Por tanto, no hay observaciones influyentes importantes.

# 7) Predicción
nuevo <- data.frame(PRESION = 180,
                    TEMP_ESC = 1700,
                    LN_RFC = 10.3089)

# Estimación puntual en escala log
predict(modelo_final, newdata = nuevo)

# Intervalo de confianza para la media en escala log
predict(modelo_final, newdata = nuevo, interval = "confidence", level = 0.95)

# Intervalo de predicción individual en escala log
predict(modelo_final, newdata = nuevo, interval = "prediction", level = 0.95)

# Pasamos a escala original:
pred_media_log <- predict(modelo_final, newdata = nuevo, interval = "confidence", level = 0.95)
pred_ind_log   <- predict(modelo_final, newdata = nuevo, interval = "prediction", level = 0.95)

exp(pred_media_log)
exp(pred_ind_log)

# Estimación puntual:
# log(EMPUJE) = 8.4048
# En escala original:
# EMPUJE = 4468.47
#
# Intervalo de confianza para la media (95%) en escala log:
# [8.3851, 8.4245]
# En escala original:
# [4381.21, 4557.48]
#
# Intervalo de predicción individual (95%) en escala log:
# [8.2626, 8.5470]
# En escala original:
# [3876.00, 5151.51]
#
# ¿Podemos concluir que el empuje será superior a 4000?
# No podemos asegurarlo para una observación individual, porque el intervalo
# de predicción contiene valores por debajo de 4000.
#
# ¿Y en promedio?
# Sí, porque todo el intervalo de confianza para la media está por encima de 4000.

