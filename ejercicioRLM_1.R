# Ejercicios RLM

# Problema 1:

library("readxl")
d<-read_xlsx('Datos/cemento_RLM.xlsx')
View(d)

'1) Realiza un análisis descriptivo previo de las variables del problema 
 y comenta los resultados más relevantes. ¿Podemos suponer que 
 nuestra variable respuesta es Normal? '

summary(d)
boxplot(d)  # No hay valores atípicos
boxplot(d$HEAT, xlab = "HEAT") # Variable respuesta

shapiro.test(d$HEAT)  # p-valor = 0.3857 > 0.05 acepta H0, sigue una distribución normal
qqnorm(d$HEAT)
qqline(d$HEAT)
# Se podría decir que sí que sigue una distribución normal

# --------------------------------------------

'2) Calcula la matriz de correlaciones de las cinco variables. ¿Qué información
 proporciona esta matriz? ¿Qué regresores del modelo presentan una más estrecha
 relación lineal entre sí? ¿Cuál es la primera variable que debería entrar en el modelo?'

matriz_d<-cor(d)
matriz_d
'Nos proporciona la fuerza y dirección de la relación lineal que existe entre cada par de variables
 Los regresores que presentan una mayor relación son B con D, y A con C.
 La primera variable en entrar debería ser D por su relación con HEAT'

# --------------------------------------------

'3) Realiza la selección del modelo mediante regresión por pasos, hacia delante y hacia
 atrás. Indica el orden de entrada y salida de las variables para cada uno de los métodos.
 Comenta los resultados obtenidos. '

modelo_completo <- lm(HEAT ~ ., data = d)
summary(modelo_completo)
'Multiple R-squared:  0.9824,	Adjusted R-squared:  0.9736, ambos > 0.8, buen ajuste
 Pr(>|t|) > 0.10 (menos A) luego es reducible'

modelo_backward <- step(modelo_completo, direction = "backward")  # Hacia atrás
modelo_backward$coefficients
# Primero eliminamos C (mayor p-valor), nos quedamos con A, B y D

modelo_cte <- lm(HEAT ~ 1 , data = d)
modelo_forward <- step(modelo_cte, direction = "forward", scope = formula(modelo_completo))
modelo_forward$coefficients   
# Empiezan entrando D, A y B, dejamos fuera C

modelo_stepwise <- step(modelo_cte, direction = "both", scope = formula(modelo_completo))
modelo_stepwise$coefficients
# Igual que con el método hacia adelante

modelo_final_1 <- lm(HEAT ~ A + B + D, data = d)
summary(modelo_final_1)

# --------------------------------------------

'4) Estudia si hay colinealidad entre los regresores de los modelos resultantes en el
 apartado anterior y en caso afirmativo explica cuál es tu decisión para solventarlo.'

# library("rms")  NO FUNCIONA
library("car")
vif(modelo_final_1) # A bien, pero B y D tienen colinealidad
'También lo puedo mirar como vif(modelo_forward)'

'Para eliminarla hay que eliminar B o D'
summary(modelo_final_1)

'Como D y B son los de mayor p-valor entonces elimino B y pruebo con A y D'
modelo_final_2 <- lm(HEAT ~ A + D, data = d)
summary(modelo_final_2)
vif(modelo_final_2) # Ahora sí ambos cerca de 1, genial

# --------------------------------------------

'5) ¿Propondrías un único modelo o varios? ¿Cuál o cuáles y por qué?'

'La primera propuesta sería el modelo_final_2 y la otra opción sería la análoga,
 en vez de las variables A y B coger C y D'
modelo_final_3 <- lm(HEAT ~ B + C, data = d)  # Creo que es mejor no tocar las de C
summary(modelo_final_3)
vif(modelo_final_3)
# Como podemos ver no hay colinealidad luego también es válido ese modelo
modelo_final_4 <- lm(HEAT ~ A + B, data = d)
summary(modelo_final_4)
vif(modelo_final_4) # Este también

modelo_final_5 <- lm(HEAT ~ C + D, data = d)
summary(modelo_final_5)
vif(modelo_final_5) # Y este también

# --------------------------------------------

'6) Determina el (los) modelo(s) ajustado(s) y los intervalos de confianza 
 al 95% para los parámetros de regresión.'

summary(modelo_final_2) # HEAT = 103.09738 + 1.43996·A - 0.61395·D
summary(modelo_final_3) # HEAT = 72.0747 + 0.7313·B - 1.0084·C
summary(modelo_final_4) # HEAT = 52.57735 + 1.46831·A + 0.66225·B
summary(modelo_final_5) # HEAT = 131.28241 - 1.19985·C - 0.72460·D

confint(modelo_final_2, level=0.95) # Entre 98.3648512 y 107.8299120
confint(modelo_final_3, level=0.95) # Entre 55.6234177 y 88.5259298
confint(modelo_final_4, level=0.95) # Entre 47.4834350 y 57.6712627
confint(modelo_final_5, level=0.95) # Entre 123.9857727 y 138.579040

# --------------------------------------------

'7) Para el modelo que contempla sólo los regresores A y D, estudia si se verifican las
 hipótesis del modelo de regresión múltiple, comentando los procesos utilizados.
 Estudia si hay colinealidad entre los regresores y si aparecen observaciones
 influyentes, comentando los procesos utilizados. En caso de que se presente alguno
 de estos problemas, explica cuál es tu decisión para solventarlo.'

# Normalidad OK
shapiro.test(modelo_final_2$residuals)  # p-valor > 0.05 siguen distribución normal
qqnorm(modelo_final_2$residuals)
qqline(modelo_final_2$residuals)  # Se ve claramente aquí

# Homocedasticidad OK
plot(modelo_final_2$fitted.values, modelo_final_2$residuals)
'Los residuos no dependen de la magnitud de la variable predicha,
 es decir, buscamos que haya una nube aleatoria, no un patrón'

# Independencia OK
ts.plot(modelo_final_2$residuals)
library("lmtest")
dwtest(modelo_final_2, alternative="two.sided")
'Los residuos no dependen de la fila y el p-valor = 0.693 (> 0.10) 
 nos indica que se puede suponer la independencia'

# Multicolinealidad OK
# library("rms")  NO FUNCIONA
library("car")
vif(modelo_final_2) # Ambos cerca de 1 

# Distancia de Cook OK
cook <- cooks.distance(modelo_final_2)
cook
plot(cook)  # Todos debajo de 1, no hay observaciones influyentes

'Se cumplen todas las hipótesis con éxito'

# --------------------------------------------

'8) Obtén una estimación puntual del calor emitido por el cemento sabiendo que A=15,
 B=39, C=4.5 y D=40. Determina también un intervalo de confianza para el calor
 emitido en ese caso, así como un intervalo de predicción. ¿Podemos concluir que el
 calor emitido por el cemento superará las 95 cal/gr? ¿Y en promedio? '

nuevos <- data.frame(A = 15, B = 39, C = 4.5, D = 40)
predict(modelo_final_2, newdata = nuevos, interval = "confidence", level = 0.95)
predict(modelo_final_2, newdata = nuevos, interval = "prediction", level = 0.95)
'Intervalo de confianza para el calor: (96.87177, 103.4055)
 Intervalo de predicción para el calor: (93.22567, 107.0515)
 Sí porque nos dice que la estimación es de 100.1386 cal/gr. En promedio, el intervalo 
 de confianza del 95% nos da un valor mínimo de 96.87177 > 95 cal/gr.'

# --------------------------------------------

'9) Responde a la cuestión anterior sabiendo que A=45 y D=40. '

nuevos2 <- data.frame(A = 45, B = 39, C = 4.5, D = 40)
predict(modelo_final_2, newdata = nuevos2, interval = "confidence", level = 0.95)
predict(modelo_final_2, newdata = nuevos2, interval = "prediction", level = 0.95)
'Confianza: (131.3281, 155.3467)
 Predicción: (129.8711, 156.8036)
 De nuevo, es mayor que 95 cal/gr tanto el intervalo como la estimación puntual'












