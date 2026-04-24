library("readxl")
datos <- read_xlsx("cemento_RLM.xlsx")
View(datos)
summary(datos)

# 1) Descriptivo
boxplot(datos)
plot(datos)
shapiro.test(datos$HEAT)
qqnorm(datos$HEAT)
qqline(datos$HEAT)

'Con los diagramas de caja no aparecen atípicos claros en ninguna de las 5 variables.
Además, la variable respuesta HEAT pasa el test de normalidad, p-valor = 0.3857.
Por tanto, sí podemos suponer normalidad de la variable respuesta.
Lo más relevante del descriptivo es:
HEAT tiene media ≈ 95.42
A, B y D parecen las variables con más relación con HEAT
No hay indicios de outliers por caja'


# 2) Correlaciones
cor(datos)

'La matriz de correlaciones informa de la intensidad y signo de la relación lineal entre pares de variables.
La relación más fuerte entre regresores es:
corr(B, D) = -0.9730.
B y D están fuertemente correlacionadas entre sí, así que son candidatas a generar colinealidad.
La primera variable que debería entrar en el modelo es D, porque es la que tiene mayor correlación lineal en valor absoluto con la respuesta:
|corr(D, HEAT)| = 0.8213
aunque B está prácticamente al mismo nivel.

CONCLUSIÓN:
D sería la primera en entrar y B y D son los regresores con relación lineal más estrecha entre sí.'


# 3) Modelo completo
modelo_completo <- lm(HEAT ~ A + B + C + D, data = datos)
summary(modelo_completo)

# Selección backward
modelo_backward <- step(modelo_completo, direction = "backward")
summary(modelo_backward)
modelo_backward$coefficients

'Orden de salida:
1. C'

# Selección forward
modelo_cte <- lm(HEAT ~ 1, data = datos)
modelo_forward <- step(modelo_cte, direction = "forward", scope = formula(modelo_completo))
summary(modelo_forward)
modelo_forward$coefficients

'Orden de entrada: 
1. D
2. A
3. B'

'Coeficientes: HEAT=71.6483+1.4519A+0.4161B−0.2365D
Bondad de ajuste (se mide con el valor de R-cuadrado (multiple R-squared) o el 
R-cuadrado ajustado (adjusted R-squared): 
multiple R-squared: 0.9824
adjusted R-squared: 0.9764'


# 4) Colinealidad  
library("car")          # No funciona library("rms")
vif(modelo_backward)
vif(modelo_forward)

'VIF(A) = 1.07
VIF(B) = 18.78
VIF(D) = 18.94
Como en la práctica se toma que valores superiores a 7 indican 
multicolinealidad, sí hay colinealidad fuerte entre B y D.

Decisión para solventarlo:
No conviene dejar juntas B y D.
La solución es comparar modelos alternativos quitando una de las dos.
Los dos modelos razonables son:
HEAT ~ A + B
HEAT ~ A + D
Ambos evitan la colinealidad.'

# Esto de aquí arriba también es del 5, que explica el por que de los modelos.
# 5) ¿Propondrías un único modelo o varios? ¿Cuál o cuáles y por qué? 
modelo_AB <- lm(HEAT ~ A + B, data = datos)
summary(modelo_AB)

modelo_AD <- lm(HEAT ~ A + D, data = datos)
summary(modelo_AD)


# 6) Intervalos de confianza al 95%
modelo_AB$coefficients
confint(modelo_AB, level = 0.95)

modelo_AD$coefficients
confint(modelo_AD, level = 0.95)

'Para el modelo A + B:
HEAT = 52.5773+1.4683A+0.6623B
Intervalo de confianza:
Intercepto: [47.4834, 57.6713]
A:[1.1980, 1.7386]
B:[0.5601, 0.7644]

Para el modelo A + D:
HEAT = 103.0974+1.4400A−0.6140D
Intervalo de confianza:
Intercepto: [98.3649, 107.8299]
A:[1.1315, 1.7484]
D:[−0.7223, −0.5056]
'


# 7) Validación del modelo A y D
modelo_AD <- lm(HEAT ~ A + D, data = datos)
shapiro.test(modelo_AD$residuals)
qqnorm(modelo_AD$residuals)
qqline(modelo_AD$residuals)
plot(modelo_AD$fitted.values, modelo_AD$residuals)
ts.plot(modelo_AD$residuals)
library(lmtest)
dwtest(modelo_AD, alternative="two.sided")
vif(modelo_AD)
cook <- cooks.distance(modelo_AD)
cook
plot(cook)

'Normalidad:
Los residuos son compatibles con normalidad, p-valor = 0.9468

Homocedasticidad:
El gráfico residuos frente a ajustados no muestra patrón claro ni ensanchamiento apreciable.

Independecia:
Con el gráfico de la serie de residuos observamos que los residuos oscilan 
alrededor de 0 y no presentan una tendencia creciente o decreciente ni un 
patrón periódico claro. Además, el contraste de Durbin-Watson da un valor 
próximo a 2, lo que indica que no hay evidencia de autocorrelación.

Colinealidad:
VIF(A) = 1.06
VIF(D) = 1.06

No hay colinealidad.

Observaciones influyentes:
La máxima distancia de Cook es: 0.3675
Como no supera 1, no hay observaciones influyentes importantes.
'

# 8) Predicción para A=15, B=39, C=4.5, D=40
# Si el modelo final usa solo A y D:
nuevo1 <- data.frame(A = 15, D = 40)
predict(modelo_AD, newdata = nuevo1)
predict(modelo_AD, newdata = nuevo1, interval = "confidence", level = 0.95)
predict(modelo_AD, newdata = nuevo1, interval = "prediction", level = 0.95)

'Estimación puntual: HEAT = 100.1386
Intervalo de confianza para la media (95%): [96.8717, 103.4055]
Intervalo de predicción individual (95%): [93.2257, 107.0515]

¿Superará las 95 cal/gr?: no podemos asegurarlo, porque el intervalo de predicción
contiene valores por debajo de 95.

¿Y en promedio?: Sí. Como el intervalo de confianza de la media es [96.8717, 103.4055]
entero por encima de 95, sí podemos concluir que en promedio superará las 95 cal/gr.
'
# Esto sería si fuese con el modelo completo:
modelo_completo <- lm(HEAT ~ A + B + C + D, data = datos)

nuevo <- data.frame(A = 15, B = 39, C = 4.5, D = 40)

predict(modelo_completo, newdata = nuevo)
predict(modelo_completo, newdata = nuevo, interval = "confidence", level = 0.95)
predict(modelo_completo, newdata = nuevo, interval = "prediction", level = 0.95)


# 9) Predicción para A=45 y D=40
nuevo2 <- data.frame(A = 45, D = 40)
predict(modelo_AD, newdata = nuevo2)
predict(modelo_AD, newdata = nuevo2, interval = "confidence", level = 0.95)
predict(modelo_AD, newdata = nuevo2, interval = "prediction", level = 0.95)

'Estimación puntual: HEAT = 143.3374
Intervalo de confianza para la media (95%): [131.3332, 155.3415]
Intervalo de predicción individual (95%): [129.8711, 156.8036]

Aquí sí, tanto para una nueva observación como en promedio, el calor supera 
claramente las 95 cal/gr.
'