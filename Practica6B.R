# -----------------------------------------------------------------------------
# PRÁCTICA 3B: Regresión Multinomial
# -----------------------------------------------------------------------------

# 1. Conjunto de datos y análisis descriptivo previo
'Analizar cómo afectan las variables write y ses en el programa a cursar por el
 estudiante'

library("tidyverse")
mydata <- read.csv2("Datos/RegMultinomial_example.csv")
summary(mydata)
View(mydata)

'Pasamos las variables prog y ses a tipo factor:'
mydata$prog <- factor(mydata$prog)
mydata$ses <- factor(mydata$ses, levels = c("low", "middle", "high"))
# ponemos el argumento levels para ordenar niveles
summary(mydata)

'Análisis descriptivo inicial'
# Diagramas de caja comparativos de "write" por nivel
mydata %>%
  ggplot(aes(x = prog, y = write)) +
  geom_boxplot(aes(color = prog))

# Boxplot por nivel del predictor "ses"
mydata %>%
  ggplot(aes(x = prog, y = write)) +
  geom_boxplot(aes(fill=prog))+
  facet_grid (.~ ses)

'Si hay celdas vacías el modelo RMultinomial puede ser inestable y poco fiable'
addmargins(table(mydata$ses, mydata$prog,
                 dnn = c("status", "program")))
# Observamos influencia de los predictores write y ses en la variable respuesta


# --------------------------------------------

# 2. Estimación de los parámetros del modelo RMultinomial y métodos de selección de regresores
'El modelo teórico de Regresión Multinomial supone que el logaritmo de los odds de pertenecer
 a una clase de la variable respuesta frente a pertenecer a la clase de referencia viene explicado por un modelo
 lineal de los predictores. En este caso:
 log(pj/pref) = @0^j + @1^j·write + @2^j·sesmiddle + @3^j·seshigh
 donde pj es la prob de pertenecer a la clase j y pref la prob de
 pertenecer a la clase de referencia (academic)'

'Las variables sesmiddle y seshigh son binarias, son 1 si ses se corresponde con su nivel, 0 en caso contrario'

'Tomamos el programa academico como clase de referencia'
library("nnet")
mydata$prog <- relevel(mydata$prog, ref = "academic")
mymultinom <- multinom(prog ~ ses + write, data = mydata)

summary(mymultinom)

'De los resultados anteriores, obtenemos que el modelo lineal ajustado 
 que explica el logaritmo de los odds de participar en el programa 
 general frente al académico viene dado por:
 Coefficients -> general     2.852198 -0.5332810 -1.1628226 -0.0579287
 
 y el modelo lineal ajustado que explica el log de los odds de participar en el programa
 vocacional frente al academico viene dado por: 
 Coefficients -> vocation    5.218260  0.2913859 -0.9826649 -0.1136037
'

'La interpretación de los coeficientes sería:
 - por cada unidad adicional de write, el log de los odds general disminuye 0.0579
 - por cada unidad adicional de write, el log de los odds vocacional disminuye 0.1136037
 - el log de los odds general decrecerá en 1.1628 si pasamos de estatus bajo a alto
 ...'


'Métodos de selección de regresores backward, fordward y stepwise, 
 para ver si el modelo completo es reducible a otro más sencillo'
modelo_backward <- step(mymultinom, direction = "backward")

modelo_nulo <- multinom(prog ~ 1, data = mydata)
modelo_forward <- step(modelo_nulo, scope = formula(mymultinom),
                       direction = "forward")

modelo_stepwise <- step(modelo_nulo, scope = formula(mymultinom),
                        direction = "both")
# Los 3 métodos conducen al modelo completo

'La bondad del ajuste se mide con AIC (criterio de Akaike), mejor ajuste cuanto menor sea el AIC.
 También se puede medir con la devianza residual, mejor ajuste cuanto menor sea la devianza'
mymultinom$AIC #Valor AIC del modelo

mymultinom$deviance #Valor de la devianza

#Comprobamos que la devianza del modelo coincide con -2*log(likelihood)
-2*logLik(mymultinom)


# -----------------------------------------

# 3. Inferencias en el modelo RMultinomial. Predicciones
'Una vez validado el modelo RMultinomial, tenemos garantías para realizar inferencias con dicho modelo.
 Obtención de intervalos de confianza para los parámetros de refresión, prueba de significación,
 pruebas individuales de significación de los predictores (test de Wald).'

'Intervalos de confianza para los parámetros de regresión'
confint(mymultinom, level = 0.95)

# Variaciones producidas sobre los odds (sin log)
exp(coef(mymultinom))

'Significación del modelo comparando devianza del modelo completo frente al nulo'
diferencia_devianzas <- modelo_nulo$deviance - mymultinom$deviance
n = nrow(mydata)
c = nlevels(mydata$prog)
l = length(mymultinom$coefnames)
df_nulo = n - 1*(c-1)
df_mymultinom = n- l*(c-1)
grados_libertad <- df_nulo - df_mymultinom
p_valor <- pchisq(diferencia_devianzas, df = grados_libertad, lower.tail = FALSE)
p_valor
# el p-valor (1.063001e-08) permite concluir que el modelo completo es significativo.

'La signficación individual de cada predictor se obtiene con el test de Wald y el estadístico Z.'
valores_z <- summary(mymultinom)$coefficients/summary(mymultinom)$standard.errors
valores_z

p_valores <- 2*(1 - pnorm(abs(valores_z), 0, 1))
p_valores
# El estatus económico medio (sesmiddle) no es significativo (p-valores > 0.10).

'Predicciones de las probabilidades asociadas a cada clase (argumento type = "probs") y de la clase de
 la variable respuesta (argumento por defecto, type = "class").'
# Estudiante con write=58 y ses=high, veremos que lo clasificaría como que cursará el programa academic.
nuevos_1 <- data.frame(write = 58, ses = "high")
predict(mymultinom, newdata = nuevos_1, type = "probs")

predict(mymultinom, newdata = nuevos_1)

'Los valores ajustados nos proporcionan, para cada fila de inputs (predictores), 
 la probabilidad de pertenecer a cada uno de los 3 niveles de la variable respuesta'
fitted(mymultinom)

'Podemos mantener un predictor constante y variar el otro:'
dses <- data.frame(ses = c("low", "middle", "high"), write = mean(mydata$write))
predict(mymultinom, newdata = dses, type = "probs")

predict(mymultinom, newdata = dses, type = "class")
'Interpretación: para un alumno con nota de la prueba escrita igual a la media, 
 lo más probable es que seleccione el programa académico sea cual sea su estatus social, 
 aumentando dicha probabilidad conforme aumenta el estatus.'


# Ahora variamos los resultados de la variable write:
dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41),
                     write = rep(c(30:70), 3))
dwrite
'Tabla con alumnos ficticios que cubren las notas de 30 a 70 para los 3 niveles'

# Guardamos las probabilidades predichas para cada valor de ses y write
pp.write <- cbind(dwrite, predict(mymultinom, newdata = dwrite, type = "probs",
                                  se = TRUE))
pp.write
'Le digo a mi modelo que mire los alumnos que he creado y devuelva probabilidades
 Luego el cbind (unir columnas) coge la tabla ficticia y le pega las probabilidades'


# Modificamos el dataframe para usar ggplot2 y representar las probabilidades predichas.
pp.write_longer <- pivot_longer(pp.write, cols = c(3:5),
                                names_to = "program", values_to = "probability")
head(pp.write_longer)
'Este comando coge las 3 columnas de probabilidades de pertenencia a clases distintas,
 las comprime y muestra una columna con el programa y su probabilidad'

# Representación
pp.write_longer %>%
  ggplot(aes(x = write, y = probability, colour = ses)) +
  geom_line() +
  facet_grid (program ~.)
'Por último, coge la tabla modificada, da valores a las variables, y usa 
 facet-grid(program ~.) para separar las lineas, es decir, divide el gráfico en 3 paneles.'

'Cuando aumenta la nota en write, los alumnos se decantan por la 
 parte académica, y ocurre lo contrario con la vocacional. Y tiene 
 poca relación con la clase general.'


# -----------------------------------------

# 4. La Regresión Multinomial como un problema de clasificación
'En RMultinomial la variable respuesta es categórica, al realizar las 
 predicciones usando el argumento type="class" estamos definiendo un clasificador.'

prog_predict <- predict(mymultinom, newdata = mydata, "class")
mydata_predict <- cbind(mydata, prog_predict)
mydata_predict  # Predicción y datos originales juntos

# Matriz de confusión 
matriz_confusion <- table(mydata_predict$prog, mydata_predict$prog_predict,
                          dnn = c("real", "predicho"))
matriz_confusion  # x = originales, y = predicción
'Filas (horizontal) son la realidad, columnas (vertical) son la predicción'

# Obtenemos una medida de bondad del ajuste (accuracy), dividiendo aciertos entre total de datos.
accuracy <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
accuracy
'En este ejemplo no hemos dividido el conjunto de datos en entrenamiento y test. Por
 tanto, se puede dar sobreajuste (overfitting) al calcular la matriz de confusión 
 y el accuracy (tasa de aciertos).'







