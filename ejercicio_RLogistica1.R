
# Ejercicios RLogística
# Problema 1:

# --------------------------------------------

'Datos correspondientes a un estudio sobre enfermedad
 cardíaca realizada por Cleveland Clinic Foundation.
 14 columnas, correspondientes a las siguientes variables:
 age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, tal y num.
 La variable “num” toma valores 0, 1, 2, 3 y 4, indicando el tipo de anomalía cardíaca.'

'Se desea realizar un análisis de Regresión Logística con el fin de 
 predecir la presencia (o no) de enfermedad cardíaca en función del 
 resto de variables (predictores). Se pide: '


'1) Importar los datos del fichero processed.cleveland.data y poner el nombre de cada
 variable como se indica en el enunciado. Sustituir la variable “num” por una nueva
 variable llamada “disease” que valga 0 si no hay enfermedad y que valga 1 cuando
 haya anomalía cardíaca.'

library("tidyverse")
nombres_variables <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
                       "thalach", "exang", "oldpeak", "slope", "ca", "tal", "num")
d <- read.csv("Datos/processed.cleveland.data", col.names = nombres_variables, na.strings = "?")
summary(d)
View(d)

d$disease<-ifelse(d$num == 0, 0, 1)

d$num<-NULL
View(d)

# --------------------------------------------

'2) Eliminar todas las filas que tengan algún valor perdido. IMPORTANTE:
 confirmar primero si todas las variables son de tipo numérico para identificar
 adecuadamente los valores perdidos.'

d <- na.omit(d)

# --------------------------------------------

'3) Pasar a tipo factor las variables que por naturaleza sean de tipo categórico.'

d$sex <- factor(d$sex)
d$cp <- factor(d$cp)
d$fbs <- factor(d$fbs)
d$restecg <- factor(d$restecg)
d$exang <- factor(d$exang)
d$slope <- factor(d$slope)
d$ca <- factor((d$ca))
d$tal <- factor(d$tal)
d$disease <- factor(d$disease)
summary(d)

# --------------------------------------------

'4) Dividir el conjunto de datos en entrenamiento y prueba (70% entrenamiento, 
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

'5) Con los datos de entrenamiento, obtener el modelo ajustado de Regresión
 Logística usando todos los predictores. ¿Son todos los predictores significativos? '

logit <- glm(disease ~ ., data = d_entrenamiento, family = "binomial")
summary(logit)
'Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -4.242981   3.603172  -1.178 0.238969    
age         -0.043510   0.032074  -1.357 0.174930    
sex1         1.940435   0.683372   2.839 0.004518 ** 
cp2          1.448952   0.918169   1.578 0.114545    
cp3         -0.203372   0.797775  -0.255 0.798782    
cp4          2.213266   0.795912   2.781 0.005423 ** 
trestbps     0.038500   0.015161   2.539 0.011104 *  
chol         0.002936   0.005499   0.534 0.593430    
fbs1        -1.516165   0.879781  -1.723 0.084827 .  
restecg1    -1.310777   9.640498  -0.136 0.891848    
restecg2     0.790779   0.501850   1.576 0.115089    
thalach     -0.032775   0.014928  -2.195 0.028129 *  
exang1       1.042263   0.565254   1.844 0.065200 .  
oldpeak      0.732811   0.325008   2.255 0.024149 *  
slope2       0.887689   0.599486   1.481 0.138673    
slope3       1.076804   1.137338   0.947 0.343753    
ca           1.189398   0.345356   3.444 0.000573 ***
tal6         0.523613   1.153499   0.454 0.649876    
tal7         1.374113   0.536994   2.559 0.010500 * 

 -> No todos los predictores son significativos (p-valor < 0.05)
 Son significativos: sex1, cp4, trestbps, thalach, oldpeak, ca, tal7.'

# --------------------------------------------

'6) Obtener las predicciones para los datos del conjunto de prueba, es decir, la
 probabilidad predicha de padecer enfermedad cardíaca para cada individuo del
 conjunto de testeo.'

pred_test <- predict(logit, newdata = d_test, type = "response")
pred_test

# --------------------------------------------

'7) Veamos ahora el problema de Regresión Logística como un problema de
 clasificación. Usando las predicciones del apartado anterior y tomando como
 punto de corte la probabilidad de 0.5, obtener la clase predicha para los individuos
 del conjunto de prueba. Medir la eficiencia del modelo calculando la matriz de
 confusión, accuracy, sensibilidad y especificidad. '

predic_grupos <- if_else(condition = pred_test >= 0.5,
                         true = 1, false = 0)

'La matriz de confusión comparando los valores de la muestra es:'
matriz_confusion <- table(d_test$disease, predic_grupos,
                          dnn = c("observado", "predicciones"))
matriz_confusion

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

# --------------------------------------------

'8) Para los datos del conjunto de prueba, obtener la curva ROC del método de
 clasificación, calcular el AUC (área bajo la curva) e interpretar el resultado.'

library("pROC")
roc(d_test$disease, pred_test, plot = TRUE,
    legacy.axes = TRUE, percent = FALSE,
    xlab = "1-especificidad", ylab = "sensibilidad",
    col = "blue", lwd = 2, print.auc = TRUE)

'El AUC es de 0.8796, al ser muy cercano a 1, interpretamos que nuestro 
 modelo tiene una capacidad muy buena para discriminar y separar correctamente
 a los pacientes sanos de los enfermos'

# --------------------------------------------

'9) Repetir el análisis (apartado 5 y siguientes) pero aplicando primero los métodos
 de selección de regresores, con el fin de proponer un modelo más parsimonioso.'

logit$aic
modelo_backward <- step(logit, direction = "backward")   # Modelo no reducible
'disease ~ sex + cp + trestbps + fbs + thalach + exang + oldpeak + 
    ca + tal
           Df Deviance    AIC
<none>          126.36 152.36
- fbs       1   129.34 153.34
- exang     1   129.67 153.67
- tal       2   133.00 155.00
- trestbps  1   131.68 155.68
- thalach   1   132.39 156.39
- sex       1   137.49 161.49
- ca        1   137.68 161.68
- oldpeak   1   138.46 162.46
- cp        3   145.50 165.50'

modelo_nulo <- glm(disease ~ 1, data = d_entrenamiento, family = "binomial")
modelo_forward <- step(modelo_nulo, scope = formula(logit), direction = "forward")
'disease ~ tal + ca + cp + oldpeak + slope + sex + trestbps + 
    exang + thalach

          Df Deviance    AIC
<none>         195.72 223.72
+ chol     1   194.19 224.19
+ fbs      1   194.94 224.94
+ age      1   195.61 225.61
+ restecg  2   193.62 225.62'

'La diferencia es que el forward coge "slope" pero el backward "fbs" por
 tanto lo que tengo que mirar es el AIC (el de <none>), 152.36 < 223.72 luego,
 me quedo con el modelo backward.'

# ---

# Repetimos los apartados desde el 5)

summary(modelo_backward)
'La mayoría de los predictores son altamente significativos (p<0.05),
 algunas variables como fbs y exang se mantienen en el modelo a pesar de tener 
 un p-valor>0.05 ya que el modelo prioriza minimizar el AIC.'

# 6)
pred_test_ajustado <- predict(modelo_backward, newdata = d_test, type = "response")
pred_test_ajustado


# 7)
predic_grupos_ajustado <- if_else(condition = pred_test_ajustado >= 0.5,
                         true = 1, false = 0)

'La matriz de confusión comparando los valores de la muestra es:'
matriz_confusion2 <- table(d_test$disease, predic_grupos_ajustado,
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


# 8)
library("pROC")
roc(d_test$disease, pred_test_ajustado, plot = TRUE,
    legacy.axes = TRUE, percent = FALSE,
    xlab = "1-especificidad", ylab = "sensibilidad",
    col = "blue", lwd = 2, print.auc = TRUE)

'El AUC es de 0.878, al ser muy cercano a 1, interpretamos que nuestro 
 modelo tiene una capacidad muy buena para discriminar y separar correctamente
 a los pacientes sanos de los enfermos

 No necesitamos saber la edad del paciente (age), ni su colesterol (chol), ni 
 hacerle un electro en reposo (restecg) para saber si tiene una enfermedad cardíaca'


# --------------------------------------------

# --------------------------------------------

# Problema 2

# --------------------------------------------

'¿Qué sucede en el problema anterior si no se realiza el apartado 4? Es decir, qué
 sucede si no separamos el conjunto de datos en dos subconjuntos: entrenamiento
 y prueba.
 Puedes intentar repetir todo el ejercicio en este nuevo escenario y ver qué sucede
 con las medidas de bondad del ajuste o medidas de eficiencia del método
 clasificador.'

logit2 <- glm(disease ~ ., data = d, family = "binomial")
summary(logit2)


pred2 <- predict(logit2, newdata = d, type = "response")
pred2


predic_grupos2 <- if_else(condition = pred2 >= 0.5,
                         true = 1, false = 0)

'La matriz de confusión comparando los valores de la muestra es:'
matriz_confusion3 <- table(d$disease, predic_grupos2,
                          dnn = c("observado", "predicciones"))
matriz_confusion3

VP3 <- matriz_confusion3[2, 2]
FN3 <- matriz_confusion3[2, 1]
VN3 <- matriz_confusion3[1, 1]
FP3 <- matriz_confusion3[1, 2]
sensibilidad3 <- VP3/(VP3+FN3)
sensibilidad3

especificidad3 <- VN3/(VN3+FP3)
especificidad3

accuracy3 <- (VP3+VN3)/(VP3+FP3+VN3+FN3)
accuracy3


library("pROC")
roc(d$disease, pred2, plot = TRUE,
    legacy.axes = TRUE, percent = FALSE,
    xlab = "1-especificidad", ylab = "sensibilidad",
    col = "blue", lwd = 2, print.auc = TRUE)

'El AUC es de 0.935, al ser muy cercano a 1, interpretamos que nuestro 
 modelo tiene una capacidad muy buena para discriminar, sin embargo al 
 no hacer una partición de los datos, el AUC y las medidas de eficiencia son
 artificialmente altas y optimistas. Esto pasa por el sobreajuste.'










