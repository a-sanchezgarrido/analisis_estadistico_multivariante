# Alumno: Alejandro Sánchez Garrido

# Examen RLogística y RMultinomial


library(MASS)
library(tidyverse)
library(nnet)

d <- fgl
View(d)

summary(d)


d %>%
  ggplot(aes(x = type, y = RI)) +
  geom_boxplot(aes(color = type)) +
  labs(title = "Diagrama de caja de RI según el tipo de vidrio",
       x = "Tipo de vidrio",
       y = "RI")

d %>%
  ggplot(aes(x = type, y = Mg)) +
  geom_boxplot(aes(color = type)) +
  labs(title = "Diagrama de caja de Mg según el tipo de vidrio",
       x = "Tipo de vidrio",
       y = "Mg")

d %>%
  ggplot(aes(x = type, y = Al)) +
  geom_boxplot(aes(color = type)) +
  labs(title = "Diagrama de caja de Al según el tipo de vidrio",
       x = "Tipo de vidrio",
       y = "Al")

# dividir

set.seed(10)

ntotal<-nrow(d)
n_entrenamiento<-round(0.95*ntotal)

filas_entrenamiento <- sample(1:ntotal, size = n_entrenamiento)
filas_entrenamiento

d_entrenamiento <- d[filas_entrenamiento, ] 
d_test <- d[-filas_entrenamiento, ]         
View(d_test)

# multinom

library("nnet")
d_entrenamiento$type <- relevel(d_entrenamiento$type, ref = "Head")
mymultinom <- multinom(type ~ ., data = d_entrenamiento)
mymultinom_inter <- multinom(type ~ . + I(Mg*Ca), data = d_entrenamiento) 
summary(mymultinom)
summary(mymultinom_inter)

coef(mymultinom)
coef(mymultinom_inter)


# seleccion regresores

modelo_backward <- step(mymultinom, direction = "backward")

modelo_nulo <- multinom(type ~ 1, data = d_entrenamiento)
modelo_forward <- step(modelo_nulo, scope = formula(mymultinom),
                       direction = "forward")

modelo_stepwise <- step(modelo_nulo, scope = formula(mymultinom),
                        direction = "both")

mymultinom$AIC
modelo_backward$AIC
modelo_forward$AIC
modelo_stepwise$AIC

formula(modelo_backward)
formula(modelo_forward)
formula(modelo_stepwise)

# con inter

modelo_backward_i <- step(mymultinom_inter, direction = "backward")

modelo_nulo_i <- multinom(type ~ 1, data = d_entrenamiento)
modelo_forward_i <- step(modelo_nulo_i, scope = formula(mymultinom_inter),
                       direction = "forward")

modelo_stepwise_i <- step(modelo_nulo_i, scope = formula(mymultinom_inter),
                        direction = "both")

mymultinom_inter$AIC
modelo_backward_i$AIC
modelo_forward_i$AIC
modelo_stepwise_i$AIC

formula(mymultinom_inter)
formula(modelo_backward_i)
formula(modelo_forward_i)
formula(modelo_stepwise_i)

'RI + Na + Mg + Al + Si + K + Ca + Fe + I(Mg * Ca)'

# 

modelo_final_i <- modelo_backward_i


diferencia_devianzas <- modelo_nulo$deviance - mymultinom$deviance
n = nrow(d_entrenamiento)
c = nlevels(d_entrenamiento$type)
l = length(mymultinom$coefnames)
df_nulo = n - 1*(c-1)
df_mymultinom = n- l*(c-1)
grados_libertad <- df_nulo - df_mymultinom
p_valor <- pchisq(diferencia_devianzas, df = grados_libertad, lower.tail = FALSE)
p_valor

mymultinom$AIC #Valor AIC del modelo
mymultinom$deviance

'Intervalos de confianza para los parámetros de regresión'
confint(mymultinom, level = 0.90)


# inter
 
diferencia_devianzas_i <- modelo_nulo_i$deviance - modelo_final_i$deviance
n = nrow(d_entrenamiento)
c = nlevels(d_entrenamiento$type)
l = length(mymultinom_inter$coefnames)
df_nulo = n - 1*(c-1)
df_mymultinom = n- l*(c-1)
grados_libertad <- df_nulo - df_mymultinom
p_valor_i <- pchisq(diferencia_devianzas_i, df = grados_libertad, lower.tail = FALSE)
p_valor_i

modelo_final_i$AIC #Valor AIC del modelo
modelo_final_i$deviance

confint(modelo_final_i, level = 0.90)


# problema clasif

d_test_predict <- predict(mymultinom, newdata = d_test, "class")
d_predict <- cbind(d_test, d_test_predict)
d_predict  # Predicción y datos originales juntos
'Falla : 116  0.46 13.41 3.89 1.33 72.38 0.51  8.28 0.00 0.00 WinNF WinF'


matriz_confusion <- table(d_predict$type, d_predict$d_test_predict,
                          dnn = c("real", "predicho"))
matriz_confusion

accuracy <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
1-accuracy

# inter

d_test_predict_i <- predict(modelo_final_i, newdata = d_test, "class")
d_predict_i <- cbind(d_test, d_test_predict)
d_predict_i  # Predicción y datos originales juntos
'Falla : 116  0.46 13.41 3.89 1.33 72.38 0.51  8.28 0.00 0.00 WinNF WinF'

matriz_confusion_i <- table(d_predict_i$type, d_predict_i$d_test_predict,
                          dnn = c("real", "predicho"))
matriz_confusion_i

accuracy_i <- sum(diag(matriz_confusion_i)) / sum(matriz_confusion_i)
1-accuracy_i


# 2 
View(d)
d2 <- d %>%
  filter(type %in% c("Head", "WinNF"))

d2$Ca_categorica <- ifelse(d2$Ca <= 9, "bajo",
                              ifelse(d2$Ca < 11, "medio", "alto"))

# Pasamos a factor las variables categóricas
d2$type <- factor(d2$type)

d2$Ca_categorica <- factor(d2$Ca_categorica,
                              levels = c("bajo", "medio", "alto"))

summary(d2)

# representativa 

addmargins(table(d2$type, d2$Ca_categorica,
                 dnn = c("tipo", "Ca_categorica")))


# usar predictores

d2$type <- relevel(d2$type, ref = "Head")

# Ajustamos el modelo logístico
modelo_logit <- glm(type ~ Mg + Ca_categorica,
                    data = d2,
                    family = "binomial")

# Resumen del modelo para ver coeficientes y p-valores
summary(modelo_logit)
modelo_logit$coefficients


# interpretacion

# Odds ratios: exponencial de los coeficientes.
exp(coef(modelo_logit)) 

# Intervalos de confianza para los coeficientes.
confint(modelo_logit)   

# Intervalos de confianza para los odds ratios. 
exp(cbind(OR = coef(modelo_logit), confint(modelo_logit)))  # Calcula los odds ratios junto con sus intervalos de confianza.
# Primero junta en una tabla:cbind(OR = coef(modelo_logit), confint(modelo_logit)) y luego aplica exp()



# roc
library("pROC")
roc(d2$type, modelo_logit$fitted.values, plot = TRUE,
    legacy.axes = TRUE, percent = FALSE,
    xlab = "1-especificidad", ylab = "sensibilidad",
    col = "blue", lwd = 2, print.auc = TRUE)


































