# Examen práctica 2A: Regresión Lineal
# Alejandro Sánchez Garrido


d <- longley

View(d)

library("MASS")
boxcox(lm(Unemployed ~ 1, data = d), lambda = seq(-3, 3, 1/10))
boxcox(lm(Unemployed ~ 1, data = d), lambda = seq(-3, 3, 1/10), plotit = FALSE)

d$Ynew <- d$Unemployed^(1/2)
View(d)


d<-d[ , c(1,2,4,5,8)]
View(d)

summary(d)

boxplot(d) 
plot(d)


shapiro.test(d$Ynew)  
qqnorm(d$Ynew)
qqline(d$Ynew)


# -----

matriz_d<-cor(d)
matriz_d

# ----

modelo_completo <- lm(Ynew ~ ., data = d)
summary(modelo_completo)
modelo_completo$coefficients
'Multiple R-squared:  0.8864,	Adjusted R-squared:  0.8451   ambas > 0.8, buen ajuste'

modelo_backward <- step(modelo_completo, direction = "backward")  # Hacia atrás
modelo_backward$coefficients


modelo_cte <- lm(Ynew ~ 1 , data = d)
modelo_forward <- step(modelo_cte, direction = "forward", scope = formula(modelo_completo))
modelo_forward$coefficients

# -----

summary(modelo_completo)
summary(modelo_forward)

modelo_backward <- step(modelo_completo, direction = "backward")  # Hacia atrás
modelo_backward$coefficients

modelo_cte <- lm(Ynew ~ 1 , data = d)
modelo_forward <- step(modelo_cte, direction = "forward", scope = formula(modelo_completo))
modelo_forward$coefficients


# ---
library("car")
vif(modelo_forward)
vif(modelo_backward)

summary(modelo_forward)


modelo_final_1 <- lm(Ynew ~ Armed.Forces + GNP, data = d)
vif(modelo_final_1)


# ----

shapiro.test(modelo_final_1$residuals)  
qqnorm(modelo_final_1$residuals)
qqline(modelo_final_1$residuals)  # Se ve claramente aquí

# Homocedasticidad OK
plot(modelo_final_1$fitted.values, modelo_final_1$residuals)

# Independencia OK
ts.plot(modelo_final_1$residuals)
library("lmtest")
dwtest(modelo_final_1, alternative="two.sided")

# Multicolinealidad OK
library("car")
vif(modelo_final_1) # Ambos cerca de 1 

# Distancia de Cook OK
cook <- cooks.distance(modelo_final_1)
cook
plot(cook)  # Todos debajo de 1, no hay observaciones influyentes


# ----

confint(modelo_final_1, level=0.90)


# ---
d<-longley
nuevos <- data.frame(GNP.deflator = 90, GNP = 300, Armed.Forces = 200, Population = 110)
predict(modelo_final_1, newdata = nuevos, interval = "confidence", level = 0.90)
predict(modelo_final_1, newdata = nuevos, interval = "prediction", level = 0.95)



predict(modelo_final_1, newdata = d$Unemployed, interval = "confidence", level = 0.90)




