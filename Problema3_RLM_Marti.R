library("readxl")
datos <- read_xlsx("cemento_RLM.xlsx")

View(datos)
str(datos)
summary(datos)
names(datos)

# 1) Modelo usando todos los predictores A, B, C y D

# a) Ajuste con lm()
modelo_completo <- lm(HEAT ~ A + B + C + D, data = datos)
summary(modelo_completo)
modelo_completo$coefficients

# El modelo ajustado con lm() es:
# HEAT = 62.4054 + 1.5511A + 0.5102B + 0.1019C - 0.1441D

# b) Ajuste usando la inversa de (t(M)%*%M)
y <- datos$HEAT
M <- cbind(1, datos$A, datos$B, datos$C, datos$D)
theta <- solve(t(M) %*% M) %*% t(M) %*% y
theta

# El vector de coeficientes obtenido es:
# (Intercepto, A, B, C, D) =
# 62.4054, 1.5511, 0.5102, 0.1019, -0.1441

# Conclusión:
# El ajuste obtenido con lm() y con la fórmula matricial de mínimos cuadrados coincide.
# Esto ocurre porque ambos métodos resuelven el mismo problema de mínimos cuadrados.

# c) Gradiente descendente
# Este método no aparece desarrollado en la Práctica 2A.
# Si se exige usar solo lo de la práctica, este apartado no debería desarrollarse.
# En caso de hacerlo igualmente, habría que implementarlo aparte.

# 2) Repetir usando solo los predictores A y D

# a) Ajuste con lm()
modelo_AD <- lm(HEAT ~ A + D, data = datos)
summary(modelo_AD)
modelo_AD$coefficients

# El modelo ajustado con lm() es:
# HEAT = 103.0974 + 1.4400A - 0.6140D

# b) Ajuste usando la inversa de (t(M)%*%M)
M2 <- cbind(1, datos$A, datos$D)
theta2 <- solve(t(M2) %*% M2) %*% t(M2) %*% y
theta2

# El vector de coeficientes obtenido es:
# (Intercepto, A, D) =
# 103.0974, 1.4400, -0.6140

# Conclusión:
# De nuevo, el ajuste con lm() y el ajuste matricial coinciden exactamente,
# ya que ambos calculan los estimadores de mínimos cuadrados del mismo modelo.
