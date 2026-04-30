# Cargamos los paquetes necesarios
library(MASS)
library(tidyverse)
library(nnet)

# Cargamos los datos Glass
Glass<-fgl
View(Glass)
summary(Glass)
str(Glass)

# Pasamos la variable respuesta type a factor
Glass$type <- factor(Glass$type)         # Creo que esto no es necesario porque ya es de tipo factor
summary(Glass)
str(Glass)

############################################################
# 1.a) Recuperar los datos y realizar un estudio descriptivo previo
#      atendiendo a nuestro objetivo.
#      Centrarse solo en analizar la influencia de los predictores
#      Na y Ca sobre el tipo de vidrio mediante diagramas
#      de caja-bigotes.
############################################################

# Resumen inicial de los datos
summary(Glass)

# Diagrama de caja-bigotes de Na según el tipo de vidrio
Glass %>%
  ggplot(aes(x = type, y = Na)) +
  geom_boxplot(aes(color = type)) +
  labs(title = "Diagrama de caja de Na según el tipo de vidrio",
       x = "Tipo de vidrio",
       y = "Na")

# Diagrama de caja-bigotes de Ca según el tipo de vidrio
Glass %>%
  ggplot(aes(x = type, y = Ca)) +
  geom_boxplot(aes(color = type)) +
  labs(title = "Diagrama de caja de Ca según el tipo de vidrio",
       x = "Tipo de vidrio",
       y = "Ca")


'Comentario de la gráfica de Na:
En el diagrama de caja de Na también se aprecian diferencias entre tipos de vidrio.
Los tipos Tabl y Head presentan valores de sodio más altos que los tipos WinF, WinNF, Veh y Con.
En concreto, Head y Tabl tienen medianas alrededor de 14-14.5, mientras que otros grupos tienen 
medianas más cercanas a 13.
El tipo Con parece tener valores de Na algo más bajos que Head y Tabl. También se observan valores
atípicos, por ejemplo un valor muy alto en Tabl y algunos valores bajos en Head y Con.
Por tanto, Na también parece influir en el tipo de vidrio, porque algunos tipos tienen distribuciones 
distintas. Sin embargo, hay solapamiento entre varios grupos, así que Na por sí sola no permite clasificar
perfectamente el tipo de vidrio.

Comentario de la gráfica de Ca:
En el diagrama de caja de Ca se observa que el calcio sí presenta diferencias entre algunos tipos de vidrio.
Los vidrios de tipo Con tienen valores de Ca claramente más altos que la mayoría de los demás tipos.
Su mediana está aproximadamente alrededor de 11, mientras que en otros grupos como WinF, WinNF, Veh y
Head los valores se concentran más cerca de 8-9. También se observan bastantes valores atípicos, 
especialmente en el grupo WinNF, donde aparecen valores muy altos de calcio. En Head aparecen algunos 
valores atípicos bajos.
Por tanto, Ca parece ser una variable útil para diferenciar algunos tipos de vidrio, especialmente el 
tipo Con, aunque no separa perfectamente todos los grupos.



Respuesta breve:
En los diagramas de caja se observa que tanto Na como Ca presentan diferencias entre los tipos de vidrio.
En Ca, el tipo Con destaca por tener valores más altos, mientras que WinNF presenta bastantes valores 
atípicos altos. En Na, los tipos Head y Tabl muestran valores generalmente más elevados que el resto, 
mientras que Con tiene valores algo más bajos. Aun así, existe solapamiento entre grupos, por lo que
estas variables ayudan a distinguir algunos tipos de vidrio, pero no permiten una separación perfecta 
por sí solas.'




############################################################
# 1.b) Dividir el conjunto de datos en entrenamiento y prueba
#      usando 95% para entrenamiento y 5% para prueba.
#      Usar la semilla 100 para realizar la división.
############################################################

set.seed(100)

n <- nrow(Glass)

ind_train <- sample(1:n, size = round(0.95*n))

train <- Glass[ind_train, ]
test <- Glass[-ind_train, ]

# Número de observaciones en entrenamiento y prueba
nrow(train)
nrow(test)


############################################################
# 1.c) Con los datos de entrenamiento, obtener el modelo ajustado
#      de Regresión Multinomial usando todos los predictores,
#      seleccionando como clase de referencia el vidrio tipo Head.
############################################################

# Fijamos Head como clase de referencia
train$type <- relevel(train$type, ref = "Head")

# Ajustamos el modelo multinomial completo
modelo_completo <- multinom(type ~ RI + Na + Mg + Al + Si + K + Ca + Ba + Fe,
                            data = train)

# Resumen del modelo
summary(modelo_completo)

# Coeficientes del modelo
coef(modelo_completo)


'Para WinF frente a Head
log(P(WinF)/P(Head)))=−144.35052−15.155665RI−12.24357Na+51.42799Mg−4.563343Al−5.380047Si+29.98010K+65.24629Ca+30.78836Ba+426.2444Fe
Para WinNF frente a Head
log(P(WinNF)/P(Head))=144.02282−14.849968RI−14.44312Na+46.62279Mg−2.831630Al−8.337338Si+28.15708K+61.85720Ca+26.26402Ba+429.0596Fe
Para Veh frente a Head
log(P(Veh)/P(Head)=42.72892−16.476038RI−12.36161Na+51.63145Mg−4.331511Al−8.156761Si+26.81702K+67.01300Ca+28.78285Ba+424.7400Fe
Para Con frente a Head
log(P(Con)/P(Head))=14.18939−14.584434RI−15.77203Na+41.06075Mg+13.163777Al−6.272237Si+25.59110K+60.02525Ca+23.84294Ba+411.5460Fe
Para Tabl frente a Head
log(P(Tabl)/P(Head))=−21.18007−7.087965RI+19.44700Na+20.07961Mg+16.955453Al−7.057637Si−235.46780K+24.61029Ca−67.80809Ba−647.9250Fe'


############################################################
# 1.d) Con los datos de entrenamiento, aplicar los métodos de
#      selección de regresores para comprobar si el modelo completo
#      es reducible.
#      Indicar el modelo resultante para cada método y justificar
#      cuál sería el modelo final adecuado atendiendo al AIC.
############################################################

# Selección backward: parte del modelo completo y elimina variables
modelo_backward <- step(modelo_completo, direction = "backward")

# Modelo nulo: modelo sin predictores
modelo_nulo <- multinom(type ~ 1, data = train)

# Selección forward: parte del modelo nulo y añade variables
modelo_forward <- step(modelo_nulo,
                       scope = formula(modelo_completo),
                       direction = "forward")

# Selección stepwise: combina forward y backward
modelo_stepwise <- step(modelo_nulo,
                        scope = formula(modelo_completo),
                        direction = "both")

# Comparación de AIC
modelo_completo$AIC
modelo_backward$AIC
modelo_forward$AIC
modelo_stepwise$AIC

# Fórmulas de los modelos seleccionados
formula(modelo_backward)
formula(modelo_forward)
formula(modelo_stepwise)


'Al aplicar los métodos backward, forward y stepwise, los tres seleccionan el mismo modelo reducido,
formado por los predictores RI, Mg, Al, K, Ca, Ba y Fe. El modelo completo tenía AIC = 343.2493,
mientras que el modelo reducido tiene AIC = 340.2547. Como el AIC menor corresponde al modelo reducido,
concluimos que el modelo completo es reducible y tomamos como modelo final el 
modelo type ~ RI + Mg + Al + K + Ca + Ba + Fe.'


############################################################
# 2.a) Considerar solo los vidrios de tipo Head y WinF,
#      eliminando el resto del estudio.
#      Crear una variable categórica a partir de K llamada
#      K_categorica:
#      - bajo si K <= 0.2
#      - medio si 0.2 < K < 0.5
#      - alto si K >= 0.5
#      Pasar a tipo factor las variables categóricas.
############################################################

# Nos quedamos solo con los tipos Head y WinF
Glass2 <- Glass %>%
  filter(type %in% c("Head", "WinF"))

# Creamos la variable K_categorica
Glass2$K_categorica <- ifelse(Glass2$K <= 0.2, "bajo",
                              ifelse(Glass2$K < 0.5, "medio", "alto"))

# Pasamos a factor las variables categóricas
Glass2$type <- factor(Glass2$type)

Glass2$K_categorica <- factor(Glass2$K_categorica,
                              levels = c("bajo", "medio", "alto"))

# Resumen de los nuevos datos
summary(Glass2)


############################################################
# 2.b) Comprobar que la matriz de datos es completamente
#      representativa.
#      Es decir, comprobar que se observan los dos niveles de la
#      variable respuesta para cada nivel del predictor categórico.
############################################################

# Tabla de contingencia entre type y K_categorica
addmargins(table(Glass2$type, Glass2$K_categorica,
                 dnn = c("tipo", "K_categorica")))


'La matriz de datos es completamente representativa, ya que para cada nivel de la 
variable categórica K_categorica se observan los dos niveles de la variable respuesta type.
En concreto, para K_categorica = bajo hay 18 vidrios WinF y 22 vidrios Head; para 
K_categorica = medio hay 4 vidrios WinF y 1 vidrio Head; y para K_categorica = alto hay 48 vidrios WinF y 6 vidrios Head.

Por tanto, no hay celdas vacías en la tabla de contingencia, lo cual es adecuado para ajustar
el modelo de regresión logística. Sin embargo, el nivel medio tiene pocos casos, 
especialmente solo 1 observación de tipo Head, por lo que esa categoría debe interpretarse con cierta cautela.'


############################################################
# 2.c) Obtener el modelo ajustado de Regresión Logística usando
#      los predictores Na y K_categorica.
#      Indicar si dichos predictores son significativos.
#      No eliminar ninguna variable.
############################################################

# Fijamos Head como clase de referencia
Glass2$type <- relevel(Glass2$type, ref = "Head")

# Ajustamos el modelo logístico
modelo_logit <- glm(type ~ Na + K_categorica,
                    data = Glass2,
                    family = "binomial")

# Resumen del modelo para ver coeficientes y p-valores
summary(modelo_logit)


'El modelo logístico ajustado es

log(P(WinF)/P(Head))=46.8536−3.3176Na+2.6503Kmedio−0.3994Kalto


El predictor Na resulta significativo porque su p-valor es 7.21e-06, menor que 0.05.
Sin embargo, los niveles medio y alto de K_categorica no son significativos, ya que 
sus p-valores son 0.405 y 0.636 respectivamente. Aun así, no se elimina ninguna variable, siguiendo el enunciado.'


############################################################
# 2.d) Dar la interpretación de los parámetros resultantes.
############################################################

# Coeficientes del modelo en escala logit.
coef(modelo_logit)    # Muestra los coeficientes del modelo en escala logit.

# Odds ratios: exponencial de los coeficientes.
exp(coef(modelo_logit))    # Convierte esos coeficientes en odds ratios.

# Intervalos de confianza para los coeficientes.
confint(modelo_logit)   # Calcula los intervalos de confianza de los coeficientes.
# Es lo mismo que poner esto: confint(modelo_logit, level=0.95)  

# Intervalos de confianza para los odds ratios. 
exp(cbind(OR = coef(modelo_logit), confint(modelo_logit)))  # Calcula los odds ratios junto con sus intervalos de confianza.
# Primero junta en una tabla:cbind(OR = coef(modelo_logit), confint(modelo_logit)) y luego aplica exp()


'El modelo compara la probabilidad de ser WinF frente a Head. El coeficiente de Na es -3.3176,
por lo que al aumentar Na una unidad, el logaritmo de los odds de ser WinF frente a Head disminuye en 3.3176.
Su odds ratio es 0.0362, por lo que los odds se multiplican por 0.0362. Por tanto, 
valores mayores de Na reducen la probabilidad relativa de ser WinF frente a Head.

Para K_categorica, la referencia es bajo. El nivel medio tiene coeficiente 2.6503 y 
odds ratio 14.16, por lo que sus odds serían mayores que en el nivel bajo, manteniendo fijo Na. 
Sin embargo, este efecto no es significativo. El nivel alto tiene coeficiente -0.3994 y odds ratio 0.67,
por lo que sus odds serían menores que en el nivel bajo, pero tampoco es significativo. 
Por tanto, la variable claramente significativa en el modelo es Na, mientras que K_categorica
no presenta un efecto significativo.'

 