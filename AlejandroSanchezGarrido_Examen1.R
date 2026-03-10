
# Examen 1

load("madres.rda")
d<-d[, 2:10]

# 1.1 Calcular Y1 y Y2
summary(d)  # Veo que debo de seguir
'Como hay escalas distintas usaremos correlaciones'
plot(d, pch=20, cex=0.8)

PCA <- princomp(d, cor=TRUE)
summary(PCA) # Para ver importancia

PCA$loadings
L<-PCA$loadings
'Las componentes principales se calculan con los siguientes coeficientes:
 0.02657788 -0.03003229  0.47587051  0.07608856  0.09228497  0.47117689  0.45823736  0.43765241  0.36620237'
L[,1]
'0.58526446  0.44853455 -0.02122962  0.50381903  0.44171124 -0.04097149 -0.04241554 -0.05253508 -0.02552380'
L[,2]
L

PCA$scores
'[23,]  0.63879503  0.51315730  1.26276309 -0.157636961  0.279323412  0.14682078  0.522640292 -0.247634858 -0.0890835439'

z<-scale(d)
z[23,]
'0.79409951  0.94700958 -0.04880283 -0.26735340 -0.39302661  0.03691320  0.60131503  0.66713454  0.37644354'

# ----

SAT<-cor(d,PCA$scores)
SAT
'0.05397843  0.82749170'

COM2<- SAT[,1]^2 + SAT[,2]^2
COM2
'0.6876562'
'Info de todas:
   PESOM    TALLAM       SEM      PASM      PADM     PESOR    TALLAR       PTR       PCR 
0.6876562 0.4058950 0.9349668 0.5313055 0.4251604 0.9190866 0.8697221 0.7955745 0.5544514 '

# ----

biplot(PCA, cex=0.7)
PCA$scores[23,1:2] 
'0.6387950 0.5131573'

'las variables tienen media 0 y varianza igual al valor propio'
PCA$scores/PCA$sdev[1] # 0.31452964
PCA$scores/PCA$sdev[2] # 0.362943492

'madre y bebe mas grandes'
which.max(PCA$scores[,1]) # mejor bebe
which.max(PCA$scores[,2]) # mejor madre

# ----

eigen(cor(d))
'Valores propios [1] 4.12477155 1.99904695 1.72524809 0.53410871 0.25021526 0.14989850 0.11870904 0.07183447 0.02616742'

summary(PCA)
screeplot(PCA)  
plot(eigen(cor(d))$values,type='l',ylab='valores propios')
'hay pico en m=2 bastante pronunciado'

summary(PCA)
'Como estamos con correlaciones la varianza esta estandarizada y son 1, por tanto cogemos las superiores a 1'
'2.030953 1.4138766 1.3134870 0.73082742 0.5002152 0.38716727 0.34454178 0.268019542 0.161763480'

eigen(cor(d))$values->Lambda
mean(Lambda[3:9])->ma
exp(mean(log(Lambda[3:9])))->mg
(100-(2*9+11)/6)*(9-2)*log(ma/mg)->TB
0.5*(9-2-1)*(9-2+2)->gl
1-pchisq(TB,gl)
''
ma
mg
TB
gl

'Rechazo hipotesis nula de que las restantes son iguales, queda información relevante en las sobrantes'

summary(PCA)






