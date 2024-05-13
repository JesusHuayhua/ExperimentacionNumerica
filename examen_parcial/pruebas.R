# Pruebas de R
freq<-c(24,36,90,50)
esperado <-c(50,50,50,50)
sum(((freq - esperado) ** 2) / esperado)

chi_cuadrado = chisq.test(freq)
chi_cuadrado
sqrt(49.44)
qchisq(0.95,2)

defectos <- c(0,1,2,3,4,5)
prendas_defectuosas <- c(58,55,22,10,4,1)

alpha <- 0.05

#
# X es el numero de defectos por prenda
# 

lambda = sum(defectos * prendas_defectuosas) / sum(prendas_defectuosas)
esperado <- rep(0,6)
for (k in 0:6) {
  esperado[k + 1] = sum(prendas_defectuosas) * dpois(k,lambda = 1)
}
esperado

U = sum( (defectos - esperado)^2 / esperado)
U



## Consumo de agua por hogar (normal)+
library(MASS)
data = c(15,18,21,22,22,23,24,25,26,27,27,28,29,30,31,32,32,32,33,33,33,33,33,34,34,34,34,34,35,35,35,36,36,36,36,36,37,38,39,40,41,42,42,43,43,44,45,46,47,47)

mean(data)
var(data)
sqrt(var(data))

ls = c(24,29,34,39,49)

FO = pnorm(ls,mean(data),sd = sqrt(var(data))) # acumulada

po =  rep(0,5)
po[1] = FO[1]
for (k in 0:3) {
  po[k + 2] = FO[k + 2] - FO[k + 1]
}
po
rango = c(7,6,15,11,11)
esperado = c(50 * po)
nose = sum( (rango - esperado)^2 / esperado)
nose

1 - pchisq(2.07,2)


shapiro.test(data)

#histograma
auxhist = hist(data)
cortes = auxhist$breaks
probAcum = pnorm(cortes,mean = mean(data), sd = sd(data))

n = length(probAcum)

intervalos = data.frame(probAcum[-n],probAcum[-1])
prop.esperada = intervalos[,2] - intervalos[,1]
freq.esperada = (prop.esperada/sum(prop.esperada)) * length(data)

freq.observada  = auxhist$counts
chisq.test(freq.observada,p = prop.esperada,rescale.p = TRUE,simulate.p.value = T)

#============================
# Prueba de Wilcoxon
#============================

x = c(6,9,5,3,4,2)
length(x)
r = wilcox.test(x,
                mu = 5,
                alternative="greater",
                conf.level =0.95,
                correct = F)
print(r)

y <- c(4,6,3,8,10,5,4,3,5,3)
length(y)
wilcox.test(y,mu = 5, alternative = "greater",conf.level =0.97,correct=F)



alpha = 0.05
qf(alpha/2,14,14)
qf(1-alpha/2,14,14)


qt(1-alpha,n1 + n2 - 2)

# Problemas

fo = (0.1811 / 0.067)^2

alpha = 0.05
valor_minimo = qf(alpha/2,4,4)
valor_maximo = qf(1 - alpha/2,4,4)

if (valor_minimo < fo && fo < valor_maximo) {
  print("Se aceptan varianzas iguales")
}else{
  print("NO: Se aceptan varianzas iguales")
}


qt(1-alpha,8)


n1 = 5
n2 = 5
s1 = 0.1811
s2 = 0.067
media1 = 0.417
media2 = 0.151
sp = sqrt(((n1 - 1)*(s1^2) + (n2 - 1)*(s2^2))   /   (n1 + n2 - 2) )

to = (media1 - media2) / (sp * sqrt(1/n1 + 1/n2))
to


estandar = c(32,37,35,28,41,44,35,31,34)
nuevo    = c(35,31,29,25,34,40,27,32)

(sd(estandar) / sd(nuevo)) ^ 2
n1 = length(nuevo)
n2 = length(estandar)
alpha = 0.05
qf(alpha/2,n1 - 1, n2 - 1)
qf(1 - alpha/2,n1 - 1, n2 - 1)


s1 = sd(nuevo)
s2 = sd(estandar)
media1 = mean(nuevo)
media2 = mean(estandar)
sp = sqrt(((n1 - 1)*(s1^2) + (n2 - 1)*(s2^2))   /   (n1 + n2 - 2) )
to = (media1 - media2) / (sp * sqrt(1/n1 + 1/n2))

t = qt(alpha,n1 + n2 - 2)
if (to > t) {
  print("NO se rechaza H0")
}else{
  print("SI se rechaza H0")
}


sistema1 = c(25,29,42,16,31,14,33,45,26,34,30)
sistema2 = c(18,37,40,56,49,28,20,34,39,47)
alpha = 0.01

# Hipotesis
#
# H0 : me1 = me2
#
# H1 : me1 != me2

wilcox.test(sistema1,sistema2,mu=0,conf.level=0.99)
wilcox.test(sistema1,sistema2)
# nos brinad el p-valor







