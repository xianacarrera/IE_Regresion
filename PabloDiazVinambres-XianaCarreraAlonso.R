#*******************************************************************************
# Trabajo de Evaluación Continua de Modelos de Regresión
# Pablo Díaz Viñambres, Xiana Carrera Alonso
#*******************************************************************************


#***************** Librerías
#install.packages("ggplot2")
library(ggplot2)  



#***************** Preparación de datos
datos <- read.table("datos_trabajo_temas6y7.txt", header=T, sep=" ", dec=".")
str(datos)
summary(datos)
head(datos)


datos = datos[,c("X", "Y47")]
str(datos)
X <- datos[,"X"]
Y <- datos[,"Y47"]

n <- length(Y)






#*******************************************************************************
# Ejercicio 1
#*******************************************************************************


# No podemos sacar conclusiones de la magnitud de la covarianza (pues esta tiene unidades,
# el producto de las de cada variable al cuadrado), pero que tenga signo negativo
# implica que la recta de regresión será decreciente (i.e., pendiente negativa).
covar = cov(X,Y)*(n-1)/n; covar           # Covarianza
cov(X,Y)                   # Cuasicovarianza o como quieran llamarla


cor(X, Y)

mX <- mean(X)
mY <- mean(Y)

representar <- function(){
  plot(X, Y,
       main="Diagrama de dispersión", pch=16,
       sub="Subtítulo")
  
  # Añadimos un punto para el vector de medias
  points(mX, mY, pch=12, col=3, cex=2)
  
  # Añadimos dos rectas para dividir en cuadrantes
  abline(v=mX, col=3, lty=1, lwd=2)   # Vertical
  abline(h=mY, col=3, lty=1, lwd=2)   # Horizontal
  
  
  grid(nx = NULL, ny = NULL, lty = 2, col = "lightgray", lwd = 1)
}

representar()


modelo=lm(Y~X)
modelo






#*******************************************************************************
# Ejercicio 2
#*******************************************************************************

#*********** Estimación puntual "a mano"

var.X <- var(X)*(n-1)/n

beta0.gorro = mY - covar*mX/var.X; beta0.gorro
beta1.gorro = covar/var.X; beta1.gorro
var.error = sum((Y - beta0.gorro - beta1.gorro*X)^2)/(n-2); var.error
# Ponemos también la desviación típica?

#*********** Estimación puntual "automática"
modelo    # Información del modelo
modelo$coefficients         # beta0 gorro y beta1 gorro

# En modelo$residuals están los residuos
sum(modelo$residuals^2)/(n-2)


representar()
abline(modelo, col="red", lwd=2)



# Gráfica adicional en ggplot con región de confianza del 99%
p3 <- ggplot(datos, aes(x=X, y=Y)) +
  geom_point() +
  geom_smooth(formula=y~x, level=0.99, method=lm, color="red", fill="#666666", se=TRUE) + 
  labs(y = "Variable respuesta",
       x = "Variable explicativa",
       title = "Modelo lineal simple, con región de confianza al 95%")



#*******************************************************************************
# Ejercicio 3
#*******************************************************************************

#******************************* "A mano" **************************************

alfa <- 1 - 0.99

#***************** beta0
# pivote beta0.gorro - beta0/(sqrt(var.gorro*(1/n+mX^2/(n*var.X))) que es una
# T de Student con n-2 grados de libertad


beta0.cuantil <- qt(1-alfa/2, df=n-2); beta0.cuantil

beta0.extremoinferior <- beta0.gorro - beta0.cuantil * sqrt(var.error * (1/n + mX^2/(n*var.X)))
beta0.extremosuperior <- beta0.gorro + beta0.cuantil * sqrt(var.error * (1/n + mX^2/(n*var.X)))

beta0.IC <- c(beta0.extremoinferior, beta0.extremosuperior); beta0.IC


#***************** beta1
# pivote beta1.gorro - beta1/sqrt(var.gorro/ (var.X * n)) que es una
# T de Student con n-2 grados de libertad

beta1.cuantil <- beta0.cuantil

beta1.extremoinferior <- beta1.gorro - beta1.cuantil*sqrt(var.error/(var.X * n))
beta1.extremosuperior <- beta1.gorro + beta1.cuantil*sqrt(var.error/(var.X * n))

beta1.IC <- c(beta1.extremoinferior, beta1.extremosuperior); beta1.IC


#***************** varianza del error
# pivote -> (n-2)*var.error^2/varianzaerror^2 que es una chi-cuadrado con n-2
# grados de libertad

var.error.cuantilinferior <- qchisq(alfa/2, df=n-2)
var.error.cuantilsuperior <- qchisq(1-alfa/2, df=n-2)

var.error.extremoinferior <- (n-2)*var.error^2/var.error.cuantilsuperior
var.error.extremosuperior <- (n-2)*var.error^2/var.error.cuantilinferior

var.error.IC <- c(var.error.extremoinferior, var.error.extremosuperior); var.error.IC


#***************************** "Automático" ************************************

# IC para beta0 y beta1 asumiendo que la varianza es desconocida
confint(modelo, level=0.99)

# No hay una automatización del cálculo de la varianza del error



#*******************************************************************************
# Ejercicio 4
#*******************************************************************************

summary(modelo)


#*******************************************************************************
# Ejercicio 5
#*******************************************************************************

x0 <- c(2, 4, 6)

y0.tilde <- beta0.gorro + beta1.gorro * x0


#*********** IC







# Con qué nivel de confianza?
predict(modelo, newdata=data.frame("X"=x0), interval = "confidence", level=0.99)
predict(modelo, newdata=data.frame("X"=x0), interval = "prediction", level=0.99)






par(mfrow=c(1,2))

representar()
abline(modelo, col="red", lwd=2)


residuos <- Y - beta0.gorro - beta1.gorro*X
plot(X, residuos,
     main="Diagrama de dispersión", pch=16,
     sub="Subtítulo")
abline(h=0, col="black", lwd=2)

par(mfrow=c(1,1))


library(sm)
sm.regression(X, Y, model="linear")
