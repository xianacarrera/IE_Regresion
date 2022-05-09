#********************************************************************************
# Título aquí
#*************************
library(ggplot2)

datos = read.table("datos_trabajo_temas6y7.txt", header=T, sep=" ", dec=".")
str(datos)
summary(datos)
head(datos)

datos = datos[,c("X", "Y47")]
str(datos)
X <- datos[,"X"]
Y47 <- datos[,"Y47"]


n=length(Y47)


plot(X, Y47,
     main="Título", pch=16,
     sub="Subtítulo")
cov(x,y)*(n-1)/n
cor(X, Y47)

# Añadimos un punto para el vector de medias
mx <- mean(X); my <- mean(Y47)
points(mx, my, pch=12, col=3, cex=2)

# Añadimos dos rectas para dividir en cuadrantes
abline(v=mx, col=3, lty=1, lwd=2)   # Vertical
abline(h=my, col=3, lty=1, lwd=2)   # Horizontal


grid(nx = NULL, ny = NULL, lty = 2, col = "lightgray", lwd = 1)



modelo=lm(Y47~X)
modelo



abline(modelo, col="red", lwd=2)

# Gráfica adicional en ggplot con región de confianza del 95%
p3 <- ggplot(datos, aes(x=X, y=Y47)) +
  geom_point() +
  geom_smooth(formula=y~x, level=0.95, method=lm, color="red", fill="#666666", se=TRUE) + 
  labs(y = "Variable respuesta",
       x = "Variable explicativa",
       title = "Modelo lineal simple, con región de confianza al 95%")

p3


