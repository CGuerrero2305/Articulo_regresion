library(readxl)
datos <- read_excel("datos.xlsx")

colnames(datos) <- c("Tipo.de.explante", "Concentración.2..4.D..g.L.1.", 
                     "%_Formación.de.callo","%_Fenolización","%_Organogénesis")
str(datos)
dim(datos)
head(datos)
names(datos)
summary(datos)
attach(datos)


media_exp <- aggregate(datos[,3:5],
          by = list(Explante = Tipo.de.explante),
          FUN = mean)
media_concentracion <- aggregate(
  datos[, 3:5],
  by = list(Concentracion = Concentración.2..4.D..g.L.1.),
  FUN = mean)

hist(datos[["%_Formación.de.callo"]],
     main = "Distribución de formación de callo",
     xlab = "Formación de callo (%)",
     ylab = "Frecuencia",
     col = "skyblue")

hist(datos[["%_Fenolización"]],
     main = "Distribución de fenolización",
     xlab = "Fenolización (%)",
     ylab = "Frecuencia",
     col = "skyblue")

hist(datos[["%_Organogénesis"]],
     main = "Distribución de organogénesis",
     xlab = "Organogénesis (%)",
     ylab = "Frecuencia",
     col = "skyblue")


boxplot(datos[["%_Formación.de.callo"]] ~ datos$Tipo.de.explante,
        main = "Formación de callo por tipo de explante",
        xlab = "Tipo de explante",
        ylab = "Formación de callo (%)",
        col = "lightgreen")
boxplot(datos[["%_Fenolización"]] ~ datos$Tipo.de.explante,
        main = "Fenolización por tipo de explante",
        xlab = "Tipo de explante",
        ylab = "Fenolización (%)",
        col = "lightgreen")
boxplot(datos[["%_Organogénesis"]] ~ datos$Tipo.de.explante,
        main = "Organogénesis por tipo de explante",
        xlab = "Tipo de explante",
        ylab = "Organogénesis (%)",
        col = "lightgreen")


aggregate(datos[, 3:5],
          by = list(Explante = Tipo.de.explante,
                    concentracion = Concentración.2..4.D..g.L.1.),
          FUN = mean)


plot(datos$Concentración.2..4.D..g.L.1.,
     datos[["%_Formación.de.callo"]],
     xlab = "Concentración de 2,4-D (g/L)",
     ylab = "Formación de callo (%)",
     main = "Formación de callo según concentración",
     pch = 19,
     col = as.factor(Tipo.de.explante))
legend("topright",
       legend = levels(as.factor(Tipo.de.explante)),
       col = 1:3,
       pch = 19)
plot(datos$Concentración.2..4.D..g.L.1.,
     datos[["%_Fenolización"]],
     xlab = "Concentración de 2,4-D (g/L)",
     ylab = "Fenolización (%)",
     main = "Fenolización según concentración",
     pch = 19,
     col = as.factor(Tipo.de.explante))
legend("topright",
       legend = levels(as.factor(Tipo.de.explante)),
       col = 1:3,
       pch = 19)
plot(datos$Concentración.2..4.D..g.L.1.,
     datos[["%_Organogénesis"]],
     xlab = "Concentración de 2,4-D (g/L)",
     ylab = "Organogénesis (%)",
     main = "Organogénesis según concentración",
     pch = 19,
     col = as.factor(Tipo.de.explante))
legend("topright",
       legend = levels(as.factor(Tipo.de.explante)),
       col = 1:3,
       pch = 19)

max(datos[["%_Formación.de.callo"]])
min(datos[["%_Formación.de.callo"]])

max(datos[["%_Fenolización"]])
min(datos[["%_Fenolización"]])

max(datos[["%_Organogénesis"]])
min(datos[["%_Organogénesis"]])

