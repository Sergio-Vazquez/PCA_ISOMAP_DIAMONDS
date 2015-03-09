# Voy a realizar un analisis PCA de un dataset contenido en R.
# Para esta practica, he elegido el dataset "diamonds" ya que me parece bastante interesante por la gran cantidad
# de informacion que presenta.

# Intentare averiguar que variables de las 10 que presenta el dataset, son las mas relevantes.

# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:

# price: Precio en dolares americanos ($326–$18,823)
# carat: peso del diamante (0.2–5.01)
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el mejor SI1, SI2, VS1, VS2, VVS1, VVS2, IF, hasta el peor I1)
# x: longitud en mm (0–10.74)
# y: ancho en  mm (0–58.9)
# z: profundidad en mm (0–31.8)
# depth: porcentaje total de profundidad (total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79))
# table: anchura de la parte superior de diamante con relación al punto más ancho (43-95)


# 1- Obtenemos el dataset diamonds.


library(RDRToolbox)
library(rgl)
library(dplyr)
library(ggplot2)


data(diamonds)

# 2- Analizamos de forma rapida los datos presentes en el dataset.

summary(diamonds) # obtenemos un resumen de los datos

# 3 - Modificacion de datos
# Debido a que hay 3 variables muy importantes y que no son numericas, deberemos de realizar una reclasificacion para incluirlas
# El objetivo sera cambiar las variables cut, colour y clarity que son valores factor para pasarlos a valores numericos

# Creamos un nuevo data frame que modificaremos 

df.diamonds <- diamonds


# cambiamos los valores de la variable cut, y las renombramos del 1 al 5, de mejor(Ideal) a peor(Fair):

df.diamonds$cut2 <- as.numeric(df.diamonds$cut)

df.diamonds$cut2[df.diamonds$cut == "Ideal"] <- 1
df.diamonds$cut2[df.diamonds$cut == "Premium"] <- 2
df.diamonds$cut2[df.diamonds$cut == "Very Good"] <- 3
df.diamonds$cut2[df.diamonds$cut == "Good"] <- 4
df.diamonds$cut2[df.diamonds$cut == "Fair"] <- 5

# hacemos lo mismo para la variable color:

df.diamonds$color2 <- as.numeric(df.diamonds$color)

df.diamonds$color2[df.diamonds$color == "D"] <- 1
df.diamonds$color2[df.diamonds$color == "E"] <- 2
df.diamonds$color2[df.diamonds$color == "F"] <- 3
df.diamonds$color2[df.diamonds$color == "G"] <- 4
df.diamonds$color2[df.diamonds$color == "H"] <- 5
df.diamonds$color2[df.diamonds$color == "I"] <- 6
df.diamonds$color2[df.diamonds$color == "J"] <- 7


# hacemos lo mismo para la variable clarity:

df.diamonds$clarity2 <- as.numeric(df.diamonds$clarity)

df.diamonds$clarity2[df.diamonds$clarity == "IF"] <- 1
df.diamonds$clarity2[df.diamonds$clarity == "VVS1"] <- 2
df.diamonds$clarity2[df.diamonds$clarity == "VVS2"] <- 3
df.diamonds$clarity2[df.diamonds$clarity == "VS1"] <- 4
df.diamonds$clarity2[df.diamonds$clarity == "VS2"] <- 5
df.diamonds$clarity2[df.diamonds$clarity == "SI1"] <- 6
df.diamonds$clarity2[df.diamonds$clarity == "SI2"] <- 7
df.diamonds$clarity2[df.diamonds$clarity == "I1"] <- 8


# Una vez tenemos todas las variables como numericas, creo un data frame con las columnas que me interesan:
df.estudio <- df.diamonds[, c(1,5:13)]

