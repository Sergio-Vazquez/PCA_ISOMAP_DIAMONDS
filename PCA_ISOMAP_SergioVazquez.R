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


# 0- Instalacion de librerias.

library(ggplot2)
library(quantmod)

### Para crear plots 3D hay que instalar el paquete rlg
library(rgl) 

### Packete RDRToolbox para Reducción de Dimensiones No-Lineal
library(RDRToolbox)
library(dplyr)

# 1- Obtenemos el dataset diamonds.
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


############################## ESTUDIO PCA ###############################################

# Realizamos un analisis previo para ver medias y varianzas de cada variable:
apply(df.estudio, 2, mean)
apply(df.estudio, 2, var)

# Calculamos con prcomp los datos normalizados, con medias 0 y varianzas 1
pr.out = prcomp(df.estudio, scale=TRUE)  # con scale = True conseguimos que todos los datos tengan la misma escala

# Realizamos un biplot para obtener las 2 primeras componentes y sus valores
biplot(pr.out, scale = 0)

# Vamos a obtener informacion de cada uno de los compenentes que me da el pr.out:
names(pr.out) # obtengo cada componente.
pr.out$sdev # obtengo la desviacion standard
pr.out$rotation # obtengo la matriz P de cambio de base
pr.out$x # obtengo la matriz Y de datos despues del cambio de base

# Para determinar las direcciones principales con las que me quedare, puedo utilizar dos metodos entre otros:
# Criterio del "codo" en el que se verá como decrece la pendiente en funcio de las componentes principales que tenga
# Calculo de la proporcion de varianza explicada (PVE)

# En primer lugar calculamos el PVE:
## Calculo de la varianza.
pr.var <- pr.out$sdev^2

## Calculo de la proporción PVE 
pve <- pr.var/sum(pr.var)
pve

cumsum(pve) * 100 # vemos el porcentaje acumulado de cada componente principal.

## Se puede ver que con la eleccion de las 2 primeras componentes explica un 64.4½ de la variacion en el dato y 
## si selecciono las 3 primeras componentes, aumenta hasta un 76.8%

# Representamos el criterio del codo:
plot(pve,  xlab="Componente Principal", ylab="Proporción de Varianzas Explicadas", type = 'b')
plot(cumsum(pve), xlab="Componente Principal", ylab="Proporción Acumulatica de Varianzas Explicadas",ylim=c(0,1),type='b')


## Despues de observar el grafico resultante, se ve que claramente eligiendo las 2 primeras componentes explicaria
## perfectamente la variacion del dato, aunque eligiendo 3 aunmente el PVE, en cambio, en el grafico del "codo" a penas hay variacion.

## Como ya sabemos que con las 2 primeras CP sera suficiente, analizo las correlaciones entre las variables, realizando de nuevo:
pr.out$rotation

# OBSERVACIONES:
# Se observa algo muy curioso con la informacion obtenida. Se puede ver una alta correlacion en PC1 respecto las variables
# "carat", "price", "x", "y", "z", lo que puede arrojar varias conclusiones:
# 1- Es evidente que x, y, z, es decir, las dimensiones del diamante influyen totalmente en el peso ("carat") del mismo.
# 2- Se puede deducir, que lo que mas influye en el precio de un diamante sera su peso y evidentemente sus dimensiones.
# 3- A diferencia de lo que pensaba antes de realizar el estudio, el corte, claridad y color no influyen significativamente en el precio.

# Si analizamos la segunda componente PC2, observamos una altisima correlacion en las variables "table" y "cut2", de lo que podemos deducir:
# 1- La variable "table" nos indicaba la anchura de la parte superior del diamante con relacion al pnto mas ancho,
# es totalmente normal que dicho valor dependa del tipo de corte del diamante, ya que en funcion del tipo de corte, la forma
# del diamante cambia totalmente.


################################### ESTUDIO ISOMAP #######################################################

## Debido a que el dataset "df.estudio" presenta muchos registros, mi maquina virtual no puede procesar toda la información
# por lo que decido realizar una muestra con 500 registros en vez de los 53.940 que tiene.

# Se puede comprobar las dimensiones del data frame con el que no puedo ejecutar Isomap:
dim(as.matrix(df.estudio)) # tiene 53940 filas y 10 columnas, que son las variables

muestra.estudio <- df.estudio[1:500, ]

# Estimamos la dimensión dibujando los residuos con 4 vecinos.
IsomapIndicadores_1to10 = Isomap(data = as.matrix(muestra.estudio), dims=1:10, k=4, plotResiduals=TRUE)
# Se puede observar una vez realizado este Isomap, como a partir de la dimension 2-3 la gráfica decae.


#Calculamos Isomap con 2 dimensiones
IsomapIndicadores_2 = Isomap(data = as.matrix(muestra.estudio), dim = 2, k = 4)
# Plot con 2 dimensiones
plotDR(data=IsomapIndicadores_2$dim2,axesLabels = c('CP1', 'CP2'))


#Calculamos Isomap con 3 dimensiones
IsomapIndicadores_3 = Isomap(data = as.matrix(muestra.estudio), dim = 3, k = 4)
# Plot con 3 dimensiones
plotDR(data=IsomapIndicadores_3$dim3, axesLabels = c('CP1', 'CP2', 'CP3'))

# Se puede ver como no perdemos mucha informacion al pasar de 3 a 2 dimensiones.
# Nos quedamos con dimension intrínseca 2 para seguir el análisis.
# Se puede observar una especie de parabola que crea los datos.
# Los datos se agrupan mas cuando V2 < 5 y V1 < 10

# Creamos unos datos filtrados con las 2 dimensiones intrinsecas anteriores en donde se agrupan los datos:
Isomap_filtrado_v1 <- as.data.frame(row.names = rownames(muestra.estudio),IsomapIndicadores_2$dim2)

# Indico los valores de V1 y V2 donde se concentran los datos:
Isomap_filtrado_v2 <- rownames(Isomap_filtrado_v1[Isomap_filtrado_v1$V1 < 10 & filter_isomap$V2 < 5,])

#Obtenemos un dataframe con esos datos filtrados:
df.filtrado.isomap <- muestra.estudio[Isomap_filtrado_v2, ]

# Ahora si comparamos los resultados obtenidos, con los obtenidos en el dataset original, veremos que son datos muy parecidos:

summary(df.filtrado.isomap)
summary(df.estudio)

# Se puede deducir que el estudio de ISOMAP con 2 variables intrínsecas es correcto y no perdemos prácticamente informacion
# respecto al dataset original.



