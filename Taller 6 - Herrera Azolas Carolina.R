## Taller 6 - Minería de datos
## Carolina Herrera Azolas

## Incorporación de datos 
datos <- read.csv("/Users/user/Desktop/DatosPaises.csv",sep=";", header=TRUE)

## Visualización general de datos
head(datos)

## Resumen estadístico 
summary(datos)

## Creamos un vector con 28 columnas, sin considerar país (variable string )
cor(datos[c(2:30)])

##Unimos la matriz creada relacionandola con el PIB
cor(datos$PIB,datos[c(3:30)])
