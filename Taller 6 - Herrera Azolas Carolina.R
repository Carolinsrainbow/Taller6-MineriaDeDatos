## Taller 6 - Minería de datos
## Carolina Herrera Azolas

## Incorporación de datos 
datos <- read.csv("/Users/user/Desktop/DatosPaises.csv",sep=";", header=TRUE)

##Incorporación de paquetes
install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")

library(caret)
library(lattice)
library(ggplot2)

## Visualización general de datos
head(datos)

## Resumen estadístico 
summary(datos)

## Creamos un vector con 28 columnas, sin considerar país (variable string )
cor(datos[c(2:30)])

##Unimos la matriz creada relacionandola con el PIB
cor(datos$PIB,datos[c(3:30)])

## Seleccionamos las variables e incorporamos el método para el cruce

### PRIMER MODELO 

## ANÁLISIS DE LAS CORRELACIONES 

primerModelo <- lm(PIB ~ IDH + FAO + GENERO + ELECTRICIDAD + ESCOLARIDAD + DIOXIDO + INTERNET + INMIGRANTES,
                   data = datos)

summary(primerModelo)

plot(datos$PIB , predict(primerModelo))
cor(datos$PIB , predict(primerModelo))

### SEGUNDO MODELO 
segundoModelo <- lm(PIB ~ IDH + FAO + GENERO + ELECTRICIDAD + ESCOLARIDAD + DIOXIDO + INTERNET + INMIGRANTES + TURISMO + VIOLENCIA,
                    data = datos)

summary(segundoModelo)

plot(datos$PIB , predict(segundoModelo))
cor(datos$PIB , predict(segundoModelo))

## REGRESIÓN BINOMIAL

## Creamos una nueva columna 

datos$PIB_binomial <- as.numeric(datos$PIB > 11)

primerModeloBinomial <- glm(PIB_binomial ~ IDH + FAO + GENERO + ELECTRICIDAD + ESCOLARIDAD + DIOXIDO + INTERNET + INMIGRANTES,
                           data = datos,
                           family=binomial())

summary(primerModeloBinomial)

pred2 <- as.numeric(predict(primerModeloBinomial, type = "response") > 0.5)

confusionMatrix(as.factor(pred2), as.factor(datos$PIB_binomial))


## REGRESIÓN MULTINOMIAL

## Incorporación de librería
library(nnet)

## Resumen estadístico
summary(datos$PIB)


## creamos una columna para analizar los datos multinomial 

datos$PIB_multinomial <- 4 * ( datos$PIB > 23 ) + 
  3 * ( datos$PIB > 11 ) * ( datos$PIB <= 23 ) +
  2 * ( datos$PIB > 3.5 ) * ( datos$PIB <= 11 ) +
  1 * ( datos$PIB <= 3.5 )

primerModeloMultinomial<- multinom(PIB_multinomial ~ IDH + FAO + GENERO + ELECTRICIDAD + ESCOLARIDAD + DIOXIDO + INTERNET + INMIGRANTES,
                     data = datos)

summary(primerModeloMultinomial)

summary(primerModeloMultinomial)$coefficients / summary(primerModeloMultinomial)$standard.errors

predict(primerModeloMultinomial)

confusionMatrix(predict(primerModeloMultinomial), as.factor(datos$PIB_multinomial))
