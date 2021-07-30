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


