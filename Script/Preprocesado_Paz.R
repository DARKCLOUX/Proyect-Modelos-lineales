
#### Los factores no se ajustan a un modelo ANCOVA, por ende se escogerán nuevas
# variables.

# Librerías ---------------------------------------------------------------
library(dplyr)
library(readxl)
library(ggplot2)
library(GGally)
library(RcmdrMisc)
library(car)
library(tidyverse)
library(modelr)
library(devtools)
library(conflicted)
library(MASS)
library(effects)
library(nortest)
library(DescTools)
library(lmtest)
library(MASS)
library(sjPlot)
library(olsrr)

Sys.setlocale("LC_ALL", "Spanish_Spain.utf8")

# Importando datos ## no dejaba por medio del comando solo de read_exel##
setwd("C:/Users/user/3D Objects/Modelos/Proyecto_Icfes/Data")
datos_paz <- readxl::read_excel("SABER11_PAZ_DE_ARIPORO _CALENDARIOA_2024_2.xlsx");datos_paz

### Modelo ANCOVA A DOS VIAS.

# Variables a analizar  ---------------------------------------------------

## Variable respuesta: Puntaje de matemáticas 
## Variables explicativas (factores): nivel socio económico del
## Nivel del estudiante, nivel educativo de la madre
## Covariable: Puntaje de lectura critica

# Cambiando variables "estu_nse_individual" y "fami_educacionmadre"

datos_paz$estu_nse_individual <- factor(datos_paz$estu_nse_individual, ordered = TRUE) 
datos_paz$fami_educacionmadre <- factor(datos_paz$fami_educacionmadre,
                                    levels = c("Ninguno", "Primaria incompleta", "Secundaria (Bachillerato) incompleta", 
                                               "Primaria completa", "Secundaria (Bachillerato) completa", 
                                               "Técnica o tecnológica incompleta", "Técnica o tecnológica completa", 
                                               "Educación profesional incompleta", "Educación profesional completa", 
                                               "Postgrado", "No sabe"),
                                    labels = c("Educación básica (sin completar)", 
                                               "Educación básica (sin completar)", 
                                               "Educación básica (sin completar)", 
                                               "Educación básica completa", 
                                               "Educación básica completa", 
                                               "Educación superior", 
                                               "Educación superior", 
                                               "Educación superior", 
                                               "Educación superior", 
                                               "Educación superior", 
                                               "Educación básica (sin completar)"))  # "No sabe" a "Educación básica (sin completar)"

# Convertir a factor ordinal
datos_paz$fami_educacionmadre <- ordered(datos_paz$fami_educacionmadre, 
                                     levels = c("Educación básica (sin completar)", 
                                                "Educación básica completa",
                                                "Educación superior"))

## Procedemos a observar si se volvieron de carácter "factor" ordinal

str(datos_paz)

# Escoger el mejor modelo ANCOVA -------------------------------------------

### Realizamos el modelo ANCOVA a dos vías y aplicamos la función "stepwise"

M <- lm(punt_matematicas ~ fami_educacionmadre + estu_nse_individual +
           punt_lectura_critica + punt_lectura_critica*fami_educacionmadre + 
           punt_lectura_critica*estu_nse_individual, 
         data = datos_paz)
summary(M)

### utilizamos "backward" <-> "pasos hacia atrás" (utilizar solo cuando sean datos
###                                                           Completos)
#library(RcmdrMisc)
#stepwise(M1,direction = "backward")


#### NOTA 

# El error surge porque la base de datos contiene datos faltantes en las variables
# por ende para realizar este proceso "backward" lo ideal sería tener la base de datos, 
# sin ningún dato faltante, recordar que la cantidad de datos faltantes, si supera el 5%
# de la muestra no es una buena opción imputar esos datos, así que, es mejor utilizar,
# otras técnicas. en este caso se trabajo con los datos faltantes, y se sabe que la variables
# explicativas son de carácter fijo.

M_final <- lm(punt_matematicas ~ fami_educacionmadre + estu_nse_individual +
                punt_lectura_critica, data = datos_paz)

summary(M_final)


# Verificación de supuestos -----------------------------------------------

### Independencia de residuos (Errores).
## Prueba de durbin-watson
library(lmtest)
dwt(M_final) # o
dwtest(M_final)

### Relación lineal (Variable respuestas vs Covariable) ajustada por un factor
# se verifica por medio de un gráfico.

ggplot(datos_paz, aes(x = punt_lectura_critica, y = punt_matematicas,
                  color = estu_nse_individual)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relación entre puntaje en lectura y puntaje en matemáticas",
       x = "punt_lectura", y = "punt_matemáticas")

### Homogeneidad de varianzas (ANOVA) o pendientes.
## para comparar la homogeneidad de pendientes en este caso se realizo a partir 
## de compara dos modelos, uno con y sin iteraciones, y comprobar el p-valor
## usando la función anova, este debe ser no significativo (P-valor > 0.05).

modelo_sin_interaccion <- lm(punt_matematicas ~ fami_educacionmadre +
                               estu_nse_individual + punt_lectura_critica, data = datos_paz)
modelo_con_interaccion <- lm(punt_matematicas ~ fami_educacionmadre + 
                               estu_nse_individual + punt_lectura_critica * fami_educacionmadre,
                             data = datos_paz)
anova(modelo_sin_interaccion,modelo_con_interaccion)

### Homocedasticidad y un gráfico.
bptest(M_final,studentize = TRUE)
plot(M_final, which = 3)

### Normalidad de residuos mediante gráficos y test de kolmo-gorov

ks.test(residuals(M_final), "pnorm", 0, sd(residuals(M_final)))

## Creación del objeto "residuos"
residuos <- residuals(M_final)

# Histograma 
Hist_residuales <- ggplot(data.frame(Residuos = residuos), aes(x = Residuos)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribución de Residuos", x = "Residuos", y = "Densidad");Hist_residuales

# Q_Q
qqnorm(residuos, main = "QQ-Plot de Residuos")
qqline(residuos, col = "red")

# Detección de datos influyentes ------------------------------------------
## Objetos para gráficas 
influence_measures <- influence.measures(M_final)
dffits_values <- dffits(M_final)
cooks_d <- cooks.distance(M_final)
hat_values <- hatvalues(M_final)
stud_res <- rstudent(M_final)

## Gráfico DFFITS
plot(dffits_values, type = "h", col = "blue", main = "DFFITS", ylab = "DFFITS", xlab = "Observación")
abline(h = c(2*sqrt(length(coef(M_final))/nrow(datos)), 
             -2*sqrt(length(coef(M_final))/nrow(datos))), col = "red")

## Gráfico D-DCOOKS

plot(cooks_d, type = "h", col = ifelse(cooks_d > 4/length(cooks_d), "red", "blue"),
     main = "Cook's D", ylab = "Cook's Distance")
abline(h = 4/length(cooks_d), col = "red")

## Gráfico de ATÍPICOS Y APALANCAMIENTO
influencePlot(M_final, id.method = "identify", main = "Outlier and Leverage", 
              sub = "Tamaño = Cook's D")

## Gráfico de datos influyentes
df_influence <- data.frame(Observacion = 1:length(hat_values),
                           Hat = hat_values,
                           Residual = stud_res,
                           CooksD = cooks_d)

ggplot(df_influence, aes(x = Hat, y = Residual)) +
  geom_point(aes(size = CooksD), alpha = 0.5) +
  scale_size_continuous(range = c(1, 10)) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  geom_vline(xintercept = 2 * mean(hat_values), linetype = "dashed") +
  theme_minimal() +
  labs(title = "Influence Plot",
       x = "Hat Values",
       y = "Studentized Residuals")


# Imputando los datos influyentes de la muestra ---------------------------

diagnosticos <- data.frame(
  observacion = as.numeric(rownames(M_final$model)),
  rstudent = rstudent(M_final),
  leverage = hatvalues(M_final),
  cooksD = cooks.distance(M_final),
  dffits = dffits(M_final)
)


n <- nrow(M_final$model)
p <- length(coef(M_final))

umbral_residuo <- 2
umbral_leverage <- 2 * p / n
umbral_cooks <- 4 / n
umbral_dffits <- 2 * sqrt(p / n)

outliers <- diagnosticos[
  abs(diagnosticos$rstudent) > umbral_residuo |
    diagnosticos$leverage > umbral_leverage |
    diagnosticos$cooksD > umbral_cooks |
    abs(diagnosticos$dffits) > umbral_dffits,
  "observacion"
]


datos_limpios <- datos[-outliers, ]

summary(M_final)

## Observar la R^2 explicación de variabilidad de datos del modelo

