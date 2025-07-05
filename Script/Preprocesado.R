<<<<<<< HEAD

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

### para configurar UTF8 por si la base de datos tiene caracteres 

Sys.setlocale("LC_ALL", "Spanish_Spain.utf8")

# Importando datos ## no dejaba por medio del comando solo de read_exel##
datos <- readxl::read_excel("Datos/DATOS_conjunto.xlsx");datos

### Modelo ANCOVA A DOS VIAS.

# Variables a analizar  ---------------------------------------------------

## Variable respuesta: Puntaje de matemáticas 
## Variables explicativas (factores): nivel socio económico del
## Nivel del estudiante, nivel educativo de la madre
## Covariable: Puntaje de lectura critica

# Cambiando variables "estu_nse_individual" y "fami_educacionmadre"

datos$estu_nse_individual <- factor(datos$estu_nse_individual, ordered = TRUE) 
datos$fami_educacionmadre <- factor(datos$fami_educacionmadre,
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
datos$fami_educacionmadre <- ordered(datos$fami_educacionmadre, 
                                     levels = c("Educación básica (sin completar)", 
                                                "Educación básica completa",
                                                "Educación superior"))

## Procedemos a observar si se volvieron de carácter "factor" ordinal

str(datos)

# Escoger el mejo modelo ANCOVA -------------------------------------------

### Realizamos el modelo ANCOVA a dos vías y aplicamos la función "stepwise"

M1 <- lm(punt_matematicas ~ fami_educacionmadre + estu_nse_individual +
                                   punt_lectura_critica + punt_lectura_critica*fami_educacionmadre + 
                                   punt_lectura_critica*estu_nse_individual, 
                                 data = datos)
summary(M1)

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
           punt_lectura_critica, data = datos)

summary(M_final)


# Verificación de supuestos -----------------------------------------------

### Independencia de residuos (Errores).
## Prueba de durbin-watson
library(lmtest)
dwt(M_final) # o
dwtest(M_final)

### Relación lineal (Variable respuestas vs Covariable) ajustada por un factor
# se verifica por medio de un gráfico.

lineal <- ggplot(datos, aes(x = punt_lectura_critica, 
                            y = punt_matematicas, 
                            color = estu_nse_individual)) +
  geom_point(alpha = 0.6, size = 2) +  # Transparencia y tamaño moderado
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +  # Línea de regresión general
  theme_minimal(base_size = 12) +
  labs(
    title = "Relación entre Puntaje en Lectura Crítica y Matemáticas",
    subtitle = "Agrupado por Nivel Socioeconómico Individual",
    x = "Puntaje en Lectura Crítica",
    y = "Puntaje en Matemáticas",
    color = "NSE Individual"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "right"
  )


# Este gráfico es para identificar si las pendientes son paralelas

#ggplot(datos, aes(x = punt_lectura_critica, y = punt_matematicas,
 #                 color = estu_nse_individual)) +
  #geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  #theme_minimal() +
  #labs(title = "Relación entre puntaje en lectura y puntaje en matemáticas",
   #    x = "punt_lectura", y = "punt_matemáticas")

### Homogeneidad de varianzas (ANOVA) o pendientes.
## para comparar la homogeneidad de pendientes en este caso se realizo a partir 
## de compara dos modelos, uno con y sin iteraciones, y comprobar el p-valor
## usando la función anova, este debe ser no significativo (P-valor > 0.05).

modelo_sin_interaccion <- lm(punt_matematicas ~ fami_educacionmadre +
        estu_nse_individual + punt_lectura_critica, data = datos)
modelo_con_interaccion <- lm(punt_matematicas ~ fami_educacionmadre + 
        estu_nse_individual + punt_lectura_critica * fami_educacionmadre,
        data = datos)
anova(modelo_sin_interaccion,modelo_con_interaccion)

### Homocedasticidad y un gráfico.
bptest(M_final,studentize = TRUE)

df <- data.frame(
  fitted = fitted(M_final),
  sqrt_resid = sqrt(abs(resid(M_final)))
)

homoceda <- ggplot(df, aes(x = fitted, y = sqrt_resid)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Gráfico de Escala-Localización",
    x = "Valores ajustados",
    y = "Raíz cuadrada de |residuos estandarizados|"
  ) +
  theme_minimal(base_size = 12)
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

residuos_df <- data.frame(residuos = residuos)
Q_Q <- ggplot(residuos_df, aes(sample = residuos)) +
  stat_qq(color = "steelblue", size = 2) +
  stat_qq_line(color = "darkred", linetype = "dashed", size = 1) +
  labs(
    title = "QQ-Plot de Residuos",
    x = "Cuantiles teóricos",
    y = "Cuantiles de los residuos"
  ) +
  theme_minimal(base_size = 12)

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

#### graficos unidos
win.graph(height = 20,width = 20)
par(mfrow=c(3,1))

plot(dffits_values, type = "h", col = "blue", main = "DFFITS", ylab = "DFFITS", xlab = "Observación")
abline(h = c(2*sqrt(length(coef(M_final))/nrow(datos)), 
             -2*sqrt(length(coef(M_final))/nrow(datos))), col = "red")

plot(cooks_d, type = "h", col = ifelse(cooks_d > 4/length(cooks_d), "red", "blue"),
     main = "Cook's D", ylab = "Cook's Distance")
abline(h = 4/length(cooks_d), col = "red")

influencePlot(M_final, id.method = "identify", main = "Outlier and Leverage", 
              sub = "Tamaño = Cook's D")

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


# Exportando --------------------------------------------------------------

saveRDS(homoceda,"Resultados_Casanare/homoceda.rds")
saveRDS(Hist_residuales,"Resultados_Casanare/Hist_residuales.rds")
saveRDS(Q_Q,"Resultados_Casanare/Q_Q.rds")
saveRDS(lineal,"Resultados_Casanare/lineal.rds")
