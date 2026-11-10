# ==========================================================================
# Ejercicio 02. Dispersión y correlación: curva de calibración de Bradford
# (dispersión + recta de regresión)
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "ggplot" — Ejercicio 02
#
# Descripción: Un ejercicio con gráficos de dispersión y correlación de la
#   familia de gráficos de https://r-graph-gallery.com/ (sección Correlation).
#   Usamos mediciones reales de absorbancia a 595 nm de estándares de
#   albúmina sérica bovina (BSA) para construir la curva de calibración del
#   ensayo de Bradford, un método colorimétrico para cuantificar proteína
#   total. Todo se calcula de cero: los datos van primero como mediciones
#   reales (CSV) y cada valor (correlación, pendiente, intercepto y R²) se
#   deriva de los datos.
#
# El ensayo de Bradford: el colorante azul de Coomassie se une a las
#   proteínas y cambia de color; la absorbancia a 595 nm crece con la
#   concentración de proteína. La curva de calibración relaciona la
#   absorbancia medida con la concentración conocida de los estándares, y
#   después permite estimar la concentración de una muestra desconocida a
#   partir de su absorbancia.
#
# Objetivos de aprendizaje:
#   - Hacer un gráfico de dispersión con ggplot2.
#   - Ajustar una recta de regresión lineal y calcular el coeficiente de
#     correlación de Pearson (r) y el coeficiente de determinación (R²).
#   - Interpretar la pendiente y el intercepto de la recta de calibración.
#
# Requisitos: tidyverse.
#   Instálalo con: install.packages("tidyverse")
#
# Datos: datos_absorbancia_bradford.csv — absorbancia a 595 nm de estándares
#   de BSA (0, 25, 50, 100, 200, 400, 600 y 800 µg/mL), n = 3 réplicas por
#   concentración. El estándar de 0 µg/mL es el blanco (solo reactivo, sin
#   proteína).
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
# Fijar codificación UTF-8 para que los acentos (ó, í, µ, ²) se dibujen bien
# en el gráfico. Si el sistema no tiene esa locale, no pasa nada: se sigue
# con la locale que ya tenga.
suppressWarnings(try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE))

library(tidyverse)

# --------------------------------------------------------------------------
# 2. Leer los datos (mediciones reales, formato largo)
# --------------------------------------------------------------------------
datos <- read_csv("datos_absorbancia_bradford.csv", show_col_types = FALSE)

# Revisar los datos
glimpse(datos)
summary(datos$absorbancia_595)

# --------------------------------------------------------------------------
# 3. Correlación y recta de regresión (todo calculado de los datos)
# --------------------------------------------------------------------------
# El coeficiente de correlación de Pearson (r) mide qué tan lineal es la
# relación entre concentración y absorbancia: r = 1 es una línea perfecta
# creciente. La recta de regresión (mínimos cuadrados) da la pendiente y el
# intercepto; R² dice qué proporción de la variación de la absorbancia se
# explica por la concentración.

r_pearson <- cor(datos$concentracion_ug_ml, datos$absorbancia_595)

modelo <- lm(absorbancia_595 ~ concentracion_ug_ml, data = datos)
pendiente <- coef(modelo)[["concentracion_ug_ml"]]
intercepto <- coef(modelo)[["(Intercept)"]]
r_cuadrado <- summary(modelo)$r.squared

print(paste0("r de Pearson = ", round(r_pearson, 3)))
print(paste0("Pendiente = ", round(pendiente, 6), " unidades de absorbancia por µg/mL"))
print(paste0("Intercepto = ", round(intercepto, 3)))
print(paste0("R² = ", round(r_cuadrado, 3)))

# --------------------------------------------------------------------------
# 4. Gráfico de dispersión + recta de regresión
# --------------------------------------------------------------------------
# Cada punto es una medición real (una réplica). La recta azul es la recta
# de regresión ajustada a todos los puntos; la banda gris es su intervalo
# de confianza al 95%. La etiqueta muestra r y R² calculados de los datos.

etiqueta <- paste0(
  "r = ", round(r_pearson, 3),
  "   R² = ", round(r_cuadrado, 3)
)

grafica_dispersion <- ggplot(
  datos,
  aes(x = concentracion_ug_ml, y = absorbancia_595)
) +
  geom_point(size = 3, color = "#2E86AB", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#F24236", linewidth = 1.1) +
  annotate(
    "text",
    x = 120, y = 0.78, label = etiqueta,
    hjust = 0, size = 4.5, color = "#1a1a1a"
  ) +
  labs(
    x = "Concentración de BSA (µg/mL)",
    y = "Absorbancia a 595 nm",
    title = "Curva de calibración de Bradford (BSA)",
    subtitle = "Absorbancia a 595 nm vs concentración de proteína"
  ) +
  theme_bw(base_size = 13)

print(grafica_dispersion)
ggsave("02_dispersion_bradford.png", grafica_dispersion, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: dispersión + recta de regresión
# --------------------------------------------------------------------------
# - Eje X: concentración de BSA en microgramos por mililitro (µg/mL). Es la
#   concentración conocida de cada estándar, de 0 (blanco) a 800 µg/mL.
# - Eje Y: absorbancia a 595 nm, adimensional (no tiene unidades: es una
#   medida de cuánta luz absorbe la muestra a 595 nm; crece cuando hay más
#   complejo colorante-proteína).
# - Lo que se aprecia: los puntos suben de forma ordenada de izquierda a
#   derecha, casi sobre una línea recta, y las tres réplicas de cada
#   concentración quedan muy juntas. La recta roja pasa por el centro de
#   los puntos y la banda gris (intervalo de confianza al 95%) es estrecha.
# - Qué significa: hay una relación lineal fuerte entre concentración y
#   absorbancia (r cercano a 1). La pendiente dice cuánto sube la
#   absorbancia por cada µg/mL de proteína; el intercepto es la absorbancia
#   del blanco (sin proteína). Con R² cercano a 1, la recta explica casi
#   toda la variación de la absorbancia, así que esta curva sirve para
#   estimar la concentración de una muestra desconocida a partir de su
#   absorbancia.

# --------------------------------------------------------------------------
# 5. Resumen numérico por concentración (calculado de los datos)
# --------------------------------------------------------------------------
resumen <- datos %>%
  group_by(concentracion_ug_ml) %>%
  summarise(
    n = n(),
    media = mean(absorbancia_595),
    desviacion = sd(absorbancia_595),
    minimo = min(absorbancia_595),
    maximo = max(absorbancia_595),
    .groups = "drop"
  )

print(resumen)

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El gráfico de dispersión muestra la relación entre dos variables
#   numéricas: aquí, concentración de proteína (µg/mL) y absorbancia a
#   595 nm (adimensional). Cuando los puntos forman una línea, la relación
#   es lineal.
# - El coeficiente de correlación de Pearson (r) mide la fuerza de esa
#   relación lineal; R² dice qué proporción de la variación de la
#   absorbancia se explica por la concentración. Con r y R² cercanos a 1,
#   la curva de calibración es confiable para cuantificar proteína en
#   muestras desconocidas.
# - La pendiente de la recta es la sensibilidad del ensayo (cuánto cambia
#   la absorbancia por cada µg/mL) y el intercepto es la absorbancia del
#   blanco, el estándar sin proteína.
