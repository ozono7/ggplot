# ==========================================================================
# Ejercicio 05. Caja y violín: actividad de la alfa-amilasa según el pH
# (gráfico de caja con violín para comparar distribuciones entre grupos)
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "ggplot" — Ejercicio 05
#
# Descripción: Un ejercicio con gráficos de caja y de violín de la
#   familia de gráficos de https://r-graph-gallery.com/ (sección
#   Distribution). Usamos mediciones reales de la actividad de la
#   alfa-amilasa, una enzima que rompe el almidón, medida a cinco valores
#   de pH. Todo se calcula de cero: los datos van primero como mediciones
#   reales (CSV) y cada valor (mediana, cuartiles, rango intercuartílico,
#   media y desviación estándar por pH) se deriva de los datos.
#
# La alfa-amilasa y el pH: la actividad de una enzima depende del pH del
#   medio porque el pH cambia la carga de los aminoácidos del sitio
#   activo. Cada enzima tiene un pH óptimo, donde trabaja más rápido; a
#   pH muy ácido o muy básico la actividad cae. Un gráfico de caja muestra
#   esa comparación entre grupos: en cada pH se ve dónde está el centro
#   de las mediciones (la mediana), cuánto se dispersan (la caja y los
#   bigotes) y si hay valores apartados. El violín muestra la forma de la
#   distribución: dónde se concentran las mediciones de cada grupo.
#
# Datos: datos_actividad_amilasa_ph.csv — actividad de la alfa-amilasa
#   (U/mL) medida a pH 4.0, 5.0, 6.0, 7.0 y 8.0, n = 8 réplicas por pH.
#   La actividad se midió a 37 °C con almidón soluble como sustrato y se
#   expresa en unidades por mililitro (U/mL): una unidad (U) es la
#   cantidad de enzima que libera un micromol de producto por minuto.
#   El pH es adimensional: es el logaritmo negativo de la concentración
#   de iones hidrógeno, por eso no lleva unidades.
#
# Objetivos de aprendizaje:
#   - Hacer un gráfico de caja con ggplot2 para comparar la distribución
#     de una variable entre varios grupos.
#   - Añadir el violín para ver la forma de la distribución de cada
#     grupo y los puntos de las réplicas medidas.
#   - Calcular la mediana, los cuartiles y el rango intercuartílico por
#     grupo y leerlos directamente en la caja del gráfico.
#
# Requisitos: tidyverse.
#   Instálalo con: install.packages("tidyverse")
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
# Fijar codificación UTF-8 para que los acentos (ó, í) se dibujen bien en
# el gráfico. Si el sistema no tiene esa locale, no pasa nada: se sigue con
# la locale que ya tenga.
suppressWarnings(try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE))

library(tidyverse)

# --------------------------------------------------------------------------
# 2. Leer los datos (mediciones reales, formato largo)
# --------------------------------------------------------------------------
datos <- read_csv("datos_actividad_amilasa_ph.csv", show_col_types = FALSE)

# Revisar los datos
glimpse(datos)
summary(datos$actividad_U_mL)

# El pH es una variable de grupo en este ejercicio: cada pH es una
# categoría con sus ocho réplicas. Se convierte a factor para que el
# gráfico ordene los grupos de pH 4.0 a pH 8.0.
datos$ph <- factor(datos$ph, levels = c("4", "5", "6", "7", "8"))

# --------------------------------------------------------------------------
# 3. Resumen por pH (todo calculado de los datos)
# --------------------------------------------------------------------------
# La mediana es el valor central de las ocho réplicas de cada pH: la
# mitad de las mediciones queda por arriba y la mitad por abajo. El
# primer cuartil (Q1) es el valor por debajo del cual queda el 25 % de
# las mediciones y el tercer cuartil (Q3) el valor por debajo del cual
# queda el 75 %. El rango intercuartílico (RIQ) es Q3 menos Q1 y mide la
# dispersión de la mitad central de los datos: es la altura de la caja
# del gráfico.

resumen <- datos %>%
  group_by(ph) %>%
  summarise(
    n = n(),
    media = mean(actividad_U_mL),
    mediana = median(actividad_U_mL),
    q1 = quantile(actividad_U_mL, 0.25),
    q3 = quantile(actividad_U_mL, 0.75),
    minimo = min(actividad_U_mL),
    maximo = max(actividad_U_mL),
    desviacion = sd(actividad_U_mL),
    .groups = "drop"
  ) %>%
  mutate(riq = q3 - q1)

print(resumen)

# --------------------------------------------------------------------------
# 4. pH óptimo y comparación entre extremos (calculados de los datos)
# --------------------------------------------------------------------------
# El pH óptimo es el pH cuya mediana de actividad es la más alta de los
# cinco grupos. La comparación entre extremos es la mediana del pH
# óptimo dividida entre la mediana del pH con menos actividad: dice
# cuántas veces más activa es la enzima en su óptimo que en el peor pH.

ph_optimo <- resumen %>% filter(mediana == max(mediana)) %>% pull(ph)
mediana_optimo <- resumen %>% filter(ph == ph_optimo) %>% pull(mediana)
ph_minimo <- resumen %>% filter(mediana == min(mediana)) %>% pull(ph)
mediana_minimo <- resumen %>% filter(ph == ph_minimo) %>% pull(mediana)
veces <- mediana_optimo / mediana_minimo

print(paste0("pH optimo (mayor mediana) = pH ", ph_optimo, " (mediana ", round(mediana_optimo, 1), " U/mL)"))
print(paste0("pH con menor actividad = pH ", ph_minimo, " (mediana ", round(mediana_minimo, 1), " U/mL)"))
print(paste0("La enzima es ", round(veces, 1), " veces mas activa en su pH optimo que a pH ", ph_minimo))

# --------------------------------------------------------------------------
# 5. Gráfico de caja y violín: actividad por pH
# --------------------------------------------------------------------------
# El violín muestra la forma de la distribución de cada grupo: es ancho
# donde se concentran las mediciones y angosto donde hay pocas. La caja
# marca la mitad central de los datos: la línea gruesa de en medio es la
# mediana, los bordes de la caja son el primer y el tercer cuartil, y los
# bigotes llegan hasta la medición más alejada que todavía queda dentro
# de 1.5 veces el rango intercuartílico. Los puntos son las ocho
# réplicas medidas en cada pH.

grafica_caja <- ggplot(datos, aes(x = ph, y = actividad_U_mL, fill = ph)) +
  geom_violin(trim = FALSE, alpha = 0.45, color = NA) +
  geom_boxplot(width = 0.35, outlier.shape = 21, outlier.size = 2.5,
               outlier.fill = "white", alpha = 0.9) +
  geom_jitter(width = 0.12, size = 2, alpha = 0.7, color = "black") +
  scale_fill_manual(values = c("4" = "#F4A582", "5" = "#F7C873",
                               "6" = "#7FB069", "7" = "#4E9A9A",
                               "8" = "#5B7DB1")) +
  labs(
    x = "pH (adimensional)",
    y = "Actividad (U/mL)",
    fill = "pH",
    title = "Actividad de la alfa-amilasa según el pH",
    subtitle = "Actividad enzimática (U/mL), n = 8 por pH"
  ) +
  theme_bw(base_size = 13)

print(grafica_caja)
ggsave("05_caja_violin_actividad_amilasa_ph.png", grafica_caja, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: gráfico de caja y violín (comparación entre grupos)
# --------------------------------------------------------------------------
# - Eje X: pH, adimensional (el pH es el logaritmo negativo de la
#   concentración de iones hidrógeno, por eso no lleva unidades). Cada
#   grupo es un valor de pH: 4.0, 5.0, 6.0, 7.0 y 8.0.
# - Eje Y: actividad de la alfa-amilasa en unidades por mililitro
#   (U/mL). Una unidad (U) es la cantidad de enzima que libera un
#   micromol de producto por minuto.
# - Lo que se aprecia: la actividad sube con el pH hasta un máximo y
#   luego baja. La caja de pH 6.0 está más alta que todas: mediana 60.9
#   U/mL, con la mitad central de las réplicas entre 59.6 y 62.7 U/mL.
#   A pH 7.0 la actividad sigue alta (mediana 52.4 U/mL) y a pH 5.0 ya
#   baja a 28.1 U/mL. En los extremos la actividad es mucho menor: a pH
#   4.0 la mediana es 10.3 U/mL y a pH 8.0 es 24.3 U/mL. Los violines
#   son simétricos y las cajas estrechas, así que las réplicas de cada
#   pH coinciden entre sí; no hay puntos fuera de los bigotes, es decir,
#   no hay valores apartados en ningún grupo.
# - Qué significa: el pH óptimo de esta alfa-amilasa está cerca de pH
#   6.0, donde la enzima es 5.9 veces más activa que a pH 4.0. Lejos del
#   óptimo, a pH ácido o básico, la actividad cae porque el pH cambia la
#   carga de los aminoácidos del sitio activo y la enzima ya no se une
#   igual al almidón. El rango de actividad total va de 8.7 a 65.1 U/mL.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El gráfico de caja compara la distribución de una variable entre
#   varios grupos: aquí, la actividad de la alfa-amilasa a cinco valores
#   de pH. La línea de en medio de cada caja es la mediana, los bordes
#   son el primer y el tercer cuartil, y la altura de la caja es el
#   rango intercuartílico, la dispersión de la mitad central de los
#   datos.
# - El violín añade la forma de la distribución: es ancho donde se
#   concentran las mediciones. Junto con los puntos de las réplicas, se
#   ve de un vistazo si los datos de cada grupo están juntos o
#   dispersos y si hay valores apartados fuera de los bigotes.
# - La mediana y los cuartiles se calculan de los datos y se leen
#   directamente en la caja: en este ejercicio el pH óptimo es pH 6.0
#   porque su caja está más alta que las demás, con mediana 60.9 U/mL.
# - Cuando los grupos se ordenan por una variable (el pH), el gráfico de
#   caja muestra el patrón completo: la actividad sube hasta el óptimo
#   y luego baja, en lugar de comparar solo dos valores sueltos.
