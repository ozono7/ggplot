# ==========================================================================
# Ejercicio 01. Distribución de OD680 en Chlorella vulgaris
# (histograma + densidad + violín)
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "ggplot" — Ejercicio 01
#
# Descripción: Un ejercicio con gráficos de distribución (histograma,
#   densidad y violín) de la familia de gráficos de
#   https://r-graph-gallery.com/ (sección Distribution). Usamos mediciones
#   de densidad óptica (OD680) de Chlorella vulgaris cultivada con
#   luz azul, luz blanca (control) y luz roja. Los valores son simulados
#   con fines didácticos, con magnitudes realistas. Todo se calcula de
#   cero: los datos van primero como mediciones en un archivo CSV y cada
#   valor se deriva de los datos.
#
# Nota sobre el diseño: el conjunto de datos tiene tres tratamientos
#   (luz azul, luz blanca como control y luz roja), con n = 3 réplicas
#   por día y tratamiento. La comparación es de cada luz contra el
#   control con luz blanca.
#
# Objetivos de aprendizaje:
#   - Hacer un histograma y una curva de densidad con ggplot2.
#   - Comparar tres tratamientos con un gráfico de violín + caja.
#   - Personalizar colores, títulos y tema para un gráfico claro.
#
# Requisitos: tidyverse.
#   Instálalo con: install.packages("tidyverse")
#
# Datos: datos_od680_chlorella.csv — OD680 de Chlorella vulgaris, días 0-10,
#   luz Azul, luz Blanca (control) y luz Roja, n = 3 réplicas por día y tratamiento.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# 2. Leer los datos (mediciones en formato largo)
# --------------------------------------------------------------------------
datos <- read_csv("datos_od680_chlorella.csv", show_col_types = FALSE)

# Revisar los datos
glimpse(datos)
summary(datos$od680)

# --------------------------------------------------------------------------
# 3. Resumen numérico por tratamiento (calculado de los datos)
# --------------------------------------------------------------------------
resumen <- datos %>%
  group_by(tratamiento) %>%
  summarise(
    n = n(),
    media = mean(od680),
    mediana = median(od680),
    desviacion = sd(od680),
    minimo = min(od680),
    maximo = max(od680),
    .groups = "drop"
  )

print(resumen)

# --------------------------------------------------------------------------
# 4. Histograma + densidad de OD680 (todas las mediciones)
# --------------------------------------------------------------------------
# ¿Cómo se distribuyen las mediciones de OD680? Un histograma muestra la
# forma de la distribución; la curva de densidad la suaviza. Como los datos
# mezclan todos los días (0-10), la mayoría de los valores están cerca de
# cero (días iniciales) y hay una cola larga hacia valores altos (días
# finales). Eso es esperable en datos de crecimiento.

grafica_hist <- ggplot(datos, aes(x = od680)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20, fill = "#2E86AB", color = "white", alpha = 0.8
  ) +
  geom_density(color = "#F24236", linewidth = 1.2) +
  labs(
    x = "Densidad óptica (OD680)",
    y = "Densidad",
    title = "Distribución de OD680 en Chlorella vulgaris",
    subtitle = "Histograma y curva de densidad (todas las mediciones)"
  ) +
  theme_bw(base_size = 13)

print(grafica_hist)
ggsave("01_histograma_od680.png", grafica_hist,
       width = 20, height = 13, units = "cm", dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: histograma + densidad de OD680
# --------------------------------------------------------------------------
# - Eje X: densidad óptica (OD680), adimensional (no tiene unidades: es
#   una medida de cuánta luz absorbe el cultivo a 680 nm, y crece cuando
#   hay más células). Va de 0 (cultivo casi sin células) a poco más de 2
#   (cultivo denso).
# - Eje Y: densidad (de probabilidad), adimensional. No es un conteo: es
#   la altura de la distribución. La altura de cada barra dice qué tan
#   frecuentes son los valores en ese rango; la curva roja (densidad)
#   suaviza el histograma.
# - Lo que se aprecia: la mayoría de las mediciones están cerca de cero
#   (la barra más alta, a la izquierda) y hay una cola larga hacia la
#   derecha (valores altos, hasta ~2.2). La distribución está sesgada a
#   la derecha: no es simétrica.
# - Qué significa: esto es esperable porque los datos mezclan todos los
#   días (0-10). Los días iniciales tienen OD680 casi cero y los días
#   finales tienen OD680 alto. El histograma junta todas las fases de
#   crecimiento en una sola distribución, por eso se ve ese pico a la
#   izquierda y la cola a la derecha.

# --------------------------------------------------------------------------
# 5. Violín + caja + puntos: OD680 por tratamiento de luz
# --------------------------------------------------------------------------
# El gráfico de violín muestra la forma de la distribución por tratamiento;
# la caja muestra mediana y cuartiles; los puntos son las mediciones.

grafica_violin <- ggplot(datos, aes(x = tratamiento, y = od680, fill = tratamiento)) +
  geom_violin(alpha = 0.6, color = NA) +
  geom_boxplot(width = 0.15, fill = "white", color = "#1a1a1a", outlier.shape = NA) +
  geom_jitter(width = 0.12, size = 1.6, alpha = 0.6, color = "#1a1a1a") +
  scale_fill_manual(values = c("Azul" = "#2E86AB", "Blanco" = "#E69F00", "Rojo" = "#F24236")) +
  labs(
    x = "Tratamiento de luz",
    y = "Densidad óptica (OD680)",
    title = "OD680 de Chlorella vulgaris por tratamiento de luz",
    subtitle = "Violín + caja + puntos (mediciones)"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

print(grafica_violin)
ggsave("01_violin_od680_por_luz.png", grafica_violin,
       width = 20, height = 13, units = "cm", dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: violín + caja + puntos por tratamiento de luz
# --------------------------------------------------------------------------
# - Eje X: tratamiento de luz (Azul, Blanco o Rojo). No es una cantidad,
#   son tres grupos.
# - Eje Y: densidad óptica (OD680), adimensional, igual que arriba (0 =
#   casi sin células, 2 = cultivo denso).
# - Lo que se aprecia: cada violín muestra la forma de la distribución de
#   un tratamiento. La parte ancha del violín es donde se concentran más
#   mediciones. La caja blanca dentro muestra la mediana (línea del medio)
#   y el 50% central de los datos. Los puntos son las mediciones.
#   El violín azul es más alto y más ancho arriba; el blanco (control)
#   queda en medio y el rojo queda abajo: la azul tiene los valores
#   mayores, la blanca los intermedios y la roja los menores.
# - Qué significa: en conjunto, la luz azul da valores de OD680 más altos
#   (mayor crecimiento) que el control con luz blanca, y la luz roja da
#   valores más bajos que el control. La dispersión (el alto del violín
#   y de los puntos) refleja que los valores varían entre días, porque el
#   cultivo crece con el tiempo.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El histograma y la densidad muestran la forma de la distribución de OD680.
#   La OD680 es adimensional (no tiene unidades) y crece cuando hay más
#   células: 0 es casi sin células y 2 es un cultivo denso.
# - El violín compara la distribución entre los tres tratamientos: la luz
#   azul llega más alto (mayor crecimiento) que el control con luz blanca,
#   y la luz roja queda por debajo del control.
