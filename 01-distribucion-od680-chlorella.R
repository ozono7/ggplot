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
# Descripción: Un ejercicio muy llamativo con gráficos de distribución
#   (histograma, densidad y violín) de la familia de gráficos de
#   https://r-graph-gallery.com/ (sección Distribution). Usamos mediciones
#   reales de densidad óptica (OD680) de Chlorella vulgaris cultivada con
#   luz azul y luz roja. Todo se calcula de cero: los datos van primero
#   como mediciones reales (CSV) y cada valor se deriva de los datos.
#
# Objetivos de aprendizaje:
#   - Hacer un histograma y una curva de densidad con ggplot2.
#   - Comparar dos tratamientos con un gráfico de violín + caja.
#   - Personalizar colores, títulos y tema para un gráfico estilo revista.
#
# Requisitos: tidyverse.
#   Instálalo con: install.packages("tidyverse")
#
# Datos: datos_od680_chlorella.csv — OD680 de Chlorella vulgaris, días 0-10,
#   luz Azul y luz Roja, n = 3 réplicas por día y tratamiento.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Cargar paquetes
# --------------------------------------------------------------------------
library(tidyverse)

# --------------------------------------------------------------------------
# 2. Leer los datos (mediciones reales, formato largo)
# --------------------------------------------------------------------------
datos <- read_csv("datos_od680_chlorella.csv", show_col_types = FALSE)

# Revisar los datos
glimpse(datos)
summary(datos$od680)

# --------------------------------------------------------------------------
# 3. Histograma + densidad de OD680 (todas las mediciones)
# --------------------------------------------------------------------------
# ¿Cómo se distribuyen las mediciones de OD680? Un histograma muestra la
# forma de la distribución; la curva de densidad la suaviza.

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
    subtitle = "Histograma y curva de densidad (todas las mediciones, luz azul y roja)"
  ) +
  theme_bw(base_size = 13)

print(grafica_hist)
ggsave("01_histograma_od680.png", grafica_hist, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# 4. Violín + caja + puntos: OD680 por tratamiento de luz
# --------------------------------------------------------------------------
# El gráfico de violín muestra la forma de la distribución por tratamiento;
# la caja muestra mediana y cuartiles; los puntos son las mediciones reales.

grafica_violin <- ggplot(datos, aes(x = tratamiento, y = od680, fill = tratamiento)) +
  geom_violin(alpha = 0.6, color = NA) +
  geom_boxplot(width = 0.15, fill = "white", color = "#1a1a1a", outlier.shape = NA) +
  geom_jitter(width = 0.12, size = 1.6, alpha = 0.6, color = "#1a1a1a") +
  scale_fill_manual(values = c("Azul" = "#2E86AB", "Rojo" = "#F24236")) +
  labs(
    x = "Tratamiento de luz",
    y = "Densidad óptica (OD680)",
    title = "OD680 de Chlorella vulgaris por tratamiento de luz",
    subtitle = "Violín + caja + mediciones reales (n = 3 por día, días 0-10)"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

print(grafica_violin)
ggsave("01_violin_od680_por_luz.png", grafica_violin, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# 5. Resumen numérico por tratamiento (calculado de los datos)
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
# Qué aprendimos
# --------------------------------------------------------------------------
# - El histograma y la densidad muestran la forma de la distribución de OD680.
# - El violín compara la distribución entre luz azul y luz roja.
# - La luz azul alcanza OD680 más altos (mayor crecimiento) que la roja.
# - Cada punto es una medición real; nada se inventó, todo salió del CSV.
