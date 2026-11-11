# ==========================================================================
# Ejercicio 03. Barras y ranking: adsorción de azul de metileno
# (gráfico de barras ordenado de mayor a menor)
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "ggplot" — Ejercicio 03
#
# Descripción: Un ejercicio con gráficos de barras y ranking de la familia
#   de gráficos de https://r-graph-gallery.com/ (sección Ranking).
#   Usamos mediciones reales de la capacidad de adsorción de azul de
#   metileno, un colorante usado en la industria textil que contamina el
#   agua, por seis adsorbentes distintos. Todo se calcula de cero: los datos
#   van primero como mediciones reales (CSV) y cada valor (media y
#   desviación estándar por adsorbente) se deriva de los datos.
#
# La adsorción: los adsorbentes retienen el colorante en su superficie y lo
#   quitan del agua. La capacidad de adsorción (q) dice cuántos miligramos
#   de colorante retiene cada gramo de adsorbente. Un gráfico de barras
#   ordenado de mayor a menor (ranking) muestra de un vistazo qué
#   adsorbente retiene más colorante y cuál menos.
#
# Datos: datos_adsorcion_azul_metileno.csv — capacidad de adsorción q
#   (mg/g) de azul de metileno, medida en ensayos por lotes a 25 °C, n = 3
#   réplicas por adsorbente: carbón activado, quitosano, biochar de
#   cáscara de naranja, zeolita natural, cáscara de plátano y aserrín de
#   pino. El carbón activado es el adsorbente comercial de referencia.
#
# Objetivos de aprendizaje:
#   - Hacer un gráfico de barras con ggplot2 y ordenarlo de mayor a menor.
#   - Calcular la media y la desviación estándar por grupo y mostrarlas
#     como barras de error.
#   - Comparar categorías con los valores de las réplicas a la vista.
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
datos <- read_csv("datos_adsorcion_azul_metileno.csv", show_col_types = FALSE)

# Revisar los datos
glimpse(datos)
summary(datos$q_mg_g)

# --------------------------------------------------------------------------
# 3. Resumen por adsorbente (todo calculado de los datos)
# --------------------------------------------------------------------------
# La media de las tres réplicas es la capacidad de adsorción de cada
# adsorbente; la desviación estándar mide cuánto varían las réplicas entre
# sí. Después ordenamos los adsorbentes de mayor a menor media, que es el
# orden en el que aparecerán las barras.

resumen <- datos %>%
  group_by(adsorbente) %>%
  summarise(
    n = n(),
    media = mean(q_mg_g),
    desviacion = sd(q_mg_g),
    minimo = min(q_mg_g),
    maximo = max(q_mg_g),
    .groups = "drop"
  ) %>%
  arrange(desc(media))

print(resumen)

# Ordenar el adsorbente como factor según la media, de menor a mayor.
# En el eje Y el primer nivel queda abajo, así el adsorbente con mayor
# media queda arriba y las barras se leen como ranking de mayor a menor.
datos$adsorbente <- factor(
  datos$adsorbente,
  levels = rev(resumen$adsorbente)
)
resumen$adsorbente <- factor(
  resumen$adsorbente,
  levels = rev(resumen$adsorbente)
)

# --------------------------------------------------------------------------
# 4. Gráfico de barras ordenado (ranking)
# --------------------------------------------------------------------------
# Cada barra es la media de las tres réplicas de un adsorbente. La barra de
# error es la desviación estándar. Los puntos son las réplicas medidas:
# quedan dentro de la barra de error cuando las réplicas coinciden. El
# número al final de cada barra es la media en mg/g.

grafica_barras <- ggplot(resumen, aes(x = media, y = adsorbente)) +
  geom_col(fill = "#2E86AB", width = 0.65) +
  geom_errorbar(
    aes(xmin = media - desviacion, xmax = media + desviacion),
    width = 0.2, color = "#1a1a1a", linewidth = 0.8
  ) +
  geom_point(
    data = datos,
    aes(x = q_mg_g, y = adsorbente),
    size = 2.5, color = "#F24236",
    position = position_jitter(height = 0.12, width = 0, seed = 1)
  ) +
  geom_text(
    aes(x = media + desviacion + 6, label = round(media, 1)),
    hjust = 0, size = 4.5, color = "#1a1a1a"
  ) +
  labs(
    x = "Capacidad de adsorción, q (mg/g)",
    y = "Adsorbente",
    title = "Adsorción de azul de metileno por adsorbente",
    subtitle = "Capacidad de adsorción (mg/g), media ± DE, n = 3"
  ) +
  xlim(0, 275) +
  theme_bw(base_size = 13)

print(grafica_barras)
ggsave("03_barras_adsorcion_azul_metileno.png", grafica_barras, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: gráfico de barras ordenado
# --------------------------------------------------------------------------
# - Eje X: capacidad de adsorción q en miligramos de colorante por gramo
#   de adsorbente (mg/g). Es la cantidad de azul de metileno que retiene
#   cada gramo de adsorbente.
# - Eje Y: adsorbente, una categoría (no tiene unidades). Las barras están
#   ordenadas de mayor a menor capacidad de adsorción.
# - Lo que se aprecia: el carbón activado retiene mucho más colorante que
#   los demás (más de 230 mg/g), seguido del quitosano (alrededor de 157
#   mg/g) y del biochar de cáscara de naranja (alrededor de 109 mg/g). La
#   zeolita natural, la cáscara de plátano y el aserrín de pino quedan muy
#   por debajo (menos de 60 mg/g). Las barras de error son cortas y los
#   puntos de las réplicas quedan juntos, así que las mediciones de cada
#   adsorbente coinciden entre sí.
# - Qué significa: entre los materiales probados, el carbón activado es el
#   mejor adsorbente para quitar azul de metileno del agua, y el quitosano
#   y el biochar de cáscara de naranja son las alternativas naturales con
#   mayor capacidad. El aserrín de pino retiene lo menos, pero es el
#   material más barato y abundante de los seis.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El gráfico de barras compara categorías: aquí, seis adsorbentes. Cada
#   barra es la media de las réplicas medidas, y la barra de error es la
#   desviación estándar, que dice cuánto varían esas réplicas.
# - Ordenar las barras de mayor a menor convierte el gráfico en un
#   ranking: el mejor adsorbente queda arriba y el peor abajo, y la
#   comparación se lee de un vistazo.
# - Mostrar las réplicas como puntos sobre las barras deja ver si las
#   mediciones coinciden: cuando los puntos quedan juntos y la barra de
#   error es corta, la media es confiable.
# - La capacidad de adsorción q se mide en mg/g (miligramos de colorante
#   por gramo de adsorbente): mientras mayor es q, más colorante quita el
#   material del agua.
