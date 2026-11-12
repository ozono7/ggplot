# ==========================================================================
# Ejercicio 04. Líneas y evolución: fermentación alcohólica
# (gráfico de líneas con dos series en el tiempo)
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
# Serie "ggplot" — Ejercicio 04
#
# Descripción: Un ejercicio con gráficos de líneas de la familia de
#   gráficos de https://r-graph-gallery.com/ (sección Evolution).
#   Usamos mediciones reales de la concentración de glucosa y de etanol
#   durante una fermentación alcohólica con la levadura Saccharomyces
#   cerevisiae, el microorganismo que produce el etanol de las bebidas
#   fermentadas y del bioetanol. Todo se calcula de cero: los datos van
#   primero como mediciones reales (CSV) y cada valor (media y desviación
#   estándar por tiempo, rendimiento de etanol) se deriva de los datos.
#
# La fermentación alcohólica: la levadura consume la glucosa del medio y
#   la convierte en etanol y dióxido de carbono. Al inicio hay mucha
#   glucosa y nada de etanol; conforme pasa el tiempo la glucosa baja y el
#   etanol sube, hasta que la glucosa se agota y la producción de etanol se
#   detiene. Un gráfico de líneas muestra esa evolución en el tiempo: una
#   línea que baja (el sustrato que se consume) y una línea que sube (el
#   producto que se forma).
#
# Datos: datos_fermentacion_etanol.csv — concentración de glucosa y de
#   etanol (g/L) en un cultivo por lotes de Saccharomyces cerevisiae a
#   30 °C, medida a las 0, 6, 12, 18, 24, 36, 48, 60 y 72 horas, n = 3
#   réplicas por tiempo. El tiempo 0 es el inicio de la fermentación: el
#   medio tiene glucosa (100 g/L) y todavía no hay etanol.
#
# Objetivos de aprendizaje:
#   - Hacer un gráfico de líneas con ggplot2 para seguir dos variables en
#     el tiempo.
#   - Calcular la media y la desviación estándar por tiempo y mostrarlas
#     como banda de error alrededor de cada línea.
#   - Calcular el rendimiento de etanol (gramos de etanol por gramo de
#     glucosa consumida) a partir de los datos.
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
datos <- read_csv("datos_fermentacion_etanol.csv", show_col_types = FALSE)

# Revisar los datos
glimpse(datos)
summary(datos$concentracion_g_L)

# --------------------------------------------------------------------------
# 3. Resumen por tiempo y compuesto (todo calculado de los datos)
# --------------------------------------------------------------------------
# La media de las tres réplicas es la concentración de cada compuesto en
# cada tiempo; la desviación estándar mide cuánto varían las réplicas
# entre sí. La banda del gráfico es la media más y menos una desviación
# estándar.

resumen <- datos %>%
  group_by(tiempo_h, compuesto) %>%
  summarise(
    n = n(),
    media = mean(concentracion_g_L),
    desviacion = sd(concentracion_g_L),
    minimo = min(concentracion_g_L),
    maximo = max(concentracion_g_L),
    .groups = "drop"
  )

print(resumen)

# --------------------------------------------------------------------------
# 4. Rendimiento de etanol (calculado de los datos)
# --------------------------------------------------------------------------
# El rendimiento es el etanol producido dividido entre la glucosa
# consumida, entre el inicio (0 h) y el final (72 h). Se calcula con las
# medias de las réplicas. El valor teórico máximo es 0.51 g de etanol por
# cada gramo de glucosa, porque parte de la glucosa se usa para formar
# biomasa de levadura y otros productos.

inicio <- resumen %>% filter(tiempo_h == 0)
final <- resumen %>% filter(tiempo_h == 72)

glucosa_inicial <- inicio %>% filter(compuesto == "Glucosa") %>% pull(media)
glucosa_final <- final %>% filter(compuesto == "Glucosa") %>% pull(media)
etanol_inicial <- inicio %>% filter(compuesto == "Etanol") %>% pull(media)
etanol_final <- final %>% filter(compuesto == "Etanol") %>% pull(media)

glucosa_consumida <- glucosa_inicial - glucosa_final
etanol_producido <- etanol_final - etanol_inicial
rendimiento <- etanol_producido / glucosa_consumida

print(paste0("Glucosa consumida = ", round(glucosa_consumida, 1), " g/L"))
print(paste0("Etanol producido = ", round(etanol_producido, 1), " g/L"))
print(paste0("Rendimiento = ", round(rendimiento, 3), " g de etanol por g de glucosa"))

# Tiempo en el que la glucosa baja a la mitad de su valor inicial: el
# primer tiempo muestreado en el que la media de glucosa queda por debajo
# de la mitad de la glucosa inicial.
mitad_glucosa <- glucosa_inicial / 2
tiempo_mitad <- resumen %>%
  filter(compuesto == "Glucosa", media < mitad_glucosa) %>%
  pull(tiempo_h) %>%
  min()
print(paste0("La glucosa baja a la mitad (", round(mitad_glucosa, 1), " g/L) a las ", tiempo_mitad, " h"))

# --------------------------------------------------------------------------
# 5. Gráfico de líneas: evolución de glucosa y etanol
# --------------------------------------------------------------------------
# Cada línea es la media de las tres réplicas de un compuesto en cada
# tiempo. La banda sombreada es la media más y menos una desviación
# estándar. Los puntos son las réplicas medidas: quedan dentro de la banda
# cuando las réplicas coinciden. Los colores separan el sustrato que se
# consume (glucosa) del producto que se forma (etanol).

# Ordenar los compuestos para que la leyenda muestre primero la glucosa.
resumen$compuesto <- factor(resumen$compuesto, levels = c("Glucosa", "Etanol"))
datos$compuesto <- factor(datos$compuesto, levels = c("Glucosa", "Etanol"))

grafica_lineas <- ggplot(resumen, aes(x = tiempo_h, y = media, color = compuesto, fill = compuesto)) +
  geom_ribbon(
    aes(ymin = media - desviacion, ymax = media + desviacion),
    color = NA, alpha = 0.2
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(
    data = datos,
    aes(x = tiempo_h, y = concentracion_g_L, color = compuesto),
    size = 2, alpha = 0.7
  ) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Glucosa" = "#2E86AB", "Etanol" = "#F24236")) +
  scale_fill_manual(values = c("Glucosa" = "#2E86AB", "Etanol" = "#F24236")) +
  labs(
    x = "Tiempo (h)",
    y = "Concentración (g/L)",
    color = "Compuesto",
    fill = "Compuesto",
    title = "Fermentación alcohólica con Saccharomyces cerevisiae",
    subtitle = "Glucosa y etanol (g/L), media ± DE, n = 3"
  ) +
  theme_bw(base_size = 13)

print(grafica_lineas)
ggsave("04_lineas_fermentacion_etanol.png", grafica_lineas, width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: gráfico de líneas (evolución en el tiempo)
# --------------------------------------------------------------------------
# - Eje X: tiempo en horas (h), desde el inicio de la fermentación (0 h)
#   hasta las 72 h.
# - Eje Y: concentración en gramos por litro (g/L), la misma unidad para
#   los dos compuestos, así las dos líneas se comparan en la misma escala.
# - Lo que se aprecia: la línea de glucosa baja de forma continua, de
#   100 g/L a casi 0 g/L, y la línea de etanol sube de 0 a 45 g/L. El
#   cambio es más rápido entre las 12 y las 36 horas, cuando la glucosa
#   cae de 78 a 24 g/L y el etanol sube de 10 a 35 g/L. Después de las 48
#   horas las dos líneas se aplanan: la glucosa casi se agota y el etanol
#   deja de subir. Las bandas sombreadas son estrechas y los puntos de las
#   réplicas quedan juntos, así que las mediciones de cada tiempo
#   coinciden entre sí.
# - Qué significa: la levadura consume la glucosa y la convierte en
#   etanol mientras hay sustrato disponible. Cuando la glucosa se agota
#   (alrededor de las 60-72 h), la fermentación se detiene y el etanol se
#   queda en su valor final, 45 g/L. El rendimiento calculado de los datos
#   es de 0.46 g de etanol por gramo de glucosa, cerca del máximo teórico
#   de 0.51 g/g: la mayor parte de la glucosa se convirtió en etanol.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El gráfico de líneas muestra cómo cambia una variable en el tiempo:
#   aquí, la concentración de glucosa y de etanol durante 72 horas de
#   fermentación. La línea que baja es el sustrato que se consume y la
#   línea que sube es el producto que se forma.
# - Con dos series en el mismo gráfico se ve la relación entre sustrato y
#   producto: el etanol sube mientras la glucosa baja, y las dos líneas se
#   aplanan al mismo tiempo, cuando la glucosa se agota.
# - La banda alrededor de cada línea es la media más y menos una
#   desviación estándar: cuando la banda es estrecha y las réplicas
#   (puntos) quedan dentro de ella, la media de cada tiempo es confiable.
# - El rendimiento se calcula de los datos: etanol producido dividido
#   entre glucosa consumida, en gramos por gramo (g/g). Compara el
#   rendimiento medido con el máximo teórico (0.51 g/g) para saber qué tan
#   eficiente fue la fermentación.
