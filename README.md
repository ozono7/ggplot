# ggplot — Ejercicios con ggplot2

Ejercicios con ggplot2, uno por cada tipo de gráfico de
https://r-graph-gallery.com/, aplicados a química, bioquímica,
microalgas, biotecnología ambiental y estadística.

**Autor:** Dr. José Juan Torres Martínez
**Docente de Ciencias Químicas, UASLP**

## Reglas

- Todo calculado **de cero**: los datos van primero como mediciones reales
  (CSV) y cada valor se deriva de los datos; nunca parámetros pre-cocinados.
- Todos los scripts exportan sus gráficos al final con `ggsave()` **activo**
  (nunca comentado), PNG, 300 dpi.
- Tidyverse, gráficos claros y bien presentados, bien documentados en español.

## Ejercicios

| # | Archivo | Tipo de gráfico |
|---|---|---|
| 01 | `01-distribucion-od680-chlorella.R` | Distribución: histograma + densidad + violín (OD680 de *Chlorella vulgaris*) |
