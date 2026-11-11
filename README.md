# ggplot — Ejercicios con ggplot2

Ejercicios con ggplot2, uno por cada tipo de gráfico de
https://r-graph-gallery.com/, aplicados a química, bioquímica,
microalgas, biotecnología ambiental y estadística. Los temas varían:
no todo es microalgas ni luz azul y roja.

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
| 02 | `02-dispersion-correlacion-bradford.R` | Dispersión y correlación: curva de calibración de Bradford (BSA, absorbancia a 595 nm) |
| 03 | `03-barras-ranking-adsorcion-azul-metileno.R` | Barras y ranking: adsorción de azul de metileno por adsorbente (capacidad q en mg/g) |
