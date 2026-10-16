# nglib

`nglib` is a small R package of **ggplot2 themes, color scales and quick
exploratory data analysis (EDA) plots**, plus a few small data-wrangling
helpers.

What's included:

| Area | Functions |
|---|---|
| Themes | `theme_quant()`, `theme_poster()`, `set_theme_quant()`, `set_theme_poster()`, `update_font_defaults()` |
| Scales | `scale_color_quant()`, `scale_fill_quant()`, `scale_color_quant_manual()`, `scale_fill_quant_manual()` |
| Palettes | `get_palettes()`, `quant_pal()`, `plot_colors()`, `test_pal()`, `convert_mm_to_pts()` |
| EDA: univariate | `eda_plot_histograms()`, `eda_plot_column()`, `eda_plot_missing_counts()`, `eda_plot_missing_values()`, `eda_plot_normal_qq()` |
| EDA: bivariate | `eda_plot_scatter()`, `eda_plot_box()`, `eda_plot_bar()`, `eda_plot_density()`, `eda_plot_stacked_bar()` |
| EDA: multivariate | `eda_plot_scatter_cat()`, `eda_plot_scatter_cat_facets()`, `eda_plot_heatmap()`, `eda_plot_correlation_matrix()`, `plot_correlations_gg()` |
| Data helpers | `find_mode()`, `%notin%`, `change_na_to()`, `change_na_to_most_common_value()`, `median_by_grouping()`, `most_common_value_by_grouping()`, `significant_digits()`, `pct_diff_lagging()`, `pct_diff_leading()`, `nearest_multiple()`, `correlation_reorder_matrix()` |

## Installation

From GitHub:

```r
# install.packages("remotes")
remotes::install_github("nitingupta2/nglib")
```

Or from a local clone:

```r
install.packages("path/to/nglib", repos = NULL, type = "source")
```

## Interactive use

`nglib` is built for interactive analysis. Loading it
attaches `tidyverse`, `ggplot2`, `dplyr`, `tidyr`, `purrr`, `forcats`,
`magrittr`, `scales`, `glue`, `rlang` and `ggridges`, so pipelines and
`ggplot()` work straight after `library(nglib)`. The cost is a busier search
path (e.g. `filter()` and `lag()` are masked) and a slower startup.

## Fonts

`theme_quant()` defaults to **Roboto Condensed** and `theme_poster()` to
**Inter**. Both are free on [Google Fonts](https://fonts.google.com/). If a
font isn't installed, either install it or pass `font_family = ""` to use the
device's default font. On Windows, the font is found most reliably when
plots are rendered with [ragg](https://ragg.r-lib.org/): set
*Tools > Global Options > Graphics > Backend: AGG* in RStudio, or pass
`device = ragg::agg_png` to `ggsave()`.

## Usage

```r
library(nglib)
set_theme_quant()   # theme_quant() for all plots, with matching geom_text fonts

eda_plot_histograms(mtcars, c("mpg", "wt", "disp", "hp"))
```

![](man/figures/eda-histograms.png)

Categories are ordered by the median of the numerical feature, unless they are
an ordered factor:

```r
eda_plot_box(ggplot2::mpg, "class", "hwy")
```

![](man/figures/eda-box.png)

```r
eda_plot_density(ggplot2::diamonds, "cut", "price", xLog = TRUE,
                 xLabelsScale = scales::label_dollar())
```

![](man/figures/eda-density.png)

```r
eda_plot_scatter_cat_facets(ggplot2::mpg, "displ", "hwy", "drv")
```

![](man/figures/eda-scatter-facets.png)

```r
eda_plot_heatmap(ggplot2::diamonds, "cut", "color", "price")
```

![](man/figures/eda-heatmap.png)

```r
eda_plot_correlation_matrix(mtcars, outlineVar = "mpg")
```

![](man/figures/eda-correlation.png)

```r
eda_plot_missing_counts(airquality)
```

![](man/figures/eda-missing.png)

### Themes and scales

The scales work in any ggplot. `discrete = FALSE` turns a palette into a
continuous gradient:

```r
ggplot(ggplot2::economics_long, aes(date, value01, color = variable)) +
    geom_line(linewidth = 1) +
    scale_color_quant(palette = "metro") +
    theme_poster()
```

![](man/figures/theme-poster.png)

The palettes are `main`, `withgrey`, `pnl`, `weights`, `metro`, `greyscale`
and `heatmap`. To preview one, run `test_pal("metro")`; to get its hex codes,
run `plot_colors("metro", n = 5)`.

## License

MIT © Nitin Gupta
