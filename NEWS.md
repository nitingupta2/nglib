# nglib 1.1.0

* `theme_quant()` and `theme_poster()`, with `set_theme_quant()`,
  `set_theme_poster()` and `update_font_defaults()` to apply them to a whole
  session.
* Color and fill scales built on seven named palettes: `scale_color_quant()`,
  `scale_fill_quant()`, `scale_color_quant_manual()`,
  `scale_fill_quant_manual()` and their `scale_colour_*` aliases. An unknown
  palette name raises an error.
* Palette utilities: `get_palettes()`, `quant_pal()`, `plot_colors()`,
  `test_pal()` and `convert_mm_to_pts()`.
* EDA plots for univariate, bivariate and multivariate views of a data frame
  (`eda_plot_*()`), and the clustered correlation heat map
  `plot_correlations_gg()`.
  * Categories are ordered by the median of the numerical feature unless
    they are an ordered factor. Numeric or logical columns can also be used
    as categories.
  * `eda_plot_missing_values()` and `eda_plot_correlation_matrix()` use
    `theme_quant()` in the font of the current session theme, so they work on
    any graphics device.
  * `plot_correlations_gg()` uses only the numerical columns of a data frame,
    with pairwise-complete observations.
* Data helpers: `find_mode()`, `%notin%`, `change_na_to()`,
  `change_na_to_most_common_value()`, `median_by_grouping()`,
  `most_common_value_by_grouping()`, `significant_digits()`,
  `pct_diff_lagging()`, `pct_diff_leading()`, `nearest_multiple()` and
  `correlation_reorder_matrix()`.
