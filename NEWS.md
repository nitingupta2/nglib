# nglib 1.3.0

New EDA plot:

* `eda_plot_residuals()`: the four residual plots of a linear model that
  `plot(lm)` draws (residuals vs fitted, normal Q-Q, scale-location, and
  residuals vs leverage with Cook's distance contours) as one ggplot in the
  package theme, labelling the most extreme points as `plot.lm()` does.

Changes:

* `eda_plot_actual_vs_predicted()` draws its 45-degree line in red, as wide
  as the fitted lines of the other scatter plots.
* Labels of individual points in `eda_plot_scatter()`,
  `eda_plot_actual_vs_predicted()` and `eda_plot_residuals()` are placed
  with 'ggrepel' so they don't overlap. 'ggrepel' is a new import.

# nglib 1.2.0

New EDA plots:

* `eda_plot_target_correlations()`: sorted bars of the correlation of every
  numerical feature with a target, red for negative and blue for positive.
  Pearson or Spearman, with `minAbsCor` and `nTop` filters. Useful when a
  correlation matrix has too many features to read.
* `eda_plot_box_by_bins()`: box plots of a target by category within
  quantile bins of a numerical confounder, to show a category's effect with
  the confounder held roughly constant.
* `eda_plot_transforms()`: a target against the identity, log, square root
  and cube root of a feature, each panel labelled with the R-squared of a
  linear fit, so the most linear transform stands out.
* `eda_plot_category_dominance()`: the share of the most common level of
  every categorical feature, highlighting near-constant features above a
  threshold and counting rare levels.
* `eda_plot_actual_vs_predicted()`: actual against predicted values with a
  45-degree line, RMSE and R-squared in the subtitle, one panel per model,
  and the worst predictions labelled.
* `eda_plot_association_matrix()` and `association_matrix()`: association
  between features of every type, which `eda_plot_correlation_matrix()`
  limits to numerical ones. Numerical pairs use Pearson or Spearman
  correlation, categorical pairs Cramer's V and mixed pairs the correlation
  ratio, by default bias-corrected so that many-level features do not look
  associated with everything. Features are clustered so redundant ones sit
  together, and ordered factors can be treated as numerical with
  `ordinalAsNumeric = TRUE`.

New helper:

* `flag_outliers()` flags values more than `nIQR` (default 2.5) interquartile
  ranges from the median, a rule that the outliers themselves hardly move.

Changes:

* `eda_plot_heatmap()` greys out cells with fewer than `minCount` rows
  (default 10), leaves them out of the color scale and counts them in the
  caption, so a median of a handful of rows no longer looks as reliable as
  the rest. Each tile shows its row count unless `showCounts = FALSE`. Use
  `minCount = 1` for the previous coloring.
* `eda_plot_scatter()` can highlight and label the points that most
  influence a linear fit: `nLabel` points per panel, picked by leverage or by
  Cook's distance (`labelBy`) above the usual `4 / n` cutoff, labelled with
  `feature_id` or the row number. In Ames, `Gr_Liv_Area` against
  `Sale_Price` picks out the three very large partial sales.

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
