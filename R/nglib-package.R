#' nglib: Plotting Themes, Color Scales and EDA Plots
#'
#' ggplot2 themes, color scales built on named palettes, quick exploratory
#' data analysis (EDA) plots, and a few small data-wrangling helpers.
#'
#' @section Themes:
#' [theme_quant()], [theme_poster()], [set_theme_quant()],
#' [set_theme_poster()], [update_font_defaults()].
#'
#' @section Scales and palettes:
#' [scale_color_quant()], [scale_fill_quant()], [scale_color_quant_manual()],
#' [scale_fill_quant_manual()], [get_palettes()], [quant_pal()],
#' [plot_colors()], [test_pal()], [convert_mm_to_pts()].
#'
#' @section EDA plots:
#' * Univariate: [eda_plot_histograms()], [eda_plot_column()],
#'   [eda_plot_missing_counts()], [eda_plot_missing_values()],
#'   [eda_plot_normal_qq()], [eda_plot_category_dominance()].
#' * Bivariate: [eda_plot_scatter()], [eda_plot_box()], [eda_plot_bar()],
#'   [eda_plot_density()], [eda_plot_stacked_bar()], [eda_plot_transforms()].
#' * Multivariate: [eda_plot_scatter_cat()], [eda_plot_scatter_cat_facets()],
#'   [eda_plot_heatmap()], [eda_plot_correlation_matrix()],
#'   [plot_correlations_gg()], [eda_plot_association_matrix()],
#'   [eda_plot_target_correlations()], [eda_plot_box_by_bins()].
#' * Model fit: [eda_plot_predicted_vs_actual()], [eda_plot_residuals()].
#'
#' @section Data helpers:
#' [find_mode()], [find_most_common_value()], [`%notin%`], [change_na_to()],
#' [change_na_to_most_common_value()], [median_by_grouping()],
#' [most_common_value_by_grouping()], [significant_digits()],
#' [pct_diff_lagging()], [pct_diff_leading()], [nearest_multiple()],
#' [correlation_reorder_matrix()], [association_matrix()], [flag_outliers()].
#'
#' @keywords internal
#' @import ggplot2
#' @importFrom dplyr across all_of arrange count desc everything filter group_by
#'   mutate select summarise ungroup
#' @importFrom forcats fct_inorder fct_reorder fct_rev
#' @importFrom ggridges geom_density_ridges
#' @importFrom glue glue
#' @importFrom grDevices col2rgb colorRampPalette rgb
#' @importFrom graphics pie
#' @importFrom magrittr %>% set_names
#' @importFrom purrr map_chr
#' @importFrom rlang %||%
#' @importFrom scales label_comma
#' @importFrom stats as.dist cor hclust median
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom tidyverse tidyverse_conflicts
#' @importFrom utils globalVariables
"_PACKAGE"

globalVariables(c(
    "AssocVal", "CorVal", "Var1", "Var2", "abs_error", "actual", "bin",
    "correlation", "dominant_level", "dominated", "feature", "feature_class",
    "id", "influence", "intercept", "label", "level", "panel", "rank", "side",
    "slope",
    "median_x", "median_y", "median_z", "model", "n", "n_levels", "n_rare",
    "na_count", "predicted", "prop", "r_squared", "share", "sparse", "v",
    "value", "x", "y", "z", "z_fill", "z_max", "z_min"
))
