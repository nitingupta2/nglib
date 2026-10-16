#' Histograms of numerical features
#'
#' UNIVARIATE (numerical): histograms, or density curves, of one or more
#' numerical features. Multiple features are drawn as facets with free scales.
#'
#' @param dfInput A data frame.
#' @param vFeaturesNum Character vector of numerical column names.
#' @param nBins Number of histogram bins.
#' @param nCols Number of facet columns; `NULL` lets ggplot2 decide.
#' @param xLog If `TRUE`, use a log10 x axis.
#' @param xLabelsScale Labels for the x axis, e.g. [scales::label_comma()].
#' @param eda_plot_density If `TRUE`, draw density curves instead of
#'   histograms.
#' @param plotTitle Plot title, used when more than one feature is plotted.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_histograms(mtcars, c("mpg", "wt", "disp", "hp"))
#' eda_plot_histograms(ggplot2::diamonds, "price", xLog = TRUE,
#'                     xLabelsScale = scales::label_dollar())
eda_plot_histograms <- function(dfInput, vFeaturesNum, nBins = 30, nCols = NULL,
                                xLog = FALSE, xLabelsScale = waiver(),
                                eda_plot_density = FALSE,
                                plotTitle = "Distributions of numerical features") {

    df <- dfInput %>%
        select(all_of(vFeaturesNum)) %>%
        pivot_longer(everything(), names_to = "feature", values_to = "value") %>%
        group_by(feature) %>%
        drop_na()

    plotTitle <- ifelse(length(vFeaturesNum) == 1,
                        ifelse(xLog, glue::glue("Distribution of {vFeaturesNum} (log scale)"), glue::glue("Distribution of {vFeaturesNum}")),
                        plotTitle)

    if(eda_plot_density) {
        p <- ggplot(data = df, aes(x = value)) +
            geom_density(color = "#F37735") +
            labs(title = plotTitle, x = "")
    } else {
        p <- ggplot(data = df, aes(x = value)) +
            geom_histogram(bins = nBins, color = "white", fill = "#F37735") +
            labs(title = plotTitle, x = "")
    }

    if(length(vFeaturesNum) > 1) {
        p <- p + facet_wrap(~feature, scales = "free", ncol = nCols)
    } else {
        p <- p + labs(x = vFeaturesNum)
    }

    if(xLog) {
        p <- p + scale_x_log10(labels = xLabelsScale)
    }

    return(p)
}

#' Column plot of a categorical feature
#'
#' UNIVARIATE (categorical): counts of each category, ordered by count unless
#' the feature is an ordered factor.
#'
#' @inheritParams eda_plot_histograms
#' @param feature_cat Name of a categorical column.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_column(ggplot2::mpg, "class")
eda_plot_column <- function(dfInput, feature_cat) {

    df <- data.frame(x = dfInput[[feature_cat]]) %>%
        count(x) %>%
        mutate(x = if(is.ordered(x)) {x} else {fct_reorder(as.factor(x), n)})

    p <- ggplot(df, aes(x = x, y = n, fill = x)) +
        geom_col(color = "white") +
        scale_fill_quant(palette = "metro", guide = "none") +
        coord_flip() +
        labs(title = feature_cat, x = "", y = "") +
        theme(plot.title = element_text(size = 14))

    return(p)
}


#' Counts of missing values by feature
#'
#' UNIVARIATE (missing values): column plot of the number of `NA`s in each
#' feature that has any, colored by the feature's class.
#'
#' @inheritParams eda_plot_histograms
#'
#' @return A ggplot object, or invisibly `NULL` with a message if `dfInput`
#'   has no missing values.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_missing_counts(airquality)
eda_plot_missing_counts <- function(dfInput) {
    vColClass <- map_chr(dfInput, function(Z) class(Z)[1])

    dfMissing <- tibble::tibble(feature = names(dfInput),
                                n = nrow(dfInput),
                                na_count = unname(colSums(is.na(dfInput)))) %>%
        mutate(perc_na = na_count*100/n,
               feature_class = vColClass[feature]) %>%
        dplyr::filter(na_count > 0) %>%
        arrange(desc(na_count)) %>%
        mutate(feature = fct_reorder(feature, na_count))

    if(nrow(dfMissing) > 0) {
        p <- ggplot(dfMissing, aes(x = feature, y = na_count)) +
            geom_col(aes(fill = feature_class)) +
            geom_label(aes(label = na_count), hjust = -0.1, size = 2.8) +
            expand_limits(y = c(0, 1.2*max(dfMissing$na_count))) +
            scale_fill_quant(palette = "metro", name = "Type") +
            coord_flip() +
            labs(title = "Missing values",
                 x = "",
                 y = "count of NAs")

        return(p)
    } else {
        message("No missing values in dataset")
        invisible(NULL)
    }
}


#' Map of missing values and column types
#'
#' UNIVARIATE (missing values): a [visdat::vis_dat()] map of every cell,
#' colored by column class with missing values highlighted, styled with
#' [theme_quant()] in the font of the current session theme.
#'
#' @inheritParams eda_plot_histograms
#' @param plotTitle Plot title. Defaults to `"Missing values"`.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_missing_values(airquality)
eda_plot_missing_values <- function(dfInput, plotTitle = NULL) {
    if(is.null(plotTitle)) plotTitle <- "Missing values"

    p <- visdat::vis_dat(dfInput) +
        theme_quant(font_family = .session_font_family()) +
        theme(panel.border = element_rect(color = "white")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
        labs(title = plotTitle)
    return(p)
}


#' Normal Q-Q plot
#'
#' UNIVARIATE (numerical): normal quantile-quantile plot of a sample, e.g.
#' model residuals.
#'
#' @inheritParams eda_plot_histograms
#' @param feature_sample Name of a numerical column.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' df <- data.frame(resid = residuals(lm(mpg ~ wt, data = mtcars)))
#' eda_plot_normal_qq(df, "resid")
eda_plot_normal_qq <- function(dfInput, feature_sample) {

    df <- data.frame(x = dfInput[[feature_sample]])

    p <- ggplot(df, aes(sample = x)) +
        stat_qq() +
        stat_qq_line() +
        labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
        theme(plot.title = element_text(size = 14))

    return(p)
}


#' Scatter plots of numerical features against a target
#'
#' BIVARIATE (numerical-numerical): scatter plots of `feature_y` against each
#' of `vFeaturesNum`, faceted when there is more than one, with an optional
#' linear fit.
#'
#' @inheritParams eda_plot_histograms
#' @param feature_y Name of the numerical column for the y axis.
#' @param alpha Point transparency.
#' @param add_lm If `TRUE`, add a linear regression line.
#' @param xLimits Limits of the x axis, or `NULL`.
#' @param yLimits Limits of the y axis, or `NULL`.
#' @param yLog If `TRUE`, use a log10 y axis.
#' @param yLabelsScale Labels for the y axis.
#'
#' @details `xLog` only applies when a single feature is plotted.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_scatter(mtcars, c("wt", "disp", "hp"), feature_y = "mpg")
#' eda_plot_scatter(ggplot2::diamonds, "carat", "price", alpha = 0.05,
#'                  xLog = TRUE, yLog = TRUE)
eda_plot_scatter <- function(dfInput, vFeaturesNum, feature_y, alpha = 0.3, add_lm = TRUE,
                             xLog = FALSE, xLimits = NULL, xLabelsScale = waiver(),
                             yLog = FALSE, yLimits = NULL, yLabelsScale = waiver()) {

    vFeaturesUnique <- unique(c(vFeaturesNum, feature_y))

    df <- dfInput %>%
        select(all_of(vFeaturesUnique)) %>%
        pivot_longer(-all_of(feature_y), names_to = "feature", values_to = "x") %>%
        select(all_of(feature_y), "feature", "x") %>%
        set_names(c("y", "feature", "x"))

    p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(color = "#00AEDB", alpha = alpha) +
        labs(x = "")

    if(add_lm) {
        p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#D11141")
    }

    if(xLog & length(vFeaturesNum) == 1) {
        p <- p +
            scale_x_log10(name = glue::glue("{vFeaturesNum} (log scale)"), limits = xLimits, labels = xLabelsScale)
    }

    if(yLog) {
        p <- p +
            scale_y_log10(name = glue::glue("{feature_y} (log scale)"), limits = yLimits, labels = yLabelsScale) +
            labs(title = glue::glue("Scatterplots of {feature_y} w.r.t numerical features"))
    } else {
        p <- p +
            scale_y_continuous(name = feature_y, limits = yLimits, labels = yLabelsScale) +
            labs(title = glue::glue("Scatterplots of {feature_y} w.r.t numerical features"))
    }

    if(length(vFeaturesNum) > 1) {
        p <- p + facet_wrap(~feature, scales = "free_x")
    } else {
        p <- p + labs(x = vFeaturesNum, title = glue::glue("Scatterplot of {feature_y} v/s {vFeaturesNum}"))
    }
    return(p)
}


#' Box plots of a numerical feature by category
#'
#' BIVARIATE (categorical-numerical): box plots of `feature_num` for each
#' category of `feature_cat`, ordered by median unless the feature is an
#' ordered factor.
#'
#' @inheritParams eda_plot_scatter
#' @param feature_cat Name of a categorical column.
#' @param feature_num Name of a numerical column.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_box(ggplot2::mpg, "class", "hwy")
eda_plot_box <- function(dfInput, feature_cat, feature_num, yLog = FALSE, yLimits = NULL, yLabelsScale = waiver()) {

    df <- data.frame(x = dfInput[[feature_cat]], y = dfInput[[feature_num]]) %>%
        group_by(x) %>%
        mutate(median_x = median(y, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(x = if(is.ordered(x)) {x} else {fct_reorder(as.factor(x), median_x)})

    p <- ggplot(df, aes(x = x, y = y, fill = x)) +
        geom_boxplot() +
        scale_fill_quant(palette = "metro", name = feature_cat, guide = "none") +
        coord_flip(ylim = yLimits) +
        labs(title = feature_cat, x = "")

    if(yLog) {
        p <- p + scale_y_log10(name = glue::glue("{feature_num} (log scale)"), labels = yLabelsScale)
    } else {
        p <- p + scale_y_continuous(name = feature_num, labels = yLabelsScale)
    }
    return(p)
}


#' Bar plot of the median of a numerical feature by category
#'
#' BIVARIATE (categorical-numerical): median of `feature_num` for each
#' category of `feature_cat`.
#'
#' @inheritParams eda_plot_box
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_bar(ggplot2::mpg, "class", "hwy")
eda_plot_bar <- function(dfInput, feature_cat, feature_num, yLabelsScale = waiver()) {

    df <- data.frame(x = dfInput[[feature_cat]], y = dfInput[[feature_num]]) %>%
        group_by(x) %>%
        summarise(median_x = median(y, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(x = if(is.ordered(x)) {x} else {fct_reorder(as.factor(x), median_x)})

    p <- ggplot(df, aes(x = x, y = median_x, fill = x)) +
        geom_col() +
        scale_fill_quant(palette = "metro", name = feature_cat, guide = "none") +
        scale_y_continuous(labels = yLabelsScale) +
        coord_flip() +
        labs(title = feature_cat, x = "", y = glue::glue("Median {feature_num}"))
    return(p)
}


#' Ridgeline densities of a numerical feature by category
#'
#' BIVARIATE (categorical-numerical): density of `feature_num` for each
#' category of `feature_cat`, drawn as ridgelines with
#' [ggridges::geom_density_ridges()].
#'
#' @inheritParams eda_plot_box
#' @inheritParams eda_plot_scatter
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_density(ggplot2::diamonds, "cut", "price", xLog = TRUE)
eda_plot_density <- function(dfInput, feature_cat, feature_num, alpha = 0.9, xLog = FALSE, xLimits = NULL, xLabelsScale = waiver()) {

    df <- data.frame(x = dfInput[[feature_num]], y = dfInput[[feature_cat]]) %>%
        group_by(y) %>%
        mutate(median_y = median(x, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(y = if(is.ordered(y)) {y} else {fct_reorder(as.factor(y), median_y)})

    p <- ggplot(df) +
        geom_density_ridges(aes(x = x, y = y, fill = y), color = "lightgrey", alpha = alpha, scale = 4) +
        scale_fill_quant(palette = "metro", name = feature_cat, guide = "none") +
        labs(title = feature_cat, x = feature_num, y = "")

    if(xLog) {
        p <- p + scale_x_log10(name = glue::glue("{feature_num} (log scale)"), limits = xLimits, labels = xLabelsScale)
    } else {
        p <- p + scale_x_continuous(name = feature_num, limits = xLimits, labels = xLabelsScale)
    }
    return(p)
}


#' Stacked proportions of one categorical feature by another
#'
#' BIVARIATE (categorical-categorical): for each category of `feature_cat1`,
#' the proportion of each category of `feature_cat2`.
#'
#' @inheritParams eda_plot_histograms
#' @param feature_cat1 Name of the categorical column on the axis.
#' @param feature_cat2 Name of the categorical column used for fill.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_stacked_bar(ggplot2::mpg, "class", "drv")
eda_plot_stacked_bar <- function(dfInput, feature_cat1, feature_cat2) {
    df <- data.frame(x = dfInput[[feature_cat1]], y = dfInput[[feature_cat2]]) %>%
        mutate(y = as.factor(y))

    p <- ggplot(df) +
        geom_bar(aes(x = x, y = after_stat(prop), fill = y, group = y), position = "fill") +
        coord_flip() +
        scale_fill_quant(palette = "metro", name = feature_cat2, discrete = TRUE) +
        labs(title = feature_cat1, x = "")

    return(p)
}


#' Scatter plot of two numerical features colored by a category
#'
#' MULTIVARIATE (numerical-numerical-categorical): scatter plot of
#' `feature_y` against `feature_x` with points colored by `feature_cat`.
#' Categories are ordered by their median `feature_y`; a two-level category
#' uses the blue/red `heatmap` palette.
#'
#' @inheritParams eda_plot_scatter
#' @param feature_x Name of the numerical column for the x axis.
#' @param feature_cat Name of the categorical column used for color.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_scatter_cat(ggplot2::mpg, "displ", "hwy", "drv")
eda_plot_scatter_cat <- function(dfInput, feature_x, feature_y, feature_cat, alpha = 0.3,
                                 xLog = FALSE, xLimits = NULL, xLabelsScale = waiver(),
                                 yLog = FALSE, yLimits = NULL, yLabelsScale = waiver()) {

    df <- data.frame(x = dfInput[[feature_x]], y = dfInput[[feature_y]], z = dfInput[[feature_cat]]) %>%
        group_by(z) %>%
        mutate(median_y = median(y, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(z = if(is.ordered(z)) {z} else {fct_reorder(as.factor(z), median_y)})

    plotPalette <- "metro"
    if(length(levels(df$z)) == 2) {
        plotPalette <- "heatmap"
    }

    p <- ggplot(df, aes(x = x, y = y, color = z)) +
        geom_point(alpha = alpha, size = 2) +
        scale_color_quant(name = feature_cat, palette = plotPalette, reverse = TRUE) +
        labs(title = paste("Scatterplot of", feature_y, "v/s", feature_x, "w.r.t.", feature_cat),
             x = feature_x,
             y = feature_y) +
        guides(color = guide_legend(reverse = TRUE))

    if(xLog) {
        p <- p + scale_x_log10(name = glue::glue("{feature_x} (log scale)"), limits = xLimits, labels = xLabelsScale)
    } else {
        p <- p + scale_x_continuous(name = feature_x, limits = xLimits, labels = xLabelsScale)
    }

    if(yLog) {
        p <- p + scale_y_log10(name = glue::glue("{feature_y} (log scale)"), limits = yLimits, labels = yLabelsScale)
    } else {
        p <- p + scale_y_continuous(name = feature_y, limits = yLimits, labels = yLabelsScale)
    }

    return(p)
}


#' Faceted scatter plots of two numerical features by category
#'
#' MULTIVARIATE (numerical-numerical-categorical): one scatter panel per
#' category of `feature_cat`, optionally over a grey background of all
#' points so each category can be compared with the whole.
#'
#' @inheritParams eda_plot_scatter_cat
#' @param overlayAllPoints If `TRUE`, draw all points in light grey behind
#'   each panel.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_scatter_cat_facets(ggplot2::mpg, "displ", "hwy", "drv")
eda_plot_scatter_cat_facets <- function(dfInput, feature_x, feature_y, feature_cat, alpha = 0.3,
                                        xLog = FALSE, xLimits = NULL, xLabelsScale = waiver(),
                                        yLog = FALSE, yLimits = NULL, yLabelsScale = waiver(),
                                        overlayAllPoints = TRUE) {

    df <- data.frame(x = dfInput[[feature_x]], y = dfInput[[feature_y]], z = dfInput[[feature_cat]]) %>%
        group_by(z) %>%
        mutate(median_z = median(y, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(z = if(is.ordered(z)) {z} else {fct_reorder(as.factor(z), median_z)})

    p <- ggplot(df, aes(x = x, y = y, color = z))

    if(overlayAllPoints) {
        df_bg <- data.frame(x = dfInput[[feature_x]], y = dfInput[[feature_y]])
        p <- p + geom_point(data = df_bg, aes(x = x, y = y), color = "lightgrey", inherit.aes = FALSE, show.legend = FALSE)
    }

    p <- p + geom_point(alpha = alpha, size = 2) +
        facet_wrap(~z) +
        scale_color_quant(palette = "metro", name = feature_cat) +
        labs(title = paste("Scatterplot of", feature_y, "v/s", feature_x, "facetted by", feature_cat),
             x = feature_x,
             y = feature_y)

    if(xLog) {
        p <- p + scale_x_log10(name = glue::glue("{feature_x} (log scale)"), limits = xLimits, labels = xLabelsScale)
    } else {
        p <- p + scale_x_continuous(name = feature_x, limits = xLimits, labels = xLabelsScale)
    }

    if(yLog) {
        p <- p + scale_y_log10(name = glue::glue("{feature_y} (log scale)"), limits = yLimits, labels = yLabelsScale)
    } else {
        p <- p + scale_y_continuous(name = feature_y, limits = yLimits, labels = yLabelsScale)
    }

    return(p)
}


#' Heat map of a numerical median across two categorical features
#'
#' MULTIVARIATE (categorical-categorical-numerical): tiles of the median of
#' `feature_num` for each combination of `feature_cat1` and `feature_cat2`.
#' Both axes are ordered by median unless they are ordered factors.
#'
#' @inheritParams eda_plot_stacked_bar
#' @param feature_cat1 Name of the categorical column for the x axis.
#' @param feature_cat2 Name of the categorical column for the y axis.
#' @param feature_num Name of the numerical column to summarise.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_heatmap(ggplot2::diamonds, "cut", "color", "price")
eda_plot_heatmap <- function(dfInput, feature_cat1, feature_cat2, feature_num) {
    df <- data.frame(x = dfInput[[feature_cat1]], y = dfInput[[feature_cat2]], z = dfInput[[feature_num]]) %>%
        drop_na(z) %>%
        group_by(x) %>%
        mutate(median_x = median(z, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(x = if(is.ordered(x)) {x} else {fct_reorder(as.factor(x), median_x)}) %>%
        group_by(y) %>%
        mutate(median_y = median(z, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(y = if(is.ordered(y)) {y} else {fct_reorder(as.factor(y), median_y)}) %>%
        group_by(x, y) %>%
        summarise(z = median(z, na.rm = TRUE), .groups = "drop")

    p <- ggplot(df) +
        geom_tile(aes(x = x, y = y, fill = z), color = "white") +
        scale_fill_quant(palette = "heatmap", discrete = FALSE, name = paste("Median", feature_num)) +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = glue::glue("Heatmap of median {feature_num} w.r.t. {feature_cat1} and {feature_cat2}"),
             x = feature_cat1,
             y = feature_cat2)
    return(p)
}


#' Correlation matrix of numerical features
#'
#' MULTIVARIATE (numerical): clustered correlation heat map of the numerical
#' columns of `dfInput`, styled with [theme_quant()] in the font of the
#' current session theme. Non-numerical columns are ignored. See
#' [plot_correlations_gg()].
#'
#' @inheritParams eda_plot_histograms
#' @param outlineVar Optional name of a feature whose row and column are
#'   outlined.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_correlation_matrix(mtcars, outlineVar = "mpg")
eda_plot_correlation_matrix <- function(dfInput, outlineVar = NA, plotTitle = "Correlation of numerical features") {
    p <- plot_correlations_gg(dfInput, outlineVar = outlineVar, plotTitle = plotTitle) +
        theme_quant(font_family = .session_font_family()) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

    return(p)
}
