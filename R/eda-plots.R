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
#' linear fit. Optionally, the points that most influence a linear fit, such
#' as a few very large houses sold at a low price, are highlighted and
#' labelled.
#'
#' @inheritParams eda_plot_histograms
#' @param feature_y Name of the numerical column for the y axis.
#' @param alpha Point transparency.
#' @param add_lm If `TRUE`, add a linear regression line.
#' @param xLimits Limits of the x axis, or `NULL`.
#' @param yLimits Limits of the y axis, or `NULL`.
#' @param yLog If `TRUE`, use a log10 y axis.
#' @param yLabelsScale Labels for the y axis.
#' @param nLabel Maximum number of influential points to label in each panel;
#'   `0` labels none.
#' @param feature_id Optional name of a column used to label points. If
#'   `NULL`, points are labelled with their row number.
#' @param labelBy How influential points are picked: `"leverage"`, the hat
#'   value of a point in the linear fit of `feature_y` on the feature, which
#'   is high when its x is far from the rest; or `"cooks"`, Cook's distance,
#'   which is high only when such a point also has a large residual, i.e. it
#'   pulls the fit.
#'
#' @details `xLog` only applies when a single feature is plotted.
#'
#' Influence is computed from `lm(y ~ x)` in each panel, on log10 scales
#' where the axes are logged. Only points beyond the usual cutoff of `4 / n`
#' (twice the mean leverage for a simple regression, or the usual Cook's
#' distance cutoff) are labelled, at most `nLabel` per panel, the most
#' influential first.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_scatter(mtcars, c("wt", "disp", "hp"), feature_y = "mpg")
#' eda_plot_scatter(ggplot2::diamonds, "carat", "price", alpha = 0.05,
#'                  xLog = TRUE, yLog = TRUE)
#'
#' df <- tibble::rownames_to_column(mtcars, "car")
#' eda_plot_scatter(df, "hp", "mpg", nLabel = 3, feature_id = "car")
#' eda_plot_scatter(df, c("hp", "wt"), "mpg", nLabel = 2, feature_id = "car",
#'                  labelBy = "cooks")
eda_plot_scatter <- function(dfInput, vFeaturesNum, feature_y, alpha = 0.3, add_lm = TRUE,
                             xLog = FALSE, xLimits = NULL, xLabelsScale = waiver(),
                             yLog = FALSE, yLimits = NULL, yLabelsScale = waiver(),
                             nLabel = 0, feature_id = NULL, labelBy = c("leverage", "cooks")) {
    labelBy <- match.arg(labelBy)

    vFeaturesUnique <- unique(c(vFeaturesNum, feature_y))

    vId <- if(is.null(feature_id)) {seq_len(nrow(dfInput))} else {dfInput[[feature_id]]}

    df <- dfInput %>%
        select(all_of(vFeaturesUnique)) %>%
        mutate(.id = vId) %>%
        pivot_longer(-all_of(c(feature_y, ".id")), names_to = "feature", values_to = "x") %>%
        select(all_of(feature_y), "feature", "x", ".id") %>%
        set_names(c("y", "feature", "x", "id"))

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

    if(nLabel > 0) {
        xLogFit <- xLog & length(vFeaturesNum) == 1
        dfLabel <- df %>%
            dplyr::filter(!is.na(x), !is.na(y), !xLogFit | x > 0, !yLog | y > 0) %>%
            group_by(feature) %>%
            mutate(influence = .influence(if(xLogFit) {log10(x)} else {x},
                                          if(yLog) {log10(y)} else {y},
                                          labelBy)) %>%
            dplyr::filter(influence > 4 / dplyr::n()) %>%
            dplyr::slice_max(influence, n = nLabel, with_ties = FALSE) %>%
            ungroup()

        if(nrow(dfLabel) > 0) {
            p <- p +
                geom_point(data = dfLabel, color = "#D11141") +
                ggrepel::geom_label_repel(data = dfLabel, aes(label = id), size = 2.8,
                                          max.overlaps = Inf, seed = 1)
        }
    }
    return(p)
}

# Leverage (hat values) or Cook's distance of each point in lm(y ~ x).
.influence <- function(x, y, labelBy) {
    if(length(x) < 3) return(rep(0, length(x)))
    fit <- stats::lm(y ~ x)
    vInfluence <- if(labelBy == "leverage") {stats::hatvalues(fit)} else {stats::cooks.distance(fit)}
    vInfluence[is.na(vInfluence)] <- 0
    return(unname(vInfluence))
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
#' A median of a few rows can look as meaningful as one of hundreds, so cells
#' with fewer than `minCount` rows are drawn in grey and left out of the color
#' scale, and the caption says how many there are. Each tile can also show its
#' row count. Combinations with no rows are left blank.
#'
#' @inheritParams eda_plot_stacked_bar
#' @param feature_cat1 Name of the categorical column for the x axis.
#' @param feature_cat2 Name of the categorical column for the y axis.
#' @param feature_num Name of the numerical column to summarise.
#' @param minCount Cells with fewer rows than this are greyed out. Use `1` to
#'   color every cell.
#' @param showCounts If `TRUE`, label each tile with its number of rows.
#'
#' @return A ggplot object. Its data has one row per cell with the median `z`
#'   and the row count `n`.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_heatmap(ggplot2::diamonds, "cut", "color", "price")
#' eda_plot_heatmap(ggplot2::mpg, "class", "manufacturer", "hwy", minCount = 5)
eda_plot_heatmap <- function(dfInput, feature_cat1, feature_cat2, feature_num,
                             minCount = 10, showCounts = TRUE) {
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
        summarise(z = median(z, na.rm = TRUE), n = dplyr::n(), .groups = "drop") %>%
        mutate(sparse = n < minCount,
               z_fill = ifelse(sparse, NA, z))

    nSparse <- sum(df$sparse)

    p <- ggplot(df) +
        geom_tile(aes(x = x, y = y, fill = z_fill), color = "white") +
        scale_fill_quant(palette = "heatmap", discrete = FALSE, na.value = "grey85",
                         name = paste("Median", feature_num)) +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = glue::glue("Heatmap of median {feature_num} w.r.t. {feature_cat1} and {feature_cat2}"),
             x = feature_cat1,
             y = feature_cat2,
             caption = if(nSparse > 0) {glue::glue("{nSparse} of {nrow(df)} cells have fewer than {minCount} rows (grey)")} else {NULL})

    if(showCounts) {
        p <- p + geom_text(aes(x = x, y = y, label = n), size = 2.5,
                           color = ifelse(df$sparse, "grey40", "white"))
    }
    return(p)
}


#' Correlation matrix of numerical features
#'
#' MULTIVARIATE (numerical): clustered correlation heat map of the numerical
#' columns of `dfInput`, styled with [theme_quant()] in the font of the
#' current session theme. Non-numerical columns are ignored. See
#' [plot_correlations_gg()], and [eda_plot_association_matrix()] to include
#' categorical features.
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


#' Association matrix of features of any type
#'
#' MULTIVARIATE (all types): clustered heat map of [association_matrix()],
#' which extends [eda_plot_correlation_matrix()] to categorical features:
#' correlation between numerical features, Cramer's V between categorical
#' ones and the correlation ratio between the two kinds. Features are ordered
#' by hierarchical clustering on `1 - |association|`, so redundant features
#' end up next to each other whatever their type. Correlations range from -1
#' (red) to 1 (blue); the other measures from 0 to 1. Undefined associations
#' and the diagonal are grey. The plot is styled with [theme_quant()] in the
#' font of the current session theme.
#'
#' @inheritParams association_matrix
#' @param outlineVar Optional name of a feature whose row and column are
#'   outlined, e.g. the target.
#' @param showValues If `TRUE`, print each association on its tile. `NULL`
#'   prints them when there are at most 25 features.
#' @param plotTitle Plot title.
#'
#' @return A ggplot object. Its data has one row per pair of features, with
#'   the association `AssocVal` and the `measure` used.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_association_matrix(ggplot2::mpg, outlineVar = "hwy")
#' eda_plot_association_matrix(ggplot2::diamonds[1:5000, ], outlineVar = "price")
eda_plot_association_matrix <- function(dfInput, method = c("pearson", "spearman"),
                                        ordinalAsNumeric = FALSE, biasCorrect = TRUE,
                                        outlineVar = NA, showValues = NULL,
                                        plotTitle = "Association of features") {
    method <- match.arg(method)

    mAssoc <- association_matrix(dfInput, method = method, ordinalAsNumeric = ordinalAsNumeric,
                                 biasCorrect = biasCorrect)
    mMeasure <- attr(mAssoc, "measure")

    mDist <- 1 - abs(mAssoc)
    mDist[is.na(mDist)] <- 1
    vFeatures <- rownames(mAssoc)[stats::hclust(as.dist(mDist))$order]
    nFeatures <- length(vFeatures)

    if(is.null(showValues)) showValues <- nFeatures <= 25

    dfAssoc <- expand.grid(Var1 = vFeatures, Var2 = vFeatures, stringsAsFactors = FALSE) %>%
        mutate(AssocVal = ifelse(Var1 == Var2, NA, mAssoc[cbind(Var1, Var2)]),
               measure = mMeasure[cbind(Var1, Var2)],
               Var1 = factor(Var1, levels = vFeatures),
               Var2 = factor(Var2, levels = rev(vFeatures)))

    vMeasures <- unique(dfAssoc$measure[!is.na(dfAssoc$AssocVal)])
    vSubtitle <- c(pearson = "Numerical: Pearson r",
                   spearman = "Numerical: Spearman rho",
                   cramers_v = "Categorical: Cram\u00e9r's V",
                   eta = "Mixed: correlation ratio")
    plotSubtitle <- paste(vSubtitle[intersect(names(vSubtitle), vMeasures)], collapse = "; ")
    if(biasCorrect && any(c("cramers_v", "eta") %in% vMeasures)) {
        plotSubtitle <- paste0(plotSubtitle, "\n(V and correlation ratio bias-corrected)")
    }

    p <- ggplot(dfAssoc, aes(x = Var1, y = Var2, fill = AssocVal)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(name = NULL, low = "#D11141", mid = "#F8F5F5", high = "#00AEDB",
                             midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
        coord_fixed() +
        labs(title = plotTitle, subtitle = plotSubtitle, x = "", y = "") +
        theme_quant(font_family = .session_font_family()) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

    if(showValues) {
        p <- p + geom_text(aes(label = ifelse(is.na(AssocVal), "", sprintf("%.2f", AssocVal))),
                           color = "black", size = 2.5)
    }

    if(!is.na(outlineVar) && outlineVar %in% vFeatures) {
        xOutline <- match(outlineVar, vFeatures)
        yOutline <- match(outlineVar, rev(vFeatures))
        p <- p +
            annotate("rect", xmin = xOutline - 0.5, xmax = xOutline + 0.5, ymin = 0.5, ymax = nFeatures + 0.5,
                     fill = NA, color = "black", linewidth = 0.75) +
            annotate("rect", xmin = 0.5, xmax = nFeatures + 0.5, ymin = yOutline - 0.5, ymax = yOutline + 0.5,
                     fill = NA, color = "black", linewidth = 0.75)
    }

    return(p)
}


#' Correlations of numerical features with a target
#'
#' MULTIVARIATE (numerical): horizontal bars of the correlation of every
#' other numerical column with `feature_y`, sorted by value and labelled.
#' Negative correlations are red and positive ones blue, as in the `heatmap`
#' palette. Use this when a full correlation matrix has too many features to
#' read. Correlations use pairwise-complete observations; features whose
#' correlation is undefined (e.g. constant columns) are dropped.
#'
#' @inheritParams eda_plot_scatter
#' @param method Correlation method, `"pearson"` or `"spearman"`.
#' @param minAbsCor Only show features whose absolute correlation is at
#'   least this.
#' @param nTop If not `NULL`, only show the `nTop` features with the largest
#'   absolute correlation.
#' @param plotTitle Plot title. If `NULL`, a title naming the method and
#'   `feature_y` is used.
#'
#' @return A ggplot object, or invisibly `NULL` with a message if no feature
#'   is left to plot.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_target_correlations(mtcars, "mpg")
#' eda_plot_target_correlations(ggplot2::diamonds, "price", method = "spearman")
eda_plot_target_correlations <- function(dfInput, feature_y, method = c("pearson", "spearman"),
                                         minAbsCor = 0, nTop = NULL, plotTitle = NULL) {
    method <- match.arg(method)

    if(!is.numeric(dfInput[[feature_y]])) {
        stop(glue::glue("`{feature_y}` must be a numerical column"), call. = FALSE)
    }

    dfNum <- as.data.frame(dfInput)[vapply(dfInput, is.numeric, logical(1))]
    vFeatures <- setdiff(names(dfNum), feature_y)

    if(length(vFeatures) == 0) {
        stop("`dfInput` has no other numerical columns", call. = FALSE)
    }

    vCor <- cor(dfNum[vFeatures], dfNum[[feature_y]], use = "pairwise.complete.obs", method = method)[, 1]

    df <- tibble::tibble(feature = vFeatures, correlation = unname(vCor)) %>%
        dplyr::filter(!is.na(correlation), abs(correlation) >= minAbsCor) %>%
        arrange(desc(abs(correlation)))

    if(!is.null(nTop)) {
        df <- dplyr::slice_head(df, n = nTop)
    }

    if(nrow(df) == 0) {
        message(glue::glue("No features with an absolute correlation of at least {minAbsCor}"))
        return(invisible(NULL))
    }

    df <- df %>%
        mutate(sign = factor(ifelse(correlation < 0, "negative", "positive"), levels = c("positive", "negative")),
               feature = fct_reorder(feature, correlation))

    if(is.null(plotTitle)) {
        plotTitle <- glue::glue("{stringr::str_to_title(method)} correlation with {feature_y}")
    }

    p <- ggplot(df, aes(x = feature, y = correlation, fill = sign)) +
        geom_col() +
        geom_hline(yintercept = 0, color = "darkgrey") +
        geom_text(aes(label = sprintf("%.2f", correlation), hjust = ifelse(correlation < 0, 1.15, -0.15)), size = 3) +
        scale_fill_quant(palette = "heatmap", drop = FALSE, guide = "none") +
        scale_y_continuous(expand = expansion(mult = 0.15)) +
        coord_flip() +
        labs(title = plotTitle, x = "", y = "correlation")

    return(p)
}


#' Box plots by category within quantile bins of a confounder
#'
#' MULTIVARIATE (categorical-numerical-numerical): box plots of `feature_y`
#' for each category of `feature_cat`, faceted by quantile bins of the
#' numerical `feature_bin`. Holding `feature_bin` roughly constant shows the
#' effect of the category that a plain box plot can hide: e.g. diamonds of a
#' better clarity look cheaper overall only because they tend to be smaller.
#' Categories are ordered by median `feature_y` unless they are an ordered
#' factor. Each facet is labelled with the range of `feature_bin` in it.
#'
#' @inheritParams eda_plot_box
#' @param feature_y Name of the numerical column to compare across
#'   categories.
#' @param feature_bin Name of the numerical column to bin.
#' @param nBins Number of quantile bins. Fewer bins are used when quantiles
#'   are tied.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_box_by_bins(ggplot2::diamonds, "clarity", "price", "carat", yLog = TRUE)
#' eda_plot_box_by_bins(ggplot2::mpg, "drv", "hwy", "displ", nBins = 3)
eda_plot_box_by_bins <- function(dfInput, feature_cat, feature_y, feature_bin, nBins = 4,
                                 yLog = FALSE, yLabelsScale = waiver()) {

    if(!is.numeric(dfInput[[feature_bin]])) {
        stop(glue::glue("`{feature_bin}` must be a numerical column"), call. = FALSE)
    }

    df <- data.frame(x = dfInput[[feature_cat]], y = dfInput[[feature_y]], z = dfInput[[feature_bin]]) %>%
        drop_na(y, z)

    vBreaks <- unique(stats::quantile(df$z, probs = seq(0, 1, length.out = nBins + 1), names = FALSE))
    df$bin <- cut(df$z, breaks = vBreaks, include.lowest = TRUE, labels = FALSE)

    dfBins <- df %>%
        group_by(bin) %>%
        summarise(z_min = min(z), z_max = max(z), .groups = "drop") %>%
        mutate(label = paste0(feature_bin, ": ",
                              prettyNum(signif(z_min, 3), big.mark = ","), "\u2013",
                              prettyNum(signif(z_max, 3), big.mark = ",")),
               label = make.unique(label, sep = " "))

    df <- df %>%
        mutate(bin = factor(bin, levels = dfBins$bin, labels = dfBins$label)) %>%
        group_by(x) %>%
        mutate(median_x = median(y, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(x = if(is.ordered(x)) {x} else {fct_reorder(as.factor(x), median_x)})

    p <- ggplot(df, aes(x = x, y = y, fill = x)) +
        geom_boxplot() +
        facet_wrap(~bin) +
        scale_fill_quant(palette = "metro", name = feature_cat, guide = "none") +
        coord_flip() +
        labs(title = glue::glue("{feature_y} by {feature_cat} within quantile bins of {feature_bin}"), x = "")

    if(yLog) {
        p <- p + scale_y_log10(name = glue::glue("{feature_y} (log scale)"), labels = yLabelsScale)
    } else {
        p <- p + scale_y_continuous(name = feature_y, labels = yLabelsScale)
    }
    return(p)
}


#' Scatter plots of a target against transforms of a feature
#'
#' BIVARIATE (numerical-numerical): one panel per transform of `feature_x`,
#' scattered against `feature_y` with a linear fit. Each panel is labelled
#' with the \eqn{R^2} of a linear regression of `feature_y` (log10 when
#' `yLog = TRUE`) on the transformed `feature_x`, so the transform that makes
#' the relationship most linear stands out.
#'
#' Non-positive `feature_x` values are dropped for `"log"`, negative ones for
#' `"sqrt"`, and non-positive `feature_y` values when `yLog = TRUE`. The
#' caption says how many were dropped.
#'
#' @inheritParams eda_plot_scatter_cat
#' @param transforms Transforms of `feature_x` to compare: any of
#'   `"identity"`, `"log"` (natural log), `"sqrt"` and `"cuberoot"`.
#'
#' @return A ggplot object. Its data has an `r_squared` column with the
#'   \eqn{R^2} of each transform.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_transforms(ggplot2::diamonds, "carat", "price")
#' eda_plot_transforms(mtcars, "hp", "mpg", transforms = c("identity", "log"),
#'                     yLog = FALSE, alpha = 0.6)
eda_plot_transforms <- function(dfInput, feature_x, feature_y,
                                transforms = c("identity", "log", "sqrt", "cuberoot"),
                                yLog = TRUE, alpha = 0.1) {
    transforms <- match.arg(transforms, several.ok = TRUE)

    if(!is.numeric(dfInput[[feature_x]])) {
        stop(glue::glue("`{feature_x}` must be a numerical column"), call. = FALSE)
    }

    lTransforms <- list(identity = function(x) x,
                        log = log,
                        sqrt = sqrt,
                        cuberoot = function(x) sign(x) * abs(x)^(1/3))
    vLabels <- c(identity = feature_x,
                 log = paste0("log(", feature_x, ")"),
                 sqrt = paste0("sqrt(", feature_x, ")"),
                 cuberoot = paste0("cuberoot(", feature_x, ")"))

    df0 <- data.frame(x = dfInput[[feature_x]], y = dfInput[[feature_y]]) %>%
        drop_na()

    vCaption <- character(0)
    if(yLog && any(df0$y <= 0)) {
        vCaption <- c(vCaption, glue::glue("{sum(df0$y <= 0)} non-positive {feature_y} values dropped for the log scale"))
        df0 <- dplyr::filter(df0, y > 0)
    }
    if("log" %in% transforms && any(df0$x <= 0)) {
        vCaption <- c(vCaption, glue::glue("{sum(df0$x <= 0)} non-positive {feature_x} values dropped for log"))
    }
    if("sqrt" %in% transforms && any(df0$x < 0)) {
        vCaption <- c(vCaption, glue::glue("{sum(df0$x < 0)} negative {feature_x} values dropped for sqrt"))
    }

    df <- dplyr::bind_rows(lapply(transforms, function(tr) {
        dfTr <- df0
        if(tr == "log") dfTr <- dplyr::filter(dfTr, x > 0)
        if(tr == "sqrt") dfTr <- dplyr::filter(dfTr, x >= 0)
        xTr <- lTransforms[[tr]](dfTr$x)
        yFit <- if(yLog) log10(dfTr$y) else dfTr$y
        tibble::tibble(transform = tr,
                       x = xTr,
                       y = dfTr$y,
                       r_squared = summary(stats::lm(yFit ~ xTr))$r.squared)
    }))

    dfLabels <- df %>%
        dplyr::distinct(transform, r_squared) %>%
        mutate(label = paste0(vLabels[transform], "\nR\u00b2 = ", sprintf("%.3f", r_squared)))

    df <- df %>%
        mutate(transform = factor(transform, levels = dfLabels$transform, labels = dfLabels$label))

    p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(color = "#00AEDB", alpha = alpha) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#D11141") +
        facet_wrap(~transform, scales = "free_x") +
        coord_cartesian(ylim = range(df$y)) +
        labs(title = glue::glue("{feature_y} v/s transforms of {feature_x}"),
             x = "",
             caption = if(length(vCaption) > 0) paste(vCaption, collapse = "; ") else NULL)

    if(yLog) {
        p <- p + scale_y_log10(name = glue::glue("{feature_y} (log scale)"))
    } else {
        p <- p + scale_y_continuous(name = feature_y)
    }
    return(p)
}


#' Share of the most common level of each categorical feature
#'
#' UNIVARIATE (categorical): one bar per categorical (factor, character or
#' logical) column with the share of rows in its most common level, sorted,
#' with a dashed line at `threshold`. Bars at or above the threshold are
#' highlighted in red: such near-constant features carry little information.
#' Each bar is labelled with its dominant level and with how many of its
#' levels have fewer than `minCount` rows. Missing values count as a level
#' called `"NA"`.
#'
#' @inheritParams eda_plot_histograms
#' @param threshold Share of the most common level at or above which a
#'   feature is highlighted.
#' @param minCount Levels with fewer rows than this are counted as rare.
#'
#' @return A ggplot object, or invisibly `NULL` with a message if `dfInput`
#'   has no categorical columns. Its data has one row per feature with the
#'   columns `dominant_level`, `share`, `n_levels` and `n_rare`.
#' @family EDA plots
#' @export
#'
#' @examples
#' eda_plot_category_dominance(ggplot2::mpg, threshold = 0.6)
#' eda_plot_category_dominance(ggplot2::diamonds)
eda_plot_category_dominance <- function(dfInput, threshold = 0.95, minCount = 10) {
    vCategorical <- names(dfInput)[vapply(dfInput, function(Z) is.factor(Z) | is.character(Z) | is.logical(Z), logical(1))]

    if(length(vCategorical) == 0) {
        message("No categorical features in dataset")
        return(invisible(NULL))
    }

    df <- dplyr::bind_rows(lapply(vCategorical, function(feature_cat) {
        vValues <- as.character(dfInput[[feature_cat]])
        vValues[is.na(vValues)] <- "NA"
        vCounts <- sort(table(vValues), decreasing = TRUE)
        tibble::tibble(feature = feature_cat,
                       dominant_level = names(vCounts)[1],
                       share = as.numeric(vCounts[1]) / sum(vCounts),
                       n_levels = length(vCounts),
                       n_rare = sum(vCounts < minCount))
    })) %>%
        mutate(dominated = factor(share >= threshold, levels = c(FALSE, TRUE)),
               label = paste0(dominant_level, " (", n_rare, " of ", n_levels, " levels < ", minCount, " rows)"),
               feature = fct_reorder(feature, share))

    p <- ggplot(df, aes(x = feature, y = share)) +
        geom_col(aes(fill = dominated)) +
        geom_hline(yintercept = threshold, linetype = "dashed") +
        geom_label(aes(label = label), hjust = -0.05, size = 2.8) +
        scale_fill_quant(palette = "heatmap", drop = FALSE, guide = "none") +
        scale_y_continuous(labels = scales::label_percent(), breaks = seq(0, 1, by = 0.25)) +
        expand_limits(y = c(0, 1.6)) +
        coord_flip() +
        labs(title = "Share of the most common level",
             subtitle = glue::glue("Highlighted: at least {scales::percent(threshold)} of rows in one level"),
             x = "",
             y = "share of rows")

    return(p)
}


#' Actual versus predicted values
#'
#' MODEL FIT: scatter plot of actual against predicted values with a dashed
#' 45-degree line, and the RMSE and \eqn{R^2} in the subtitle. \eqn{R^2} is
#' the squared correlation of actual and predicted values, as in
#' `yardstick::rsq()`. With `feature_model`, each model gets its own panel and
#' its own line in the subtitle. The `nLabel` points with the largest absolute
#' error in each panel are highlighted and labelled with `feature_id`, or
#' with their row number if `feature_id` is `NULL`.
#'
#' @inheritParams eda_plot_histograms
#' @param feature_actual Name of the column of actual values (y axis).
#' @param feature_predicted Name of the column of predicted values (x axis).
#' @param feature_model Optional name of a column identifying the model, to
#'   compare several models stacked in long format.
#' @param feature_id Optional name of a column used to label the worst
#'   predictions.
#' @param nLabel Number of points with the largest absolute error to label in
#'   each panel; `0` labels none.
#' @param yLog If `TRUE`, use a log10 y axis.
#' @param labelsScale Labels for both axes, e.g. [scales::label_dollar()]. If
#'   it is a function, it also formats the RMSE.
#'
#' @return A ggplot object.
#' @family EDA plots
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' df <- data.frame(car = rownames(mtcars), mpg = mtcars$mpg, pred = fitted(fit))
#' eda_plot_actual_vs_predicted(df, "mpg", "pred", feature_id = "car")
#'
#' fit2 <- lm(mpg ~ wt, data = mtcars)
#' dfBoth <- rbind(data.frame(df, model = "wt + hp"),
#'                 data.frame(df[1:2], pred = fitted(fit2), model = "wt"))
#' eda_plot_actual_vs_predicted(dfBoth, "mpg", "pred", feature_model = "model",
#'                              feature_id = "car", nLabel = 3)
eda_plot_actual_vs_predicted <- function(dfInput, feature_actual, feature_predicted, feature_model = NULL,
                                         feature_id = NULL, nLabel = 5, xLog = FALSE, yLog = FALSE,
                                         labelsScale = waiver()) {

    df <- data.frame(actual = dfInput[[feature_actual]],
                     predicted = dfInput[[feature_predicted]],
                     model = if(is.null(feature_model)) {"all"} else {dfInput[[feature_model]]},
                     id = if(is.null(feature_id)) {seq_len(nrow(dfInput))} else {dfInput[[feature_id]]}) %>%
        drop_na(actual, predicted) %>%
        mutate(model = if(is.factor(model)) {model} else {fct_inorder(as.character(model))},
               abs_error = abs(actual - predicted))

    dfMetrics <- df %>%
        group_by(model) %>%
        summarise(rmse = sqrt(mean((actual - predicted)^2)),
                  rsq = cor(actual, predicted)^2,
                  .groups = "drop")

    vRmse <- if(is.function(labelsScale)) {labelsScale(dfMetrics$rmse)} else {prettyNum(signif(dfMetrics$rmse, 3), big.mark = ",")}
    vMetrics <- paste0("RMSE = ", vRmse, ", R\u00b2 = ", sprintf("%.3f", dfMetrics$rsq))
    if(!is.null(feature_model)) {
        vMetrics <- paste0(dfMetrics$model, ": ", vMetrics)
    }

    dfLabel <- df %>%
        group_by(model) %>%
        dplyr::slice_max(abs_error, n = nLabel, with_ties = FALSE) %>%
        ungroup()

    vValues <- c(df$actual, df$predicted)
    if(xLog | yLog) {
        vRange <- range(vValues[vValues > 0])
        dfLine <- data.frame(v = exp(seq(log(vRange[1]), log(vRange[2]), length.out = 100)))
    } else {
        vRange <- range(vValues)
        dfLine <- data.frame(v = vRange)
    }

    p <- ggplot(df, aes(x = predicted, y = actual)) +
        geom_line(data = dfLine, aes(x = v, y = v), linetype = "dashed", color = "#D11141", linewidth = 1) +
        geom_point(color = "#00AEDB", alpha = 0.5) +
        expand_limits(x = vRange, y = vRange) +
        labs(title = glue::glue("Actual v/s predicted {feature_actual}"),
             subtitle = paste(vMetrics, collapse = "\n"))

    if(nrow(dfLabel) > 0) {
        p <- p +
            geom_point(data = dfLabel, color = "#D11141") +
            ggrepel::geom_label_repel(data = dfLabel, aes(label = id), size = 2.8,
                                      max.overlaps = Inf, seed = 1)
    }

    if(!is.null(feature_model)) {
        p <- p + facet_wrap(~model)
    }

    if(xLog) {
        p <- p + scale_x_log10(name = glue::glue("predicted {feature_actual} (log scale)"), labels = labelsScale)
    } else {
        p <- p + scale_x_continuous(name = glue::glue("predicted {feature_actual}"), labels = labelsScale)
    }

    if(yLog) {
        p <- p + scale_y_log10(name = glue::glue("actual {feature_actual} (log scale)"), labels = labelsScale)
    } else {
        p <- p + scale_y_continuous(name = glue::glue("actual {feature_actual}"), labels = labelsScale)
    }

    return(p)
}


#' Residual plots of a linear model
#'
#' MODEL FIT: the four diagnostic plots of [stats::plot.lm()] as one ggplot,
#' in the theme of the other plots instead of base graphics:
#'
#' 1. Residuals vs Fitted: a trend in the smooth suggests a missing
#'    non-linear term.
#' 2. Normal Q-Q: standardized residuals against normal quantiles, with a line
#'    through the quartiles; departures in the tails mean heavy tails or skew.
#' 3. Scale-Location: the square root of the absolute standardized residuals
#'    against fitted values; a rising smooth means the variance grows with
#'    the fit.
#' 4. Residuals vs Leverage: standardized residuals against leverage, with
#'    dashed contours where Cook's distance is 0.5 and 1; points beyond them
#'    pull the fit.
#'
#' As in [stats::plot.lm()], the smooths in the first and third panels are
#' [stats::lowess()] curves, the fitted values are on the scale of the linear
#' predictor, and the `nLabel`
#' most extreme points of each panel are labelled: by absolute residual in
#' the first three panels and by Cook's distance in the last.
#'
#' @param lmModel A model fitted with [stats::lm()], or a [stats::glm()] (its
#'   residuals are deviance residuals).
#' @param which Which of the four panels to draw, a subset of `1:4`.
#' @param nLabel Number of points to label in each panel; `0` labels none.
#' @param vLabels Optional labels for the observations, one per residual,
#'   e.g. an ID column. If `NULL`, the row names of the model data are used.
#' @param alpha Point transparency.
#' @param plotTitle Plot title. If `NULL`, "Residual plots" is used, with the
#'   model formula as subtitle.
#'
#' @return A ggplot object with one panel per plot. Its data has one row per
#'   observation and panel, with the panel's `x` and `y` values.
#' @family EDA plots
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' eda_plot_residuals(fit)
#' eda_plot_residuals(fit, which = c(1, 4), nLabel = 2)
#'
#' fitDiamonds <- lm(log(price) ~ log(carat) + cut + color + clarity,
#'                   data = ggplot2::diamonds)
#' eda_plot_residuals(fitDiamonds, alpha = 0.05, nLabel = 0)
eda_plot_residuals <- function(lmModel, which = 1:4, nLabel = 3, vLabels = NULL,
                               alpha = 0.5, plotTitle = NULL) {

    if(!inherits(lmModel, "lm")) {
        stop("`lmModel` must be a model fitted with lm() or glm()", call. = FALSE)
    }
    which <- sort(unique(which))
    if(length(which) == 0 || !all(which %in% 1:4)) {
        stop("`which` must be a subset of 1:4", call. = FALSE)
    }

    vResid <- stats::residuals(lmModel)
    vFitted <- stats::predict(lmModel)
    vStdResid <- stats::rstandard(lmModel)
    vLeverage <- stats::hatvalues(lmModel)
    vCooks <- stats::cooks.distance(lmModel)
    nObs <- length(vResid)

    if(is.null(vLabels)) {
        vLabels <- names(vResid) %||% as.character(seq_len(nObs))
    } else if(length(vLabels) != nObs) {
        stop(glue::glue("`vLabels` must have one label per residual ({nObs})"), call. = FALSE)
    }

    vPanels <- c("Residuals vs Fitted\ny: residuals, x: fitted values",
                 "Normal Q-Q\ny: standardized residuals, x: theoretical quantiles",
                 "Scale-Location\ny: sqrt(|standardized residuals|), x: fitted values",
                 "Residuals vs Leverage\ny: standardized residuals, x: leverage")

    # x, y and the ordering used to pick the points to label, per panel
    lPanels <- list(
        data.frame(x = vFitted, y = vResid, rank = abs(vResid)),
        data.frame(x = stats::qnorm(stats::ppoints(nObs))[order(order(vStdResid))], y = vStdResid,
                   rank = abs(vStdResid)),
        data.frame(x = vFitted, y = sqrt(abs(vStdResid)), rank = abs(vStdResid)),
        data.frame(x = vLeverage, y = vStdResid, rank = vCooks)
    )

    df <- dplyr::bind_rows(lapply(which, function(i) {
        lPanels[[i]] %>%
            mutate(panel = vPanels[i], id = vLabels)
    })) %>%
        dplyr::filter(is.finite(x), is.finite(y)) %>%
        mutate(panel = factor(panel, levels = vPanels[which]))

    dfLabel <- df %>%
        group_by(panel) %>%
        dplyr::slice_max(rank, n = nLabel, with_ties = FALSE) %>%
        ungroup()

    # lowess smooths, as panel.smooth() draws them
    dfSmooth <- dplyr::bind_rows(lapply(split(df, df$panel, drop = TRUE), function(dfPanel) {
        if(!as.character(dfPanel$panel[1]) %in% vPanels[c(1, 3)] || nrow(dfPanel) < 3) return(NULL)
        data.frame(panel = dfPanel$panel[1], stats::lowess(dfPanel$x, dfPanel$y, f = 2/3, iter = 3))
    }))

    p <- ggplot(df, aes(x = x, y = y))

    if(any(c(1, 4) %in% which)) {
        dfZero <- data.frame(panel = factor(vPanels[intersect(c(1, 4), which)], levels = levels(df$panel)))
        p <- p + geom_hline(data = dfZero, aes(yintercept = 0), linetype = "dotted", color = "grey50")
    }

    p <- p +
        geom_point(color = "#00AEDB", alpha = alpha) +
        facet_wrap(~panel, scales = "free", ncol = min(2, length(which))) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
        labs(title = plotTitle %||% "Residual plots",
             subtitle = if(is.null(plotTitle)) {.short_formula(lmModel)} else {NULL},
             x = "", y = "")

    if(nrow(dfSmooth) > 0) {
        p <- p + geom_line(data = dfSmooth, color = "#D11141", linewidth = 1)
    }

    if(2 %in% which) {
        vQuartiles <- stats::quantile(vStdResid[is.finite(vStdResid)], c(0.25, 0.75), names = FALSE)
        slope <- diff(vQuartiles) / diff(stats::qnorm(c(0.25, 0.75)))
        dfQQ <- data.frame(panel = factor(vPanels[2], levels = levels(df$panel)),
                           slope = slope,
                           intercept = vQuartiles[1] - slope * stats::qnorm(0.25))
        p <- p + geom_abline(data = dfQQ, aes(slope = slope, intercept = intercept),
                             linetype = "dashed", color = "#D11141", linewidth = 1)
    }

    if(4 %in% which) {
        dfCook <- .cooks_contours(vLeverage, vStdResid, lmModel$rank, vPanels[4], levels(df$panel))
        if(nrow(dfCook) > 0) {
            dfCookLabel <- dfCook %>%
                group_by(level, side) %>%
                dplyr::slice_max(x, n = 1, with_ties = FALSE) %>%
                ungroup()
            p <- p +
                geom_line(data = dfCook, aes(group = interaction(level, side)),
                          linetype = "dashed", color = "grey50") +
                geom_text(data = dfCookLabel, aes(label = level), color = "grey50", size = 2.5,
                          hjust = 1, vjust = -0.4)
        }
    }

    if(nrow(dfLabel) > 0) {
        p <- p +
            geom_point(data = dfLabel, color = "#D11141") +
            ggrepel::geom_text_repel(data = dfLabel, aes(label = id), size = 2.8,
                                     max.overlaps = Inf, seed = 1)
    }

    return(p)
}

# Lines where Cook's distance is 0.5 and 1 in the residuals vs leverage panel,
# kept within the range of the standardized residuals so they don't stretch
# the axis.
.cooks_contours <- function(vLeverage, vStdResid, nCoef, panelLabel, panelLevels) {
    vLeverage <- vLeverage[is.finite(vLeverage) & vLeverage > 0 & vLeverage < 1]
    if(length(vLeverage) == 0 || is.null(nCoef)) return(data.frame())
    yMax <- max(abs(vStdResid[is.finite(vStdResid)]))
    vH <- seq(min(vLeverage), max(vLeverage), length.out = 101)

    dfCook <- expand.grid(x = vH, level = c(0.5, 1), side = c(-1, 1)) %>%
        mutate(y = side * sqrt(level * nCoef * (1 - x) / x),
               panel = factor(panelLabel, levels = panelLevels)) %>%
        dplyr::filter(abs(y) <= yMax)
    return(dfCook)
}

# The model formula as one line, shortened if long.
.short_formula <- function(lmModel) {
    vFormula <- paste(trimws(deparse(stats::formula(lmModel))), collapse = " ")
    if(nchar(vFormula) > 90) vFormula <- paste0(substr(vFormula, 1, 87), "...")
    return(vFormula)
}
