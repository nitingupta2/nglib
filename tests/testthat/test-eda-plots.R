mpg <- ggplot2::mpg
diamonds <- ggplot2::diamonds[seq(1, 53940, by = 20), ]

test_that("univariate plots build", {
    expect_buildable(eda_plot_histograms(mtcars, c("mpg", "wt", "disp")))
    expect_buildable(eda_plot_histograms(mtcars, "mpg", eda_plot_density = TRUE))
    expect_buildable(eda_plot_column(mpg, "class"))
    expect_buildable(eda_plot_column(diamonds, "cut"))
    expect_buildable(eda_plot_missing_counts(airquality))
    expect_buildable(eda_plot_missing_values(airquality, plotTitle = "NAs"))
    expect_buildable(eda_plot_normal_qq(mtcars, "mpg"))
})

test_that("histogram titles and log scale follow the inputs", {
    p <- eda_plot_histograms(diamonds, "price", xLog = TRUE)
    expect_equal(p$labels$title, "Distribution of price (log scale)")
    expect_equal(p$scales$get_scales("x")$get_transformation()$name, "log-10")

    p <- eda_plot_histograms(mtcars, c("mpg", "wt"), plotTitle = "Two")
    expect_equal(p$labels$title, "Two")
    expect_s3_class(p$facet, "FacetWrap")
})

test_that("eda_plot_column orders unordered categories by count", {
    p <- eda_plot_column(mpg, "class")
    expect_equal(levels(p$data$x), names(sort(table(mpg$class))))
})

test_that("eda_plot_missing_counts reports a complete dataset", {
    expect_message(res <- eda_plot_missing_counts(mtcars), "No missing values")
    expect_null(res)
})

test_that("eda_plot_missing_counts counts NAs by feature", {
    p <- eda_plot_missing_counts(airquality)
    expect_equal(as.character(p$data$feature), c("Ozone", "Solar.R"))
    expect_equal(p$data$na_count, c(37, 7))
    expect_equal(unname(p$data$feature_class), c("integer", "integer"))
})

test_that("bivariate plots build", {
    expect_buildable(eda_plot_scatter(mtcars, c("wt", "disp"), "mpg"))
    expect_buildable(eda_plot_scatter(mtcars, "wt", "mpg", add_lm = FALSE, xLog = TRUE, yLog = TRUE))
    expect_buildable(eda_plot_box(mpg, "class", "hwy"))
    expect_buildable(eda_plot_box(diamonds, "cut", "price", yLog = TRUE))
    expect_buildable(eda_plot_bar(mpg, "class", "hwy"))
    expect_buildable(suppressMessages(eda_plot_density(mpg, "drv", "hwy")))
    expect_buildable(suppressMessages(eda_plot_density(diamonds, "cut", "price", xLog = TRUE)))
    expect_buildable(eda_plot_stacked_bar(mpg, "class", "drv"))
})

test_that("eda_plot_scatter labels a single feature", {
    p <- eda_plot_scatter(mtcars, "wt", "mpg")
    expect_equal(p$labels$title, "Scatterplot of mpg v/s wt")
    expect_equal(p$labels$x, "wt")
    expect_length(p$layers, 2)
})

test_that("box and bar plots order categories by median", {
    medians <- tapply(mpg$hwy, mpg$class, median)
    expected <- levels(forcats::fct_reorder(mpg$class, mpg$hwy, median))
    expect_equal(levels(eda_plot_box(mpg, "class", "hwy")$data$x), expected)
    expect_equal(levels(eda_plot_bar(mpg, "class", "hwy")$data$x), expected)
    expect_false(is.unsorted(medians[expected]))
})

test_that("ordered factors keep their order", {
    p <- eda_plot_box(diamonds, "cut", "price")
    expect_equal(levels(p$data$x), levels(diamonds$cut))
})

test_that("stacked bar proportions sum to one per category", {
    b <- ggplot2::ggplot_build(eda_plot_stacked_bar(mpg, "class", "drv"))$data[[1]]
    sums <- tapply(b$ymax - b$ymin, b$x, sum)
    expect_true(all(abs(sums - 1) < 1e-8))
})

test_that("multivariate plots build", {
    expect_buildable(eda_plot_scatter_cat(mpg, "displ", "hwy", "drv"))
    expect_buildable(eda_plot_scatter_cat(diamonds, "carat", "price", "cut", xLog = TRUE, yLog = TRUE))
    expect_buildable(eda_plot_scatter_cat_facets(mpg, "displ", "hwy", "drv"))
    expect_buildable(eda_plot_scatter_cat_facets(mpg, "displ", "hwy", "drv", overlayAllPoints = FALSE))
    expect_buildable(eda_plot_heatmap(diamonds, "cut", "color", "price"))
    expect_buildable(eda_plot_correlation_matrix(mtcars, outlineVar = "mpg"))
})

test_that("two-level categories use the heatmap palette", {
    df <- data.frame(x = 1:4, y = c(1, 2, 3, 4), z = c("a", "a", "b", "b"))
    b <- ggplot2::ggplot_build(eda_plot_scatter_cat(df, "x", "y", "z"))$data[[1]]
    expect_setequal(unique(b$colour), get_palettes()$heatmap)
})

test_that("eda_plot_heatmap summarises medians per cell", {
    p <- eda_plot_heatmap(mpg, "drv", "year", "hwy")
    expect_equal(nrow(p$data), length(unique(paste(mpg$drv, mpg$year))))
    cell <- p$data$z[p$data$x == "f" & p$data$y == "1999"]
    expect_equal(cell, median(mpg$hwy[mpg$drv == "f" & mpg$year == 1999]))
})
