diamonds <- ggplot2::diamonds[seq(1, 53940, by = 20), ]

test_that("eda_plot_box_by_bins builds", {
    expect_buildable(eda_plot_box_by_bins(diamonds, "clarity", "price", "carat"))
    expect_buildable(eda_plot_box_by_bins(diamonds, "clarity", "price", "carat", yLog = TRUE,
                                          yLabelsScale = scales::label_dollar()))
    expect_buildable(eda_plot_box_by_bins(ggplot2::mpg, "drv", "hwy", "displ", nBins = 3))
})

test_that("bins are quantiles labelled with their ranges", {
    p <- eda_plot_box_by_bins(diamonds, "clarity", "price", "carat")
    expect_s3_class(p$facet, "FacetWrap")
    expect_equal(nlevels(p$data$bin), 4)

    breaks <- quantile(diamonds$carat, probs = seq(0, 1, 0.25), names = FALSE)
    bins <- cut(diamonds$carat, breaks, include.lowest = TRUE, labels = FALSE)
    expect_equal(as.integer(p$data$bin), bins)

    lo <- tapply(diamonds$carat, bins, min)
    hi <- tapply(diamonds$carat, bins, max)
    expect_equal(levels(p$data$bin),
                 paste0("carat: ", signif(lo, 3), "\u2013", signif(hi, 3)))
})

test_that("tied quantiles give fewer bins", {
    df <- data.frame(g = rep(c("a", "b"), 10), y = 1:20, z = c(rep(1, 15), 2:6))
    p <- eda_plot_box_by_bins(df, "g", "y", "z", nBins = 4)
    expect_lt(nlevels(p$data$bin), 4)
    expect_buildable(p)
})

test_that("categories keep ordered factor order, else order by median", {
    p <- eda_plot_box_by_bins(diamonds, "clarity", "price", "carat")
    expect_equal(levels(p$data$x), levels(diamonds$clarity))

    mpg <- ggplot2::mpg
    p <- eda_plot_box_by_bins(mpg, "class", "hwy", "displ")
    expect_equal(levels(p$data$x), levels(forcats::fct_reorder(mpg$class, mpg$hwy, median)))
})

test_that("log scale and titles follow the inputs", {
    p <- eda_plot_box_by_bins(diamonds, "clarity", "price", "carat", yLog = TRUE)
    expect_equal(p$scales$get_scales("y")$get_transformation()$name, "log-10")
    expect_equal(p$labels$title, "price by clarity within quantile bins of carat")
})

test_that("feature_bin must be numerical", {
    expect_error(eda_plot_box_by_bins(diamonds, "clarity", "price", "cut"), "numerical")
})
