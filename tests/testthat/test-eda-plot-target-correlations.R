test_that("eda_plot_target_correlations builds", {
    expect_buildable(eda_plot_target_correlations(mtcars, "mpg"))
    expect_buildable(eda_plot_target_correlations(ggplot2::mpg, "hwy", method = "spearman"))
    expect_buildable(eda_plot_target_correlations(airquality, "Ozone", plotTitle = "Ozone"))
})

test_that("correlations match cor() and are sorted by value", {
    p <- eda_plot_target_correlations(mtcars, "mpg")
    expected <- cor(mtcars[-1], mtcars$mpg)[, 1]
    expect_equal(nrow(p$data), ncol(mtcars) - 1)
    expect_equal(p$data$correlation, unname(expected[as.character(p$data$feature)]))
    expect_equal(levels(p$data$feature), names(sort(expected)))
    expect_equal(p$labels$title, "Pearson correlation with mpg")
})

test_that("spearman correlations use ranks", {
    p <- eda_plot_target_correlations(mtcars, "mpg", method = "spearman")
    expected <- cor(mtcars[-1], mtcars$mpg, method = "spearman")[, 1]
    expect_equal(p$data$correlation, unname(expected[as.character(p$data$feature)]))
    expect_equal(p$labels$title, "Spearman correlation with mpg")
})

test_that("correlations use pairwise-complete observations", {
    p <- eda_plot_target_correlations(airquality, "Ozone")
    solar <- p$data$correlation[p$data$feature == "Solar.R"]
    expect_equal(solar, cor(airquality$Ozone, airquality$Solar.R, use = "complete.obs"))
})

test_that("bars are colored by sign with the heatmap palette", {
    b <- ggplot2::ggplot_build(eda_plot_target_correlations(mtcars, "mpg"))$data[[1]]
    vHeatmap <- get_palettes()$heatmap
    expect_true(all(b$fill[b$y > 0] == vHeatmap[1]))
    expect_true(all(b$fill[b$y < 0] == vHeatmap[2]))

    # a single sign still gets its own color
    b <- ggplot2::ggplot_build(eda_plot_target_correlations(mtcars, "mpg", nTop = 2))$data[[1]]
    expect_true(all(b$y < 0))
    expect_true(all(b$fill == vHeatmap[2]))
})

test_that("minAbsCor and nTop filter features", {
    p <- eda_plot_target_correlations(mtcars, "mpg", minAbsCor = 0.8)
    expect_setequal(as.character(p$data$feature), c("wt", "cyl", "disp"))

    p <- eda_plot_target_correlations(mtcars, "mpg", nTop = 4)
    expect_setequal(as.character(p$data$feature), c("wt", "cyl", "disp", "hp"))
})

test_that("eda_plot_target_correlations handles bad inputs", {
    expect_error(eda_plot_target_correlations(ggplot2::mpg, "class"), "numerical")
    expect_error(eda_plot_target_correlations(data.frame(y = 1:3, z = letters[1:3]), "y"), "no other")
    expect_message(res <- eda_plot_target_correlations(mtcars, "mpg", minAbsCor = 0.99), "No features")
    expect_null(res)
})
