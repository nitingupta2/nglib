fit <- lm(mpg ~ wt + hp, data = mtcars)

panel_data <- function(p, prefix) {
    p$data[startsWith(as.character(p$data$panel), prefix), ]
}

find_layer <- function(p, geom, column = NULL) {
    lLayers <- Filter(function(l) inherits(l$geom, geom) &&
                          (is.null(column) || column %in% names(l$data)), p$layers)
    if(length(lLayers) == 0) return(NULL)
    lLayers[[1]]
}

test_that("eda_plot_residuals builds", {
    expect_buildable(eda_plot_residuals(fit))
    expect_buildable(eda_plot_residuals(fit, which = 2, nLabel = 0))
    expect_buildable(eda_plot_residuals(glm(am ~ wt, data = mtcars, family = binomial)))
    expect_buildable(eda_plot_residuals(lm(Ozone ~ Wind + Temp, data = airquality)))
})

test_that("panels hold the values plot.lm() draws", {
    p <- eda_plot_residuals(fit)
    expect_equal(levels(p$data$panel)[1], "Residuals vs Fitted\ny: residuals, x: fitted values")
    expect_equal(nlevels(p$data$panel), 4)

    rs <- rstandard(fit)
    expect_equal(unname(panel_data(p, "Residuals vs Fitted")$x), unname(fitted(fit)))
    expect_equal(unname(panel_data(p, "Residuals vs Fitted")$y), unname(residuals(fit)))
    expect_equal(unname(panel_data(p, "Normal Q-Q")$x), qqnorm(rs, plot.it = FALSE)$x)
    expect_equal(unname(panel_data(p, "Normal Q-Q")$y), unname(rs))
    expect_equal(unname(panel_data(p, "Scale-Location")$y), unname(sqrt(abs(rs))))
    expect_equal(unname(panel_data(p, "Residuals vs Leverage")$x), unname(hatvalues(fit)))
})

test_that("smooths are lowess curves in the fitted-value panels", {
    p <- eda_plot_residuals(fit)
    dfSmooth <- find_layer(p, "GeomLine")$data
    expect_setequal(substr(as.character(unique(dfSmooth$panel)), 1, 14), c("Residuals vs F", "Scale-Location"))
    smooth <- lowess(fitted(fit), residuals(fit), f = 2/3, iter = 3)
    expect_equal(dfSmooth$y[startsWith(as.character(dfSmooth$panel), "Residuals vs Fitted")], smooth$y)
})

test_that("the Q-Q line goes through the quartiles", {
    p <- eda_plot_residuals(fit)
    dfQQ <- find_layer(p, "GeomAbline")$data
    rs <- rstandard(fit)
    q <- quantile(rs, c(0.25, 0.75), names = FALSE)
    expect_equal(dfQQ$intercept + dfQQ$slope * qnorm(c(0.25, 0.75)), q)
})

test_that("labels follow plot.lm(): largest residuals, then Cook's distance", {
    p <- eda_plot_residuals(fit, nLabel = 3)
    dfLabel <- find_layer(p, "GeomTextRepel", "id")$data
    labels_of <- function(prefix) dfLabel$id[startsWith(as.character(dfLabel$panel), prefix)]
    top3 <- function(v) names(v)[order(v, decreasing = TRUE)[1:3]]

    expect_equal(labels_of("Residuals vs Fitted"), top3(abs(residuals(fit))))
    expect_equal(labels_of("Normal Q-Q"), top3(abs(rstandard(fit))))
    expect_equal(labels_of("Scale-Location"), top3(abs(rstandard(fit))))
    expect_equal(labels_of("Residuals vs Leverage"), top3(cooks.distance(fit)))
    expect_true("Maserati Bora" %in% labels_of("Residuals vs Leverage"))

    p <- eda_plot_residuals(fit, nLabel = 0)
    expect_null(find_layer(p, "GeomTextRepel", "id"))
})

test_that("vLabels replace the row names", {
    p <- eda_plot_residuals(fit, which = 1, nLabel = 1, vLabels = paste0("car", 1:32))
    expect_equal(find_layer(p, "GeomTextRepel", "id")$data$id,
                 paste0("car", which.max(abs(residuals(fit)))))
    expect_error(eda_plot_residuals(fit, vLabels = 1:3), "one label per residual")
})

test_that("Cook's distance contours stay within the residual range", {
    p <- eda_plot_residuals(fit)
    dfCook <- find_layer(p, "GeomLine", "level")$data
    expect_setequal(unique(dfCook$level), c(0.5, 1))
    expect_equal(dfCook$y, dfCook$side * sqrt(dfCook$level * 3 * (1 - dfCook$x) / dfCook$x))
    expect_true(all(abs(dfCook$y) <= max(abs(rstandard(fit)))))
})

test_that("which picks panels and the title follows plotTitle", {
    p <- eda_plot_residuals(fit, which = c(4, 1))
    expect_equal(substr(levels(p$data$panel), 1, 21), c("Residuals vs Fitted\ny", "Residuals vs Leverage"))
    expect_equal(p$labels$subtitle, "mpg ~ wt + hp")
    expect_equal(p$labels$title, "Residual plots")

    p <- eda_plot_residuals(fit, plotTitle = "mtcars")
    expect_equal(p$labels$title, "mtcars")
    expect_null(p$labels$subtitle)
})

test_that("eda_plot_residuals rejects bad inputs", {
    expect_error(eda_plot_residuals(mtcars), "lm")
    expect_error(eda_plot_residuals(fit, which = 5), "subset of 1:4")
})
