dfCars <- tibble::rownames_to_column(mtcars, "car")

label_data <- function(p) {
    lLabels <- Filter(function(l) inherits(l$geom, "GeomLabel"), p$layers)
    if(length(lLabels) == 0) return(NULL)
    lLabels[[1]]$data
}

# the n most influential points above 4 / n, as eda_plot_scatter() picks them
top_influence <- function(fit, n, ids, labelBy = "leverage") {
    v <- if(labelBy == "leverage") hatvalues(fit) else cooks.distance(fit)
    keep <- v > 4 / length(v)
    ids[keep][order(v[keep], decreasing = TRUE)][seq_len(min(n, sum(keep)))]
}

test_that("flag_outliers uses median +/- nIQR * IQR", {
    expect_equal(flag_outliers(c(1:10, 50)), c(rep(FALSE, 10), TRUE))

    price <- ggplot2::diamonds$price
    expect_equal(flag_outliers(price), abs(price - median(price)) > 2.5 * IQR(price))
    expect_equal(flag_outliers(price, nIQR = 4), abs(price - median(price)) > 4 * IQR(price))
    expect_gt(sum(flag_outliers(price)), sum(flag_outliers(price, nIQR = 4)))

    expect_equal(rownames(mtcars)[flag_outliers(mtcars$hp)], "Maserati Bora")
})

test_that("flag_outliers keeps NAs and handles a zero IQR", {
    expect_equal(flag_outliers(c(NA, 1:10, 50)), c(NA, rep(FALSE, 10), TRUE))
    expect_equal(flag_outliers(c(rep(0, 10), 1)), c(rep(FALSE, 10), TRUE))
})

test_that("eda_plot_scatter labels nothing by default", {
    p <- eda_plot_scatter(dfCars, "hp", "mpg")
    expect_null(label_data(p))
    expect_length(p$layers, 2)
})

test_that("leverage labels match hatvalues()", {
    p <- eda_plot_scatter(dfCars, "hp", "mpg", nLabel = 3, feature_id = "car")
    expected <- top_influence(lm(mpg ~ hp, data = mtcars), 3, rownames(mtcars))
    expect_equal(label_data(p)$id, expected)
    expect_true("Maserati Bora" %in% label_data(p)$id)
    expect_buildable(p)

    # row numbers without feature_id
    p <- eda_plot_scatter(mtcars, "hp", "mpg", nLabel = 1)
    expect_equal(label_data(p)$id, which(rownames(mtcars) == "Maserati Bora"))
})

test_that("an isolated x is labelled and evenly spread points are not", {
    set.seed(1)
    df <- data.frame(x = c(1:20, 60), y = 2 * c(1:20, 60) + rnorm(21))
    p <- eda_plot_scatter(df, "x", "y", nLabel = 5)
    expect_equal(label_data(p)$id, 21)

    df <- data.frame(x = 1:20, y = 2 * (1:20) + rnorm(20))
    expect_null(label_data(eda_plot_scatter(df, "x", "y", nLabel = 5)))
})

test_that("Cook's distance labels each panel", {
    p <- eda_plot_scatter(dfCars, c("hp", "wt"), "mpg", nLabel = 2, feature_id = "car", labelBy = "cooks")
    dfLabel <- label_data(p)
    for(feature in c("hp", "wt")) {
        fit <- lm(reformulate(feature, "mpg"), data = mtcars)
        expect_equal(dfLabel$id[dfLabel$feature == feature],
                     top_influence(fit, 2, rownames(mtcars), "cooks"))
    }
    expect_buildable(p)
})

test_that("influence is computed on log scales when the axes are logged", {
    p <- eda_plot_scatter(dfCars, "hp", "mpg", nLabel = 2, feature_id = "car", xLog = TRUE, yLog = TRUE,
                          labelBy = "cooks")
    fit <- lm(log10(mpg) ~ log10(hp), data = mtcars)
    expect_equal(label_data(p)$id, top_influence(fit, 2, rownames(mtcars), "cooks"))
    expect_buildable(p)
})

test_that("labelBy is validated", {
    expect_error(eda_plot_scatter(mtcars, "hp", "mpg", nLabel = 1, labelBy = "residual"))
})
