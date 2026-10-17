diamonds <- ggplot2::diamonds[seq(1, 53940, by = 20), ]

r_squared_of <- function(p, transform) {
    unique(p$data$r_squared[startsWith(as.character(p$data$transform), transform)])
}

test_that("eda_plot_transforms builds", {
    expect_buildable(eda_plot_transforms(diamonds, "carat", "price"))
    expect_buildable(eda_plot_transforms(mtcars, "hp", "mpg", transforms = c("identity", "log"), yLog = FALSE))
})

test_that("R-squared per transform matches lm()", {
    p <- eda_plot_transforms(diamonds, "carat", "price")
    rsq <- function(f) summary(lm(f, data = diamonds))$r.squared
    expect_equal(r_squared_of(p, "carat\n"), rsq(log10(price) ~ carat))
    expect_equal(r_squared_of(p, "log("), rsq(log10(price) ~ log(carat)))
    expect_equal(r_squared_of(p, "sqrt("), rsq(log10(price) ~ sqrt(carat)))
    expect_equal(r_squared_of(p, "cuberoot("), rsq(log10(price) ~ I(carat^(1/3))))

    # log(carat) is the most linear with log(price)
    expect_gt(r_squared_of(p, "log("), r_squared_of(p, "cuberoot("))
})

test_that("facets are labelled with R-squared in transform order", {
    p <- eda_plot_transforms(mtcars, "hp", "mpg", transforms = c("log", "identity"), yLog = FALSE)
    rsqLog <- summary(lm(mpg ~ log(hp), data = mtcars))$r.squared
    expect_equal(levels(p$data$transform)[1], sprintf("log(hp)\nR\u00b2 = %.3f", rsqLog))
    expect_true(startsWith(levels(p$data$transform)[2], "hp\n"))
    expect_equal(r_squared_of(p, "hp\n"), summary(lm(mpg ~ hp, data = mtcars))$r.squared)
    expect_null(p$labels$caption)
})

test_that("non-positive values are dropped and counted in the caption", {
    df <- data.frame(x = c(-8, 0, 1, 2, 3, 4), y = c(1, 2, 3, 4, 5, 0))
    p <- eda_plot_transforms(df, "x", "y")
    expect_equal(p$labels$caption,
                 paste("1 non-positive y values dropped for the log scale",
                       "2 non-positive x values dropped for log",
                       "1 negative x values dropped for sqrt", sep = "; "))
    nRows <- table(sub("\n.*", "", p$data$transform))
    expect_equal(as.vector(nRows[c("x", "log(x)", "sqrt(x)", "cuberoot(x)")]), c(5, 3, 4, 5))
    expect_equal(sort(p$data$x[startsWith(as.character(p$data$transform), "cuberoot")]),
                 c(-2, 0, 1, 2^(1/3), 3^(1/3)))
})

test_that("yLog sets the y scale", {
    p <- eda_plot_transforms(diamonds, "carat", "price")
    expect_equal(p$scales$get_scales("y")$get_transformation()$name, "log-10")
    p <- eda_plot_transforms(diamonds, "carat", "price", yLog = FALSE)
    expect_equal(p$scales$get_scales("y")$get_transformation()$name, "identity")
})

test_that("eda_plot_transforms rejects bad inputs", {
    expect_error(eda_plot_transforms(diamonds, "cut", "price"), "numerical")
    expect_error(eda_plot_transforms(diamonds, "carat", "price", transforms = "square"))
})
