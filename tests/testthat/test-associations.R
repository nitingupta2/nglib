mpg <- ggplot2::mpg
diamonds <- ggplot2::diamonds[seq(1, 53940, by = 20), ]

test_that("association_matrix is symmetric with the right measures", {
    m <- association_matrix(mpg)
    expect_equal(dim(m), c(ncol(mpg), ncol(mpg)))
    expect_equal(unname(diag(m)), rep(1, ncol(mpg)))
    expect_equal(m, t(m))

    measure <- attr(m, "measure")
    expect_equal(measure["hwy", "cty"], "pearson")
    expect_equal(measure["class", "drv"], "cramers_v")
    expect_equal(measure["hwy", "class"], "eta")
    expect_equal(measure["class", "hwy"], "eta")
    expect_true(all(m[measure != "pearson"] >= 0, na.rm = TRUE))
})

test_that("numerical pairs match cor()", {
    m <- association_matrix(mtcars)
    expect_equal(unclass(m)[1:11, 1:11], cor(mtcars), ignore_attr = TRUE)

    m <- association_matrix(mtcars, method = "spearman")
    expect_equal(m["mpg", "wt"], cor(mtcars$mpg, mtcars$wt, method = "spearman"))

    m <- association_matrix(airquality)
    expect_equal(m["Ozone", "Solar.R"], cor(airquality$Ozone, airquality$Solar.R, use = "complete.obs"))
})

test_that("Cramer's V matches chi-squared, and |r| for two binary features", {
    m <- association_matrix(mpg, biasCorrect = FALSE)
    tab <- table(mpg$class, mpg$drv)
    chi2 <- suppressWarnings(chisq.test(tab, correct = FALSE))$statistic
    expect_equal(m["class", "drv"], unname(sqrt(chi2 / (nrow(mpg) * (min(dim(tab)) - 1)))))

    df <- data.frame(a = mtcars$am == 1, b = factor(mtcars$vs))
    m <- association_matrix(df, biasCorrect = FALSE)
    expect_equal(m["a", "b"], abs(cor(mtcars$am, mtcars$vs)))
})

test_that("bias-corrected Cramer's V follows Bergsma (2013)", {
    m <- association_matrix(mpg)
    tab <- table(mpg$class, mpg$drv)
    n <- sum(tab)
    r <- nrow(tab)
    k <- ncol(tab)
    phi2 <- suppressWarnings(chisq.test(tab, correct = FALSE))$statistic / n
    phi2 <- max(0, phi2 - (k - 1) * (r - 1) / (n - 1))
    rc <- r - (r - 1)^2 / (n - 1)
    kc <- k - (k - 1)^2 / (n - 1)
    expect_equal(m["class", "drv"], unname(sqrt(phi2 / min(rc - 1, kc - 1))))
    expect_lt(m["class", "drv"], association_matrix(mpg, biasCorrect = FALSE)["class", "drv"])
})

test_that("the correlation ratio matches aov(), and |r| for two groups", {
    m <- association_matrix(mpg, biasCorrect = FALSE)
    ss <- summary(aov(hwy ~ class, data = mpg))[[1]][["Sum Sq"]]
    expect_equal(m["hwy", "class"], sqrt(ss[1] / sum(ss)))

    df <- data.frame(mpg = mtcars$mpg, am = factor(mtcars$am))
    m <- association_matrix(df, biasCorrect = FALSE)
    expect_equal(m["mpg", "am"], abs(cor(mtcars$mpg, mtcars$am)))

    m <- association_matrix(mpg)
    n <- nrow(mpg)
    k <- length(unique(mpg$class))
    expect_equal(m["hwy", "class"], sqrt(1 - (1 - ss[1] / sum(ss)) * (n - 1) / (n - k)))
})

test_that("bias correction removes spurious association of many-level features", {
    set.seed(1)
    df <- data.frame(id = sample(letters, 200, replace = TRUE),
                     grp = sample(LETTERS[1:20], 200, replace = TRUE),
                     y = rnorm(200))
    raw <- association_matrix(df, biasCorrect = FALSE)
    corrected <- association_matrix(df)
    expect_gt(raw["id", "y"], 0.25)
    expect_gt(raw["id", "grp"], 0.25)
    expect_lt(corrected["id", "y"], 0.1)
    expect_lt(corrected["id", "grp"], 0.1)
})

test_that("ordered factors are categorical unless ordinalAsNumeric", {
    m <- association_matrix(diamonds[c("cut", "price")])
    expect_equal(attr(m, "measure")["cut", "price"], "eta")

    m <- association_matrix(diamonds[c("cut", "price")], method = "spearman", ordinalAsNumeric = TRUE)
    expect_equal(attr(m, "measure")["cut", "price"], "spearman")
    expect_equal(m["cut", "price"], cor(as.integer(diamonds$cut), diamonds$price, method = "spearman"))
})

test_that("missing categories are a level and other columns are ignored", {
    df <- data.frame(pool = c(rep(NA, 8), rep("yes", 4)),
                     area = c(rep(1, 8), rep(5, 4)),
                     when = as.Date("2020-01-01") + 0:11,
                     stringsAsFactors = FALSE)
    m <- association_matrix(df, biasCorrect = FALSE)
    expect_equal(rownames(m), c("pool", "area"))
    expect_equal(m["pool", "area"], 1)
})

test_that("undefined associations are NA and too few columns is an error", {
    df <- data.frame(a = 1:5, const = 1, g = "x")
    m <- association_matrix(df)
    expect_true(is.na(m["a", "const"]))
    expect_true(is.na(m["a", "g"]))
    expect_error(association_matrix(data.frame(a = 1:3)), "at least two")
})

test_that("eda_plot_association_matrix builds", {
    expect_buildable(eda_plot_association_matrix(mpg, outlineVar = "hwy"))
    expect_buildable(eda_plot_association_matrix(diamonds, method = "spearman", ordinalAsNumeric = TRUE))
    expect_buildable(eda_plot_association_matrix(mtcars, showValues = FALSE))
})

test_that("the plot data holds every pair, clustered, with a blank diagonal", {
    p <- eda_plot_association_matrix(mpg)
    m <- association_matrix(mpg)
    expect_equal(nrow(p$data), ncol(mpg)^2)
    expect_setequal(levels(p$data$Var1), colnames(mpg))
    expect_equal(levels(p$data$Var2), rev(levels(p$data$Var1)))
    expect_true(all(is.na(p$data$AssocVal[p$data$Var1 == p$data$Var2])))

    pair <- p$data[p$data$Var1 == "class" & p$data$Var2 == "hwy", ]
    expect_equal(pair$AssocVal, m["class", "hwy"])
    expect_equal(pair$measure, "eta")

    # redundant features sit next to each other
    expect_equal(abs(diff(match(c("cty", "hwy"), levels(p$data$Var1)))), 1)
})

test_that("subtitle lists the measures used", {
    p <- eda_plot_association_matrix(mpg)
    expect_equal(p$labels$subtitle,
                 paste0("Numerical: Pearson r; Categorical: Cram\u00e9r's V; Mixed: correlation ratio",
                        "\n(V and correlation ratio bias-corrected)"))

    p <- eda_plot_association_matrix(mtcars, method = "spearman")
    expect_equal(p$labels$subtitle, "Numerical: Spearman rho")
})

test_that("values are shown for small matrices and the outline is drawn", {
    p <- eda_plot_association_matrix(mpg, outlineVar = "hwy")
    expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
    expect_equal(sum(vapply(p$layers, function(l) class(l$geom)[1] == "GeomRect", logical(1))), 2)

    dfWide <- as.data.frame(matrix(rnorm(30 * 26), ncol = 26))
    p <- eda_plot_association_matrix(dfWide)
    expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})
