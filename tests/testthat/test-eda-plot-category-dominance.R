mpg <- ggplot2::mpg

test_that("eda_plot_category_dominance builds", {
    expect_buildable(eda_plot_category_dominance(mpg))
    expect_buildable(eda_plot_category_dominance(ggplot2::diamonds, threshold = 0.5))
})

test_that("shares, levels and rare counts are correct", {
    p <- eda_plot_category_dominance(mpg, minCount = 10)
    vCategorical <- c("manufacturer", "model", "trans", "drv", "fl", "class")
    expect_setequal(as.character(p$data$feature), vCategorical)

    for(feature in vCategorical) {
        counts <- sort(table(mpg[[feature]]), decreasing = TRUE)
        row <- p$data[p$data$feature == feature, ]
        expect_equal(row$dominant_level, names(counts)[1])
        expect_equal(row$share, as.numeric(counts[1]) / nrow(mpg))
        expect_equal(row$n_levels, length(counts))
        expect_equal(row$n_rare, sum(counts < 10))
    }

    fl <- p$data[p$data$feature == "fl", ]
    expect_equal(fl$share, 168 / 234)
    expect_equal(fl$label, "r (3 of 5 levels < 10 rows)")
})

test_that("features are sorted by share and highlighted at the threshold", {
    p <- eda_plot_category_dominance(mpg, threshold = 0.6)
    expect_equal(levels(p$data$feature), as.character(p$data$feature[order(p$data$share)]))
    expect_equal(as.character(p$data$feature[p$data$dominated == "TRUE"]), "fl")

    b <- ggplot2::ggplot_build(p)$data[[1]]
    vHeatmap <- get_palettes()$heatmap
    expect_equal(sum(b$fill == vHeatmap[2]), 1)
    expect_equal(sum(b$fill == vHeatmap[1]), 5)
    expect_equal(ggplot2::ggplot_build(p)$data[[2]]$yintercept, 0.6)
})

test_that("logical columns and missing values count as levels", {
    df <- data.frame(flag = c(rep(TRUE, 19), FALSE),
                     grp = c(rep(NA, 12), rep("a", 8)),
                     num = 1:20)
    p <- eda_plot_category_dominance(df, threshold = 0.9, minCount = 5)
    expect_setequal(as.character(p$data$feature), c("flag", "grp"))

    flag <- p$data[p$data$feature == "flag", ]
    expect_equal(flag$dominant_level, "TRUE")
    expect_equal(flag$share, 0.95)
    expect_equal(flag$n_rare, 1)
    expect_equal(as.character(flag$dominated), "TRUE")

    grp <- p$data[p$data$feature == "grp", ]
    expect_equal(grp$dominant_level, "NA")
    expect_equal(grp$share, 0.6)
    expect_equal(grp$n_levels, 2)
})

test_that("eda_plot_category_dominance reports a dataset without categories", {
    expect_message(res <- eda_plot_category_dominance(mtcars), "No categorical")
    expect_null(res)
})
