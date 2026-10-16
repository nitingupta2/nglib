test_that("correlation_reorder_matrix keeps a symmetric permutation", {
    m <- cor(mtcars)
    r <- correlation_reorder_matrix(m)
    expect_setequal(rownames(r), colnames(m))
    expect_equal(rownames(r), colnames(r))
    expect_equal(r, m[rownames(r), rownames(r)])
})

test_that("plot_correlations_gg ignores non-numeric columns and blanks the diagonal", {
    df <- data.frame(Date = seq(as.Date("2020-01-31"), by = "month", length.out = 24),
                     a = 1:24, b = (1:24)^2, c = sin(1:24), label = letters[1:24])
    p <- plot_correlations_gg(df)
    expect_buildable(p)
    expect_setequal(levels(p$data$Var1), c("a", "b", "c"))
    expect_true(all(is.na(p$data$CorVal[as.character(p$data$Var1) == as.character(p$data$Var2)])))
    expect_equal(p$labels$title, "Correlations Of Monthly Returns \n Jan 2020 - Dec 2021")
})

test_that("plot_correlations_gg titles and outlines", {
    expect_equal(plot_correlations_gg(mtcars)$labels$title, "Correlations")
    expect_equal(plot_correlations_gg(mtcars, plotTitle = "x")$labels$title, "x")
    expect_length(plot_correlations_gg(mtcars, outlineVar = "mpg")$layers, 5)
    expect_length(plot_correlations_gg(mtcars, outlineVar = "nope")$layers, 2)
})

test_that("plot_correlations_gg handles missing values pairwise", {
    p <- plot_correlations_gg(airquality)
    expect_buildable(p)
    off_diag <- as.character(p$data$Var1) != as.character(p$data$Var2)
    expect_false(anyNA(p$data$CorVal[off_diag]))
})
