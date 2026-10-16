test_that("find_mode returns the most frequent value", {
    expect_equal(find_mode(c(1, 2, 2, 3)), 2)
    expect_equal(find_mode(c("b", "a", "a", "b", "c")), "b")
    expect_equal(find_most_common_value(factor(c("x", "y", "y"))), factor("y", levels = c("x", "y")))
})

test_that("%notin% negates %in%", {
    expect_equal(c("a", "z") %notin% letters[1:3], c(FALSE, TRUE))
})

test_that("change_na_to replaces NAs", {
    expect_equal(change_na_to(c(1, NA, 3), 0), c(1, 0, 3))
    expect_equal(change_na_to_most_common_value(c("a", NA, "b", "b")), c("a", "b", "b", "b"))
})

test_that("grouped summaries", {
    df <- data.frame(g = c("a", "a", "b"), v = c(1, 3, 10), w = c("x", "x", "y"))
    expect_equal(median_by_grouping(df, "g", "v")$v, c(2, 10))
    expect_equal(most_common_value_by_grouping(df, "g", "w")$w, c("x", "y"))
})

test_that("number helpers", {
    expect_equal(significant_digits(pi, 3), "3.142")
    expect_equal(pct_diff_lagging(c(100, 110, 99)), c(NA, 0.1, -0.1))
    expect_equal(pct_diff_leading(c(110, 100)), c(0.1, NA))
    expect_equal(pct_diff_lagging(c(100, 110, 121), k = 2), c(NA, NA, 0.21))
    expect_identical(nearest_multiple(c(12, 12.5, 17, 25), 5), c(10L, 15L, 15L, 25L))
})
