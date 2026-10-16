#' Most frequent value
#'
#' Returns the mode of a vector: the value with the highest frequency. Ties
#' resolve to the value that appears first.
#'
#' @param x A numeric, character or factor vector.
#'
#' @return A single value of the same type as `x`.
#' @export
#'
#' @examples
#' find_mode(c(1, 2, 2, 3))
#' find_mode(c("b", "a", "a", "b", "c"))
find_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#' @rdname find_mode
#' @export
find_most_common_value <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#' Value matching: not in
#'
#' The negation of `%in%`.
#'
#' @param x Vector of values to look up.
#' @param y Vector of values to match against.
#'
#' @return A logical vector the length of `x`, `TRUE` where `x` is not in `y`.
#' @export
#'
#' @examples
#' c("a", "b", "z") %notin% letters[1:3]
`%notin%` <- function(x, y) !(`%in%`(x, y))

#' Replace missing values
#'
#' `change_na_to()` replaces `NA`s with a given value.
#' `change_na_to_most_common_value()` replaces them with the most common
#' value of `x` and returns a character vector.
#'
#' @param x A vector.
#' @param value Replacement value. Ignored by
#'   `change_na_to_most_common_value()`.
#'
#' @return A vector the length of `x` with no `NA`s (unless every value is
#'   `NA`).
#' @export
#'
#' @examples
#' change_na_to(c(1, NA, 3), 0)
#' change_na_to_most_common_value(c("a", NA, "b", "b"))
change_na_to <- function(x, value) {
    ifelse(is.na(x), value, x)
}

#' @rdname change_na_to
#' @export
change_na_to_most_common_value <- function(x, value) {
    ifelse(is.na(x), find_most_common_value(x) %>% as.character(), x)
}

#' Summaries by group
#'
#' `median_by_grouping()` returns the median of each `varsMedian` column, and
#' `most_common_value_by_grouping()` the most common value of each `varsMode`
#' column, for every combination of the `varsGrouping` columns.
#'
#' @param dfInput A data frame.
#' @param varsGrouping Character vector of grouping column names.
#' @param varsMedian Character vector of numerical column names.
#' @param varsMode Character vector of column names.
#'
#' @return A data frame with one row per group.
#' @export
#'
#' @examples
#' median_by_grouping(ggplot2::mpg, "class", c("hwy", "cty"))
#' most_common_value_by_grouping(ggplot2::mpg, "class", c("manufacturer", "drv"))
median_by_grouping <- function(dfInput, varsGrouping, varsMedian) {
    dfSummary <- dfInput %>%
        select(all_of(c(varsGrouping, varsMedian))) %>%
        group_by(across(all_of(varsGrouping))) %>%
        summarise(across(all_of(varsMedian), ~ median(.x, na.rm = TRUE)), .groups = "drop_last")

    return(dfSummary)
}

#' @rdname median_by_grouping
#' @export
most_common_value_by_grouping <- function(dfInput, varsGrouping, varsMode) {
    dfSummary <- dfInput %>%
        select(all_of(c(varsGrouping, varsMode))) %>%
        group_by(across(all_of(varsGrouping))) %>%
        summarise(across(all_of(varsMode), find_mode), .groups = "drop_last")

    return(dfSummary)
}

#' Format numbers to a fixed number of decimals
#'
#' @param x A numeric vector.
#' @param d Number of decimal places.
#'
#' @return A character vector with `d` decimals and no scientific notation.
#' @export
#'
#' @examples
#' significant_digits(c(pi, 1e-10), d = 4)
significant_digits <- function(x, d = 8) {
    format(round(x, d), nsmall = d, scientific = FALSE)
}

#' k-period percent differences
#'
#' `pct_diff_lagging()` returns `x[t] / x[t - k] - 1`, like
#' `quantmod::Delt()`. `pct_diff_leading()` returns `x[t] / x[t + k] - 1`.
#'
#' @param x A numeric vector.
#' @param k Number of periods.
#'
#' @return A numeric vector the length of `x`, with `NA` where there is no
#'   value `k` periods away.
#' @export
#'
#' @examples
#' prices <- c(100, 110, 99, 120)
#' pct_diff_lagging(prices)
#' pct_diff_leading(prices)
pct_diff_lagging <- function(x, k = 1) {
    return(x/dplyr::lag(x, n = k) - 1)
}

#' @rdname pct_diff_lagging
#' @export
pct_diff_leading <- function(x, k = 1) {
    return(x/dplyr::lead(x, n = k) - 1)
}

#' Round to the nearest multiple
#'
#' @param rawNumber A numeric vector.
#' @param m The multiple to round to. Halves round up.
#'
#' @return An integer vector.
#' @export
#'
#' @examples
#' nearest_multiple(c(12, 17, 25), 5)
nearest_multiple <- function(rawNumber, m) {
    remainder <- rawNumber %% m
    nearestNumber <- ifelse((remainder + 1e-10) > (m/2), rawNumber + m - remainder, rawNumber - remainder)
    return(as.integer(nearestNumber))
}
