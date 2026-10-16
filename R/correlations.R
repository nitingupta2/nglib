#' Reorder a correlation matrix by hierarchical clustering
#'
#' Clusters variables using the distance `(1 - r) / 2` so that strongly
#' correlated variables end up next to each other.
#'
#' @param mCor A square correlation matrix.
#'
#' @return The correlation matrix with rows and columns reordered.
#' @export
#'
#' @examples
#' correlation_reorder_matrix(cor(mtcars[, 1:5]))
correlation_reorder_matrix <- function(mCor) {
    dd <- as.dist((1-mCor)/2)
    hc <- hclust(dd)
    mCor <- mCor[hc$order, hc$order]
    return(mCor)
}


#' Clustered correlation heat map
#'
#' Heat map of pairwise correlations between the numerical columns of a data
#' frame, ordered by [correlation_reorder_matrix()], with the diagonal blank.
#' Non-numerical columns (including a `Date` column) are ignored, and
#' correlations use pairwise-complete observations.
#'
#' @param dfReturns A data frame, typically of returns with a `Date` column
#'   followed by one numerical column per series.
#' @param returnFrequency Return frequency, used only in the default title.
#' @param outlineVar Optional name of a variable whose row and column are
#'   outlined.
#' @param plotTitle Plot title. If `NULL`, the title is
#'   "Correlations Of <Frequency> Returns" followed by the date range when
#'   `dfReturns` has a `Date` column, or "Correlations" otherwise.
#'
#' @return A ggplot object.
#' @seealso [eda_plot_correlation_matrix()]
#' @export
#'
#' @examples
#' plot_correlations_gg(mtcars, plotTitle = "mtcars correlations", outlineVar = "mpg")
#'
#' set.seed(1)
#' returns <- data.frame(
#'     Date = seq(as.Date("2020-01-31"), by = "month", length.out = 36),
#'     A = rnorm(36), B = rnorm(36), C = rnorm(36)
#' )
#' returns$D <- returns$A + rnorm(36, sd = 0.5)
#' plot_correlations_gg(returns)
plot_correlations_gg <- function(dfReturns, returnFrequency = c("monthly", "daily", "weekly"), outlineVar = NA, plotTitle = NULL) {

    if(is.null(plotTitle)) {
        if("Date" %in% names(dfReturns)) {
            firstPerfDate <- as.Date(dplyr::first(dfReturns$Date))
            lastPerfDate <- as.Date(dplyr::last(dfReturns$Date))
            plotTitle <- stringr::str_to_title(glue::glue("Correlations of {returnFrequency[1]} Returns"))
            plotTitle <- paste(plotTitle, "\n", format(firstPerfDate,"%b %Y"), "-", format(lastPerfDate,"%b %Y"))
        } else {
            plotTitle <- "Correlations"
        }
    }

    df <- dfReturns[vapply(dfReturns, is.numeric, logical(1))]

    mCor <- cor(df, use = "pairwise.complete.obs") %>%
        round(digits = 2) %>%
        correlation_reorder_matrix()

    diag(mCor) <- NA

    dfCor <- mCor %>%
        tibble::as_tibble(rownames = "Var1") %>%
        pivot_longer(-Var1, names_to = "Var2", values_to = "CorVal") %>%
        mutate(Var1 = fct_inorder(Var1),
               Var1 = fct_rev(Var1),
               Var2 = factor(Var2, levels = levels(Var1)),
               Var2 = fct_rev(Var2))

    p <- ggplot(dfCor, aes(x = Var1, y = Var2, fill = CorVal)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(name = NULL, low = "#D11141", mid = "#F8F5F5", high = "#00AEDB", midpoint = 0, limits = c(-1, 1)) +
        labs(title = plotTitle, x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        theme(plot.title=element_text(hjust = 0.5, vjust = 1, size=12)) +
        coord_fixed() +
        geom_text(aes(Var1, Var2, label = ifelse(is.na(CorVal), "", CorVal)), color = "black", size = 3)

    if(!is.na(outlineVar) && outlineVar %in% as.character(dfCor$Var1)) {
        dfOutline <- dfCor %>% dplyr::filter(Var1 == outlineVar)

        p <- p + geom_tile(data = dfOutline, aes(x = Var2, y = Var1, group = Var1), color = "black", linewidth = 0.75) +
            geom_tile(data = dfOutline, aes(x = Var1, y = Var2, group = Var2), color = "black", linewidth = 0.75) +
            geom_text(aes(Var1, Var2, label = ifelse(is.na(CorVal), "", CorVal)), color = "black", size = 3)
    }

    return(p)
}
