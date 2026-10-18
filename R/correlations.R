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


#' Association matrix of features of any type
#'
#' Pairwise association between every numerical and categorical column of a
#' data frame, with the measure chosen by the types of the pair:
#'
#' * numerical-numerical: Pearson or Spearman correlation, from -1 to 1.
#' * categorical-categorical: Cramer's V, from 0 to 1.
#' * categorical-numerical: the correlation ratio \eqn{\eta}, from 0 to 1,
#'   the square root of the share of the numerical feature's variance
#'   explained by the category means.
#'
#' Factor, character and logical columns are categorical, and their missing
#' values count as a level called `"NA"`, because a missing category often
#' means "none" (e.g. no pool). Missing numerical values are dropped pair by
#' pair. Other columns, such as dates, are ignored.
#'
#' Cramer's V and \eqn{\eta} grow with the number of levels even when there
#' is no association, which can make a many-level feature such as a
#' neighborhood look related to everything. With `biasCorrect = TRUE` they
#' are bias-corrected: Cramer's V as in Bergsma (2013), and \eqn{\eta} from
#' the adjusted \eqn{\eta^2} (epsilon-squared), each floored at 0.
#'
#' @param dfInput A data frame.
#' @param method Correlation method for numerical pairs, `"pearson"` or
#'   `"spearman"`.
#' @param ordinalAsNumeric If `TRUE`, ordered factors are converted to their
#'   integer codes and treated as numerical, so `method = "spearman"` suits
#'   them best. If `FALSE`, they are categorical.
#' @param biasCorrect If `TRUE`, bias-correct Cramer's V and \eqn{\eta}.
#'
#' @return A symmetric matrix with 1 on the diagonal and `NA` where an
#'   association is undefined (e.g. a constant column). Its `"measure"`
#'   attribute is a matching character matrix of the measure used for each
#'   pair: `"pearson"`, `"spearman"`, `"cramers_v"` or `"eta"`.
#' @references Bergsma, W. (2013). A bias-correction for Cramer's V and
#'   Tschuprow's T. *Journal of the Korean Statistical Society*, 42(3),
#'   323-328.
#' @seealso [eda_plot_association_matrix()]
#' @export
#'
#' @examples
#' m <- association_matrix(ggplot2::mpg)
#' round(m[c("hwy", "class", "drv"), c("hwy", "class", "drv")], 2)
#' attr(m, "measure")[c("hwy", "class", "drv"), c("hwy", "class", "drv")]
#'
#' association_matrix(ggplot2::diamonds[1:2000, ], method = "spearman",
#'                    ordinalAsNumeric = TRUE)
association_matrix <- function(dfInput, method = c("pearson", "spearman"),
                               ordinalAsNumeric = FALSE, biasCorrect = TRUE) {
    method <- match.arg(method)

    lFeatures <- lapply(dfInput, function(Z) {
        if(is.ordered(Z) && ordinalAsNumeric) {
            as.numeric(Z)
        } else if(is.numeric(Z)) {
            as.numeric(Z)
        } else if(is.factor(Z) || is.character(Z) || is.logical(Z)) {
            vValues <- as.character(Z)
            vValues[is.na(vValues)] <- "NA"
            factor(vValues)
        } else {
            NULL
        }
    })
    lFeatures <- Filter(Negate(is.null), lFeatures)

    if(length(lFeatures) < 2) {
        stop("`dfInput` needs at least two numerical or categorical columns", call. = FALSE)
    }

    vFeatures <- names(lFeatures)
    vNumeric <- vapply(lFeatures, is.numeric, logical(1))
    nFeatures <- length(vFeatures)

    mAssoc <- diag(1, nFeatures)
    mMeasure <- matrix("", nFeatures, nFeatures)
    dimnames(mAssoc) <- dimnames(mMeasure) <- list(vFeatures, vFeatures)
    diag(mMeasure) <- ifelse(vNumeric, method, "cramers_v")

    for(i in seq_len(nFeatures - 1)) {
        for(j in (i + 1):nFeatures) {
            x <- lFeatures[[i]]
            y <- lFeatures[[j]]
            if(vNumeric[i] && vNumeric[j]) {
                value <- .correlation(x, y, method)
                measure <- method
            } else if(!vNumeric[i] && !vNumeric[j]) {
                value <- .cramers_v(x, y, biasCorrect)
                measure <- "cramers_v"
            } else if(vNumeric[i]) {
                value <- .correlation_ratio(y, x, biasCorrect)
                measure <- "eta"
            } else {
                value <- .correlation_ratio(x, y, biasCorrect)
                measure <- "eta"
            }
            mAssoc[i, j] <- mAssoc[j, i] <- value
            mMeasure[i, j] <- mMeasure[j, i] <- measure
        }
    }

    attr(mAssoc, "measure") <- mMeasure
    return(mAssoc)
}

# Correlation over complete pairs, or NA if it is undefined.
.correlation <- function(x, y, method) {
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]
    y <- y[ok]
    if(length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) return(NA_real_)
    cor(x, y, method = method)
}

# Cramer's V of two factors, optionally with Bergsma's bias correction.
.cramers_v <- function(x, y, biasCorrect) {
    tab <- table(x, y)
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    n <- sum(tab)
    r <- nrow(tab)
    k <- ncol(tab)
    if(r < 2 || k < 2) return(NA_real_)

    expected <- outer(rowSums(tab), colSums(tab)) / n
    phi2 <- sum((tab - expected)^2 / expected) / n

    if(biasCorrect) {
        phi2 <- max(0, phi2 - (k - 1) * (r - 1) / (n - 1))
        r <- r - (r - 1)^2 / (n - 1)
        k <- k - (k - 1)^2 / (n - 1)
    }

    denom <- min(k - 1, r - 1)
    if(denom <= 0) return(NA_real_)
    sqrt(min(1, phi2 / denom))
}

# Correlation ratio eta of a numerical feature on a factor, optionally from
# the adjusted eta-squared (epsilon-squared).
.correlation_ratio <- function(cat, num, biasCorrect) {
    ok <- !is.na(num)
    cat <- droplevels(cat[ok])
    num <- num[ok]
    n <- length(num)
    k <- nlevels(cat)
    if(k < 2 || n <= k) return(NA_real_)

    ssTotal <- sum((num - mean(num))^2)
    if(ssTotal == 0) return(NA_real_)
    vMeans <- tapply(num, cat, mean)
    vCounts <- tabulate(as.integer(cat), nbins = k)
    eta2 <- sum(vCounts * (vMeans - mean(num))^2) / ssTotal

    if(biasCorrect) {
        eta2 <- max(0, 1 - (1 - eta2) * (n - 1) / (n - k))
    }
    sqrt(min(1, eta2))
}
