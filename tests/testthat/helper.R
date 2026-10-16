# Build a plot fully (stats, scales, facets) without drawing it, so errors that
# only surface at render time are caught.
expect_buildable <- function(p) {
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
    invisible(p)
}
