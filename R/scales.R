.palette_names <- c("main", "withgrey", "pnl", "weights", "metro", "greyscale", "heatmap")

#' Named color palettes
#'
#' Returns the list of named color palettes used by the `*_quant` scales:
#'
#' * `main`: 14 saturated colors for many series.
#' * `withgrey`: `main` with a leading grey, e.g. for a benchmark series.
#' * `pnl`: green and red for gains and losses.
#' * `weights`: four colors for portfolio weights.
#' * `metro`: five bright "metro" colors, the default for EDA plots.
#' * `greyscale`: light and dark grey.
#' * `heatmap`: blue to red, for continuous gradients and two-level factors.
#'
#' @return A named list of character vectors of colors.
#' @export
#'
#' @examples
#' names(get_palettes())
#' get_palettes()$metro
get_palettes <- function() {
    lPalettes <- list(
        'main' = c("#CC0000","royalblue3","green3","#3399FF","darkorchid1","darkorchid4","tomato1","red1",
                   "deeppink1","deeppink4","tan3","goldenrod4","darkgoldenrod1","yellow1"),
        'withgrey' = c("#999999","#CC0000","royalblue3","green3","#3399FF","darkorchid1","darkorchid4","tomato1","red1",
                       "deeppink1","deeppink4","tan3","goldenrod4","darkgoldenrod1","yellow1"),
        'pnl' = c("green3","#CC0000"),
        'weights' = c("#CC0000","royalblue3","green3","yellow1"),
        'metro' = c("#D11141","#00B159","#00AEDB","#F37735","#FFC425"),
        'greyscale' = c("lightgrey","darkgrey"),
        'heatmap' = c("#00AEDB", "#D11141")
    )
    lPalettes
}

#' Color palette generator
#'
#' Creates a function that interpolates `n` colors from one of the named
#' palettes in [get_palettes()].
#'
#' @param palette Name of the palette, one of `"main"`, `"withgrey"`, `"pnl"`,
#'   `"weights"`, `"metro"`, `"greyscale"` or `"heatmap"`.
#' @param reverse If `TRUE`, reverse the order of the palette colors.
#' @param ... Further arguments passed to [grDevices::colorRampPalette()].
#'
#' @return A function that takes an integer `n` and returns `n` colors.
#' @export
#'
#' @examples
#' quant_pal("metro")(3)
#' quant_pal("heatmap", reverse = TRUE)(5)
quant_pal <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                      reverse = FALSE, ...) {
    palette <- match.arg(palette, .palette_names)
    vColors <- get_palettes()[[palette]]
    if (reverse) vColors <- rev(vColors)
    colorRampPalette(vColors, ...)
}


#' Preview a color palette as a pie chart
#'
#' @inheritParams quant_pal
#' @param n Number of colors to draw from the palette.
#'
#' @return Called for its side effect of drawing a pie chart; invisibly
#'   returns `NULL`.
#' @export
#'
#' @examples
#' test_pal("metro")
#' test_pal("main", n = 14)
test_pal <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                     reverse = FALSE,
                     n = 5) {
    vColors <- quant_pal(palette, reverse)(n)
    pie(rep(1, n), col = vColors)
}


#' Get a vector of colors from a palette
#'
#' Returns the first `n` colors of a palette as hex codes. If `n` is larger
#' than the palette, colors are interpolated instead.
#'
#' @inheritParams quant_pal
#' @param n Number of colors to return.
#'
#' @return A character vector of `n` hex color codes.
#' @export
#'
#' @examples
#' plot_colors("metro", n = 3)
#' plot_colors("pnl", n = 6)
plot_colors <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                        reverse = FALSE,
                        n) {
    palette <- match.arg(palette, .palette_names)
    vColors <- get_palettes()[[palette]]
    if (reverse) vColors <- rev(vColors)

    if(n > length(vColors)) {
        vColors <- colorRampPalette(vColors)(n)
    } else {
        vColors <- rgb(t(col2rgb(vColors[1:n])), maxColorValue = 255)
    }
    return(vColors)
}


#' Convert a font size in points to millimetres
#'
#' ggplot2 specifies text size in geoms such as [ggplot2::geom_text()] in
#' millimetres, while theme text sizes are in points. Use this to make an
#' annotation the same size as theme text.
#'
#' @param x Numeric vector of sizes in points.
#'
#' @return Numeric vector of sizes in millimetres.
#' @export
#'
#' @examples
#' convert_mm_to_pts(11)
convert_mm_to_pts <- function(x) {
    # grid's "pt" is 1/72.27 inch
    x * 25.4 / 72.27
}



#' Color and fill scales from the named palettes
#'
#' Discrete or continuous ggplot2 color/fill scales using one of the palettes
#' in [get_palettes()]. Discrete scales draw (and interpolate if needed) as
#' many colors as there are levels; continuous scales use a 256-step gradient
#' across the palette.
#'
#' @inheritParams quant_pal
#' @param discrete If `TRUE` (default) return a discrete scale, otherwise a
#'   continuous gradient scale.
#' @param ... Further arguments passed to [ggplot2::discrete_scale()] or, if
#'   `discrete = FALSE`, to [ggplot2::scale_color_gradientn()] /
#'   [ggplot2::scale_fill_gradientn()]. Typical ones are `name` and `guide`.
#'
#' @return A ggplot2 scale.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'     geom_point() +
#'     scale_color_quant(palette = "metro")
#'
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'     geom_raster() +
#'     scale_fill_quant(palette = "heatmap", discrete = FALSE)
scale_color_quant <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                              discrete = TRUE, reverse = FALSE, ...) {
    pal <- quant_pal(palette = palette, reverse = reverse)

    if (discrete) {
        discrete_scale("colour", palette = pal, ...)
    } else {
        scale_color_gradientn(colours = pal(256), ...)
    }
}

#' @rdname scale_color_quant
#' @export
scale_colour_quant <- scale_color_quant

#' @rdname scale_color_quant
#' @export
scale_fill_quant <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                             discrete = TRUE, reverse = FALSE, ...) {
    pal <- quant_pal(palette = palette, reverse = reverse)

    if (discrete) {
        discrete_scale("fill", palette = pal, ...)
    } else {
        scale_fill_gradientn(colours = pal(256), ...)
    }
}

#' Manual color and fill scales from the named palettes
#'
#' Wrappers around [ggplot2::scale_color_manual()] and
#' [ggplot2::scale_fill_manual()] that take their `values` from a named
#' palette unless `values` is given. Colors are assigned to levels in palette
#' order without interpolation.
#'
#' @inheritParams quant_pal
#' @param values Optional vector of colors. If `NULL`, the colors of `palette`
#'   are used.
#' @param ... Further arguments passed to [ggplot2::scale_color_manual()] or
#'   [ggplot2::scale_fill_manual()].
#'
#' @return A ggplot2 scale.
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(day = 1:6, pnl = c(2, -1, 3, -2, 1, 4))
#' ggplot(df, aes(day, pnl, fill = pnl > 0)) +
#'     geom_col() +
#'     scale_fill_quant_manual(palette = "pnl", values = c(`TRUE` = "green3", `FALSE` = "#CC0000"))
scale_color_quant_manual <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                                     values = NULL, ...) {
    palette <- match.arg(palette, .palette_names)
    if (is.null(values)) values <- get_palettes()[[palette]]
    scale_color_manual(..., values = values)
}

#' @rdname scale_color_quant_manual
#' @export
scale_colour_quant_manual <- scale_color_quant_manual

#' @rdname scale_color_quant_manual
#' @export
scale_fill_quant_manual <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                                    values = NULL, ...) {
    palette <- match.arg(palette, .palette_names)
    if (is.null(values)) values <- get_palettes()[[palette]]
    scale_fill_manual(..., values = values)
}
