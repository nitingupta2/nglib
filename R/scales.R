
#' Returns a list of named color palettes
#'
#' @return
#' @export
#'
#' @examples
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

#' Creates custom color palettes
#'
#' @param palette
#' @param reverse
#' @param ...
#'
#' @return
#'
#' @examples
quant_pal <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                      reverse = FALSE, ...) {
    vColors <- get_palettes()[[palette[1]]]
    if (reverse) vColors <- rev(vColors)
    colorRampPalette(vColors, ...)
}


#' Test a color palette using a pie chart
#'
#' @param palette
#' @param reverse
#' @param n
#'
#' @return
#' @export
#'
#' @examples
test_pal <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                     reverse = FALSE,
                     n = 5) {
    vColors <- quant_pal(palette[1], reverse)(n)
    pie(rep(1, n), col = vColors)
}


#' Get a vector of colors from a palette
#'
#' @param palette
#' @param reverse
#' @param n
#'
#' @return
#' @export
#'
#' @examples
getPlotColors <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                          reverse = FALSE,
                          n) {
    vColors <- get_palettes()[[palette[1]]]
    if (reverse) vColors <- rev(vColors)

    if(n > length(vColors)) {
        vColors <- colorRampPalette(vColors)(n)
    } else {
        vColors <- gplots::col2hex(vColors[1:n])
    }
    return(vColors)
}


#' Sets colors in ggplot2 plots
#'
#' @param palette
#' @param discrete
#' @param reverse
#' @param ...
#'
#' @details Seven color palettes are defined. By default discrete colors are chosen from the palette, otherwise a color gradient is generated
#' @return
#' @export
#'
#' @examples
scale_color_quant <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                              discrete = TRUE, reverse = FALSE, ...) {
    pal <- quant_pal(palette = palette[1], reverse = reverse)

    if (discrete) {
        discrete_scale("colour", paste0("lPalette_", palette[1]), palette = pal, ...)
    } else {
        scale_color_gradientn(colours = pal(256), ...)
    }
}

#' Set manual color values
#'
#' @param palette
#' @param values
#' @param ...
#'
#' @details Seven color palettes are defined. If values parameter is not set, it is determined from the palette parameter
#' @return
#' @export
#'
#' @examples
scale_color_quant_manual <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                                     values = NULL, ...) {
    if (is.null(values)) values <- get_palettes()[[palette[1]]]
    scale_color_manual(..., values = values)
}

#' Fills colors in ggplot2 plots
#'
#' @param palette
#' @param discrete
#' @param reverse
#' @param ...
#'
#' @details Seven color palettes are defined. By default discrete colors are chosen from the palette, otherwise a color gradient is generated
#' @return
#' @export
#'
#' @examples
scale_fill_quant <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                             discrete = TRUE, reverse = FALSE, ...) {
    pal <- quant_pal(palette = palette[1], reverse = reverse)

    if (discrete) {
        discrete_scale("fill", paste0("lPalette_", palette[1]), palette = pal, ...)
    } else {
        scale_fill_gradientn(colours = pal(256), ...)
    }
}

#' Set manual fill values
#'
#' @param palette
#' @param values
#' @param ...
#'
#' @details Seven color palettes are defined. If values parameter is not set, it is determined from the palette parameter
#' @return
#' @export
#'
#' @examples
scale_fill_quant_manual <- function(palette = c("main","withgrey","pnl","weights","metro","greyscale","heatmap"),
                                    values = NULL, ...) {
    if (is.null(values)) values <- get_palettes()[[palette[1]]]
    scale_fill_manual(..., values = values)
}
