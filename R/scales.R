
lPalettes <- list(
    'main' = c("#CC0000","green3","royalblue3","#3399FF","tomato1","red1","darkorchid1","darkorchid4",
               "deeppink1","deeppink4","tan3","darkgoldenrod1","goldenrod4"),
    'withgrey' = c("#999999","#CC0000","green3","royalblue3","#3399FF","tomato1","red1","darkorchid1","darkorchid4",
                   "deeppink1","deeppink4","tan3","darkgoldenrod1","goldenrod4"),
    'pnl' = c("green3","#CC0000"),
    'greyscale' = c("lightgrey","darkgrey")
)

#' Creates custom color palettes
#'
#' @param palette
#' @param reverse
#' @param ...
#'
#' @return
#'
#' @examples
quant_pal <- function(palette = c("main","withgrey","pnl","greyscale"), reverse = FALSE, ...) {
    vColors <- lPalettes[[palette[1]]]
    if (reverse) vColors <- rev(vColors)
    colorRampPalette(vColors, ...)
}

#' Sets colors in ggplot2 plots
#'
#' @param palette
#' @param discrete
#' @param reverse
#' @param ...
#'
#' @details Four color palettes are defined. By default discrete colors are chosen from the palette, otherwise a color gradient is generated
#' @return
#' @export
#'
#' @examples
scale_color_quant <- function(palette = c("main","withgrey","pnl","greyscale"), discrete = TRUE, reverse = FALSE, ...) {
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
#' @details Four color palettes are defined. If values parameter is not set, it is determined from the palette parameter
#' @return
#' @export
#'
#' @examples
scale_color_quant_manual <- function(palette = c("main","withgrey","pnl","greyscale"), values = NULL, ...) {
    if (is_null(values)) values <- lPalettes[[palette[1]]]
    scale_color_manual(..., values = values)
}

#' Fills colors in ggplot2 plots
#'
#' @param palette
#' @param discrete
#' @param reverse
#' @param ...
#'
#' @details Four color palettes are defined. By default discrete colors are chosen from the palette, otherwise a color gradient is generated
#' @return
#' @export
#'
#' @examples
scale_fill_quant <- function(palette = c("main","withgrey","pnl","greyscale"), discrete = TRUE, reverse = FALSE, ...) {
    pal <- quant_pal(palette = palette[1], reverse = reverse)

    if (discrete) {
        discrete_scale("colour", paste0("lPalette_", palette[1]), palette = pal, ...)
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
#' @details Four color palettes are defined. If values parameter is not set, it is determined from the palette parameter
#' @return
#' @export
#'
#' @examples
scale_fill_quant_manual <- function(palette = c("main","withgrey","pnl","greyscale"), values = NULL, ...) {
    if (is_null(values)) values <- lPalettes[[palette[1]]]
    scale_fill_manual(..., values = values)
}
