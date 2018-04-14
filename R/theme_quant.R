
update_font_defaults <- function(base_family = "") {
    ggplot2::update_geom_defaults("text", list(family = base_family))
    ggplot2::update_geom_defaults("label", list(family = base_family))
}

#' Defines a ggplot2 custom theme
#'
#' @param base_size
#' @param base_family
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_quant <- function(base_size = 11, base_family = "", ...) {
    ggplot2::theme_light(base_size, base_family, ...)
}

#' Sets theme_quant
#'
#' @param base_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
set_theme_quant <- function(base_size = 13, base_family = "") {
    ggplot2::theme_set(theme_quant(base_size, base_family))
    update_font_defaults(base_family)
}
