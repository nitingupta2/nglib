
#' Updates fonts
#'
#' @return
#'
#' @examples
update_font_defaults <- function() {
    update_geom_defaults("text", list(family = "Roboto Condensed"))
    update_geom_defaults("label", list(family = "Roboto Condensed"))
    # update_geom_defaults("text", list(family = "RobotoCondensed-Regular"))
    # update_geom_defaults("label", list(family = "RobotoCondensed-Regular"))
}

#' Defines a ggplot2 custom theme
#'
#' @param base_size
#' @param strip_text_size
#' @param strip_text_margin
#' @param subtitle_size
#' @param subtitle_margin
#' @param plot_title_size
#' @param plot_title_margin
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_quant <- function(base_size = 11,
                        strip_text_size = 12,
                        strip_text_margin = 5,
                        subtitle_size = 12,
                        subtitle_margin = 10,
                        plot_title_size = 18,
                        plot_title_margin = 10,
                        ...) {
    base_theme <- theme_light(base_family = "Roboto Condensed", base_size = base_size, ...) %+replace%
        theme(panel.background = element_rect(colour = "lightgrey", size = rel(0.5)),
              panel.grid.major = element_line(colour = "lightgrey", size = rel(0.2)),
              panel.grid.minor = element_line(colour = "lightgrey", size = rel(0.1)),
              legend.background = element_rect(colour = "lightgrey", size = rel(0.5)),
              strip.background = element_rect(fill = "lightgrey"),
              strip.text = element_text(hjust = 0.5, size = strip_text_size, margin = margin(b=strip_text_margin)),
              plot.subtitle = element_text(hjust = 0, size = subtitle_size, margin = margin(b=subtitle_margin)),
              plot.title = element_text(hjust = 0.5, size = plot_title_size, margin = margin(b=plot_title_margin)))
    base_theme
}

#' Sets theme_quant
#'
#' @param base_size
#'
#' @return
#' @export
#'
#' @examples
set_theme_quant <- function(base_size = 11) {
    theme_set(theme_quant(base_size))
    update_font_defaults()
}


#' Defines a custom highcharter theme
#'
#' @return
#' @export
#'
#' @examples
hc_theme_quant <- hc_theme_merge(hc_theme_smpl(),
                                 hc_theme(title = list(align = "center", style = list(fontSize = 18))))
