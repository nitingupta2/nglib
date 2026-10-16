#' A ggplot2 theme for posters
#'
#' Like [theme_quant()] but with darker grid lines and black axis text, for
#' plots printed and mounted on large-format posters where readability from a
#' distance matters.
#'
#' @inheritParams theme_quant
#' @param font_family Base font family. Defaults to `"Inter"`; use `""` for the
#'   device default if that font is not installed.
#'
#' @return A ggplot2 theme object.
#' @seealso [set_theme_poster()], [theme_quant()]
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(economics, aes(date, unemploy)) +
#'     geom_line(linewidth = 1) +
#'     labs(title = "US unemployment") +
#'     theme_poster(font_family = "")
theme_poster <- function(font_family = "Inter",
                         base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 12,
                         subtitle_margin = 10,
                         plot_title_size = 18,
                         plot_title_margin = 10,
                         ...) {
    base_theme <- theme_light(base_family = font_family, base_size = base_size, ...) %+replace%
        theme(panel.background = element_rect(colour = "grey70", linewidth = rel(0.5)),
              panel.grid.major = element_line(colour = "grey80", linewidth = rel(0.2)),
              panel.grid.minor = element_line(colour = "grey80", linewidth = rel(0.1)),
              legend.background = element_rect(colour = "grey70", linewidth = rel(0.5)),
              strip.background = element_rect(fill = "grey85"),
              strip.text = element_text(hjust = 0.5, size = strip_text_size, margin = margin(b = strip_text_margin)),
              plot.subtitle = element_text(hjust = 0.5, size = subtitle_size, margin = margin(b = subtitle_margin)),
              plot.title = element_text(hjust = 0.5, size = plot_title_size, margin = margin(b = plot_title_margin)),
              # Axes
              axis.title = element_text(color = "black"),
              axis.text  = element_text(color = "black", size = rel(0.9))
        )
    base_theme
}

#' Set theme_poster as the session default
#'
#' Calls [ggplot2::theme_set()] with [theme_poster()] and updates the default
#' font of text and label geoms to match.
#'
#' @inheritParams theme_poster
#'
#' @return Invisibly, the previous theme (as from [ggplot2::theme_set()]).
#' @export
#'
#' @examples
#' \dontrun{
#' set_theme_poster()
#' }
set_theme_poster <- function(font_family = "Inter", base_size = 11) {
    old <- theme_set(theme_poster(font_family, base_size))
    update_font_defaults(font_family)
    invisible(old)
}
