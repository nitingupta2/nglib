# Font family of the current session theme, so functions that add theme_quant()
# keep the font chosen by set_theme_quant()/set_theme_poster() and otherwise
# fall back to the device default rather than a font that may not be installed.
.session_font_family <- function() {
    theme_get()$text$family %||% ""
}

#' Update default fonts of text geoms
#'
#' Sets the default font family of [ggplot2::geom_text()] and
#' [ggplot2::geom_label()] so annotations match the plot theme.
#'
#' @param font_family Font family name. The font must be installed on the
#'   system and visible to the graphics device in use.
#'
#' @return Called for its side effect; invisibly returns `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' update_font_defaults("Roboto Condensed")
#' }
update_font_defaults <- function(font_family = "Roboto Condensed") {
    update_geom_defaults("text", list(family = font_family))
    update_geom_defaults("label", list(family = font_family))
    invisible(NULL)
}

#' A clean ggplot2 theme for analysis plots
#'
#' A light theme built on [ggplot2::theme_light()] with light grey panel
#' borders and grid lines, grey facet strips and centered titles.
#'
#' @param font_family Base font family. Defaults to `"Roboto Condensed"`;
#'   use `""` for the device default if that font is not installed.
#' @param base_size Base font size in points.
#' @param strip_text_size Facet strip text size.
#' @param strip_text_margin Margin below facet strip text.
#' @param subtitle_size Plot subtitle size.
#' @param subtitle_margin Margin below the subtitle.
#' @param plot_title_size Plot title size.
#' @param plot_title_margin Margin below the title.
#' @param ... Further arguments passed to [ggplot2::theme_light()].
#'
#' @return A ggplot2 theme object.
#' @seealso [set_theme_quant()], [theme_poster()]
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "Fuel economy vs weight") +
#'     theme_quant(font_family = "")
theme_quant <- function(font_family = "Roboto Condensed",
                        base_size = 11,
                        strip_text_size = 12,
                        strip_text_margin = 5,
                        subtitle_size = 12,
                        subtitle_margin = 10,
                        plot_title_size = 18,
                        plot_title_margin = 10,
                        ...) {
    base_theme <- theme_light(base_family = font_family, base_size = base_size, ...) %+replace%
        theme(panel.background = element_rect(colour = "lightgrey", linewidth = rel(0.5)),
              panel.grid.major = element_line(colour = "lightgrey", linewidth = rel(0.2)),
              panel.grid.minor = element_line(colour = "lightgrey", linewidth = rel(0.1)),
              legend.background = element_rect(colour = "lightgrey", linewidth = rel(0.5)),
              strip.background = element_rect(fill = "lightgrey"),
              strip.text = element_text(hjust = 0.5, size = strip_text_size, margin = margin(b = strip_text_margin)),
              plot.subtitle = element_text(hjust = 0.5, size = subtitle_size, margin = margin(b = subtitle_margin)),
              plot.title = element_text(hjust = 0.5, size = plot_title_size, margin = margin(b = plot_title_margin)))
    base_theme
}

#' Set theme_quant as the session default
#'
#' Calls [ggplot2::theme_set()] with [theme_quant()] and updates the default
#' font of text and label geoms to match.
#'
#' @inheritParams theme_quant
#'
#' @return Invisibly, the previous theme (as from [ggplot2::theme_set()]).
#' @export
#'
#' @examples
#' \dontrun{
#' set_theme_quant()
#' }
set_theme_quant <- function(font_family = "Roboto Condensed", base_size = 11) {
    old <- theme_set(theme_quant(font_family, base_size))
    update_font_defaults(font_family)
    invisible(old)
}
