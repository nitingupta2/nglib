test_that("themes are complete ggplot2 themes with the requested font", {
    for (th in list(theme_quant(font_family = ""), theme_poster(font_family = ""))) {
        expect_s3_class(th, "theme")
        expect_true(attr(th, "complete"))
    }
    expect_equal(theme_quant()$text$family, "Roboto Condensed")
    expect_equal(theme_poster()$text$family, "Inter")
    expect_equal(theme_quant(base_size = 14)$text$size, 14)
    expect_equal(theme_poster()$axis.text$colour, "black")
})

test_that("EDA plots that add theme_quant() use the session theme's font", {
    old <- ggplot2::theme_set(ggplot2::theme_grey())
    on.exit(ggplot2::theme_set(old))
    expect_equal(eda_plot_correlation_matrix(mtcars)$theme$text$family, "")

    ggplot2::theme_set(theme_quant(font_family = "serif"))
    expect_equal(eda_plot_correlation_matrix(mtcars)$theme$text$family, "serif")
    expect_equal(eda_plot_missing_values(airquality)$theme$text$family, "serif")
})

test_that("set_theme_* set the session theme and return the previous one", {
    old <- ggplot2::theme_get()
    on.exit({
        ggplot2::theme_set(old)
        ggplot2::update_geom_defaults("text", list(family = ""))
        ggplot2::update_geom_defaults("label", list(family = ""))
    })

    set_theme_quant(font_family = "serif")
    expect_equal(ggplot2::theme_get()$text$family, "serif")

    prev <- set_theme_poster(font_family = "mono")
    expect_equal(prev$text$family, "serif")
    expect_equal(ggplot2::theme_get()$text$family, "mono")
})
